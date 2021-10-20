(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml
open Js
open Globals

let main_div : Html.element t option ref = ref None 

let get_main_div () : Html.element t =
  match !main_div with
  | Some div -> div
  | None -> assert false

let load_div : Html.element t = 
  let div = document##createElement (js "div") in
  div##setAttribute (js "id") (js "load_div");
  div

let observer : IntersectionObserver.intersectionObserver t=
  let open IntersectionObserver in
  let f : intersectionObserverEntry t js_array t -> intersectionObserver t -> unit = 
    (fun entries _ ->
        let entry = unoptdef @@ array_get entries 0 in
        if entry##.isIntersecting = _true then begin
          Dom.removeChild (get_main_div ()) load_div;
          Lwt.async (fun () -> 
            let%lwt added = Requests.sendRequest () in
            if added then begin
              state.last_id <- state.last_id + 50;
              Dom.appendChild (get_main_div ()) load_div
            end;
            Lwt.return_unit)
        end;
    )
  and options : intersectionObserverOptions t = empty_intersection_observer_options () in
  let a : float js_array t= new%js array_empty  in
  array_set a 0 1.0;
  options##.threshold := a;
  new%js intersectionObserver (wrap_callback f) options

let clear_page () =
  Dom.removeChild (get_main_div ()) load_div;
  for index = 48 to 57 do
    let set_opt = get_element_by_id_opt ("packages-" ^ (fromCharCode index)) in
    Opt.iter set_opt (fun set -> 
      set##.innerHTML := js "";
      let title = getElementById ("name-" ^ (fromCharCode index)) in
      title##.style##.display := js "none")
  done;
  for index = 97 to 122 do
    let set_opt = get_element_by_id_opt ("packages-" ^ (fromCharCode index)) in
    Opt.iter set_opt (fun set -> 
      set##.innerHTML := js "";
      let title = getElementById ("name-" ^ (fromCharCode index)) in
      title##.style##.display := js "none")
  done

let update_entries_number () = 
  let entry = match filename with
    | "packages.html" -> "packages"
    | "modules.html" -> "modules"
    | "libraries.html" -> "libraries"
    | "metas.html" -> "metas"
    | "sources.html" -> "sources"
    | _ -> assert false
  in
    send_generic_request
      ~request:(Requests.getEntriesNumber entry)
      ~callback:(fun number ->
        let indicator =  unopt @@ document##getElementById  (js "item-number") in
        indicator##.innerHTML := js (number ^ " " ^ entry);
        Lwt.return_unit
      )
      ()
    
let update_page () =
  send_generic_request
    ~request:Requests.sendRequest 
    ~callback:(fun added ->
      valid_input ();
      if added then begin
        state.last_id <- state.last_id + 50;
        Dom.appendChild (get_main_div ()) load_div
      end;
      let%lwt () = update_entries_number () in
      Headfoot.footerHandler ();
      Lwt.return_unit
    )
    ~error:(fun err ->
      begin 
        match err with
        | InvalidRegex -> invalid_input ()
        | _ -> ()
      end;
      Lwt.return_unit
    )
    ()

let set_start_letter ch =
  state.starts_with <- ch;
  state.last_id <- 0;
  clear_page ();
  update_page ()


let set_onclick_handlers () =
  let onclick_handler ch _ =
    Lwt.async (fun () -> set_start_letter ch);
    _false
  in 
    let a = getElementById "all-letters" in 
    a##.onclick := Html.handler (onclick_handler ".");
    for index = 48 to 57 do
      let ch = fromCharCode index in
      let id = "letter-" ^ ch in
      match Opt.to_option @@ get_element_by_id_opt id with
      | Some a -> a##.onclick := Html.handler (onclick_handler ch)
      | None -> ()  
    done;
    for index = 97 to 122 do
      let ch = fromCharCode index in
      let id = "letter-" ^ ch in
      match Opt.to_option @@ get_element_by_id_opt id with
      | Some a -> a##.onclick := Html.handler (onclick_handler ch)
      | None -> () 
    done

let set_search_handler () = 
  let search = unopt @@ Html.CoerceTo.input @@ getElementById "search" in
  search##.onkeyup := Html.handler (fun _ ->
    let re = search##.value in
    clear_page ();
    let input = re##trim in
    begin  
      if input##.length > 0
      then state.pattern <- to_string input
      else state.pattern <- "~empty~";
    end;
    state.last_id <- 0;
    Lwt.async update_page;
    _false)

let initialise () =
  main_div := Some (getElementById "by-name");
  for index = 48 to 57 do
    let title_opt = document##getElementById (js @@ "name-" ^ fromCharCode index) in
    Opt.iter title_opt (fun title -> 
      title##.style##.display := js "none")
  done;
  for index = 97 to 122 do
    let title_opt = document##getElementById (js @@ "name-" ^ fromCharCode index) in
    Opt.iter title_opt (fun title -> 
      title##.style##.display := js "none")
  done;
  send_generic_request
    ~request:Requests.sendRequest
    ~callback:(fun _ ->
      let%lwt () = update_entries_number () in
      state.last_id <- state.last_id + 50;
      Dom.appendChild (get_main_div ()) load_div;
      let selected_elt = unopt @@ document##querySelector (js "#load_div") in
      Observer.observer##observe selected_elt;
      Lwt.return_unit)
    () 

let onload () = 
  set_search_handler ();
  set_onclick_handlers (); 
  initialise ()

