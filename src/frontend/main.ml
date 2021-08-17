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
open Global


let clear_index_page () =
  let load_opt = document##getElementById (js "load_div") in
  Opt.iter load_opt (fun _ -> Dom.removeChild (get_main_div ()) load_div);
 
  for index = 48 to 57 do
    let set_opt = document##getElementById (js @@ "packages-" ^ (fromCharCode index)) in
    Opt.iter set_opt (fun set -> 
      set##.innerHTML := js "";
      let title = unopt @@ document##getElementById (js @@ "name-" ^ (fromCharCode index)) in
      title##.style##.display := js "none")
  done;
  for index = 97 to 122 do
    let set_opt = document##getElementById (js @@ "packages-" ^ (fromCharCode index)) in
    Opt.iter set_opt (fun set -> 
      set##.innerHTML := js "";
      let title = unopt @@ document##getElementById (js @@ "name-" ^ (fromCharCode index)) in
      title##.style##.display := js "none")
  done

let update_index_page () =
  let%lwt added = Requests.sendRequest () in
  if added then begin
    state.last_id <- state.last_id + 50;
    Dom.appendChild (get_main_div ()) load_div
  end;
  let%lwt () = Requests.getEntriesNumber () in
  Headfoot.footerHandler ();
  Lwt.return_unit

let set_start_letter ch =
  state.starts_with <- ch;
  state.last_id <- 0;
  clear_index_page ();
  update_index_page ()

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
      match Opt.to_option @@ document##getElementById (js id) with
      | Some a -> a##.onclick := Html.handler (onclick_handler ch)
      | None -> ()  
    done;
    for index = 97 to 122 do
      let ch = fromCharCode index in
      let id = "letter-" ^ ch in
      match Opt.to_option @@ document##getElementById (js id) with
      | Some a -> a##.onclick := Html.handler (onclick_handler ch)
      | None -> () 
    done

let set_search_handler () = 
  let search = unopt @@ Html.CoerceTo.input @@ getElementById "search" in
  search##.onkeyup := Html.handler (fun _ ->
    let re = search##.value in
    if in_root_directory && not (filename = "about.html") 
    then begin
      clear_index_page ();
      let input = re##trim in
      begin  
        if input##.length > 0
        then state.pattern <- to_string input
        else state.pattern <- "~";
      end;
      state.last_id <- 0;
      Lwt.async update_index_page
    end;
    _false)

let onload_main () = 
  let%lwt () = Requests.api_host () in 
  Headfoot.activate_bar ();
  Headfoot.footerHandler ();
  if in_root_directory && not (filename = "about.html") 
  then begin
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
    Lwt.async (fun () -> 
      let%lwt _ = Requests.sendRequest () in
      let%lwt () = Requests.getEntriesNumber () in
      state.last_id <- state.last_id + 50;
      Dom.appendChild (get_main_div ()) load_div;
      let selected_elt = unopt @@ document##querySelector (js "#load_div") in
      Observer.observer##observe selected_elt;
      Lwt.return_unit);
  end;
  Lwt.return_unit
    

let onload _ =
  set_search_handler ();
  set_onclick_handlers ();
  Lwt.async onload_main;
  _false

(* TODO: different onload in function to the type of page : index, doc, source *)
let main () =
  let footHandler = 
    Html.handler (fun _ -> 
      Headfoot.activate_bar ();
      Headfoot.footerHandler ();
      _false) in
  window##.onresize := footHandler;
  if is_index_page 
  then window##.onload := Html.handler onload
  else window##.onload := footHandler
  
let () = main ()
