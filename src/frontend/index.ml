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

(** Module [Index] defines behaviour for index pages (packages.html,libraries.html, etc.).
    Index page is constructed dynamically by sending requests to API server. By default, shows 
    all corresponding entries that exist in the current switch. Page is loaded gradually, every time
    you scroll down to the bottom of the page - it sends request to get more 50 entries that are 
    inserted to the end of [root_div]. It is done by an object [intersection observer] that observes a
    specific marker [load_div] and if it becomes visible on the page - send request to server.
    Index page gives possibility to filter some entries by search input and by letter navigation bar.
    Each key/click produces event handled by page in order to send new request. *)


(** Root element where new entries will be loaded *)

let load_div : Html.element t = 
  let div = document##createElement (js "div") in
  div##setAttribute (js "id") (js "load_div");
  div
(** Marker element *)

let enable_marker () = 
  let root_div = get_element_by_id "by-name" in
  Dom.appendChild root_div load_div
(* Activates marker on the page *)

let disable_marker () =
  let root_div = get_element_by_id "by-name" 
  and load_div_opt = get_element_by_id_opt "load_div" in
  Opt.iter load_div_opt (fun _ -> Dom.removeChild root_div load_div)  
(* Desactivates marker on the page if it exists *)

let observer : IntersectionObserver.intersectionObserver t =
  let open IntersectionObserver in
  let f : intersectionObserverEntry t js_array t -> intersectionObserver t -> unit = 
    (* Callback called when marker becomes visible on the page *)
    (fun entries _ ->
        let entry = unoptdef @@ array_get entries 0 in
        if entry##.isIntersecting = _true then begin
          (* Send requests *)
          Lwt.async @@ 
            Requests.send_generic_request
              ~request:(Requests.getEntries entry_state)
              ~callback:(fun entries ->
                if not @@ Utils.empty_entries entries 
                then begin
                  (* insert request results in the page  *)
                  Insertion.insert_index entries;
                  (* update state *)
                  entry_state.last_id <- entry_state.last_id + 50;
                end;
                Lwt.return_unit
              )
        end;
    )
  and options : intersectionObserverOptions t = empty_intersection_observer_options () in
  let a : float js_array t= new%js array_empty  in
  array_set a 0 1.0;
  options##.threshold := a;
  new%js intersectionObserver (wrap_callback f) options
(** Intersection observer js object *)

let current_entry () =
  let open Data_types in
  match filename with
  | "packages.html" -> PACK
  | "libraries.html" -> LIB
  | "modules.html" -> MOD
  | "metas.html" -> META
  | "sources.html" -> SRC
  | _ -> 
    raise @@ 
      web_app_error 
        (Printf.sprintf "current_entry: Not recognised index page : %s" filename)
(** Returns entry type by looking up the current filename *)

let clear_page () =
  (* Remove entries from '0' to '9' *)
  for index = 48 to 57 do
    let set_opt = get_element_by_id_opt ("packages-" ^ (from_char_code index)) in
    Opt.iter set_opt (fun set -> 
      (* Remove entries *)
      set##.innerHTML := js "";
      (* Remove header *)
      let title = get_element_by_id ("name-" ^ (from_char_code index)) in
      title##.style##.display := js "none")
  done;
  (* Remove entries from 'a' to 'z' *)
  for index = 97 to 122 do
    let set_opt = get_element_by_id_opt ("packages-" ^ (from_char_code index)) in
    Opt.iter set_opt (fun set ->
      (* Remove entries *)
      set##.innerHTML := js "";
      (* Remove header *)
      let title = get_element_by_id ("name-" ^ (from_char_code index)) in
      title##.style##.display := js "none")
  done
(** Clear index page by removing all entries line and headers *)

let update_entries_indicator number =
  let indicator =  get_element_by_id "item-number" 
  and entry = Utils.entry_type_to_string entry_state.entry in
  indicator##.innerHTML := js (number ^ " " ^ entry)
(** Updates entries indicator with the number specified in argument. *)  
      
let update_entries_number () = 
  Requests.send_generic_request
      ~request:(Requests.getNumber @@ Entry entry_state)
      ~callback:(fun number ->
        update_entries_indicator number;
        Lwt.return_unit
      )
      ()
(* Sends request to get entries number and updates indicator *)

let update_page () =
  Requests.send_generic_request
    ~request:(Requests.getEntries entry_state) 
    ~callback:(fun entries ->
      (* White input *)
      valid_input "search";
      if not @@ Utils.empty_entries entries 
      then begin
        (* insert results *)
        Insertion.insert_index entries;
        (* update state *)
        entry_state.last_id <- entry_state.last_id + 50;
        enable_marker ();
      end;
      (* update number indicator *)
      Lwt.async update_entries_number;
      (* adjust footer *)
      Headfoot.footerHandler ();
      Lwt.return_unit
    )
    ~error:(fun err ->
      begin
        (* If occured error is Invalid_regex - color input to red *) 
        match err with
        | Invalid_regex -> 
          invalid_input "search"
        | _ -> ()
      end;
      (* Set entries indicator to 0 *)
      update_entries_indicator "0";
      Headfoot.footerHandler ();
      Lwt.return_unit
    )
    ()
(** Sends request to get new range of entries according to [entry_state] and updates page. *)

let set_start_letter ch =
  entry_state.starts_with <- "^" ^ ch;
  entry_state.last_id <- 0;
  disable_marker ();
  clear_page ();
  update_page ()
(** Updates state with new first letter [ch] and updates page according to updated state *)

let set_onclick_handlers () =
  (* handler that is called when onclick event is generated by one of the letter from navigatoin bar *)
  let onclick_handler ch _ =
    Lwt.async (fun () -> set_start_letter ch);
    _false
  in
    (* set handler for ALL *)
    let a = get_element_by_id "all-letters" in 
    a##.onclick := Html.handler (onclick_handler ".");
    (* set handler for every letter from '0' to '9' *)
    for index = 48 to 57 do
      let ch = from_char_code index in
      let id = "letter-" ^ ch in
      match Opt.to_option @@ get_element_by_id_opt id with
      | Some a -> a##.onclick := Html.handler (onclick_handler ch)
      | None -> ()  
    done;
    (* set handler for every letter from 'a' to 'z' *)
    for index = 97 to 122 do
      let ch = from_char_code index in
      let id = "letter-" ^ ch in
      match Opt.to_option @@ get_element_by_id_opt id with
      | Some a -> a##.onclick := Html.handler (onclick_handler ch)
      | None -> () 
    done
(** Sets onclick handlers that will be called when user click on one of the letters from navigation bar *)

let set_search_handler () = 
  let search = unopt @@ Html.CoerceTo.input @@ get_element_by_id "search" in
  (* Handler called when keyup event is generated by search input *)
  search##.onkeyup := Html.handler (fun _ ->
    (* Clear page *)
    disable_marker ();
    clear_page ();
    (* State update *)
    let re = search##.value in
    let input = re##trim in
    (* Encode pattern to make it possible to use it as path segement while making request to server *)
    entry_state.pattern <- to_string input;
    entry_state.last_id <- 0;
    (* Update page *)
    Lwt.async update_page;
    _false)
(** Sets onkeyup handler that will be called when user enters key in search input *)

let initialise () =
  clear_page ();
  entry_state.entry <- current_entry ();
  Requests.send_generic_request
    ~request:(Requests.getEntries entry_state)
    ~callback:(fun entries ->
      (* insert first entries *)
      if not @@ Utils.empty_entries entries
      then begin
        (* insert first entries *)
        Insertion.insert_index entries;
      end;  
      (* update state *)
      entry_state.last_id <- entry_state.last_id + 50;
      enable_marker ();
      (* update number indicator *)
      Lwt.async update_entries_number;
      (* Observe marker intersection *)
      let selected_elt = unopt @@ document##querySelector (js "#load_div") in
      observer##observe selected_elt;
      Lwt.return_unit)
    () 
(* Initialises page with first request and introduices marker that will be observed by intersection observer. *)

let onload () =
  (* set search input handler *)
  set_search_handler ();
  (* set handlers for letter navigation bar *)
  set_onclick_handlers ();
  (* initialise page *)
  initialise ()
(* Onload handler for index page *)

