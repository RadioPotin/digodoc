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

module StringSet = Set.Make(String)

type state_info = {
    mutable pattern : string;
    mutable entries : StringSet.t;
    mutable current_entry : string;
    mutable page : int
}

type search_state = 
    | Uninitialized
    | Search of state_info

let state = ref Uninitialized

let get_first_entry entries =
    if StringSet.mem "packages" entries
    then "packages"
    else if StringSet.mem "libraries" entries
    then "libraries"
    else if StringSet.mem "modules" entries
    then "modules"
    else if StringSet.mem "metas" entries
    then "metas"
    else "sources"

let state_of_args args =
    let state =  { pattern = ""; entries = StringSet.empty; current_entry = ""; page = 1 } in
    List.iter (fun (key,elt) ->
            match key with
            | "pattern" -> state.pattern <- elt
            | "entry" -> state.entries <- StringSet.add elt state.entries
            | "current" -> state.current_entry <- elt
            | "page" -> state.page <- int_of_string elt
            | _ -> assert false
        )
        args;
    Search state

let state_to_args ?st () =
    let state = match st with None -> !state | Some s -> s in
    match state with
    | Uninitialized -> ""
    | Search {pattern; entries; current_entry; page} ->
        Printf.sprintf "pattern=%s&%s&current=%s&page=%d"
            pattern
            (String.concat "&" 
                @@ StringSet.elements 
                @@ StringSet.map (fun elt -> ("entry=" ^ elt)) entries)
            current_entry
            page


let initialise_state () =
    let args = Url.Current.arguments in 
    if args != []
    then state := state_of_args args

let get_state_info () =
    match !state with
    | Uninitialized -> failwith "Error: search should be initialised"
    | Search state -> state

let get_input id = unopt @@ Html.CoerceTo.input @@ getElementById id

let state_to_entry_info () =
    let state = get_state_info () in
    {
        Data_types.last_id = Int64.of_int @@ (state.page - 1) * 50;
        pattern = state.pattern;
        starts_with = "."
    }

let update_state () =
    let handle_checkbox id state_info =
        let entry =
            match id with
            | "fpackages" -> "packages"
            | "flibraries" -> "libraries"
            | "fmodules" -> "modules"
            | "fmetas" -> "metas"
            | "fsources" -> "sources"
            | _ -> assert false
        in
            if to_bool @@ (get_input id)##.checked
            then state_info.entries <- StringSet.add entry state_info.entries
            else state_info.entries <- StringSet.remove entry state_info.entries

    in
        let state_info =  { pattern = ""; entries = StringSet.empty; current_entry = ""; page = 1 } in 
        let pattern_input = get_input "fpattern" in
        let value = to_string pattern_input##.value##trim in

        state_info.pattern <- if value = "" then "~" else value;
        handle_checkbox "fpackages" state_info;
        handle_checkbox "flibraries" state_info;
        handle_checkbox "fmodules" state_info;
        handle_checkbox "fmetas" state_info;
        handle_checkbox "fsources" state_info;
        match state_info.entries with
        | set when StringSet.is_empty set -> false
        | set -> 
            state_info.current_entry <- get_first_entry set;
            state := Search state_info;
            true

let update_form () =
    let check_input id =
        (get_input id)##.checked := bool true
    in
        match !state with
        | Uninitialized -> assert false
        | Search state -> 
            (get_input "fpattern")##.value := js state.pattern; 
            StringSet.iter (fun entry -> check_input @@ "f" ^ entry)
                state.entries

let set_handlers () =
    let form = unopt @@ Html.CoerceTo.form @@ getElementById "search-form" in
    let update_button = get_input "update-filters" in
    form##.onsubmit := Html.handler (fun _ ->
        if update_state () 
        then begin
            open_url @@ js @@ "search.html?" ^ state_to_args ()
        end;
        _false
    );
    update_button##.onclick := Html.handler (fun _ ->
        let form = getElementById "search-form" in
        update_button##.style##.display := js "none";
        form##.style##.display := js "";
        update_form ();
        _false
    )

let uninitialized_page () =
    let button = getElementById "update-filters" 
    and results = getElementById "result-div" in
    button##.style##.display := js "none";
    results##.style##.display := js "none";
    Lwt.return_unit 

let link_to_entry {pattern; entries; _} entry link =
    let st = { pattern; entries; current_entry = entry; page = 1} in
    let href = "search.html?" ^ state_to_args ~st:(Search st) () in
    link##setAttribute (js "href") (js href)

let pagination_info {pattern; entries; current_entry; page} entries_number =
    let pages_number = 
        if entries_number = 0
        then assert false
        else (entries_number - 1) / 50 + 1
    in  
        let first,last =
            if pages_number > 9 
               && 4 < page 
               && page < pages_number - 3
            then page - 4, page + 4
            else if not (4 < page)
            then 1, min 9 pages_number
            else if not (page < pages_number - 3)
            then max 1 (pages_number - 8), pages_number
            else max 1 (pages_number - 8), min 9 pages_number
        and pages = ref [] 
        and active_ind = ref 0 in
        for num = first to last do
            let first_entry = (num - 1) * 50 + 1
            and last_entry = if num = pages_number then entries_number else num * 50 in
            let st = {pattern; entries; current_entry; page=num} in
            let href = "search.html?" ^ state_to_args ~st:(Search st) () in
            let page_info = {num; entries_interval=(first_entry, last_entry); href} in
            pages := page_info :: !pages;
            if num < page 
            then incr active_ind
        done;
        let pages = List.rev !pages 
        and active_ind = !active_ind in
        {active_ind; pages; entries_number}

let insert_content state =
    logs "1";
    let entry_info = state_to_entry_info () in
    let entry = (get_state_info ()).current_entry in
    logs "2";
    let%lwt added = Requests.sendAdvancedSearchRequest entry entry_info in
    if added
    then begin
        logs "3";
        let%lwt entries_number = Requests.getEntriesNumber ~entry_info entry in
        logs "4";
        let number = int_of_string entries_number in
        let pages_info = pagination_info state number in
        logs "5";
        Insertion.insert_pagination pages_info;
        logs "6";
        Lwt.return_unit
    end 
    else begin
        let result_div = getElementById "result-div" in 
        result_div##.innerHTML := js "";
        let mess = Html.createSpan document in
        mess##setAttribute (js "class") (js "empty-result");
        mess##.innerHTML := js @@ "No " ^ entry ^ " found.";
        Dom.appendChild result_div mess;
        Lwt.return_unit
    end

let search_page () =
    match !state with
    | Uninitialized -> failwith "should not occur"
    | Search state -> begin
        let form = getElementById "search-form" in
        let result_nav = getElementById @@ state.current_entry ^ "-results" in
        let results = getElementById "results-list" in
        results##.innerHTML := js "";
        form##.style##.display := js "none";
        result_nav##.className := js "active-nav";
        StringSet.iter (fun entry -> 
                let nav_bar = getElementById @@ entry ^ "-results" in
                link_to_entry state entry nav_bar;
                nav_bar##.style##.display := js ""
            )
            state.entries;
        insert_content state
    end

let onload () =
    set_handlers ();
    initialise_state ();
    match !state with
    | Uninitialized -> uninitialized_page ()
    | _ -> search_page ()