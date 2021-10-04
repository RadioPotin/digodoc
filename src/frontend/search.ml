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

let redirection_handler () =
    let search_items = document##getElementsByClassName (js "search-item") in
    let _ = 
        if search_items##.length == 1
        then begin
            let item =  unopt @@ search_items##item 0 in
            let link =  unopt @@ Dom.CoerceTo.element @@ unopt @@ item##.firstChild in
            let href = unopt @@ link##getAttribute (js "href") in
            open_url href
        end
        else
            let display_query = "&current=packages&page=0" in
            let entries_query = "&entry=packages&entry=libraries&entry=modules" in
            let url = js ("search.html?pattern=" ^ encode_query_val state.pattern ^ entries_query ^ display_query) in 
            open_url url
    in
        ()

let clear_search () =
    match !search_ul with 
    | Some search_ul ->
        search_ul##.innerHTML := js "";
    | None -> begin
        let search_div = Html.createDiv document in
        search_div##.id := js "search-result";
        let ul = Html.createUl document in
        Dom.appendChild search_div ul;
        search_ul := Some ul;
        let body = document##.body in
        let eltAfter = document##getElementById (js "footer") in
        Dom.insertBefore body search_div eltAfter
    end

let update_search pattern =
    let pattern = encode_path pattern in 
    Lwt.async @@ Requests.sendSearchRequest pattern

let set_search_handler () =
    let search = unopt @@ Html.CoerceTo.input @@ getElementById "search" in
    search##.onkeyup := Html.handler (fun kbevent ->
        begin
            match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
            | Some "Enter" -> 
                redirection_handler ()
            | _ -> begin
                clear_search ();
                let re = search##.value in
                let input = re##trim in
                state.pattern <- to_string input;
                if String.length state.pattern > 2
                then update_search state.pattern;
            end
        end;
        _false)

let set_button_handler () =
    let button = unopt @@ Html.CoerceTo.button @@ getElementById "search-button" in
    button##.onclick := Html.handler (fun _ ->
        redirection_handler ();
        _false)

let onload () =
    clear_search ();
    set_search_handler ();
    set_button_handler ();
    Lwt.return_unit
