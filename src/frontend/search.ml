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

let update_search () = Lwt.async Requests.sendSearchRequest

let set_search_handler () =
    let search = unopt @@ Html.CoerceTo.input @@ getElementById "search" in
    search##.onkeyup := Html.handler (fun _ ->
        clear_search ();
        let re = search##.value in
        let input = re##trim in
        state.pattern <- to_string input;
        if String.length state.pattern > 2
        then update_search ();
        _false)

let onload_main () =
    let%lwt () = Requests.api_host () in
    clear_search ();
    set_search_handler ();
    Headfoot.activate_bar ();
    Headfoot.footerHandler ();
    Lwt.return_unit

let onload _ = 
    Lwt.async onload_main; 
    _false