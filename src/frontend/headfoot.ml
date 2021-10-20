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

open Globals

(** Module [Headfoot] unions functions that manipulate header and footer of the page. *)

(** {1 Header} *)

let activate_bar () =
  (* Activate specified item *)
  let activate item =
    let item_bar = get_element_by_id item in
    item_bar##.className := js "active"
  in
    if in_root_directory then
      match filename with
      | "about.html" -> activate "about-item";
      | "search.html" -> activate "search-page-item";
      | "packages.html" -> activate "packages-item"
      | "libraries.html" -> activate "libraries-item"
      | "metas.html" -> activate "metas-item"
      | "modules.html" -> activate "modules-item"
      | "sources.html" -> activate "sources-item"
      | _ -> 
        raise @@ 
          web_app_error (Printf.sprintf {|activate_bar : file "%s" couldn't activate the bar|} filename) 
(** Activates header's item according to current page.
    Activation is done only for files under the root directory. 
    Raise [Web_app_exception] if file under root directory doesn't have header's item. *)

(** {1 Footer} *)

let footerHandler () = 
    let footer = get_element_by_id "footer" in
    (* Footer is rendered after the last element. This is used to calculate correct client height *)
    footer##.style##.position := js "relative";
    let innerHeight = unoptdef @@ window##.innerHeight
    and clientHeight = document##.body##.clientHeight in
    (* If page height is bigger than window height *)
    if innerHeight <= clientHeight 
    then footer##.style##.position := js "relative"
    else footer##.style##.position := js "fixed"
(** Handler that places footer correctly according to the page height *)