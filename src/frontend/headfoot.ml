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

open Global

let activate_bar () =
  let activate item =
    let item_bar = getElementById item in
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
      | _ -> assert false



let footerHandler () = 
    let footer = getElementById "footer" in
    footer##.style##.position := js "relative";
    let innerHeight = unoptdef @@ window##.innerHeight
    and clientHeight = document##.body##.clientHeight in
    if innerHeight <= clientHeight 
    then footer##.style##.position := js "relative"
    else footer##.style##.position := js "fixed"
