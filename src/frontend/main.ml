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

let main () =
  let footHandler = 
    Html.handler (fun _ -> 
      Headfoot.footerHandler ();
      _false) in
  window##.onresize := footHandler;
  if is_index_page 
  then window##.onload := Html.handler Index.onload
  else window##.onload := Html.handler Search.onload
  
let () = main ()
