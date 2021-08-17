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


open EzFile.OP
open Globals

let get_script () = match !frontend with 
  | JS -> "search.js"
  | JS_API -> "search_api.js"
  | JS_OCAML -> "frontend.js"

let file_content filename =
  match Sys.getenv "DIGODOC_CONFIG" with
  | dir when EzFile.exists (dir // filename) -> 
    EzFile.read_file (dir // filename)
  | exception Not_found | _ ->
    begin   
      match Files.read filename with
      | None -> ""
      | Some file_content -> file_content
    end