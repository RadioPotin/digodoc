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

let digodoc_dir = "_digodoc"

let digodoc_html_dir = digodoc_dir // "docs"

let htmlize_sources_dir = digodoc_dir // "sources"

let db_update_index = ref false

let sources_update_index = ref false

type frontentd_type = JS | JS_API | JS_OCAML

let frontend = ref JS 

(* options that are modified only using digodoc *)
let sources = ref true

let with_header = ref false

