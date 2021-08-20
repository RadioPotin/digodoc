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


type nonrec opam_entry = Data_types.opam_entry = {
  name : string; 
  path : string;
  version : string; 
  synopsis : string;
} [@@deriving jsoo]

type nonrec packages = opam_entry list 
[@@deriving jsoo]

type nonrec lib_entry = Data_types.lib_entry = {
  name : string;
  path : string;
  opam : string;
  opampath: string; 
} [@@deriving jsoo]

type nonrec libraries = lib_entry list
[@@deriving jsoo]

type nonrec meta_entry = Data_types.meta_entry = {
  name : string ;
  path : string ;
  opam : string ;
  opampath : string;
} [@@deriving jsoo]

type nonrec metas = meta_entry list
[@@deriving jsoo]

type module_entry = Data_types.module_entry = {
  name : string;
  path : string;
  opam : string;
  opampath : string;
  libs : (string * string) list;
} [@@deriving jsoo]

type nonrec modules  = module_entry list [@@deriving jsoo]

type nonrec source_entry = Data_types.source_entry = {
  name : string;
  path : string;
  opam : string;
  opampath : string;
} [@@deriving jsoo]

type nonrec sources = source_entry list [@@deriving jsoo]

type nonrec search_result = Data_types.search_result = {
  packages : packages;
  libraries : libraries;
  modules : modules;
}
[@@deriving jsoo]