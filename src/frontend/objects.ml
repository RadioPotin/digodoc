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

(** Module [Objects] contains all js objects converted from OCaml data structure or created by js_of_ocaml. *)

type nonrec opam_entry = Data_types.opam_entry = {
  name : string; 
  path : string;
  version : string; 
  synopsis : string;
} [@@deriving jsoo]
(** Conversion from [Data_types.opam_entry] to js object *)

type nonrec packages = opam_entry list 
[@@deriving jsoo]
(** Conversion from [Data_types.packages] to js object *)

type nonrec lib_entry = Data_types.lib_entry = {
  name : string;
  path : string;
  opam : string;
  opampath: string; 
} [@@deriving jsoo]
(** Conversion from [Data_types.lib_entry] to js object *)

type nonrec libraries = lib_entry list
[@@deriving jsoo]
(** Conversion from [Data_types.libraries] to js object *)

type nonrec meta_entry = Data_types.meta_entry = {
  namemeta : string ;
  path : string ;
  opam : string ;
  opampath : string;
} [@@deriving jsoo]
(** Conversion from [Data_types.meta_entry] to js object *)

type nonrec metas = meta_entry list
[@@deriving jsoo]
(** Conversion from [Data_types.metas] to js object *)

type module_entry = Data_types.module_entry = {
  name : string;
  path : string;
  opam : string;
  opampath : string;
  libs : (string * string) list;
} [@@deriving jsoo]
(** Conversion from [Data_types.module_entry] to js object *)

type nonrec modules  = module_entry list [@@deriving jsoo]
(** Conversion from [Data_types.modules] to js object *)

type nonrec source_entry = Data_types.source_entry = {
  namesrc : string;
  path : string;
  opam : string;
  opampath : string;
} [@@deriving jsoo]
(** Conversion from [Data_types.source_entry] to js object *)

type nonrec sources = source_entry list [@@deriving jsoo]
(** Conversion from [Data_types.sources] to js object *)

type nonrec val_element = Data_types.val_element = {
  ident : string;
  value : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
}
[@@deriving jsoo]
(** Conversion from [Data_types.val_element] to js object *)

type nonrec vals = val_element list [@@deriving jsoo]
(** Conversion from [Data_types.vals] to js object *)

type nonrec search_result = Data_types.search_result = {
  packages : packages;
  libraries : libraries;
  modules : modules;
}
[@@deriving jsoo]
(** Conversion from [Data_types.search_result] to js object *)

type nonrec sources_occurence = Data_types.sources_occurence = {
  opamname : string;
  srcpath: string;
  filename: string;
  occpos: int;
  occline: string;
  occpath: string;
}
[@@deriving jsoo]
(** Conversion from [Data_types.sources_occurence] to js object *)

type nonrec sources_search_result = Data_types.sources_search_result = {
  totaloccs : int;
  occs : sources_occurence list
}
[@@deriving jsoo]
(** Conversion from [Data_types.sources_search_result] to js object *)