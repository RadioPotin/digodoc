(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Type
open EzFile.OP

(* Assumption: all modules in the same directory are added by the
   same opam_file *)

let long_name ~mdl_name ~mdl_opam =
  mdl_opam.opam_name ^ "::" ^ mdl_name

let file mdl ~ext =
  let name_n_ext = mdl.mdl_basename ^ ext in
  let path = StringMap.find ext mdl.mdl_path in
  path // name_n_ext

let find state ~mdl_name ~mdl_opam =
  let long_name = long_name ~mdl_name ~mdl_opam in
  Hashtbl.find state.ocaml_mdls_by_name long_name

let find_or_create ~mdl_ext ~mdl_path ~mdl_basename state ~mdl_opam ~objinfo =
  let mdl_name = String.capitalize mdl_basename in
  let mdl =
    match find state ~mdl_name ~mdl_opam with
    | exception Not_found ->
        let mdl = {
          mdl_name ;
          mdl_longname = mdl_opam.opam_name ^ "::" ^ mdl_name;
          mdl_basename ;
          mdl_opam ;
          mdl_path = StringMap.empty;
          mdl_libs = StringMap.empty;
          mdl_metas = StringMap.empty;
          mdl_intf = None;
          mdl_impl = None;
          mdl_cmi_info = None;
          mdl_cmt_info = None
        } in
        Hashtbl.add state.ocaml_mdls_by_name
          ( long_name ~mdl_name ~mdl_opam ) mdl;
        Hashtbl.add state.ocaml_mdls_by_name mdl_name mdl ;
        state.ocaml_mdls <- ( mdl_name, mdl ) :: state.ocaml_mdls;
        mdl_opam.opam_mdls <-
          StringMap.add mdl_name mdl mdl_opam.opam_mdls ;
        mdl
    | mdl -> mdl
  in
  mdl.mdl_path <- StringMap.add mdl_ext mdl_path mdl.mdl_path ;

  (* Extract cmi/cmt info *)
    begin
    match mdl_ext with
    | "cmt" | "cmti" | "cmi" -> begin
      match Cmt_format.read (state.opam_switch_prefix // file mdl ~ext:("." ^ mdl_ext)) with
      | Some cmi, _ when mdl.mdl_cmi_info = None -> mdl.mdl_cmi_info <- Some cmi
      | _, Some cmt when mdl.mdl_cmt_info = None -> mdl.mdl_cmt_info <- Some cmt
      | _ -> ()
      end
    | _ -> ()
  end;

  (* TODO: check that, if this mdlule is already added, it is the same
     one. Otherwise, it means two opam packages have added different
     files for this mdlule *)
  if objinfo && mdl_ext = "cmx" then begin
    match Objinfo.read state (file mdl ~ext:".cmx") with
    | [] | _ :: _ :: _ -> (* TODO: warning *) ()
    | [ unit ] ->
        mdl.mdl_impl <- Some unit ;
        match unit.unit_implementation with
        | None -> assert false
        | Some crc ->
            Hashtbl.add state.ocaml_mdls_by_cmx_crc crc mdl
  end;
  if objinfo && mdl_ext = "cmi" then begin
    match Objinfo.read state (file mdl ~ext:".cmi") with
    | [] | _ :: _ :: _ -> (* TODO: warning *) ()
    | [ unit ] ->
        mdl.mdl_intf <- Some unit;
        match StringMap.find unit.unit_name unit.unit_import_cmis with
        | exception Not_found -> assert false
        | crc ->
            Hashtbl.add state.ocaml_mdls_by_cmi_crc crc mdl
  end;
  mdl

let file m ext =
  let name_n_ext = m.mdl_basename ^ ext in
  let path = StringMap.find ext m.mdl_path in
  path // name_n_ext
