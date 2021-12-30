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
open Types
open Cmi_format

let infix name =
  match name with
  | "asr" | "land" | "lnot" | "lor" | "lsl" | "lsr"
  | "lxor" | "mod" -> "(" ^ name ^ ")"
  | _ ->
    if (String.length name > 0) then
      match name.[0] with
      | 'a' .. 'z' | '\216' .. '\246' | '\248' .. '\255' | '_'
      | 'A' .. 'Z' | '\192' .. '\214' -> name
      | _ -> "(" ^ name ^ ")"
    else name

let pp_val_desc ident pp vdesc =
    let rec pp_aux l acc =
        match l with
        | [] -> acc
        | [""] -> acc
        | [s] -> acc^s
        | ""::ll -> pp_aux ll acc
        | s::ll -> pp_aux ll (acc^s^" ")
    and vdesc_l =
        (EzString.cut_at
            @@ Format.asprintf "%a"
                (Printtyp.value_description ident)
                vdesc)
            ':'
        |> snd
        |> String.split_on_char '\n'
        |> String.concat " "
        |> String.split_on_char ' '
    in
        let vdesc = pp_aux vdesc_l "" in
        let vdesc = fst @@ EzString.cut_at vdesc '='
        and buff = Buffer.create 13
        and i = ref 0 in
        while !i < String.length vdesc do
            if vdesc.[!i] = '/'
            then begin
                incr i;
                while !i < String.length vdesc &&
                           vdesc.[!i] >= '0' &&
                           vdesc.[!i] <= '9' do
                    incr i
                done
            end;
            if !i < String.length vdesc
            then begin
                Buffer.add_char buff vdesc.[!i];
                incr i
            end;
        done;
        Format.fprintf pp "%s" (Buffer.contents buff)

let getVals {cmi_sign; _} =
    (*
       For documentation on the types matched here see:
        https://docs.ocaml.pro/docs/LIBRARY.ocamlcommon@ocaml-base-compiler.4.10.0/Types/index.html#type-signature_item
      *)
    let vals = List.filter (function | Sig_value _ -> true | _ -> false) cmi_sign in
    List.map
        (fun sign ->
            match sign with
            | Sig_value (ident, vald, _) ->
                infix (Ident.name ident), Format.asprintf "%a" (pp_val_desc ident) vald
            | _ -> failwith "should not occur")
        vals

let standardise_output s =
    String.map (function |'\n' -> ' ' | '\r' -> ' ' | '\t' -> ' ' | '\x0C' -> ' ' | n -> n) s

let rec out_type fmt =
    let open Outcometree in
    function
        | Otyp_open -> Format.fprintf fmt "..."
        | Otyp_record ((_name, _b, _out_ty)::_r as list_of_fields) ->
                let ty =
                    Format.asprintf "%a" (Format.pp_print_list
                    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
                    (fun fmt (name, _b, out_ty) ->
                        Format.fprintf fmt "%s:%a" name out_type out_ty)
                    ) list_of_fields
                in
                Format.fprintf fmt "%s" (standardise_output ty)
        | t -> !Oprint.out_type fmt t

let rec pp_type fmt (acc, tree) =
    let open Outcometree in
    (*
        For documentation on the type used for representing type declarations see:
           https://docs.ocaml.pro/docs/LIBRARY.ocamlcommon@ocaml-base-compiler.4.10.0/Outcometree/index.html#type-out_sig_item
     *)
    match tree with
    | [] -> Format.fprintf fmt "%a" (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
            (fun fmt ty -> Format.fprintf fmt "%s" ty)) (List.rev acc)
    | Osig_type (out_type_decl, _out_rec_status) :: r ->
            let ty =
                Format.asprintf "%a" out_type out_type_decl.otype_type
            in
            pp_type fmt (standardise_output ty :: acc, r)
    | _ -> failwith "should not occur"

    (* This function is a placeholder while pp_type is underconstruction*)
let fmt_type_decl f ~pp_sep l =
    Format.asprintf "%a" (Format.pp_print_list ~pp_sep (fun fmt el -> Format.fprintf fmt "%a" f el)) l
    |> standardise_output

let getTypes {cmi_sign; _} =
    let types = List.filter (function | Sig_type _ -> true | _ -> false) cmi_sign in
    List.map
        (fun sign ->
            begin
            match sign with
            | Sig_type (ident, type_decl, _, _) ->
                    (*
                       For documentation on the types matched here see:
                        https://docs.ocaml.pro/docs/LIBRARY.ocamlcommon@ocaml-base-compiler.4.10.0/Types/index.html#type-signature_item
                      *)
                    let tree = Printtyp.tree_of_signature [sign] in
                    let ident = infix (Ident.name ident) in
                    begin
                        match type_decl.type_kind with
                        | Type_abstract ->
                                ident,
                                "TYPE_ABSTRACT",
                                Format.asprintf "%a" pp_type ([], tree)
                        | Type_record (_label_declaration_list, _record_representation) ->
                                ident,
                                "TYPE_RECORD",
                                Format.asprintf "%a" pp_type ([], tree)
                                (* Note that we could also call function
                                   [fmt_type_decl Printtyp.label ~pp_sep:(fun fmt () -> Format.fprintf fmt "") label_declaration_list]
                                   here but some output seem off with that function call in corresponding `TYPES.MODULE.x` files and TYPE_RECORD listings.
                                   It seems better to fine-tune [pp_type] function in order to have full control over output format
                                   *)
                        | Type_variant constructor_declaration_list ->
                                ident,
                                "TYPE_VARIANT",
                                fmt_type_decl Printtyp.constructor ~pp_sep:(fun fmt () -> Format.fprintf fmt "|") constructor_declaration_list
                        | Type_open -> ident, "TYPE_OPEN", Format.asprintf "%a" pp_type ([], tree)

                    end
            | _ -> failwith "should not occur"
            end
        ) types
