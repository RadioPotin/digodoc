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
        and buff = Buffer.create 16
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
    List.filter_map (function
            | Sig_value (ident, vald, _) ->
                Some (infix (Ident.name ident), Format.asprintf "%a" (pp_val_desc ident) vald)
            | _ -> None)
        cmi_sign

(** [standardise_output fmt s] Removes and replaces some char occurences *)
    let standardise_output fmt s =
        let p = Format.pp_print_char fmt in
        String.iter (function
            | '{' | '}' -> p '\x00'
            | '\r' -> p ' '
            | '\t' -> p ' '
            | '\x0C' -> p ' '
            | n -> p n
        ) s

(** [out_type fmt type] recursively prints a type declaration tree *)
let rec out_type fmt sign =
    let rec aux_out_type fmt =
        let open Outcometree in
        function
            | Otyp_open -> ()
            | Otyp_abstract -> ()
            | Otyp_record list_of_fields ->
                    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
                        (fun fmt (name, _b, _out_ty) -> Format.fprintf fmt "%s" name
                    )) fmt list_of_fields
            | Otyp_manifest (out_ty1, out_ty2) -> Format.fprintf fmt "%a@.%a" aux_out_type out_ty1 aux_out_type out_ty2
            | Otyp_variant (_b, out_var, _b2, _s_l_opt) -> pp_outvar fmt out_var
            | Otyp_poly (s_l, _out_ty) ->
                    (Format.pp_print_list
                        ~pp_sep:(fun fmt ()-> Format.fprintf fmt "@.")
                        Format.pp_print_string) fmt s_l
            | Otyp_object (l, _b_opt) ->
                        (Format.pp_print_list
                            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
                            (fun fmt (s, _out_ty) -> Format.pp_print_string fmt s)) fmt l
            | Otyp_alias (_out_ty, s) -> Format.pp_print_string fmt s
            | Otyp_stuff s -> Format.pp_print_string fmt s
            | Otyp_sum l ->
                    (Format.pp_print_list
                        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
                        (fun fmt (s, _out_ty_l, _out_ty_opt) ->
                            Format.pp_print_string fmt s
                        )) fmt l
            | Otyp_module (id, _s_l, _out_ty_l) -> !Oprint.out_ident fmt id
            | Otyp_class (_b, out_id, _out_type_l) -> !Oprint.out_ident fmt out_id
            | Otyp_constr (out_id, _out_type_l) -> !Oprint.out_ident fmt out_id

            | Otyp_tuple _ | Otyp_var _ | Otyp_arrow _ | Otyp_attribute _ -> ()
    in
    let ty =
        Format.asprintf "%a" aux_out_type sign
    in
    standardise_output fmt ty

    and pp_outvar fmt =
    let open Outcometree in
    function
        | Ovar_fields list_of_fields ->
                (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
                    (fun fmt (name, _b, _out_ty_l) ->
                        Format.fprintf fmt "`%s" name
                    )) fmt list_of_fields
        | Ovar_typ out_ty -> Format.fprintf fmt "`%a" out_type out_ty

(** [pp_type fmt (acc, tree)] takes a type declaration tree and prints its content with function [out_type]*)
let rec pp_type fmt (acc, tree) =
    let open Outcometree in
    (*
        For documentation on the type used for representing type declarations see:
           https://docs.ocaml.pro/docs/LIBRARY.ocamlcommon@ocaml-base-compiler.4.10.0/Outcometree/index.html#type-out_sig_item
     *)
    match tree with
    | [] -> (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
                (fun fmt ty -> Format.fprintf fmt "%s" ty)
            ) fmt (List.rev acc)
    | Osig_type (out_type_decl, _out_rec_status) :: r ->
            let ty =
                Format.asprintf "%a" out_type out_type_decl.otype_type
            in
            pp_type fmt ( ty :: acc, r)
    | _ -> failwith "should not occur"

(** [pp_constructors fmt const_decl_l] Prints the constructors composing a Variant type *)
let pp_constructors fmt const_decl_l =
    let constructors =
        Format.asprintf "%a"
            (Format.pp_print_list
                    ~pp_sep:(fun fmt ()-> Format.fprintf fmt "@.")
                    (fun fmt constr ->
                        Format.fprintf fmt "%s" (infix (Ident.name constr.cd_id))
                    )
            ) const_decl_l
            in
            standardise_output fmt constructors


(** [pp_class_sig_item fmt sig_item] Prints the items of a class *)
let pp_class_sig_item fmt sig_item =
    let open Outcometree in
    match sig_item with
    | Ocsg_constraint _ -> ()
    | Ocsg_method ( name, _mut, _vir, _out_ty ) ->
            Format.fprintf fmt "method %s@." name
    | Ocsg_value ( name, _mut, _vir, _out_ty ) ->
            Format.fprintf fmt "val %s@." name


(** [pp_class_contents fmt class_decl] Prints a class and its contents *)
let pp_class_contents fmt class_decl =
    let open Outcometree in

    match class_decl with
    | Octy_constr ( out_id, _out_type_list ) ->
            Format.fprintf fmt "%a"
                !Oprint.out_ident out_id

    | Octy_arrow ( name, _out_ty, _out_class_type ) ->
            Format.fprintf fmt "%s" name

    | Octy_signature ( out_type_op, out_class_sig_item_list) ->
            let out_ty =
                match out_type_op with
                | None -> ""
                | Some out_ty -> Format.asprintf "%a" out_type out_ty
            in
            Format.fprintf fmt "%s%a" out_ty
                (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
                (fun fmt sig_item ->
                    Format.fprintf fmt "%a" pp_class_sig_item sig_item)
                ) out_class_sig_item_list


(** [pp_class fmt tree] handles the printing of a class_type declaration type *)
let pp_class fmt tree =
    let open Outcometree in
    (*
        For documentation on the type used for representing type declarations see:
           https://docs.ocaml.pro/docs/LIBRARY.ocamlcommon@ocaml-base-compiler.4.10.0/Outcometree/index.html#type-out_sig_item
     *)
    match tree with
    | Osig_class_type (_b1, _s1, _l, out_class_ty, _out_rec_status) ->
            Format.fprintf fmt "%a" pp_class_contents out_class_ty
    | _ -> failwith "should not occur"

    (** [getTypes cmi_sign] match on and print type and class signatures for later indexation in DB *)
let getTypes {cmi_sign; _} =
    List.filter_map
        (function
         (* For documentation on the types matched here see:
                https://docs.ocaml.pro/docs/LIBRARY.ocamlcommon@ocaml-base-compiler.4.10.0/Types/index.html#type-signature_item
            *)
            | Sig_type (ident, type_decl, _, _) as sign ->
                    let tree = Printtyp.tree_of_signature [sign] in
                    let ident = infix (Ident.name ident) in
                    begin
                        match type_decl.type_kind with
                        | Type_abstract ->
                                let contents =
                                    Format.asprintf "%a" pp_type ([], tree)
                                in
                                Some (ident, "TYPE_ABSTRACT", contents)
                        | Type_record (_label_declaration_list, _record_representation) ->
                                Some (ident, "TYPE_RECORD", Format.asprintf "%a" pp_type ([], tree) )
                        | Type_variant constructor_declaration_list ->
                                let constructors =
                                    Format.asprintf "%a" pp_constructors constructor_declaration_list
                                in
                                Some (ident, "TYPE_VARIANT", constructors )
                        | Type_open -> Some (ident, "TYPE_OPEN", Format.asprintf "%a" pp_type ([], tree) )
            end
            | Sig_class_type (id, class_type_declaration, rec_status, _visibility) ->
                    let ident = infix (Ident.name id) in
                    let tree =
                        (Printtyp.tree_of_cltype_declaration id) class_type_declaration rec_status
                    in
                    let obj_items =
                        Format.asprintf "%a" pp_class tree
                    in
                    Some (ident, "CLASS_OBJ", obj_items)
            | _ -> None
        ) cmi_sign
