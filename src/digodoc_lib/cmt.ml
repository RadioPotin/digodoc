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
    Format.fprintf pp "%s" (pp_aux vdesc_l "")

let getVals {cmi_sign; _} = 
    let vals = List.filter (function | Sig_value _ -> true | _ -> false) cmi_sign in
    List.map
        (fun sign ->
            match sign with
            | Sig_value (ident, vald, _) -> 
                Ident.name ident, Format.asprintf "%a" (pp_val_desc ident) vald
            | _ -> failwith "should not occur")
        vals