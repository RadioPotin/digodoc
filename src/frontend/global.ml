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

(********** Alias **********)

module Html = Dom_html

let js = Js.string

let window = Html.window

let document = Html.document


(********** State **********)
type state = {
  mutable last_id : int;
  mutable starts_with : string;
  mutable pattern: string;
}

let state = {
  last_id = 0;
  starts_with = ".";
  pattern = "~"
}


(********** Utils **********)

let logs s = Firebug.console##warn (Js.string s)

let unoptdef (valeur : 'a optdef) : 'a  =
  Optdef.get valeur (fun () -> assert false)

let unopt (valeur : 'a opt) : 'a  =
  Opt.get valeur (fun () -> assert false)

let concat (str1 : js_string t) (str2 : js_string t) : js_string t =
  str1##concat str2

let foreach (f : int -> 'a -> unit) (a : 'a js_array t) : unit =
  a##forEach(wrap_callback (fun elt i _ -> f i elt))   

let fromCharCode (i:int) : string = 
  let ch = Char.chr i in
  String.make 1 ch

(* TODO: getelementbyid alias rewrite all *)
let getElementById (id:string) : Html.element t = 
  unopt @@ document##getElementById (js id)

(********** Globals **********)

let reversed_path : string array = 
  let pathname : js_string t = 
    Html.window##.location##.pathname in
  let pathname = pathname##substring 1 (pathname##.length) in
  let reversed_path_js : js_string t js_array t = 
    (str_array (pathname##split (js "/")))##reverse in
  array_map to_string reversed_path_js |> to_array

let path : string array = 
  let pathname : js_string t = 
    Html.window##.location##.pathname in
  let pathname = pathname##substring 1 (pathname##.length) in
  let reversed_path_js : js_string t js_array t = 
    (str_array (pathname##split (js "/"))) in
  array_map to_string reversed_path_js |> to_array

let filename : string = reversed_path.(0) 

let main_div : Html.element t option ref = ref None 

let get_main_div () : Html.element t =
  match !main_div with
  | Some div -> div
  | None -> assert false

let load_div : Html.element t = 
  let div = document##createElement (js "div") in
  div##setAttribute (js "id") (js "load_div");
  div

let search_ul : Html.uListElement t option ref = ref None

let in_root_directory : bool = Array.length path == 1

let is_index_page : bool =
  in_root_directory && not (String.equal filename "about.html")

let is_doc_page : bool =
  path.(0) = "docs"

let is_source_page : bool =
  path.(0) = "sources"

let path_to_root : js_string t =
  let root = ref "" in 
  Array.iteri 
    (fun i _e ->
      if i > 0 then root := !root ^ "../"
      ) 
    path;
  js !root