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
open Data_types

(** Module [Global] exposes global variables, aliases and helpful functions used or have potential 
    to be used by several modules. *)

(** {1 Js_of_ocaml aliases} *)

module Html = Dom_html
(** Dom_html module alias. *)

let js = Js.string
(** Converts [string] to [js_string t]. *)

let window = Html.window
(** Window object alias. *)

let document = Html.document
(** Document object alias. *)

(** {1 Entry state} *)

type entry_state = Data_types.entry_info
(** Entry state type. Used by index pages and by pages with search input to store information about entries.
    Alias to [Data_types.entry_info] used by server. *)

let entry_state = {
  entry = PACK;
  last_id = 0;
  starts_with = "^.";
  pattern = ""
}
(** Current entry state. *)

(** {1 Pagination} *)

type page_info = {
  num : int; 
  interval : int * int;
  href : string 
}
(** Type that represents information about result page within search page. [num] is index of result page, 
    [interval] gives an interval of ids of entries/elements listed on the page and [href] specifies 
    the path to reach it. *)

type pagination_info = {
  active_ind : int;
  pages : page_info list;
  total_number: int
}
(** Inforamtion about result pages within search page. [active_ind] represents index of currently open result page,
    [pages] is a list of all existing result pages and [total_number] gives a total number of entries/elements 
    throughout all pages *)

(** {1 Useful functions} *)

exception Web_app_error of string list
(** Raised by frontend functions while occuring an error. 
    Exception keeps all occured error messages in the list. *)

let web_app_error ?(errors=[]) mess  =
  Web_app_error (mess::errors) 
(** [web_app_error ~errors mess] creates [Web_app_error] and adds [mess] error message 
    to the list of error messages [errors]. *)

let logs s = Firebug.console##log (js s)
(** [logs s] prints [s] in console. *)

let warn s = Firebug.console##warn (js s)
(** [warn s] prints [s] in console as a warning. *)

let err s = Firebug.console##error (js s)
(** [err s] prints [s] in console as an error. *)

let unoptdef (optdef : 'a optdef) : 'a  =
  Optdef.get 
    optdef 
    (fun () -> raise @@ web_app_error "unoptdef : value doesn't exists")
(** Decapsulates value of type ['a] from ['a optdef].
    Raises [Web_app_exception] if value doesn't exists. *)

let unopt (opt : 'a opt) : 'a  =
  Opt.get 
    opt 
    (fun () -> raise @@ web_app_error "unopt : value doesn't exists")
(** Decapsulates value of type ['a] from ['a opt].
    Raises [Web_app_exception] if value doesn't exists. *)

let concat (str1 : js_string t) (str2 : js_string t) : js_string t =
  str1##concat str2
(** [concat str1 str2] is the same as [str1##conat str2]. *)

let foreach (f : int -> 'a -> unit) (a : 'a js_array t) : unit =
  a##forEach(wrap_callback (fun elt i _ -> f i elt))   
(** [foreach f a] applies function [f] in turn to all
   the elements of [a] with the index of the element as first argument,
   and the element itself as second argument. *)

let from_char_code (ch_code:int) : string = 
  let ch = Char.chr ch_code in
  String.make 1 ch
(** [from_char_code ch_code] creates string from char code [ch_code]. *)

let get_element_by_id (id:string) : Html.element t = 
  try 
    unopt @@ document##getElementById (js id)
  with Web_app_error errors -> raise @@
    web_app_error 
        ~errors  
        (Printf.sprintf {|get_element_by_id : element with "%s" id doesn't exists|} id) 
(** [get_element_by_id id] returns an element that has specified [id] within current document.
    Raises [Web_app_exception] if element doesn't exists.*)

let get_element_by_id_opt (id:string) : Html.element t opt = 
  document##getElementById (js id)
(** [get_element_by_id_safe id] is the same as [get_element_by_id] but encapsulates result in [opt].*)

let open_url url =
  let _ = window##open_ url (js "_self") (Opt.return (js "")) in
  ()
(** Opens [url] page in the same window. *)

let encode_query_val qval =
  Uri.pct_encode ~component:`Query_value qval
(** [encode_query_val qval] encodes a segment of query string [qval] using module [Uri] *)

let decode_query_val qval =
  Uri.pct_decode qval
(** [decode_query_val qval] decodes a segment of query string [qval] using module [Uri] *)

let invalid_input (input : string) =
  let input = get_element_by_id input in 
  input##.style##.backgroundColor := js "#FF2E40"
(** Display input with id [input] as invalid *)

let valid_input (input : string) =
  let input = get_element_by_id input in 
  input##.style##.backgroundColor := js "white"
(** Display input with id [input] as valid *)

(** {1 Global variables} *)

let path : string array = 
  (* Js string that contains the path*)
  let pathname : js_string t = 
    window##.location##.pathname in
  (* Remove first '/' *)
  let pathname = pathname##substring 1 (pathname##.length) in
  (* Js array that stores path in reversed order *)
  let reversed_path_js : js_string t js_array t = 
    (str_array (pathname##split (js "/"))) in
  (* Conversion from 'js_string t js_array t' vers 'string array' *)
  array_map to_string reversed_path_js |> to_array
(** Array that represents the current path. *)

let reversed_path : string array = 
  path |> Array.to_list |> List.rev |> Array.of_list
(** Array that represents the current path in reversed order. *)

let filename : string = reversed_path.(0) 
(** Name of current file. *)

let in_root_directory : bool = Array.length path == 1
(** Says if current file is under root directory *)

let is_index_page : bool =
  in_root_directory 
  && not (String.equal filename "about.html")
  && not (String.equal filename "search.html")
(** Says if current file is index page *)

let is_doc_page : bool =
  path.(0) = "docs"
(** Says if current file is a documentation page (under 'docs/' directory) *)

let is_source_page : bool =
  path.(0) = "sources"
(* Says if current file is a source page (under 'sources/' directory) *)

let is_search_page : bool =
  path.(0) = "search.html"
(** Says if current file is a search page *)

let is_about_page : bool =
  path.(0) = "about.html"
(** Says if current file is a about page *)

let path_to_root : js_string t =
  let root = ref "" in 
  Array.iteri 
    (fun i _e ->
      if i > 0 then root := !root ^ "../"
      ) 
    path;
  js !root
(** Path to the root directory from current file *)