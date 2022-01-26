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

open Ez_html.V1
open Ez_subst.V1
open EzFile.OP
open Digodoc_common
open Globals
open Utils

(* generate page _digodoc/docs/${filename} *)
let generate_page ~filename ~title ~is_index f =

  let dirname = EzFile.dirname filename in
  let path_list =
    if String.contains filename '/'
    then
      String.split_on_char '/' dirname
    else [] in
  let s =
    String.concat "/"
      (List.map (fun _s -> "..")
         path_list) in
  let root = if s = "" then s else s ^ "/" in

  let rec brace () var = match var with
    | "root" -> root
    | "sources" ->
        if !Globals.sources
        then
          Printf.sprintf {|<a id="sources-item" href="%ssources.html">Sources</a>|}
            (brace () "root")
        else ""
    | "header_link" ->
        if !Globals.with_header
        then {| | <a href="#header">To the top</a>|}
        else ""
    | "search" -> 
        if is_index 
        then EZ_SUBST.string (file_content "search.html") ~ctxt:() ~brace
        else EZ_SUBST.string (file_content "search.html") ~ctxt:() ~brace
    (* trying to add fulltext search page *)
    | "fulltext_search" ->
        EZ_SUBST.string (file_content "fulltext_search.html") ~ctxt:() ~brace
    | _ ->
        Printf.kprintf failwith "Unknown var %S" var
  in
  let header = EZ_SUBST.string (file_content "header.html") ~ctxt:() ~brace
  and footer = EZ_SUBST.string (file_content "footer.html") ~ctxt:() ~brace in
  (* removed 'async' from the script line because unrecognized by ez_ml parser *)
  let bb = Buffer.create 10000 in
  Printf.bprintf bb {|<!DOCTYPE html>
    <html lang="en">
      <head>
        <title>%s</title>
        <link rel="stylesheet" href="%sstatic/styles/odoc/odoc.css"/>
        <link rel="icon" href="%sstatic/imgs/favicon.png" />
        <script type="text/javascript" src="%sstatic/scripts/%s" charset="utf-8"></script>
        <meta charset="utf-8"/>
        <meta name="generator" content="digodoc 0.1"/>
        <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
        <script src="%sstatic/scripts/highlight.pack.js"></script>
        <script>hljs.initHighlightingOnLoad();</script>
      </head>
      <body>
    |} title root root root (get_script ()) root;
  Printf.bprintf bb
    {|%s 
    <div class="content">
    |} header;
  f bb ~title;
  Printf.bprintf bb{|</div>
        %s
      </body>
    </html>
    |} footer;

  let contents = Buffer.contents bb in
  EzFile.write_file (digodoc_dir // filename) contents;
  ()

let encode = HTML.encode

open EzFile.OP
open HTML.TYPES

let check_html ~file xml =
  let rec iter =  function
    | PCData _ -> ()
    | Element (tagName, attributes, childNodes) ->
        List.iter iter childNodes;
        match tagName with
        | "a" | "A" ->
            List.iter (function
                  ("href", link) ->
                    begin
                      if link = "" then
                        Printf.eprintf "Empty Link in file %S\n%!" file
                      else
                        let link, _anchor = EzString.cut_at link '#' in
                        if link <> "" &&
                           not (EzString.starts_with link ~prefix:"http") then
                          let dir = Filename.dirname file in
                          let linked_file = dir // link in
                          if not (Sys.file_exists linked_file) then
                            Printf.eprintf "File %s: link %s is dangling\n  %s does not exist\n%!"
                              file link linked_file

                    end
                | _ -> ()) attributes
        | _ -> ()
  in
  iter xml

let rec add_trailer list =
  match list with
  | Element ("div", [ "id", "trailer" ], _ ) :: _ -> list
  | e :: list -> e :: add_trailer list
  | [] ->
      HTML.CONS.[
        div ~a:[ "id", "trailer" ]
          [ hr ;
            p [ s "Generated using ";
                a ~a:[
                  "href", "https://github.com/OCamlPro/digodoc";
                  "target", "digodoc";
                ]
                  [ s "digodoc" ];
                s " at ";
                a ~a:[
                  "href", "https://www.ocamlpro.com";
                  "target", "ocamlpro";
                ]
                  [ s "OCamlPro" ];
              ]
          ]
      ]

let insert_trailer xml =
  let rec iter xml =
    match xml with
    | PCData _ -> xml
    | Element ("div", [ "class", "content" ], childNodes) ->
        Element ("div", [ "class", "content" ], add_trailer childNodes)
    | Element (tagName, attributes, childNodes) ->
        Element ( tagName, attributes, List.map iter childNodes )
  in
  iter xml

let iter_html ?(check_links=false) ?(add_trailer=false) dir =
  Printf.eprintf "Scanning html files...\n%!";
  assert ( check_links ||  add_trailer );
  (*  EzFile.make_select *)
  EzFile.make_select EzFile.iter_dir ~deep:true ~glob:"*.html"
    ~f:(fun path ->
        let file = dir // path in
        match
          match HTML.parse_file file with
          | exception HTML.Error error ->
              Printf.eprintf "%s: invalid html (%s)\n%!"
                file (HTML.string_of_error error);
              None
          | xml -> Some xml
        with
        | None -> ()
        | Some xml ->
            if check_links then check_html ~file xml;
            if add_trailer then
              EzFile.write_file file
                ( "<!DOCTYPE html>\n" ^ HTML.to_string ( insert_trailer xml ) )
      ) dir;
  Printf.eprintf "Scan finished.\n%!"

let write_file file ~content =
  EzFile.write_file file (HTML.check content)

let add_header_footer () =
  Printf.eprintf "Adding header and footer...\n%!";
  let html_dir = digodoc_html_dir in
  let head_childs = [("link", {|<link rel="icon" href="${root}static/imgs/favicon.png" />|});
                     ("script", Printf.sprintf {|<script defer="defer" 
                        type="application/javascript" 
                        src="${root}static/scripts/%s">
                        </script>|} (get_script ()))] in
  EzFile.make_select EzFile.iter_dir ~deep:true ~glob:"index.html"
    ~f:(fun path ->
        if EzString.starts_with ~prefix:"ENTRY" (EzFile.basename path) 
        then ()
        else begin
          let file = html_dir // path in
          let rec brace () var = 
            match var with
            | "root" ->
                let dirname = EzFile.dirname path in 
                let path_list = 
                  if String.contains path '/'   
                  then 
                    String.split_on_char '/' dirname 
                  else []
                in
                let s =
                  String.concat "/"
                    (List.map (fun _s -> "..") 
                       path_list)
                in
                ".." // if s = "" then s else s ^ "/"
            | "sources" -> 
                if !sources 
                then 
                  Printf.sprintf {|<a id="sources-item" href="%ssources.html">Sources</a>|}
                    (brace () "root")
                else ""
            | "header_link" ->
                if !with_header 
                then {| | <a href="#header">To the top</a>|} 
                else ""
            | "search" -> EZ_SUBST.string (file_content "search.html") ~ctxt:() ~brace
            (* trying to add fulltext search page *)
            | "fulltext_search" ->
                EZ_SUBST.string (file_content "fulltext_search.html") ~ctxt:() ~brace
            | _ -> 
                Printf.kprintf failwith "Unknown var %S" var
          in

          let html = EzFile.read_file file 
          and header = EZ_SUBST.string (file_content "header.html") ~brace ~ctxt:()
          and footer = EZ_SUBST.string (file_content "footer.html") ~brace ~ctxt:()
          and head_childs = List.map (fun (id,child) -> id, EZ_SUBST.string child ~brace ~ctxt:()) head_childs in

          let html' = Patchtml.edit_html ~header ~footer ~head_childs html in

          EzFile.remove file;

          EzFile.write_file file html'
        end
      ) html_dir

let adjust_nav () =
  let update_doc path ?(filename="index.html") upper = 
    let file = path // filename in
    let html = EzFile.read_file file in
    let html' = Patchtml.change_link_to_upper_directory html upper in 
    EzFile.remove file;
    EzFile.write_file file html'
  and add_search_input path = 
    let file = path // "index.html" in
    let html = EzFile.read_file file in
    let html' = Patchtml.append_local_search html in
    EzFile.remove file;
    EzFile.write_file file html'
  in
    let html_dir = digodoc_html_dir in 
    Array.iter (fun file ->
        let path = html_dir // file in
        let preffix,_ = EzString.cut_at file '.' in
        match preffix with
        | "MODULE" -> begin
          Array.iter (fun modul ->
              let path = path // modul in 
              if EzFile.is_directory path then begin
                update_doc path "modules.html";
              end
            ) 
            (EzFile.read_dir path)
        end
        | "LIBRARY" -> update_doc path "libraries.html";
        | "META" -> update_doc path "metas.html"
        | "PAGES" ->
          if EzFile.exists (path // "index.html")
          then update_doc path "packages.html"
          else update_doc path "packages.html" ~filename:(EzFile.readdir path).(0)
        | "OPAM" -> 
          add_search_input path;
          update_doc path "packages.html"
        | _ -> ()
      )
      (EzFile.read_dir html_dir)



let adjust_docs () = 
  Printf.eprintf "Docs adjusting...\n%!";
  let html_dir = digodoc_html_dir in
  adjust_nav ();
  EzFile.make_select EzFile.iter_dir ~deep:true ~glob:"index.html"
    ~f:(fun path ->
        let file = html_dir // path in
        let html = EzFile.read_file file in
        let html' = Patchtml.change_link_highlight html in 
        EzFile.remove file;
        EzFile.write_file file html'
      )
    html_dir