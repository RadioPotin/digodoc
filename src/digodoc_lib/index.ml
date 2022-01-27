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

open EzFile.OP
open EzCompat
open Digodoc_common

module TYPES = struct

  type opam_entry = {
    opam_name : string ;
    opam_version : string ;
    opam_synopsis : string ;
  }

  type meta_entry = {
    meta_name : string ;
    meta_opam_name : string ;
    meta_opam_version : string ;
  }

  type library_entry = {
    lib_name : string ;
    lib_opam_name : string ;
    lib_opam_version : string ;
  }

  type module_entry = {
    mdl_name : string ; (* file/lowercase *)
    mdl_opam_name : string ;
    mdl_opam_version : string ;
    mdl_basename : string ;
    mdl_libs : library_entry list ;
  }

  type source_entry = {
    src_opam_name: string;
    src_opam_version: string;
  }

  type entry =
      Module of module_entry
    | Library of library_entry
    | Opam of opam_entry
    | Meta of meta_entry
    | Source of source_entry

end

module SAVE = struct

  let open_out filename =
    EzFile.make_dir ~p:true ( Filename.dirname filename );
    open_out filename

  open Type

  let save_opam_entry file opam =
    let oc = open_out file in
    Printf.fprintf oc "opam\n";
    Printf.fprintf oc "%s\n" opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim opam.opam_version);
    Printf.fprintf oc "%s\n" (match opam.opam_synopsis with
        | None -> "" | Some s ->
            String.trim (String.concat " " (EzString.split s '\n'))
      );
    close_out oc

  let save_meta_entry file meta =
    let oc = open_out file in
    Printf.fprintf oc "meta\n";
    Printf.fprintf oc "%s\n" meta.meta_name;
    Printf.fprintf oc "%s\n" meta.meta_opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim meta.meta_opam.opam_version);
    close_out oc

  let save_library_entry file lib =
    let oc = open_out file in
    Printf.fprintf oc "library\n";
    Printf.fprintf oc "%s\n" lib.lib_name;
    Printf.fprintf oc "%s\n" lib.lib_opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim lib.lib_opam.opam_version);
    close_out oc

  let save_module_entry file mdl =
    let oc = open_out file in
    Printf.fprintf oc "module\n";
    Printf.fprintf oc "%s\n" mdl.mdl_name;
    Printf.fprintf oc "%s\n" mdl.mdl_opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim mdl.mdl_opam.opam_version);
    Printf.fprintf oc "%s\n" mdl.mdl_basename;
    StringMap.iter (fun _ lib ->
        Printf.fprintf oc "%s@%s.%s\n"
          lib.lib_name lib.lib_opam.opam_name
          (String.trim lib.lib_opam.opam_version)
      ) mdl.mdl_libs;
    close_out oc

  let save_module_vals file mdl =
    let oc = open_out file in
    Printf.fprintf oc "%s\n" mdl.mdl_name;
    Printf.fprintf oc "%s\n" mdl.mdl_opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim mdl.mdl_opam.opam_version);
    List.iter (fun  (ident,vall) ->
        Printf.fprintf oc "%s\n%s\n" ident vall
      ) (Cmt.getVals @@ Option.get mdl.mdl_cmi_info);
    close_out oc

  let save_module_types file mdl =
    match mdl.mdl_cmi_info with
    | None -> ()
    | Some mdl_cmi_info ->
        match Cmt.getTypes mdl_cmi_info with
        | [] -> ()
        | type_sig_list ->
            let oc = open_out file in
            let ofmt = Format.formatter_of_out_channel oc in
            Format.fprintf ofmt "%s@.%s@.%s@.%a"
            mdl.mdl_name
            mdl.mdl_opam.opam_name
            (String.trim mdl.mdl_opam.opam_version)
            (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
              (fun fmt (ident,type_kind,type_decl) ->
                Format.fprintf fmt "%s@.%s@.%s@." ident type_kind type_decl
              )) type_sig_list ;
              close_out oc

end

open TYPES

let module_cut m =
  let rec iter m i len =
    if i+1 = len then
      m, ""
    else
    if m.[i] = '_' && m.[i+1] = '_' then
      (* Don't forget to capitalize (to handle for instance Stdlib__map) *)
      String.sub m 0 i, String.capitalize (String.sub m (i+2) (len - i - 2))
    else
      iter m (i+1) len
  in
  iter m 0 (String.length m)

let pkg_of_opam opam =
  Printf.sprintf "OPAM.%s.%s"
    opam.opam_name opam.opam_version

let pkg_of_lib lib =
  Printf.sprintf "LIBRARY.%s@%s.%s"
    lib.lib_name lib.lib_opam_name lib.lib_opam_version

let pkg_of_meta meta =
  Printf.sprintf "META.%s@%s.%s"
    meta.meta_name meta.meta_opam_name meta.meta_opam_version

let pkg_of_src src =
  Printf.sprintf "%s.%s"
    src.src_opam_name src.src_opam_version

let pkg_of_mdl mdl =
  let version = mdl.mdl_opam_version in
  match mdl.mdl_libs with
  | lib :: _rem -> pkg_of_lib lib
  | [] ->
      let pack, alias = module_cut mdl.mdl_basename in
      if alias = "" then
        Printf.sprintf "MODULE.%s@%s.%s"
          mdl.mdl_basename mdl.mdl_opam_name version
      else
        let pkg =
          Printf.sprintf "MODULE.%s__@%s.%s" pack mdl.mdl_opam_name version in
        if Sys.file_exists (Globals.digodoc_html_dir // pkg) then
          pkg
        else
          let pkg =
            Printf.sprintf "MODULE.%s__@%s.%s" pack mdl.mdl_opam_name version in
          if Sys.file_exists (Globals.digodoc_html_dir // pkg) then
            pkg
          else
            Printf.sprintf "MODULE.%s@%s.%s" pack mdl.mdl_opam_name version

let library_of_string s =
  let lib_name, s = EzString.cut_at s '@' in
  let lib_opam_name, lib_opam_version = EzString.cut_at s '.' in
  { lib_name ; lib_opam_name ; lib_opam_version }

let read_entry file =
  match EzFile.read_lines_to_list file with
  |
    "opam" ::
    opam_name ::
    opam_version ::
    opam_synopsis ->
      let opam_synopsis = String.concat " " opam_synopsis in
      Opam { opam_name ; opam_version ; opam_synopsis }
  | [
    "meta" ;
    meta_name ;
    meta_opam_name ;
    meta_opam_version ;
  ] -> Meta { meta_name ; meta_opam_name ; meta_opam_version }
  | [
    "library" ;
    lib_name ;
    lib_opam_name ;
    lib_opam_version ;
  ] -> Library { lib_name ; lib_opam_name ; lib_opam_version }
  | "module" ::
    mdl_name ::
    mdl_opam_name ::
    mdl_opam_version ::
    mdl_basename ::
    mdl_libs ->
      let mdl_libs = List.map library_of_string mdl_libs in
      Module { mdl_name ; mdl_opam_name ; mdl_opam_version ;
               mdl_basename ; mdl_libs }
  | _lines ->
      Printf.eprintf "Unrecognized format for entry file %S\n%!" file;
      raise Not_found


let print_index bb index entity_name =
  let map = ref StringMap.empty in
  let n = ref 0 in
  List.iter (fun (entry, line) ->
      incr n;
      let i = String.make 1 ( Char.lowercase entry.[0] ) in
      match StringMap.find i !map with
      | exception Not_found ->
          let r = ref [ entry, line ] in
          map := StringMap.add i r !map
      | r -> r := ( entry, line ) :: !r
    ) index;

  Printf.bprintf bb {|
    <!--<h4 id="item-number">%d %s</h4>-->
    <div id="by-name" class="by-name">
      <nav>
|} !n entity_name;

  Printf.bprintf bb {|<a id="all-letters" href="#" >ALL</a>|};
  StringMap.iter (fun i _ ->
      Printf.bprintf bb {|<a id="letter-%s" href="#" >%s</a>
|} i i) !map;

  Printf.bprintf bb {|
      </nav>
|};
  StringMap.iter (fun i r ->
      Printf.bprintf bb {|
     <div id="packages-set-%s" class="packages-set">
      <h3 id="name-%s">
        <a href="#name-%s" aria-hidden="true" class="anchor">
        </a>%s
      </h3>
      <ol id="packages-%s" class="packages">
|} i i i i i;
      if !Globals.frontend = Globals.JS then begin
        List.iter (fun ( _entry, line ) ->
            Printf.bprintf bb "%s\n" line;
          ) ( List.sort compare !r )
      end;
      Printf.bprintf bb {|
      </ol>
     </div>
|};
    ) !map;

  Printf.bprintf bb {|
    </div>
|};
  ()


let generate_library_index state bb =

  let index = ref [] in

  List.iter (function
      | Library lib ->

          let pkg = pkg_of_lib lib in
          let opam_pkg = pkg_of_opam
              { opam_name = lib.lib_opam_name ;
                opam_version = lib.lib_opam_version ;
                opam_synopsis = "" } in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="docs/%s/index.html" class="digodoc-lib"><code>%s</code></a> in opam <a href="docs/%s/index.html" class="digodoc-opam">%s.%s</a></li>|}
              search_id
              pkg lib.lib_name
              opam_pkg
              lib.lib_opam_name
              lib.lib_opam_version
          in
          index := ( lib.lib_name, line ) :: !index;
      | _ -> ()
    ) state ;

  print_index bb !index "libraries";

  ()

let generate_opam_index state bb =

  let index = ref [] in

  List.iter (function
      | Opam opam ->

          let pkg =
            Printf.sprintf "OPAM.%s.%s" opam.opam_name opam.opam_version in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="docs/%s/index.html" class="digodoc-opam"><code>%s.%s</code></a> %s</li>|}
              search_id
              pkg
              opam.opam_name
              opam.opam_version
              ( Html.encode opam.opam_synopsis )
          in
          index := (opam.opam_name, line ) :: !index;

      | _ -> ()
    ) state ;

  print_index bb !index "packages";
  ()


let generate_module_index state bb =

  let index = ref [] in

  let add_module pack alias mdl =
    let pkg = pkg_of_mdl mdl in
    let opam_pkg = pkg_of_opam {
        opam_name = mdl.mdl_opam_name ;
        opam_version = mdl.mdl_opam_version ;
        opam_synopsis = "" ;
      }
    in

    let search_id = Printf.sprintf "%s:%s" pkg mdl.mdl_name in

    let html_path, mdl_name =
      if alias = "" then
        Printf.sprintf "%s/%s" pkg mdl.mdl_name, mdl.mdl_name
      else
        (* In general, when we have a packed module M__N,
           M is generated and contains an alias N = M__N.
           However, when M already exists (written by the user),
           then the generated module is called M__. *)
        let path = Printf.sprintf "%s/%s__/%s" pkg pack alias in
        if Sys.file_exists (Globals.digodoc_html_dir // path) then
          path, Printf.sprintf "%s__.%s" pack alias
        else
          Printf.sprintf "%s/%s/%s" pkg pack alias,
          Printf.sprintf "%s.%s" pack alias
    in

    let line =
      Printf.sprintf
        {|<li class="package" id="%s"><a href="docs/%s/index.html"><code>%s</code></a> in opam <a href="docs/%s/index.html" class="digodoc-opam">%s.%s</a>%s</li>|}
        search_id
        html_path
        mdl_name
        opam_pkg
        mdl.mdl_opam_name
        mdl.mdl_opam_version
        (match mdl.mdl_libs with
         | [] -> ""
         | libs ->
             Printf.sprintf " in libs %s"
               ( String.concat ", "
                   (List.map (fun lib ->
                        Printf.sprintf
                          {|<a href="docs/%s/index.html" class="digodoc-lib">%s</a>|}
                          (pkg_of_lib lib) lib.lib_name
                      ) libs ))
        )
    in
    index := ( mdl.mdl_name, line ) :: !index;
  in

  List.iter (function
      | Module mdl ->
          let pack, alias = module_cut mdl.mdl_name in
          add_module pack alias mdl
      | _ -> ()
    ) state ;

  print_index bb !index "modules";
  ()

let generate_meta_index state bb =


  let index = ref [] in

  List.iter ( function
      | Meta meta ->

          let pkg = pkg_of_meta meta in
          let opam_pkg = pkg_of_opam  {
              opam_name = meta.meta_opam_name ;
              opam_version = meta.meta_opam_version ;
              opam_synopsis = "" ;
            }
          in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="docs/%s/index.html"><code>%s</code></a> in opam <a href="docs/%s/index.html" class="digodoc-opam">%s.%s</a></li>|}
              search_id
              pkg meta.meta_name
              opam_pkg
              meta.meta_opam_name
              meta.meta_opam_version
          in

          index := ( meta.meta_name , line ) :: !index;

      | _ -> ()
    ) state ;

  print_index bb !index "metas";
  ()

let generate_source_index state bb =

  let index = ref [] in

  List.iter ( function
      | Source src ->
          let pkg = pkg_of_src src in
          let opam_pkg = pkg_of_opam  {
              opam_name = src.src_opam_name ;
              opam_version = src.src_opam_version ;
              opam_synopsis = "" ;
            }
          in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="sources/%s/index.html"><code>%s</code></a> in opam <a href="docs/%s/index.html" class="digodoc-opam">%s.%s</a></li>|}
              search_id
              pkg src.src_opam_name
              opam_pkg
              src.src_opam_name
              src.src_opam_version
          in

          index := ( src.src_opam_name , line ) :: !index;

      | _ -> ()
    ) state ;

  print_index bb !index "sources";
  ()

let read_all_entries () =
  let entries = ref [] in
  let dir = Globals.digodoc_html_dir in
  Array.iter (fun pkg ->
    let dir = dir // pkg in
    Array.iter (fun file ->

      if EzString.starts_with file ~prefix:"ENTRY." then
        let entry = read_entry ( dir // file ) in
        begin
          match entry with
          | Opam {opam_name; opam_version; _ } ->
            let src = Source {src_opam_name=opam_name;src_opam_version=opam_version} in
            entries := src :: !entries
          | _ -> ()
        end;
        entries := entry :: !entries

    ) ( try Sys.readdir dir with _ -> [||] )
  ) ( Sys.readdir dir ) ;

  Printf.eprintf "%d entries read\n%!" ( List.length !entries ) ;
  !entries

let generate () =
  Printf.eprintf "Generating index...\n%!";
  if !Globals.db_update_index then begin
    Printf.eprintf "Updating DB index...\n%!";
    let promis =
      Lwt.bind
        (Cohttp_lwt_unix.Client.get (Uri.of_string "http://localhost:49002/generate"))
        (fun _ -> Lwt_io.eprintf "Done...\n%!")
    in Lwt_main.run promis
  end;
  if !Globals.sources_update_index then begin
    Printf.eprintf "Indexating sources...\n%!";
    let promis =
      Lwt.bind
        (Cohttp_lwt_unix.Client.get (Uri.of_string "http://localhost:49002/sources"))
        (fun _ -> Lwt_io.eprintf "Done...\n%!")
    in Lwt_main.run promis
  end;
  let state = read_all_entries () in

  let stdlib_version = Option.value ~default:"4.10.0" @@ List.find_map (function
      | Library {lib_opam_name = "ocaml-base-compiler" ; lib_opam_version; _} ->
          Some lib_opam_version
      | _ -> None) state in
  let header bb ~title =
    Printf.bprintf bb
      {|
  <h1>OCaml Documentation: %s</h1>
  <nav class="toc">
  <ul>
    <li><a href="https://ocaml.org/manual/">OCaml Manual</a></li>
    <li><a href="docs/LIBRARY.stdlib@ocaml-base-compiler.%s/Stdlib/index.html#modules">Stdlib Modules</a></li>
  </ul>
  </nav>
  </header>
  <h2>Index</h2>
<div class="container">
<div class="contained-left">
<h4 id="item-number" class=""></h4>
</div>
<div class="contained-right">
<input id="localsearch" placeholder="Search in ..." autocomplete="off">
</div>
</div>
<br>
<br>
|} title stdlib_version;
  in

  let trailer _bb =
    ()
  in

  Html.generate_page
    ~is_index:false
    ~filename:"about.html"
    ~title:"About"
    (fun bb ~title ->
       ignore title;
       Printf.bprintf bb "%s" (Utils.file_content "about.html"));

  Html.generate_page
    ~is_index:true
    ~filename:"packages.html"
    ~title:"Packages Index"
    (fun bb ~title ->
       header bb ~title;
       generate_opam_index state bb;
       trailer bb;
    );

  Html.generate_page
    ~is_index:true
    ~filename:"libraries.html"
    ~title:"Libraries Index"
    (fun bb ~title  ->
       header bb ~title;
       generate_library_index state bb;
       trailer bb;
    );

  Html.generate_page
    ~is_index:true
    ~filename:"metas.html"
    ~title:"Meta Index"
    (fun bb ~title ->
       header bb ~title;
       generate_meta_index state bb;
       trailer bb;
    );

  Html.generate_page
    ~is_index:true
    ~filename:"modules.html"
    ~title:"Modules Index"
    (fun bb ~title ->
       header bb ~title;
       generate_module_index state bb;
       trailer bb;
    );
  if !Globals.sources then begin
    Html.generate_page
      ~is_index:true
      ~filename:"sources.html"
      ~title:"Sources Index"
      (fun bb ~title ->
         header bb ~title;
         generate_source_index state bb;
         trailer bb;
      )
  end;

  Html.generate_page
    ~is_index:false
    ~filename:"search.html"
    ~title:"Search"
    (fun bb ~title ->
       ignore title;
       Printf.bprintf bb "%s" (Utils.file_content "search_page.html"));

  Html.generate_page
    ~is_index:false
    ~filename:"fulltext_search.html"
    ~title:"Fulltext"
    (fun bb ~title ->
       header bb ~title;
       trailer bb;
    );

    Printf.eprintf "Index generation done.\n%!";
  ()
