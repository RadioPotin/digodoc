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
open Object
open Global

let hide_with_letter first_letter = 
    let name_elt : Html.element t = 
        unopt @@ document##getElementById (concat (js "name-") first_letter) in
    name_elt##.style##.display := js ""

let insert_packages : packages_jsoo t -> unit  = 
    fun (packages : packages_jsoo t) -> 
        let pck : opam_entry_jsoo t = unoptdef @@ array_get packages 0 in
        let first_letter : js_string t ref = ref (pck##.name##charAt 0)##toLowerCase in
        hide_with_letter !first_letter;

        foreach 
            (fun _ elt -> 
                let pkg = Html.createLi document in
                pkg##setAttribute (js "class") (js "package");
                let pkg_name = Html.createA document in
                pkg_name##setAttribute (js "class") (js "digodoc-opam");
                pkg_name##setAttribute (js "href") elt##.path;
                let name = Html.createCode document in
                name##.innerHTML := elt##.name;
                Dom.appendChild pkg_name name;
                Dom.appendChild pkg pkg_name;
                pkg##.innerHTML := concat pkg##.innerHTML @@ concat (js " ") elt##.synopsis;
                let elt_first_letter = (elt##.name##charAt 0)##toLowerCase in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    hide_with_letter !first_letter;
                end;
                let pkg_set = unopt @@ document##getElementById (concat (js "packages-") !first_letter) in
                Dom.appendChild pkg_set pkg;
            )
        packages
    

let insert_libraries : libraries_jsoo t -> unit  = 
    fun (libraries : libraries_jsoo t) -> 
        let lib : lib_entry_jsoo t = unoptdef @@ array_get libraries 0 in
        let first_letter : js_string t ref = ref (lib##.name##charAt 0)##toLowerCase in
        hide_with_letter !first_letter;

        foreach 
            (fun _ elt -> 
                let lib = Html.createLi document in
                lib##setAttribute (js "class") (js "package");
                
                let lib_name = Html.createA document in
                lib_name##setAttribute (js "class") (js "digodoc-lib");
                lib_name##setAttribute (js "href") elt##.path;

                let name = Html.createCode document in
                name##.innerHTML := elt##.name;

                Dom.appendChild lib_name name;
                Dom.appendChild lib lib_name;
                lib##.innerHTML := concat lib##.innerHTML (js " in opam ");

                let lib_opam = Html.createA document in
                lib_opam##setAttribute  (js "class") (js "digodoc-opam");
                lib_opam##setAttribute  (js "href") elt##.opampath;
                lib_opam##.innerHTML := elt##.opam;

                Dom.appendChild lib lib_opam;

                let elt_first_letter = (elt##.name##charAt 0)##toLowerCase in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    hide_with_letter !first_letter;
                end;

                let lib_set = unopt @@ document##getElementById (concat (js "packages-") !first_letter) in
                Dom.appendChild lib_set lib;

            )
        libraries
    

let insert_modules : modules_jsoo t -> unit  = 
    fun (modules : modules_jsoo t) -> 
        let mdl : module_entry_jsoo t = unoptdef @@ array_get modules 0 in
        let first_letter : js_string t ref = ref (mdl##.name##charAt 0)##toLowerCase in
        hide_with_letter !first_letter;

        foreach 
            (fun _ elt -> 
                let mdl = Html.createLi document in
                mdl##setAttribute (js "class") (js "package");
                
                let mdl_name = Html.createA document in
                mdl_name##setAttribute (js "href") elt##.path;

                let name = Html.createCode document in
                name##.innerHTML := elt##.name;

                Dom.appendChild mdl_name name;
                Dom.appendChild mdl mdl_name;
                mdl##.innerHTML := concat mdl##.innerHTML (js " in opam ");

                let mdl_opam = Html.createA document in
                mdl_opam##setAttribute  (js "class") (js "digodoc-opam");
                mdl_opam##setAttribute  (js "href") elt##.opampath;
                mdl_opam##.innerHTML := elt##.opam;

                Dom.appendChild mdl mdl_opam;

                if elt##.libs##.length > 0 then begin
                    mdl##.innerHTML := concat mdl##.innerHTML (js " in libs ")
                end;

                foreach
                    (fun i lib -> 
                        let mdl_libs = Html.createA document in
                        mdl_libs##setAttribute (js "class") (js "digodoc-lib");
                        mdl_libs##setAttribute (js "href") lib##._1;
                        mdl_libs##.innerHTML := lib##._0;
                        Dom.appendChild mdl mdl_libs;
                        if i+1 < elt##.libs##.length then begin
                            mdl##.innerHTML := concat mdl##.innerHTML (js ", ")
                        end
                    )
                    elt##.libs;

                let elt_first_letter = (elt##.name##charAt 0)##toLowerCase in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    hide_with_letter !first_letter;
                end;

                let mdl_set = unopt @@ document##getElementById (concat (js "packages-") !first_letter) in
                Dom.appendChild mdl_set mdl;

            )
        modules
    

let insert_metas : metas_jsoo t -> unit  = 
    fun (metas : metas_jsoo t) -> 
        let meta : meta_entry_jsoo t = unoptdef @@ array_get metas 0 in
        let first_letter : js_string t ref = ref (meta##.name##charAt 0)##toLowerCase in
        hide_with_letter !first_letter;

        foreach 
            (fun _ elt -> 
                let meta = Html.createLi document in
                meta##setAttribute (js "class") (js "package");
                
                let meta_name = Html.createA document in
                meta_name##setAttribute (js "href") elt##.path;

                let name = Html.createCode document in
                name##.innerHTML := elt##.name;

                Dom.appendChild meta_name name;
                Dom.appendChild meta meta_name;
                meta##.innerHTML := concat meta##.innerHTML (js " in opam ");

                let meta_opam = Html.createA document in
                meta_opam##setAttribute  (js "class") (js "digodoc-opam");
                meta_opam##setAttribute  (js "href") elt##.opampath;
                meta_opam##.innerHTML := elt##.opam;

                Dom.appendChild meta meta_opam;

                let elt_first_letter = (elt##.name##charAt 0)##toLowerCase in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    hide_with_letter !first_letter;
                end;

                let meta_set = unopt @@ document##getElementById (concat (js "packages-") !first_letter) in
                Dom.appendChild meta_set meta;

            )
        metas
    

let insert_sources : sources_jsoo t -> unit  = 
    fun (sources : sources_jsoo t) -> 
        let src : source_entry_jsoo t = unoptdef @@ array_get sources 0 in
        let first_letter : js_string t ref = ref (src##.name##charAt 0)##toLowerCase in
        hide_with_letter !first_letter;

        foreach 
            (fun _ elt -> 
                let src = Html.createLi document in
                src##setAttribute (js "class") (js "package");
                
                let src_name = Html.createA document in
                src_name##setAttribute (js "href") elt##.path;

                let name = Html.createCode document in
                name##.innerHTML := elt##.name;

                Dom.appendChild src_name name;
                Dom.appendChild src src_name;
                src##.innerHTML := concat src##.innerHTML (js " in opam ");

                let src_opam = Html.createA document in
                src_opam##setAttribute (js "class") (js "digodoc-opam");
                src_opam##setAttribute (js "href") elt##.opampath;
                src_opam##.innerHTML := elt##.opam;

                Dom.appendChild src src_opam;

                let elt_first_letter = (elt##.name##charAt 0)##toLowerCase in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    hide_with_letter !first_letter;
                end;

                let src_set = unopt @@ document##getElementById (concat (js "packages-") !first_letter) in
                Dom.appendChild src_set src;

            )
        sources
    