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
        getElementById @@ "name-" ^ to_string first_letter in
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
                let pkg_set = getElementById @@ "packages-" ^ to_string !first_letter in
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

                let lib_set = getElementById @@ "packages-" ^ to_string !first_letter in
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

                let mdl_set = getElementById @@ "packages-" ^ to_string !first_letter in
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

                let meta_set = getElementById @@ "packages-" ^ to_string !first_letter in
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

                let src_set = getElementById @@ "packages-" ^ to_string !first_letter in
                Dom.appendChild src_set src;

            )
        sources

let insert_search_result : search_result_jsoo t -> unit =
    fun (result : search_result_jsoo t) ->
        let search_ul = Option.get !search_ul in
        let insert_pack pack =
            let search_item = Html.createLi document in
            search_item##setAttribute (js "class") (js "search-item");
            let item_link = Html.createA document in
            item_link##setAttribute (js "href") (path_to_root##concat pack##.path);
            Dom.appendChild search_item item_link;
            let item_indicator = Html.createDiv document
            and item_name = Html.createDiv document in
            item_indicator##setAttribute (js "class") (js ("item-indicator item-pack"));
            item_name##setAttribute (js "class") (js "item-name");
            item_name##.innerHTML := pack##.name;
            Dom.appendChild item_link item_indicator;
            Dom.appendChild item_link item_name;
            Dom.appendChild search_ul search_item
        and insert_item elt indicator =
            let search_item = Html.createLi document in
            search_item##setAttribute (js "class") (js "search-item");
            let item_link = Html.createA document in
            item_link##setAttribute (js "href") (path_to_root##concat elt##.path);
            Dom.appendChild search_item item_link;
            let item_indicator = Html.createDiv document
            and item_name = Html.createDiv document
            and item_pack = Html.createDiv document in
            item_indicator##setAttribute (js "class") (js ("item-indicator " ^ indicator));
            item_name##setAttribute (js "class") (js "item-name");
            item_name##.innerHTML := elt##.name;
            item_pack##setAttribute (js "class") (js "package-item");
            item_pack##.innerHTML := elt##.opam;
            Dom.appendChild item_link item_indicator;
            Dom.appendChild item_link item_name;
            Dom.appendChild item_link item_pack;
            Dom.appendChild search_ul search_item
        in
            foreach
                (fun _ elt -> insert_pack elt)
                result##.packages;
            foreach
                (fun _ elt -> insert_item elt "item-lib")
                result##.libraries;
            foreach
                (fun _ elt -> insert_item elt "item-mdl")
                result##.modules


let insert_search_packages : packages_jsoo t -> unit  = 
    fun (packages : packages_jsoo t) -> 
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
                let pkg_set = getElementById "results-list" in
                Dom.appendChild pkg_set pkg;
            )
        packages

let insert_search_libraries : libraries_jsoo t -> unit  = 
    fun (libraries : libraries_jsoo t) -> 
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
                let lib_set = getElementById "results-list" in
                Dom.appendChild lib_set lib;
            )
        libraries

let insert_search_modules : modules_jsoo t -> unit  = 
    fun (modules : modules_jsoo t) -> 
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
                let mdl_set = getElementById "results-list" in
                Dom.appendChild mdl_set mdl;

            )
        modules

let insert_search_metas : metas_jsoo t -> unit  = 
    fun (metas : metas_jsoo t) -> 
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
                let meta_set = getElementById "results-list" in
                Dom.appendChild meta_set meta;

            )
        metas
    

let insert_search_sources : sources_jsoo t -> unit  = 
    fun (sources : sources_jsoo t) -> 
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
                let src_set = getElementById "results-list" in
                Dom.appendChild src_set src;
            )
        sources

let s i = string_of_int i  

let insert_page_info {active_ind; pages; entries_number} =
    let { entries_interval=(f,l); _ } = List.nth pages active_ind in
    let interval = Html.createB document in
    interval##.innerHTML := js @@ s f ^ "-" ^ s l;
    let total = Html.createB document in
    total##.innerHTML := js @@ s entries_number;
    let page_info_div = getElementById "page-info"in
    page_info_div##.innerHTML := concat page_info_div##.innerHTML (js "Displaying ");
    Dom.appendChild page_info_div interval;
    page_info_div##.innerHTML := concat page_info_div##.innerHTML (js " of ");
    Dom.appendChild page_info_div total;
    page_info_div##.innerHTML := concat page_info_div##.innerHTML (js " total results") 

let insert_pagination ({active_ind; pages; _} as pagination) =
    insert_page_info pagination;
    let has_precedent = active_ind > 0
    and has_next = active_ind < (List.length pages) - 1 in
    if has_precedent
    then begin
        let arrowL = getElementById "previous-page" in
        arrowL##.style##.display := js "";
        let {href; _} = List.nth pages (active_ind - 1) in
        arrowL##setAttribute (js "href") (js href)
    end;
    if has_next
    then begin
        let arrowR = getElementById "next-page" in
        arrowR##.style##.display := js "";
        let {href; _} = List.nth pages (active_ind + 1) in
        arrowR##setAttribute (js "href") (js href)
    end;
    let pages_ol = getElementById "pages" in
    List.iteri (fun i { num; href; _} ->
            let page_li = Html.createLi document in
            let page_a = Html.createA document in
            page_a##setAttribute (js "href") (js href);
            if i = active_ind 
            then page_a##setAttribute (js "class") (js "active");
            page_a##.innerHTML := js (string_of_int num);
            Dom.appendChild page_li page_a;
            Dom.appendChild pages_ol page_li
        )
        pages
