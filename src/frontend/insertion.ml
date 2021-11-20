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
open Objects
open Globals
open Data_types

(** Module [Insertion] unions functions that make various DOM insertions. *)

let set_attr elt attr value =
    elt##setAttribute (js attr) value
(** [set_attr elt attr val] sets attribute [attr] of [elt] to [value]. *)    

let append_inner elt str =
    elt##.innerHTML := concat elt##.innerHTML str
(** [append_inner elt str] appends [str] to the content of [elt]. *)

(** {1 Index page} *)

let display_header head = 
    let name_elt : Html.element t = 
        get_element_by_id @@ "name-" ^ to_string head in
    name_elt##.style##.display := js ""
(** [display_header head] displays header [head] within index page. [head] represents first letter of entries
    that he unites. *)

let get_first_letter elt =
    (elt##.name##charAt 0)##toLowerCase
(** [get_first_letter elt] returns the first letter of an entry/element [elt]. *)

let write_message message =
    let msg_div = Html.createDiv document in
    let msg = Html.createSpan document in
    set_attr msg "class" (js "message");
    append_inner msg (js message);
    Dom.appendChild msg_div msg;
    append_content msg_div
(** Displays message at the bottom of the page. *)

let write_warning warn =
    (* Create structure *)
    let warning_div = Html.createDiv document 
    and table = Html.createTable document 
    and tr = Html.createTr document 
    and td_image = Html.createTd document 
    and td_mess = Html.createTd document  in
    let img_src = concat path_to_root (js "static/imgs/warning.png") in
    let img = Html.createImg document in
    set_attr img "src" img_src;
    set_attr td_image "class" (js "warning-img");
    Dom.appendChild td_image img;
    let mess = Html.createSpan document in
    set_attr td_mess "class" (js "warning-msg");
    append_inner mess (js warn);
    set_attr warning_div "class" (js "warning");
    Dom.appendChild td_mess mess;
    Dom.appendChild tr td_image;
    Dom.appendChild tr td_mess;
    Dom.appendChild table tr;
    Dom.appendChild warning_div table;
    append_content warning_div
(** Displays warning at the bottom of the page. *)

let insert_packages_index : packages_jsoo t -> unit  = 
    fun (packages : packages_jsoo t) ->
        let first_letter : js_string t ref = ref (js "") in
        (* for every package *)
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let pkg = Html.createLi document in
                set_attr pkg "class" (js "package");
                (* Append opam package name *)
                let pkg_name = Html.createA document in
                set_attr pkg_name "class" (js "digodoc-opam");
                set_attr pkg_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.name;
                Dom.appendChild pkg_name name;
                Dom.appendChild pkg pkg_name;
                (* Append synopsis *)
                append_inner pkg @@ concat (js " ") elt##.synopsis;
                (* Displaying header of package if it's not already done *)
                let elt_first_letter = get_first_letter elt in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    display_header elt_first_letter;
                end;
                let pkg_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
                Dom.appendChild pkg_set pkg;
            )
        packages
(** [insert_packages_index pkgs] constructs index page and inserts array of packages [pkgs] that stores 
    converted to js package objects. *)

let insert_libraries_index : libraries_jsoo t -> unit  = 
    fun (libraries : libraries_jsoo t) -> 
        let first_letter : js_string t ref = ref (js "") in
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let lib = Html.createLi document in
                set_attr lib "class" (js "package");
                (* Append library name *)
                let lib_name = Html.createA document in
                set_attr lib_name "class" (js "digodoc-lib");
                set_attr lib_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.name;
                Dom.appendChild lib_name name;
                Dom.appendChild lib lib_name;
                (* Append library package *)
                append_inner lib (js " in opam ");
                let lib_opam = Html.createA document in
                set_attr lib_opam "class" (js "digodoc-opam");
                set_attr lib_opam "href" elt##.opampath;
                append_inner lib_opam elt##.opam;
                Dom.appendChild lib lib_opam;
                let elt_first_letter = get_first_letter elt in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    display_header elt_first_letter;                
                end;
                let lib_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
                Dom.appendChild lib_set lib;
            )
        libraries
(** Same as [insert_packages_index] but instead deals with libraries. *)

let insert_modules_index : modules_jsoo t -> unit  = 
    fun (modules : modules_jsoo t) -> 
        let first_letter : js_string t ref = ref (js "") in
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let mdl = Html.createLi document in
                set_attr mdl "class" (js "package");
                (* Append module name *)
                let mdl_name = Html.createA document in
                set_attr mdl_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.name;
                Dom.appendChild mdl_name name;
                Dom.appendChild mdl mdl_name;
                (* Append module package *)
                append_inner mdl (js " in opam ");
                let mdl_opam = Html.createA document in
                set_attr mdl_opam "class" (js "digodoc-opam");
                set_attr mdl_opam "href" elt##.opampath;
                append_inner mdl_opam elt##.opam;
                Dom.appendChild mdl mdl_opam;
                (* Append module libraries *)                
                if elt##.libs##.length > 0 then append_inner mdl (js " in libs ");
                (* for every library *)
                foreach
                    (fun i lib -> 
                        let mdl_libs = Html.createA document in
                        set_attr mdl_libs "class" (js "digodoc-lib");
                        set_attr mdl_libs "href" lib##._1;
                        append_inner mdl_libs lib##._0;
                        Dom.appendChild mdl mdl_libs;
                        if i+1 < elt##.libs##.length then append_inner mdl (js ", ")
                    )
                    elt##.libs;
                let elt_first_letter = get_first_letter elt in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    display_header elt_first_letter;                
                end;
                let mdl_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
                Dom.appendChild mdl_set mdl;
            )
        modules
(** Same as [insert_packages_index] but instead deals with modules. *)

let insert_metas_index : metas_jsoo t -> unit  = 
    fun (metas : metas_jsoo t) -> 
        let first_letter : js_string t ref = ref (js "") in
        foreach 
            (fun _ elt ->
                (* Create a line *)
                let meta = Html.createLi document in
                set_attr meta "class" (js "package");
                (* Append meta name *)
                let meta_name = Html.createA document in
                set_attr meta_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.namemeta;
                Dom.appendChild meta_name name;
                Dom.appendChild meta meta_name;
                (* Append meta package *)
                append_inner meta (js " in opam ");
                let meta_opam = Html.createA document in
                set_attr meta_opam "class" (js "digodoc-opam");
                set_attr meta_opam "href" elt##.opampath;
                append_inner meta_opam elt##.opam;
                Dom.appendChild meta meta_opam;
                let elt_first_letter = (elt##.namemeta##charAt 0)##toLowerCase in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    display_header elt_first_letter;                
                end;
                let meta_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
                Dom.appendChild meta_set meta
            )
        metas
(** Same as [insert_packages_index] but instead deals with metas. *)

let insert_sources_index : sources_jsoo t -> unit  = 
    fun (sources : sources_jsoo t) -> 
        let first_letter : js_string t ref = ref (js "") in
        foreach 
            (fun _ elt ->
                (* Create a line *)
                let src = Html.createLi document in
                set_attr src "class" (js "package");
                (* Append source name *)
                let src_name = Html.createA document in
                set_attr src_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.namesrc;
                Dom.appendChild src_name name;
                Dom.appendChild src src_name;
                (* Append source package *)
                append_inner src (js " in opam ");
                let src_opam = Html.createA document in
                set_attr src_opam "class" (js "digodoc-opam");
                set_attr src_opam "href" elt##.opampath;
                append_inner src_opam elt##.opam;
                Dom.appendChild src src_opam;
                let elt_first_letter = (elt##.namesrc##charAt 0)##toLowerCase in
                if not (!first_letter = elt_first_letter) then begin
                    first_letter := elt_first_letter;
                    display_header elt_first_letter;                
                end;
                let src_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
                Dom.appendChild src_set src;
            )
        sources
(** Same as [insert_packages_index] but instead deals with sources. *)

let insert_index entries =
    match entries with
    | Opam packages -> insert_packages_index (Objects.packages_to_jsoo packages)
    | Lib libraries -> insert_libraries_index (Objects.libraries_to_jsoo libraries)
    | Mdl modules -> insert_modules_index (Objects.modules_to_jsoo modules)
    | Meta metas -> insert_metas_index (Objects.metas_to_jsoo metas)
    | Src sources -> insert_sources_index (Objects.sources_to_jsoo sources)
(** Calls specific to [entries] insertion function for index page *)

(** {1 Search input} *)

let insert_search_result : search_result_jsoo t -> unit =
    fun (result : search_result_jsoo t) ->
        (* Get ul element where search results will be inserted *)
        let search_ul = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "search-result-ul" in
        (* Insert package in search ul *)
        let insert_pack pack =
            (* Create a package line *)
            let search_item = Html.createLi document in
            set_attr search_item "class" (js "search-item");
            let item_link = Html.createA document in
            set_attr item_link "href" @@ concat path_to_root pack##.path;
            Dom.appendChild search_item item_link;
            let item_indicator = Html.createDiv document
            and item_name = Html.createDiv document in
            set_attr item_indicator "class" (js ("item-indicator item-pack"));
            set_attr item_name "class" (js "item-name");
            append_inner item_name pack##.name;
            Dom.appendChild item_link item_indicator;
            Dom.appendChild item_link item_name;
            Dom.appendChild search_ul search_item
        (* Insert module/library in search ul *)
        and insert_item elt indicator =
            (* Create an entry line *)
            let search_item = Html.createLi document in
            set_attr search_item "class" (js "search-item");
            let item_link = Html.createA document in
            set_attr item_link "href" @@ concat path_to_root elt##.path;
            Dom.appendChild search_item item_link;
            let item_indicator = Html.createDiv document
            and item_name = Html.createDiv document in
            set_attr item_indicator "class" (js ("item-indicator " ^ indicator));
            set_attr item_name "class" (js "item-name");
            append_inner item_name elt##.name;
            (* Show entry's package *)
            let item_pack = Html.createDiv document in
            set_attr item_pack "class" (js "package-item");
            append_inner item_pack elt##.opam;
            Dom.appendChild item_link item_indicator;
            Dom.appendChild item_link item_name;
            Dom.appendChild item_link item_pack;
            Dom.appendChild search_ul search_item
        in 
            (* For every package in search result *)
            foreach
                (fun _ elt -> insert_pack elt)
                result##.packages;
            (* For every library in search result *)
            foreach
                (fun _ elt -> insert_item elt "item-lib")
                result##.libraries;
            (* For every module in search result *)
            foreach
                (fun _ elt -> insert_item elt "item-mdl")
                result##.modules
(** [insert_search_result search_res] fills search list with entries in [res]. 
    [search_res] should be converted to js object from [Data_types.search_result] type. *)

(** {1 Search page} *)

let insert_packages_search : packages_jsoo t -> unit  = 
    fun (packages : packages_jsoo t) -> 
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let pkg = Html.createLi document in
                set_attr pkg "class" (js "package");
                (* Append opam package name *)
                let pkg_name = Html.createA document in
                set_attr pkg_name "class" (js "digodoc-opam");
                set_attr pkg_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.name;
                Dom.appendChild pkg_name name;
                Dom.appendChild pkg pkg_name;
                append_inner pkg @@ concat (js " ") elt##.synopsis;
                (* Insert line in list *)
                let pkg_set = get_element_by_id "results-list" in
                Dom.appendChild pkg_set pkg;
            )
        packages
(** [insert_packages_search pkgs] constructs search page and inserts array of packages [pkgs] that stores 
    converted to js package objects. *)

let insert_libraries_search : libraries_jsoo t -> unit  = 
    fun (libraries : libraries_jsoo t) -> 
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let lib = Html.createLi document in
                set_attr lib "class" (js "package");
                (* Append library name *)
                let lib_name = Html.createA document in
                set_attr lib_name "class" (js "digodoc-lib");
                set_attr lib_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.name;
                Dom.appendChild lib_name name;
                Dom.appendChild lib lib_name;
                (* Append library package *)
                append_inner lib (js " in opam ");
                let lib_opam = Html.createA document in
                set_attr lib_opam "class" (js "digodoc-opam");
                set_attr lib_opam "href" elt##.opampath;
                append_inner lib_opam elt##.opam;
                Dom.appendChild lib lib_opam;
                let lib_set = get_element_by_id "results-list" in
                Dom.appendChild lib_set lib;
            )
        libraries
(** Same as [insert_packages_search] but instead deals with libraries. *)

let insert_modules_search : modules_jsoo t -> unit  = 
    fun (modules : modules_jsoo t) -> 
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let mdl = Html.createLi document in
                set_attr mdl "class" (js "package");
                (* Append module name *)
                let mdl_name = Html.createA document in
                set_attr mdl_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.name;
                Dom.appendChild mdl_name name;
                Dom.appendChild mdl mdl_name;
                (* Append module package *)
                append_inner mdl (js " in opam ");
                let mdl_opam = Html.createA document in
                set_attr mdl_opam "class" (js "digodoc-opam");
                set_attr mdl_opam "href" elt##.opampath;
                append_inner mdl_opam elt##.opam;
                Dom.appendChild mdl mdl_opam;
                (* Append module libraries *)
                if elt##.libs##.length > 0 then append_inner mdl (js " in libs ");
                foreach
                    (fun i lib -> 
                        let mdl_libs = Html.createA document in
                        set_attr mdl_libs "class" (js "digodoc-lib");
                        set_attr mdl_libs "href" lib##._1;
                        append_inner mdl_libs lib##._0;
                        Dom.appendChild mdl mdl_libs;
                        if i+1 < elt##.libs##.length then append_inner mdl (js ", ")
                    )
                    elt##.libs;
                let mdl_set = get_element_by_id "results-list" in
                Dom.appendChild mdl_set mdl
            )
        modules
(** Same as [insert_packages_search] but instead deals with modules. *)

let insert_metas_search : metas_jsoo t -> unit  = 
    fun (metas : metas_jsoo t) -> 
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let meta = Html.createLi document in
                set_attr meta "class" (js "package");
                (* Append meta name *)
                let meta_name = Html.createA document in
                set_attr meta_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.namemeta;
                Dom.appendChild meta_name name;
                Dom.appendChild meta meta_name;
                (* Append meta package *)
                append_inner meta (js " in opam ");
                let meta_opam = Html.createA document in
                set_attr meta_opam "class" (js "digodoc-opam");
                set_attr meta_opam "href" elt##.opampath;
                append_inner meta_opam elt##.opam;
                Dom.appendChild meta meta_opam;
                let meta_set = get_element_by_id "results-list" in
                Dom.appendChild meta_set meta;
            )
        metas
(** Same as [insert_packages_search] but instead deals with metas. *)

let insert_sources_search : sources_jsoo t -> unit  = 
    fun (sources : sources_jsoo t) -> 
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let src = Html.createLi document in
                set_attr src "class" (js "package");
                (* Append source name *)
                let src_name = Html.createA document in
                set_attr src_name "href" elt##.path;
                let name = Html.createCode document in
                append_inner name elt##.namesrc;
                Dom.appendChild src_name name;
                Dom.appendChild src src_name;
                (* Append source package *)
                append_inner src (js " in opam ");
                let src_opam = Html.createA document in
                set_attr src_opam "class" (js "digodoc-opam");
                set_attr src_opam "href" elt##.opampath;
                append_inner src_opam elt##.opam;
                Dom.appendChild src src_opam;
                let src_set = get_element_by_id "results-list" in
                Dom.appendChild src_set src;
            )
        sources
(** Same as [insert_packages_search] but instead deals with sources. *)

let insert_vals_search : vals_jsoo t -> unit  = 
    fun (vals : vals_jsoo t) -> 
        foreach 
            (fun _ elt -> 
                (* Create a line *)
                let vall = Html.createLi document in
                set_attr vall "class" (js "package");
                (* Append keyword 'val' *)
                let vall_word = Html.createSpan document in
                set_attr vall_word "class" (js "keyword");
                append_inner vall_word (js "val ");
                Dom.appendChild vall vall_word;
                (* Append val name *)
                let vall_ident = Html.createA document in
                let vall_href = concat (js "#val-") elt##.ident in 
                set_attr vall_ident "href" @@ concat elt##.mdlpath vall_href;
                set_attr vall_ident "class" (js "val");
                append_inner vall_ident elt##.ident;
                Dom.appendChild vall vall_ident;
                append_inner vall (js " : ");
                (* Append val type *)
                let vall_val = Html.createSpan document in
                set_attr vall_val "class" (js "type-annot");
                append_inner vall_val elt##.value;
                Dom.appendChild vall vall_val;
                append_inner vall (js " in opam ");
                (* Append val package *)
                let vall_opam = Html.createA document in
                set_attr vall_opam "class" (js "digodoc-opam");
                set_attr vall_opam "href" elt##.opampath;
                append_inner vall_opam elt##.opam;
                Dom.appendChild vall vall_opam;
                (* Append val module *)
                append_inner vall (js " in ");
                let vall_mdl = Html.createA document in
                set_attr vall_mdl "href" elt##.mdlpath;
                append_inner vall_mdl elt##.mdl;
                Dom.appendChild vall vall_mdl;
                let vall_set = get_element_by_id "results-list" in
                Dom.appendChild vall_set vall;
            )
        vals
(** Same as [insert_packages_search] but instead deals with OCaml values. *)

let display_page_info {active_ind; pages; total_number} =
    let s i = string_of_int i  in
    (* Get interval of entries presented on an active result page *)
    let { interval=(f,l); _ } = List.nth pages active_ind in
    (* Create a "Displaying" line *)
    let interval = Html.createB document in
    (* Append entries interval *)
    append_inner interval @@ js @@ s f ^ "-" ^ s l;
    (* Append total entries number *)
    let total = Html.createB document in
    append_inner total @@ js @@ s total_number;
    let page_info_div = get_element_by_id "page-info"in
    append_inner page_info_div (js "Displaying ");
    Dom.appendChild page_info_div interval;
    append_inner page_info_div (js " of ");
    Dom.appendChild page_info_div total;
    append_inner page_info_div (js " total results")
(** [display_page_info pi] extracts inforamation from [pi] of type [Globals.pagination_info]
    and displays information about interval and total number of entries on the page *)

let insert_pagination ({active_ind; pages; _} as pagination) =
    (* Display information about active page *)
    display_page_info pagination;
    let has_precedent = active_ind > 0
    and has_next = active_ind < (List.length pages) - 1 in
    (* If it isn't the first page *)
    if has_precedent
    then begin
        (* Display arrow left *)
        let arrowL = get_element_by_id "previous-page" in
        arrowL##.style##.display := js "";
        let {href; _} = List.nth pages (active_ind - 1) in
        set_attr arrowL "href" (js href)
    end;
    (* If it isn't the last page *)
    if has_next
    then begin
        (* Display arrow right *)
        let arrowR = get_element_by_id "next-page" in
        arrowR##.style##.display := js "";
        let {href; _} = List.nth pages (active_ind + 1) in
        set_attr arrowR "href" (js href)
    end;
    let pages_ol = get_element_by_id "pages" in
    (* Insert computed previously range of pages *)
    List.iteri (fun i { num; href; _} ->
            let page_li = Html.createLi document in
            let page_a = Html.createA document in
            set_attr page_a "href" (js href);
            if i = active_ind 
            then set_attr page_a "class" (js "active");
            append_inner page_a @@ js @@ string_of_int num;
            Dom.appendChild page_li page_a;
            Dom.appendChild pages_ol page_li
        )
        pages
(* [insert_pagination pi] uses calculated [pi] pagination info about current page to display :
    - page information (range of entries id, total number of entries)
    - navigation bar to jump between pages *)

let insert_entries_search entries =
    match entries with
    | Opam packages -> insert_packages_search (Objects.packages_to_jsoo packages)
    | Lib libraries -> insert_libraries_search (Objects.libraries_to_jsoo libraries)
    | Mdl modules -> insert_modules_search (Objects.modules_to_jsoo modules)
    | Meta metas -> insert_metas_search (Objects.metas_to_jsoo metas)
    | Src sources -> insert_sources_search (Objects.sources_to_jsoo sources)
(** Calls specific to [entries] insertion function for search page *)

let insert_elements_search elements =
    match elements with
    | Val vals -> insert_vals_search (Objects.vals_to_jsoo vals)
(** Calls specific to [elements] insertion function for search page *)

