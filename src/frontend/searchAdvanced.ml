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
open Globals
open Data_types
open Utils


(**Trying stuff Elias *)
open Objects

(** Module [SearchAdvanced] defines behaviour for search pages (search.html).
    Search page is constructed dynamically by sending requests to API server. Page could have two states :
    Initialised with entry/element form or unitialized. If page is opened without arguments (query string)
    it is considered as uninitialized and it displays two forms (for entries and for elements). Submiting form
    opens initialised search page in the same window. Initialised page displays:
    1) Button 'update filters' that allows to update search filters
    2) Information about current page (range of ids on the page)
    3) Entry/element navigation bar.
    4) List of entries/elements corresponding to previously mentioned ids.
    5) Pagination bar *)

module OrderedEntry = struct
  type t = Data_types.entry_type
  let compare e1 e2 =
    match e1,e2 with
    | x,y when x=y -> 0
    | PACK,_ | LIB,SRC | LIB,META | LIB,MOD | MOD,META | MOD,SRC | META,SRC -> -1
    | _ -> 1
end
(** Ordered entry type *)

module OrderedElement = struct
  type t = Data_types.element_type
  let compare VAL VAL = 0
end
(** Ordered entry (element?) type *)

module EntrySet = Set.Make(OrderedEntry)
(** Set of entries. *)

module ElementSet = Set.Make(OrderedElement)
(** Set of elements. *)

module StringSet = Set.Make(String)
(** Set of strings. *)

module StringCoupleSet = Set.Make(struct 
  type t = string * string
  let compare (x1,x2) (y1,y2) = 
    match String.compare x1 y1 with
    | 0 -> String.compare x2 y2
    | x -> x 
end)
(** Set of strings. *)

type entry_search_state = {
  mutable pattern : string;
  mutable entries : EntrySet.t;
  mutable current_entry : entry_type;
  mutable page : int
}
(** State for entry search *)

type element_search_state = {
  mutable pattern : string;
  mutable elements : ElementSet.t;
  mutable current_element : element_type;
  mutable page : int;
  mutable regex : bool;
  mutable in_opams : StringSet.t;
  mutable in_mdls : StringCoupleSet.t
}
(** State for element search *)

type search_state =
  | Uninitialized
  | SearchEntry of entry_search_state
  | SearchElement of element_search_state
  (** Search state type within search page *)

let search_state = ref Uninitialized
(** Global variable that stores state of search page *)

let get_first_entry = EntrySet.min_elt
(** Gets entry from a set following order from below:
    - packages
    - libraries
    - modules
    - metas
    - sources *)

let get_first_element = ElementSet.min_elt
(** Gets element from a set following order from below:
    - vals *)

let state_of_args args =
  (* Match type of search *)
  match List.assoc "search" args with
  | "entry" ->
      let state = {
        pattern = "";
        entries = EntrySet.empty;
        current_entry = PACK;
        page = 1 }
      in
      (* construct the entry state *)
      List.iter (fun (key,elt) ->
          match key with
          | "search" -> ()
          | "pattern" -> state.pattern <- decode_query_val elt
          | "entry" -> state.entries <- EntrySet.add (entry_type_of_string elt) state.entries
          | "current" -> state.current_entry <- entry_type_of_string elt
          | "page" -> state.page <- int_of_string elt
          | _ -> raise @@ web_app_error (Printf.sprintf "state_of_args: key %s is not recognised" key)
        )
        args;
      SearchEntry state
  | "element" ->
      let state = {
        pattern = "";
        elements = ElementSet.empty;
        current_element = VAL;
        page = 1;
        regex = true;
        in_opams = StringSet.empty;
        in_mdls = StringCoupleSet.empty
      }
      in
      (* construct the element state *)
      List.iter (fun (key,elt) ->
          match key with
          | "search" -> ()
          | "pattern" -> state.pattern <- decode_query_val elt
          | "element" -> state.elements <- ElementSet.add (element_type_of_string elt) state.elements
          | "current" -> state.current_element <- element_type_of_string elt
          | "mode" -> (match elt with "text" -> state.regex <- false | _ -> state.regex <- true)
          | "page" -> state.page <- int_of_string elt
          | "opam" -> state.in_opams <- StringSet.add (decode_query_val elt) state.in_opams
          | "mdl" ->
            let elt = decode_query_val elt in
            begin
              match String.split_on_char '+' elt with
              | [mdl;opam] -> state.in_mdls <- StringCoupleSet.add (mdl,opam) state.in_mdls
              | _ -> raise @@ web_app_error (Printf.sprintf "state_of_args: mdl  value %s has wrong format" elt)
            end
          | _ -> raise @@ web_app_error (Printf.sprintf "state_of_args: key %s is not recognised" key)
        )
        args;
      SearchElement state
  | s -> raise @@ web_app_error (Printf.sprintf "state_of_args: search type %s is not recognised" s)
(** [state_of_args args] constructs and returns search state from the arguments passed with query in URL.
    Raises [Web_app_error] if argument are invalid *)

let state_to_args state =
  match state with
  | Uninitialized -> ""
  | SearchEntry {pattern; entries; current_entry; page} ->
      (* constructs query string from entry state *)
      Printf.sprintf "search=entry&pattern=%s&%s&current=%s&page=%d"
        (encode_query_val pattern)
        (String.concat "&"
         @@ List.map (fun elt -> ("entry=" ^ entry_type_to_string elt))
         @@ EntrySet.elements entries)
        (entry_type_to_string current_entry)
        page
  | SearchElement {pattern; elements; current_element; page; regex; in_opams; in_mdls} ->
      (* constructs query string from element state *)
      Printf.sprintf "search=element&pattern=%s&%s&current=%s&page=%d&mode=%s&%s&%s"
        (encode_query_val pattern)
        (String.concat "&"
         @@ List.map (fun elt -> ("element=" ^ element_type_to_string elt))
         @@ ElementSet.elements elements)
        (element_type_to_string current_element)
        page
        (if regex then "regex" else "text")
        (String.concat "&"
         @@ List.map (fun elt -> ("opam=" ^ encode_query_val elt))
         @@ StringSet.elements in_opams)
        (String.concat "&"
         @@ List.map (fun (mdl,opam) -> ("mdl=" ^ encode_query_val (mdl ^ "+" ^ opam)))
         @@ StringCoupleSet.elements in_mdls)
(** [state_to_args state] constructs query string from search state [state] *)

let get_entry_state () =
  match !search_state with
  | SearchEntry state -> state
  | _ -> raise @@ web_app_error "get_entry_state_info: current state isn't an entry state"
(** Get entry state from search state. Raises [Web_app_error] if current state isn't an entry state.
    Raises [Web_app_error] if current state isn't an entry state *)

let get_element_state () =
  match !search_state with
  | SearchElement state -> state
  | _ -> raise @@ web_app_error "get_element_state_info: current state isn't an element state"
(** Get element state from search state. Raises [Web_app_error] if current state isn't an element state
    Raises [Web_app_error] if current state isn't an element state *)

let entry_state_to_entry_info {pattern; current_entry; page; _} =
  let open Data_types in
  {
    entry = current_entry;
    pattern = pattern;
    last_id = (page - 1) * 50;
    starts_with = "^."
  }
(** Converts [entry_search_state] to [Data_types.entry_info] *)

let element_state_to_element_info {pattern; current_element; regex; page; in_opams; in_mdls; _} =
  let open Data_types in
  {
    element = current_element;
    pattern = pattern;
    last_id = (page - 1) * 50;
    mode = if regex then Regex else Text;
    conditions =
      List.map (fun opam -> In_opam opam) (StringSet.elements in_opams)
      @ List.map (fun (mdl,opam) -> In_mdl (mdl, opam)) (StringCoupleSet.elements in_mdls)
  }
(** Converts [element_search_state] to [Data_types.element_info] *)

let state_to_info state =
  match state with
  | Uninitialized -> raise @@ web_app_error "state_to_info: couldn't get info from uninitialized search"
  | SearchEntry state -> Entry (entry_state_to_entry_info state)
  | SearchElement state -> Element (element_state_to_element_info state)
(** Converts [search_state] to [Data_types.info].
    Raises [Web_app_error] if current state is uninitialised. *)

let get_input id = unopt @@ Html.CoerceTo.input @@ get_element_by_id id
(** Returns an input with given id *)

let update_entry_state () =
  (* Look up either checkbox is checked or not. If checked, add corresponding entry to the state *)
  let handle_checkbox id state =
    let entry =
      match id with
      | "fpackages" -> PACK
      | "flibraries" -> LIB
      | "fmodules" -> MOD
      | "fmetas" -> META
      | "fsources" -> SRC
      | _ -> raise @@
          web_app_error (Printf.sprintf "update_state_entry: can't find %s id" id)
    in
    if to_bool @@ (get_input id)##.checked
    then state.entries <- EntrySet.add entry state.entries
    else state.entries <- EntrySet.remove entry state.entries
  in
  (* Init entry search state *)
  let entry_state = {
    pattern = "";
    entries = EntrySet.empty;
    current_entry = PACK;
    page = 1
  } in
  let pattern_input = get_input "fpattern_entry" in
  let value = to_string pattern_input##.value##trim in
  entry_state.pattern <- value;
  (* Handle checkboxes *)
  handle_checkbox "fpackages" entry_state;
  handle_checkbox "flibraries" entry_state;
  handle_checkbox "fmodules" entry_state;
  handle_checkbox "fmetas" entry_state;
  handle_checkbox "fsources" entry_state;
  match entry_state.entries with
  | set when EntrySet.is_empty set -> false
  | set ->
      (* Set current entry to the least entry checked *)
      entry_state.current_entry <- get_first_entry set;
      search_state := SearchEntry entry_state;
      true
(** Looks for entry form in order to update search state. Checked checkboxes are handled to add
    corresponding to them entry to the set of entries in the current state. Function returns [true]
    if at least 1 checkbox is checked (search is made through at least 1 entry type) else returns [false]. *)

let getPackTags () =
  let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "pack_tag_container" in
  let cur_sset = ref StringSet.empty in
  if to_bool tag_container##hasChildNodes
  then begin
    let tags = tag_container##.childNodes in
    for i = 0 to tags##.length - 1
    do
      let tag_i = unopt @@ Html.CoerceTo.element @@ unopt @@ (tags##item i) in
      if (to_string tag_i##.innerText) = "" 
      then Dom.removeChild tag_container tag_i
      else cur_sset := StringSet.add (to_string tag_i##.innerText) !cur_sset;
    done
  end;
  !cur_sset
(** Retrieves the chosen packages in which search will be done *)


let getMdlTags () =
  let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "mod_tag_container" in
  let cur_sset = ref StringCoupleSet.empty in
  if to_bool tag_container##hasChildNodes
  then begin
    let tags = tag_container##.childNodes in
    for i = 0 to tags##.length - 1
    do
      let tag_i = unopt @@ Html.CoerceTo.element @@ unopt @@ (tags##item i) in
      let str_i = to_string tag_i##.innerText in
      match String.split_on_char ':' str_i with
      | [mdl_i;opam_i] -> cur_sset := StringCoupleSet.add (mdl_i, opam_i) !cur_sset
      | _ -> ()
    done
  end;
  !cur_sset
(** Retrieves the chosen modules in which search will be done *)

let update_element_state () =
  (* Look up either checkbox is checked or not. If checked, add corresponding element to the state *)
  let handle_checkbox id state =
    let element =
      match id with
      | "fvals" -> VAL
      | _ -> raise @@
          web_app_error (Printf.sprintf "update_element_state: can't find %s id" id)
    in
    if to_bool @@ (get_input id)##.checked
    then state.elements <- ElementSet.add element state.elements
    else state.elements <- ElementSet.remove element state.elements
  in
  (* Init entry search state *)
  let element_state =  {
    pattern = "";
    elements = ElementSet.empty;
    current_element = VAL;
    page = 1;
    regex = true;
    in_opams = StringSet.empty;
    in_mdls = StringCoupleSet.empty
  } in
  let pattern_input = get_input "fpattern_element" in
  let value = to_string pattern_input##.value##trim in
  element_state.pattern <- value;
  (* Handle checkboxes *)
  handle_checkbox "fvals" element_state;
  element_state.regex <- to_bool (get_input "fregex")##.checked;
  element_state.in_opams <- getPackTags () ;
  element_state.in_mdls <- getMdlTags () ;

  match element_state.elements with
  | set when ElementSet.is_empty set -> false
  | set ->
      (* Set current entry to the least entry checked *)
      element_state.current_element <- get_first_element set;
      search_state := SearchElement element_state;
      true
(** Looks for element form in order to update search state. Checked checkboxes are handled to add
    corresponding to them element to the set of elements in the current state. Function returns [true]
    if at least 1 checkbox is checked (search is made through at least 1 element type) else returns [false]. *)

let update_form () =
  let check_input id =
    (get_input id)##.checked := bool true
  in 
  let add_tag tag_container name = 
    let sp1 = Html.createSpan document in
    let sp2 = Html.createSpan document in
    sp1##.classList##add (js "tag"); 
    sp1##.innerText := js name;
    sp2##.classList##add (js "remove");
    sp2##.onclick := Html.handler (fun _ ->
      Dom.removeChild (unopt @@ sp1##.parentNode) sp1;
      _false
    );
    let tag_container_li = Html.createLi document in
    Dom.appendChild sp1 sp2;
    Dom.appendChild tag_container_li sp1;
    Dom.appendChild tag_container tag_container_li;
  in 
  match !search_state with
  | Uninitialized ->
      raise @@ web_app_error "update_form: search is unitialised"
  | SearchEntry state ->
      (* update entry form *)
      (get_input "fpattern_entry")##.value := js state.pattern;
      EntrySet.iter (fun entry -> check_input @@ "f" ^ entry_type_to_string entry)
        state.entries
  | SearchElement state ->
      (* update element form *)
      (get_input "fpattern_element")##.value := js state.pattern;
      ElementSet.iter (fun element ->  check_input @@ "f" ^ element_type_to_string element)
        state.elements;
      if state.regex then check_input "fregex";
      if not @@ StringSet.is_empty state.in_opams then begin
        check_input "showpacksearch";
        (get_element_by_id "nsbp")##.style##.display := js "block";
        let tag_packs = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "pack_tag_container" in 
        StringSet.iter (fun opam -> add_tag tag_packs opam) state.in_opams
      end;  
      if not @@ StringCoupleSet.is_empty state.in_mdls then begin
        check_input "showmodsearch";
        (get_element_by_id "nsbm")##.style##.display := js "block";
        let tag_mdls = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "mod_tag_container" in
        StringCoupleSet.iter (fun (mdl,opam) -> add_tag tag_mdls (mdl ^ ":" ^ opam)) state.in_mdls
      end
(** Looks for state in order to update corresponding form *)

let insert_packsUl_li : packages_jsoo t -> unit  = 
  fun (packages : packages_jsoo t) ->
  let packsUl = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "packsUl" in
  let input = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextpackages" in
  let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "pack_tag_container" in
  (* Start by removing all children from packsUl and replace them with result of new request 
     packsUl##.innerHTML = "";*)
  packsUl##.innerHTML := js "";
  let cur_tags = ref StringSet.empty in
  if to_bool tag_container##hasChildNodes
  then
    begin
      let chosen_tags = tag_container##.childNodes in
      for i = 0 to chosen_tags##.length - 1
      do
        let tag_li = unopt @@ Html.CoerceTo.element @@ unopt @@ (chosen_tags##item i) in
        cur_tags := StringSet.add (to_string (tag_li##.innerText)) !cur_tags;
      done
    end;
  foreach
    (fun i elt ->
       if i < 10
       then begin
         let pack_li = Html.createLi document in
         let name = to_string elt##.name in 
         pack_li##.onclick := Html.handler (fun _ ->
             if (StringSet.mem name!cur_tags)
             then Html.window##alert (js ("Error : package " ^ name ^ " already chosen,\nCheck for a different version"))
             else 
               begin
                 cur_tags := StringSet.add name !cur_tags;
                 let sp1 = Html.createSpan document in
                 let sp2 = Html.createSpan document in
                 sp1##.classList##add (js "tag"); 
                 sp1##.innerText := js name;
                 sp2##.classList##add (js "remove");
                 sp2##.onclick := Html.handler (fun _ ->
                     cur_tags := StringSet.remove name !cur_tags;
                     Dom.removeChild (unopt @@ sp1##.parentNode) sp1;
                     _false
                   );
                 let tag_container_li = Html.createLi document in
                 Dom.appendChild sp1 sp2;
                 Dom.appendChild tag_container_li sp1;
                 Dom.appendChild tag_container tag_container_li;
               end;
             input##.value := js "";
             packsUl##.style##.display := js "none";
             Headfoot.footerHandler();
             _false
           );
         let a_li = Html.createA document in
         Insertion.set_attr a_li "href" (js ("#"));
         Insertion.set_attr a_li "style" (js "color:green");
         a_li##.innerText := js name;
         pack_li##.style##.display := js "block";
         Dom.appendChild pack_li a_li;
         Dom.appendChild packsUl pack_li;
         Headfoot.footerHandler();
       end;
    )
    packages
(** preview packages propositions from which to choose *)

let insert_modsUl_li : modules_jsoo t -> unit  = 
  fun (modules : modules_jsoo t) ->
  let modsUl = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "modsUl" in
  let input = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextmodules" in
  let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "mod_tag_container" in
  (* Start by removing all children from packsUl and replace them with result of new request*) 
  modsUl##.innerHTML := js "";
  let cur_tags = ref StringSet.empty in
  if to_bool tag_container##hasChildNodes
  then
    begin
      let chosen_tags = tag_container##.childNodes in
      for i = 0 to chosen_tags##.length - 1
      do
        let tag_li = unopt @@ Html.CoerceTo.element @@ unopt @@ (chosen_tags##item i) in
        cur_tags := StringSet.add (to_string (tag_li##.innerText)) !cur_tags;
      done
    end;
  (* logs "printing selected tags  ----> ";
     StringSet.iter (fun e -> logs e) !cur_tags; *)
  foreach
    (fun i elt ->
       if i < 10
       then begin
         let pack_li = Html.createLi document in
         let pack_name = match String.index_opt (to_string elt##.opam) '.' with
         | Some i -> String.sub (to_string elt##.opam) 0 i
         | None -> to_string elt##.opam in
         let name = to_string (concat elt##.name (js @@ ":" ^ pack_name)) in
         pack_li##.onclick := Html.handler (fun _ ->
             if (StringSet.mem name !cur_tags)
             then Html.window##alert (js ("Error : package " ^ name ^ " already chosen,\nCheck for a different version"))
             else 
               begin
                 cur_tags := StringSet.add name !cur_tags;
                 let sp1 = Html.createSpan document in
                 let sp2 = Html.createSpan document in
                 sp1##.classList##add (js "tag"); 
                 sp1##.innerText := js name;
                 sp2##.classList##add (js "remove");
                 sp2##.onclick := Html.handler (fun _ ->
                     cur_tags := StringSet.remove name !cur_tags;
                     Dom.removeChild (unopt @@ sp1##.parentNode) sp1;
                     _false
                   );
                 let tag_container_li = Html.createLi document in
                 Dom.appendChild sp1 sp2;
                 Dom.appendChild tag_container_li sp1;
                 Dom.appendChild tag_container tag_container_li;
               end;
             input##.value := js "";
             modsUl##.style##.display := js "none";
             Headfoot.footerHandler();
             _false
           );
         let a_li = Html.createA document in
         Insertion.set_attr a_li "href" (js ("#"));
         let in_w = Html.createSpan document in
         Insertion.set_attr in_w "style" (js "color:black");
         in_w##.innerHTML := js " in ";
         let pkg = Html.createSpan document in 
         Insertion.set_attr pkg "style" (js "color:green");
         pkg##.innerHTML := js pack_name;
         a_li##.innerHTML := elt##.name;
         Dom.appendChild a_li in_w;
         Dom.appendChild a_li pkg;
         pack_li##.style##.display := js "block";
         Dom.appendChild pack_li a_li;
         Dom.appendChild modsUl pack_li;
         Headfoot.footerHandler();
       end;
    )
    modules
(** preview modules propositions from which to choose *)

(* let insert_Sources_fulltext : sources_search_result_jsoo t -> unit = 
  fun (sources : sources_search_result_jsoo t) ->
  sources *)
(** Insert Sources results for fulltext search *)

(* let preview_Sources pattern files =
  let sources_search_info = {
    pattern;
    files;
    is_regex = true;
    is_case_sensitive = true;
    last_match_id = 10; 
  } in
  Lwt.async @@
  Requests.send_generic_request
    ~request:(Requests.getSources_fulltext sources_search_info)
    ~callback:(fun source_search_result ->
        if not @@ (source_search_result.occs = [])
        then
          begin
            insert_Sources_fulltext (Objects.sources_search_result_to_jsoo source_search_result);
          end;  
        Lwt.return_unit
      )
    ~error:(fun err ->
        begin
          match err with
          | Unknown ->
              logs "Something went wrong in preview_Sources"
          | _ ->
              warn "Work on preview_Sources";
        end;
        Lwt.return_unit
      ) *)
(** Request to get sources for fulltext search (improve comments as ASAP) *)

let previewpacks pattern =
  let entry_info = {
    entry = PACK;
    last_id = 0;
    starts_with = "^.";
    pattern;
  } in
  Lwt.async @@
  Requests.send_generic_request
    ~request:(Requests.getEntries entry_info)
    ~callback:(fun pack_entries ->
        if not @@ Utils.empty_entries pack_entries
        then
          begin
            match pack_entries with
            | Opam packages ->
                insert_packsUl_li (Objects.packages_to_jsoo packages);
            | _ -> raise @@ web_app_error "Received object is not a package"
          end;
        Lwt.return_unit
      )
    ~error:(fun err ->
        begin
          match err with
          | Unknown ->
              logs "Something went wrong in previewpacks !";
          | _ ->
              warn "Work on this";
        end;
        Lwt.return_unit
      )
(** Request to get packages *)

let previewmods pattern =
  let entry_mod_info = {
    entry = MOD;
    last_id = 0;
    starts_with = "^.";
    pattern;
  } in
  Lwt.async @@
  Requests.send_generic_request
    ~request:(Requests.getEntries entry_mod_info)
    ~callback:(fun mod_entries ->
        if not @@ Utils.empty_entries mod_entries
        then
          begin
            match mod_entries with
            | Mdl modules ->
                insert_modsUl_li (Objects.modules_to_jsoo modules);
            | _ -> raise @@ web_app_error "Received object is not a module"
          end;
        Lwt.return_unit
      )
    ~error:(fun err ->
        begin
          match err with
          | Unknown ->
              logs "Something went wrong in previewmods !";
          | _ ->
              warn "Work on this";
        end;
        Lwt.return_unit
      )
(** Request to get modules *)

let set_handlers () =
  let entry_form = unopt @@ Html.CoerceTo.form @@ get_element_by_id "entry-form" in
  let element_form = unopt @@ Html.CoerceTo.form @@ get_element_by_id "element-form" in
  let update_button = unopt @@ Html.CoerceTo.button @@ get_element_by_id "update-filters" in
  let pack_checkbox = unopt @@ Html.CoerceTo.input @@ get_element_by_id "showpacksearch" in
  let mod_checkbox = unopt @@ Html.CoerceTo.input @@ get_element_by_id "showmodsearch" in
  let focus_packages_input = unopt @@ Html.CoerceTo.div @@ get_element_by_id "nsbp" in
  let focus_mods_input = unopt @@ Html.CoerceTo.div @@ get_element_by_id "nsbm" in
  let slider_show_hide = unopt @@ Html.CoerceTo.input @@ get_element_by_id "fregex" in
  let toggle_entry_form = unopt @@ Html.CoerceTo.button @@ get_element_by_id "col_entry" in
  let toggle_element_form = unopt @@ Html.CoerceTo.button @@ get_element_by_id "col_funcs" in
  (* let toggle_fulltext_form = unopt @@ Html.CoerceTo.button @@ get_element_by_id "col_fulltext" in *)
  let pack_tag_handling = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextpackages" in
  let mod_tag_handling = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextmodules" in
  let toogle_form formname = 
    let hidename = 
      match formname with
      | "element-search-content" -> "entry-search-content"
      | _ -> "element-search-content"
    in
    let show_this = get_element_by_id formname in
    let hide_this = get_element_by_id hidename in
    hide_this##.style##.display := js "none";
    show_this##.style##.display := js "block";
  in
  pack_checkbox##.onchange := Html.handler (fun _ ->
      let pack_to_hide = get_element_by_id "nsbp" in
      if pack_checkbox##.checked = _true
      then pack_to_hide##.style##.display := js "block"
      else pack_to_hide##.style##.display := js "none";
      _false
    );
  (* Hide/Show package input in element-form to specify packages in which search will be performed*)
  mod_checkbox##.onchange := Html.handler (fun _ ->
      let mod_to_hide = get_element_by_id "nsbm" in
      if mod_checkbox##.checked = _true
      then mod_to_hide##.style##.display := js "block"
      else mod_to_hide##.style##.display := js "none";
      _false
    );
  (* Hide/Show module input in element-form to specify modules in which search will be performed*)
  focus_packages_input##.onclick := Html.handler (fun _ ->
      let focus_to_packinput = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextpackages" in
      focus_to_packinput##focus;
      _false
    );
  (* Set focus on input text id="ftextpackages" when div of class newSearchbyPack and id=nsbp is clicked in element-form*)
  focus_mods_input##.onclick := Html.handler (fun _ ->
      let focus_to_modinput = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextmodules" in
      focus_to_modinput##focus;
      _false
    );
  (* Set focus on input text id="ftextmodules" when div of class newSearchbyModule and id=nsbm is clicked in element-form*)
  slider_show_hide##.onchange := Html.handler (fun _ ->
      let tr_tohide = get_element_by_id "tohide" in
      let tr_tohide2 = get_element_by_id "tohide2" in
      if slider_show_hide##.checked = _false
      then begin
        tr_tohide##.style##.display := js "none";
        tr_tohide2##.style##.display := js "none"
      end
      else begin
        tr_tohide##.style##.display := js "";
        tr_tohide2##.style##.display := js ""
      end;
      _false
    );
  (* Show / Hide package and module checkbox in element-form when slider is checked / unchecked *)
  toggle_entry_form##.onclick := Html.handler (fun _ ->
      toogle_form "entry-search-content";
      _false
    );
  (* Show entry-form's div when button having id="col_entry" is clicked and hide element-form's div *)
  toggle_element_form##.onclick := Html.handler (fun _ ->
      toogle_form "element-search-content";
      _false
    );
  (* Show element-form's div when button having id="col_funcs" is clicked and hide entry-form's div *)
  pack_tag_handling##.onkeyup := Html.handler (fun kbevent ->
      let cur_input_value = pack_tag_handling##.value##trim in
      let packsUl = get_element_by_id "packsUl" in
      let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "pack_tag_container" in
      begin
        match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
        | Some "Backspace" ->
            if (cur_input_value = js "" && (to_bool tag_container##hasChildNodes))
            then begin
              packsUl##.style##.display := js "none";
              let rm_pack_name_version = unopt @@ Html.CoerceTo.element @@ unopt @@ tag_container##.lastChild in
              Dom.removeChild tag_container rm_pack_name_version;
              Headfoot.footerHandler();
            end;
        | Some "Escape" -> 
            packsUl##.style##.display := js "none";
            Headfoot.footerHandler();
        | _ ->
            if (not (cur_input_value = js ""))
            then begin
              packsUl##.style##.display := js "block";
              previewpacks @@ (to_string cur_input_value);
            end
            else packsUl##.style##.display := js "none";
      end;
      _false
    );
  (* Remove and delete selected tag when pressing backspace in input having id=ftextpackages <---- Update THIS *)
  mod_tag_handling##.onkeyup := Html.handler (fun kbevent ->
      let cur_input_value = mod_tag_handling##.value##trim in
      let modsUl = get_element_by_id "modsUl" in
      let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "mod_tag_container" in
      begin
        match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
        | Some "Backspace" ->
            if (cur_input_value = js "" && (to_bool tag_container##hasChildNodes))
            then begin
              if to_bool tag_container##hasChildNodes
              then
                let rm_mod_name_version = unopt @@ Html.CoerceTo.element @@ unopt @@ tag_container##.lastChild in
                Dom.removeChild tag_container rm_mod_name_version;
                Headfoot.footerHandler();
            end;
        | Some "Escape" -> 
            modsUl##.style##.display := js "none";
            Headfoot.footerHandler();
        | _ ->
            if (not (cur_input_value = js ""))
            then begin
              modsUl##.style##.display := js "block";
              previewmods @@ to_string cur_input_value;
            end
            else modsUl##.style##.display := js "none";
      end;
      _false
    );
  (* Remove and delete selected tag when pressing backspace in input having id=ftextpackages <---- Update THIS *)
  (* Handler called when onsubmit event was generated by entry form *)
  entry_form##.onsubmit := Html.handler (fun _ ->
      (* if state was updated (at least 1 entry checkbox is checked) then redirect to the corresponding search page *)
      if update_entry_state ()
      then begin
        (* redirect to the page *)
        open_url @@ js @@ "search.html?" ^ state_to_args !search_state
      end;
      _false
    );
  (* Handler called when onsubmit event was generated by element form *)
  element_form##.onsubmit := Html.handler (fun _ ->
      (* if state was updated (at least 1 element checkbox is checked) then redirect to the corresponding search page *)
      if update_element_state ()
      then begin
        (* redirect to the page *)
        open_url @@ js @@ "search.html?" ^ state_to_args !search_state
      end;
      _false
    );
  (* Handler called when onclick event was generated by 'update filters' button *)
  update_button##.onclick := Html.handler (fun _ ->
      let form_div = get_element_by_id "forms" in
      (* display forms and hide update button *)
      update_button##.style##.display := js "none";
      form_div##.style##.display := js "";
      begin
        match !search_state with
        | SearchEntry _ -> toogle_form "entry-search-content"
        | SearchElement _ -> toogle_form "element-search-content"
        | _ -> raise @@ web_app_error "set_handlers: couldn't toogle to the form from unitialized page"
      end;
      (* fills form from state *)
      update_form ();
      _false
    )
(** Sets handlers to forms and buttons from search page. Submit event handler of a form redirect to the
    page with corresponding to search state results. Click event of a button 'update filters' shows filled
    form within result page that allows to update search request. *)

let initialise_state () =
  let args = Url.Current.arguments in
  if args != []
  then search_state := state_of_args args
(** Initialises state by looking up current URL arguments (query string) *)

let uninitialized_page () =
  let forms = get_element_by_id "forms" in
  forms##.style##.display := js "";
  Lwt.return_unit
(** Displays unitialized version of the page. *)

let pagination_info state total_number =
  (* get page number from search state *)
  let get_search_page state =
    match state with
    | Uninitialized -> raise @@ web_app_error "get_search_page: couldn't get page from unitialized page"
    | SearchEntry {page;_} -> page
    | SearchElement {page;_} -> page
  (* set page number in search state *)
  and set_search_page state page =
    match state with
    | Uninitialized -> raise @@ web_app_error "set_search_page: couldn't get page from unitialized page"
    | SearchEntry st -> SearchEntry {st with page}
    | SearchElement st -> SearchElement {st with page}
  in
  (* total number of pages *)
  let pages_number =
    if total_number <= 0
    then raise @@ web_app_error "pagination_info: total number couldn't be negativ "
    else (total_number - 1) / 50 + 1
  (* current page *)
  and page = get_search_page state in
  (* first and last page that will appear in page navigation bar *)
  let first,last =
    if pages_number > 9
    && 4 < page
    && page  < pages_number - 3
    then page - 4, page + 4
    else if not (4 < page)
    then 1, min 9 pages_number
    else if not (page < pages_number - 3)
    then max 1 (pages_number - 8), pages_number
    else max 1 (pages_number - 8), min 9 pages_number
  (* all pages that will appear inside page navigation bar and index of current page *)
  and pages = ref []
  and active_ind = ref 0 in
  for num = first to last do
    (* first and last id of an entry/element that appears on the page *)
    let first_id = (num - 1) * 50 + 1
    and last_id = if num = pages_number then total_number else num * 50
    and st = set_search_page state num in
    (* construct link to the page *)
    let href = "search.html?" ^ state_to_args st in
    let page_info = {num; interval=(first_id, last_id); href} in
    pages := page_info :: !pages;
    if num < page
    then incr active_ind
  done;
  let pages = List.rev !pages
  and active_ind = !active_ind in
  {active_ind; pages; total_number}
(** [pagination_info state total_number] constructs [pagination_info] according to the current state [state]
    and total number of entries/element that will be listed throughout all the pages.
    Raises [Web_app_error] if current state is uninitialized. *)

let insert_content info current current_number =
  (* insert pagination nav *)
  let insert_pagination () =
    let number = int_of_string current_number in
    let pages_info = pagination_info !search_state number in
    Insertion.insert_pagination pages_info;
    (* display results, entries bar, update button and set active nav *)
  and display_content () =
    let update_button = get_element_by_id "update-filters"
    and entries_nav = get_element_by_id "entries-nav"
    and result_div = get_element_by_id "result-div"
    and result_nav = get_element_by_id @@ current ^ "-results"
    and results = get_element_by_id "results-list" in
    update_button##.style##.display := js "";
    entries_nav##.style##.display := js "";
    result_div##.style##.display := js "";
    results##.innerHTML := js "";
    result_nav##.className := js "active-nav";
    (* insert message about empty search results *)
  and display_empty_message () =
    let update_button = get_element_by_id "update-filters"
    and entries_nav = get_element_by_id "entries-nav" in
    update_button##.style##.display := js "";
    entries_nav##.style##.display := js "";
    Insertion.write_message ("No " ^ current ^ " found.");
    (* insert error message *)
  and display_error err =
    let update_button = get_element_by_id "update-filters" in
    update_button##.style##.display := js "";
    (* print error message *)
    begin
      match err with
      | Invalid_regex ->
          Insertion.write_warning ("Invalid regex '" ^ pattern_from_info info ^ "'")
      | _ ->
          Insertion.write_warning ("Server error occured, please try again later.")
    end;
    Lwt.return_unit
  in
  match info with
  | Entry entry_info ->
      Requests.send_generic_request
        ~request:(Requests.getEntries entry_info)
        ~callback:(fun entries ->
            if not @@ empty_entries entries
            then begin
              display_content ();
              (* insert entries in search page *)
              Insertion.insert_entries_search entries;
              insert_pagination ();
            end
            else begin
              display_empty_message ()
            end;
            Lwt.return_unit
          )
        ~error:display_error
        ()
  | Element element_info ->
      Requests.send_generic_request
        ~request:(Requests.getElements element_info)
        ~callback:(fun elements ->
            if not @@ empty_elements elements
            then begin
              display_content ();
              (* insert elements in search page *)
              Insertion.insert_elements_search elements;
              insert_pagination ()
            end
            else begin
              display_empty_message ()
            end;
            Lwt.return_unit
          )
        ~error:display_error
        ()
(** Inserts content of the search page (search results and pagination nav bar).
    Empty results and server side errors generate specific to them message on the page. *)

let search_page () =
  (* get current entry/element as string from state *)
  let get_current state =
    match state with
    | Uninitialized -> raise @@ web_app_error "get_elt_from_state: search state is unitialised"
    | SearchEntry state -> entry_type_to_string state.current_entry
    | SearchElement state -> element_type_to_string state.current_element
  (* get current entry/element in the state *)
  and set_current state current =
    match state with
    | Uninitialized -> raise @@ web_app_error "set_current: search state is unitialised"
    | SearchEntry state -> SearchEntry {state with current_entry = entry_type_of_string current}
    | SearchElement state -> SearchElement {state with current_element = element_type_of_string current}
  (* get entries/elements as list of strings from state *)
  and get_elts_from_state state =
    match state with
    | Uninitialized -> raise @@ web_app_error "get_elts_from_state: search state is unitialised"
    | SearchEntry state -> List.map entry_type_to_string @@ EntrySet.elements state.entries
    | SearchElement state -> List.map element_type_to_string @@ ElementSet.elements state.elements
  (* construct and set the link to the navigation item *)
  and link_to_elt state link =
    let st =
      match state with
      | Uninitialized -> raise @@ web_app_error "link_to_elt: search state is unitialised"
      | SearchEntry state -> SearchEntry {state with page = 1 }
      | SearchElement state -> SearchElement {state with page = 1 }
    in
    let href = "search.html?" ^ state_to_args st in
    link##setAttribute (js "href") (js href)
  in
  let current = get_current !search_state in
  let elts = get_elts_from_state !search_state
  and info = state_to_info !search_state
  (* number of current entries/elements *)
  and current_number = ref "" in
  let%lwt () =
    (* for every entry/element type *)
    Lwt_list.iter_p (fun elt ->
        let nav_bar = get_element_by_id @@ elt ^ "-results" in
        (* set the link to the navigation item that leads to search page for corresponding entry/element *)
        let st = set_current !search_state elt in
        link_to_elt st nav_bar;
        Requests.send_generic_request
          ~request:(Requests.getNumber (state_to_info st))
          ~callback:(fun number ->
              (* display entries/elements number associated to the navigation item *)
              if elt = current then current_number:=number;
              let span = Html.createSpan document in
              nav_bar##.innerHTML := concat nav_bar##.innerHTML (js " ");
              span##.innerHTML := js ("("^number^")");
              Dom.appendChild nav_bar span;
              nav_bar##.style##.display := js "";
              Lwt.return_unit
            )
          ()
      )
      elts
  in
  (* insert page content *)
  insert_content info current !current_number
(** Constructs and displays entirely search page. *)

let onload () =
  (* set handlers to page elements *)
  set_handlers ();
  (* initialise state from query string, if exists, else state stays uninitialized *)
  initialise_state ();
  match !search_state with
  | Uninitialized -> uninitialized_page ()
  | _ -> search_page ()
(* Onload handler for search page *)
