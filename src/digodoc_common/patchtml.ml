open Soup


(* arguments must be valid html, header and footer will be inserted,
   respectively, at the top and bottom of the body *)
let edit_html ?header ?footer ~head_childs html =
  let html = parse html in
  let body = html $ "body" in
  let opt_inserter f o id =
    Option.iter (fun e ->
      f ( parse e $$ id ) ) o
  in
  opt_inserter
    (fun e -> rev e |> iter (fun e -> prepend_child body e))
    header
    "#header";
  opt_inserter
    (iter (fun e -> append_child body e))
    footer
    "#footer";
  let head = html $ "head" in
  List.iter (fun (id,child) ->
    opt_inserter
      (iter (fun e -> append_child head e))
      (Some child)
      id)
    head_childs;
  to_string html

let change_link_highlight html =
  let get_highlight_script scripts =
    let scripts = to_list scripts in
    List.find (fun script ->
      match attribute "src" script with
      | Some attr ->
        EzString.ends_with ~suffix:"highlight.pack.js" attr
      | None -> false)
      scripts
  in

  let html = parse html in
  let scripts = (html $ "head") $$ "script" in
  let script = get_highlight_script scripts in
  let new_src = match attribute "src" script with
    | Some attr ->
      let path_l = String.split_on_char '/' attr in
      let new_path_l = ".." :: EzList.take ((List.length path_l)-1) path_l in
      String.concat "/" new_path_l ^ "/static/scripts/highlight.pack.js"
    | None -> assert false
  in
  set_attribute "src" new_src script;
  to_string html

let change_link_to_upper_directory html upper =
  let get_first_elt node =
    Option.get @@ first @@ elements @@ children node
  in
  let html = parse html in
  let body = html $ "body" in
  let div = Option.get @@
    element @@
    Option.get @@
    nth 2 @@
    elements @@
    children body
  in
  let link = get_first_elt div |> get_first_elt |> get_first_elt in
  let href = EzFile.dirname (Option.get @@ attribute "href" link) ^ "/" ^ upper in
  let new_href =
    match upper with
    | "modules.html" -> "../../" ^ href
    | _ -> "../" ^ href
  in
  if upper = "modules.html"
  then begin
    let next_link = next_siblings link |> nth 2 |> Option.get |> element |> Option.get  in
    set_attribute "href" new_href next_link
  end;
  set_attribute "href" new_href link;
  to_string html

let append_local_search html = 
  let get_first_elt node =
    Option.get @@ first @@ elements @@ children node
  in
  let html = parse html in
  let body = html $ "body" in
  let div = Option.get @@
    element @@
    Option.get @@
    nth 2 @@
    elements @@
    children body
  in
  let nav = get_first_elt div |> get_first_elt in 
  iter (fun input -> append_child nav input) @@
    (parse {|<input id="localsearch" placeholder="Search in ..." autocomplete="off" style="float:right">|} 
     $$ "#localsearch");
  to_string html
  
open Omd
(* Keeps us from processing url links *)
let is_url url =
  (String.length url >= 7 && String.equal (String.sub url 0 7) "http://")
  || (String.length url >= 8 && String.equal (String.sub url 0 8) "https://")

(* Keeps us from processing mail addresses *)
let is_mail mail =
  (String.length mail >= 7 && String.equal (String.sub mail 0 7) "mailto:")

(* Keeps us from processing anchors *)
let is_anchor link =
  String.length link >= 1 && String.equal (String.sub link 0 1) "#"

(* Establish depth of given link relative to the root
 * Then prepend the corresponding number of .. to the basename
 * Then append index.html to fix link
 * *)
let handle_link ~is_raw link =

  (* Escape through this branch if the link points to an http(s) address,
   * or is an anchor, or link is empty
   * *)
  if is_url link || is_anchor link || is_mail link then
    link

  else
    let target =
      if is_raw then
        ["raw";Filename.basename link]
      else
        ["index.html"]
    in
    (* Turn the link into a list in order to process it *)
    let li = String.split_on_char '/' link in

    (* Filter out the empty strings resulting from forms like:
     * path/to/directory/ = [path;to;directory;""]
     * They're are not counted as far a depth goes.
     * And they do not change the semantics of the link
     * *)
    let llink = List.filter (fun x -> x <> "." && x <> "") li in

    let dep llink =
      let neg_depth = List.filter (fun x -> x = "..") llink in
      List.length llink - List.length neg_depth
    in

    let initial_depth = dep llink in

    let rec process link =
      match link with
      | [] -> []
      (* This handles the case when the target is under multiple subdirs like example/foo/bar
       * Since we've established the depth of the pat,
       * we know when to get to the appending of "index.html" file,
       * hence having to preppend one last level ".." before doing so.
       * *)
      | _dir :: dirs ->
        if initial_depth = dep link then
          ".."::link
        else
          process dirs
    in
    let new_link =

      if initial_depth = 0 && llink <> [] then
        [".."] @ llink
      else
        process llink
    in

    (* [newlink] string list variable should now hold:
     * 1. correct amound of .. to reach the root
     * 2. the correct basename, whether it be foo/ or foo/bar/bang
     * 3. index.html appended to the tail to fix broken MD link
     * *)
    String.concat "/" (new_link @ target)

let handle_link_omd ~is_raw link =
  {
    link with destination = handle_link ~is_raw link.destination
  }

open Ez_html
open Xml
open Xml_types

(* Function to change static links found in HTML code in md files *)
let edit_attr ~is_raw (attr_name, attr_value) =
  match attr_name with
  | "href" | "src" ->
    let newlink = handle_link ~is_raw attr_value
    in (attr_name, newlink)
  | _attr -> (attr_name, attr_value)

let rec markdown_html_patch = function
  | PCData data -> PCData data
  | Element (tag, attrs, children) ->
    match tag with
    | "a" -> Element (tag, List.map (edit_attr ~is_raw:false) attrs, List.map markdown_html_patch children)
    | "img" -> Element (tag, List.map (edit_attr ~is_raw:true) attrs, List.map markdown_html_patch children)
    | "source" -> Element (tag, List.map (edit_attr ~is_raw:true) attrs, List.map markdown_html_patch children)
    | _t -> Element (tag, attrs, List.map markdown_html_patch children)


let handle_html html =
  try
    let h = Xml.parse_string html in
    let xml = markdown_html_patch h in
    to_string_fmt xml

  with Error (msg, _loc) ->

  match msg with
  | EndOfTagExpected msg ->
    Format.eprintf {|!!Warning!!@.
    Tag "%s" has no corresponding closing tag
    OR character '/' not found in non-container tag:@.%s@.|} msg html;
    html
  | _ -> Format.printf "Ez_html.Xml error (Patchtml.handle_html)";
    html

let rec handle_inline = function
  | Concat (attr, attr_inline_list) -> Concat (attr, List.map handle_inline attr_inline_list)
  | Text (attr, s) -> Text (attr, s)
  | Emph (attr, attr_inline) -> Emph (attr, handle_inline attr_inline)
  | Strong (attr, attr_inline) -> Strong (attr, handle_inline attr_inline)
  | Code (attr, s) -> Code (attr, s)
  | Hard_break attr -> Hard_break (attr)
  | Soft_break attr -> Soft_break (attr)
  | Link (attr, attr_link) ->
    if attr_link.destination = "" then
      attr_link.label
    else
      Link (attr, handle_link_omd ~is_raw:false attr_link)
  | Image (attr, attr_link) ->
    if attr_link.destination = "" then
      attr_link.label
    else
      Image (attr, handle_link_omd ~is_raw:true attr_link)
  | Html (attr, s) -> Html (attr, handle_html s)

let rec handle_block = function
  | Paragraph (attr, inline)-> Paragraph (attr, handle_inline inline)
  | List (attr, list_type, list_spacing, attr_block_list_list) ->
    List (attr, list_type, list_spacing,
      List.map (List.map handle_block) attr_block_list_list)
  | Blockquote (attr, attr_block_list) -> Blockquote (attr, List.map handle_block attr_block_list)
  | Thematic_break attr -> Thematic_break attr
  | Heading (attr, i, attr_inline) ->
    Heading (attr, i, (handle_inline attr_inline))
  | Code_block (attr, s1, s2) -> Code_block (attr, s1, s2)
  | Html_block (attr, str) -> Html_block (attr, handle_html str)
  | Definition_list (attr, attr_def_elt_list) -> Definition_list (attr, attr_def_elt_list)

let handle_file doc  =
  List.map handle_block doc

(* This renders raw images in Source indexes *)
let render_img img =
  Format.sprintf {|<img src="raw/%s" class="centered-image"/>|} img
