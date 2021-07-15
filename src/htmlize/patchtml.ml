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
  ignore header;
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

open Omd
(* Keeps us from processing url links *)
let is_url url =
  (String.length url >= 7 && String.equal (String.sub url 0 7) "http://")
  || (String.length url >= 8 && String.equal (String.sub url 0 8) "https://")

(* Keeps us from processing anchors *)
let is_anchor link =
  String.length link >= 1 && String.equal (String.sub link 0 1) "#"

(* Establish depth of given link relative to the root
 * Then prepend the corresponding number of .. to the basename
 * Then append index.html to fix link
 * *)
let handle_link link =

  (* Escape through this branch if the link points to an http(s) address,
   * or is an anchor, or link is empty
   * *)
  if is_url link.destination || is_anchor link.destination then
    link

  else
    (* Turn the link into a list in order to process it *)
    let li = String.split_on_char '/' link.destination in

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
       * Since we've established the depth of the path,
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
    {
      link with destination =
                  String.concat "/" (new_link @ ["index.html"]);
    }


let rec handle_inline = function
  | Concat (attr, attr_inline_list) ->
    Concat (attr, List.map handle_inline attr_inline_list)
  | Text (attr, s) -> Text (attr, s)
  | Emph (attr, attr_inline) -> Emph (attr, handle_inline attr_inline)
  | Strong (attr, attr_inline) ->
    Strong (attr, handle_inline attr_inline)
  | Code (attr, s) -> Code (attr, s)
  | Hard_break attr -> Hard_break (attr)
  | Soft_break attr -> Soft_break (attr)
  | Link (attr, attr_link) ->
    if attr_link.destination = "" then
      attr_link.label
    else
      Link (attr, handle_link attr_link)
  | Image (attr, attr_link) ->
    if attr_link.destination = "" then
      attr_link.label
    else
      Image (attr, handle_link attr_link)
  | Html (attr, s) -> Html (attr, s)

let rec handle_block = function
  | Paragraph (attr, inline)->
    Paragraph (attr, handle_inline inline)
  | List (attr, list_type, list_spacing, attr_block_list_list) ->
    List (attr, list_type, list_spacing,
      List.map (List.map handle_block) attr_block_list_list)
  | Blockquote (attr, attr_block_list) -> Blockquote (attr, attr_block_list)
  | Thematic_break attr -> Thematic_break attr
  | Heading (attr, i, attr_inline) ->
    Heading (attr, i, (handle_inline attr_inline))
  | Code_block (attr, s1, s2) -> Code_block (attr, s1, s2)
  | Html_block (attr, str) -> Html_block (attr, str)
  | Definition_list (attr, attr_def_elt_list) -> Definition_list (attr, attr_def_elt_list)

let handle_file doc  =
  List.map handle_block doc
