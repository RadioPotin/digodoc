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
      