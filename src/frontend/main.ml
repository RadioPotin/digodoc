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
open Global


let clear_index_page () =
  let load_opt = document##getElementById (js "load_div") in
  Opt.iter load_opt (fun _ -> Dom.removeChild (get_main_div ()) load_div);
 
  for index = 48 to 57 do
    let set_opt = document##getElementById (js @@ "packages-" ^ (fromCharCode index)) in
    Opt.iter set_opt (fun set -> 
      set##.innerHTML := js "";
      let title = unopt @@ document##getElementById (js @@ "name-" ^ (fromCharCode index)) in
      title##.style##.display := js "none")
  done;
  for index = 97 to 122 do
    let set_opt = document##getElementById (js @@ "packages-" ^ (fromCharCode index)) in
    Opt.iter set_opt (fun set -> 
      set##.innerHTML := js "";
      let title = unopt @@ document##getElementById (js @@ "name-" ^ (fromCharCode index)) in
      title##.style##.display := js "none")
  done


let update_index_page () =
  let added = Requests.sendRequest () in
  if added then begin
    state.last_id <- state.last_id + 50;
    Dom.appendChild (get_main_div ()) load_div
  end;
  Requests.getEntriesNumber ();
  Headfoot.footerHandler ()


let set_start_letter ch = 
  state.starts_with <- ch;
  state.last_id <- 0;
  state.pattern <- "~";
  let search_elt = unopt @@ document##getElementById (js "search") in
  let search = unopt @@ Html.CoerceTo.input search_elt in
  search##.value := js "";
  clear_index_page ();
  update_index_page ()


let onload _ =
  Requests.api_host ();
  Headfoot.footerHandler ();
  if in_root_directory && not (filename = "about.html") 
  then begin
    main_div := Some (getElementById "by-name");
    for index = 48 to 57 do
      let title_opt = document##getElementById (js @@ "name-" ^ fromCharCode index) in
      Opt.iter title_opt (fun title -> 
        title##.style##.display := js "none")
    done;
    for index = 97 to 122 do
      let title_opt = document##getElementById (js @@ "name-" ^ fromCharCode index) in
      Opt.iter title_opt (fun title -> 
        title##.style##.display := js "none")
    done;
    ignore @@ Requests.sendRequest ();
    Requests.getEntriesNumber ();
    state.last_id <- state.last_id + 50;
    Dom.appendChild (get_main_div ()) load_div;
    let selected_elt = unopt @@ document##querySelector (js "#load_div") in
    Observer.observer##observe selected_elt
  end;
  let search = unopt @@ Html.CoerceTo.input @@ getElementById "search" in
  search##.onkeyup := Html.handler (fun _ ->
      let re = search##.value in
      if in_root_directory && not (filename = "about.html") 
      then begin
        clear_index_page ();
        let input = re##trim in
        begin  
          if input##.length > 0
          then state.pattern <- to_string input
          else state.pattern <- "~";
        end;
        state.last_id <- 0;
        update_index_page ()
      end;
      _false
    );
  _false

(* TODO: different onload in function to the type of page : index, doc, source *)
let main () =
  let footHandler = Html.handler (fun _ -> Headfoot.footerHandler (); _false) in
  window##.onresize := footHandler;
  if in_root_directory
  then window##.onload := Html.handler onload
  else window##.onload := footHandler
  
let () = main ()
