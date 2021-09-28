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

let genericHandler handler =
  Html.handler (fun _ ->
    Lwt.async (fun () ->
      Headfoot.activate_bar ();
      let%lwt () = Requests.api_host () in
      let%lwt () = handler () in
      Headfoot.footerHandler ();
      Lwt.return_unit
    );
    _false
  )

let main () =
  let footHandler = 
    Html.handler (fun _ -> 
      Headfoot.footerHandler ();
      _false
    )
  and searchPageHandler = (fun () ->
      let%lwt () = Search.onload () in
      Query.onload ()
    )
  in 
    window##.onresize := footHandler;
    if is_index_page 
    then window##.onload := genericHandler Index.onload
    else if is_search_page
    then window##.onload := genericHandler searchPageHandler
    else window##.onload := genericHandler Search.onload
  
let () = main ()
