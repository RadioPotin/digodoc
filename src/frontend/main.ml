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

(** Module [Main] contains an entry point for front-end. Principally it either defines common behaviour for all pages
    either uses specific to page onload handler. *)

let genericHandler handler =
  Html.handler (fun _ ->
      Lwt.async (fun () ->
          (* activate header *)
          Headfoot.activate_bar ();
          (* initialise api host *)
          Requests.send_generic_request
            ~request:Requests.api_host
            ~callback:(fun _ ->
                (* call specific to page handler handler *)
                let%lwt _ = Lwt.catch handler
                    (fun error -> begin 
                          match error with
                          | Web_app_error errors -> 
                              (** print all occured errors *)
                              print_web_app_error errors
                          | _ -> err "Undefined error"
                        end;
                        Lwt.return_unit
                    ) in

                (* adjust footer *)
                Headfoot.footerHandler ();
                Lwt.return_unit
              )
            ()
        );
      _false
    )
(** [genericHandler handler] is a generic onload handler used by all the pages. It determines some common behaviour,
    like initilisation of api host, header activation and footer adjustment. [handler] is a specific handler that is executed
    in the common for all pages context. Catches and prints [Web_app_error] *)

let main () =
  (* footer handler *)
  let footHandler = 
    Html.handler (fun _ -> 
        Headfoot.footerHandler ();
        _false
      )
  (* search page specific handler that regroups two specific handlers *)
  and searchPageHandler = (fun () ->
      let%lwt () = Search.onload () in
      SearchAdvanced.onload ()
    )
  in 
  (* footer handler when resized *)
  window##.onresize := footHandler;
  if is_index_page 
  then window##.onload := genericHandler Index.onload
  else if is_search_page
  then window##.onload := genericHandler searchPageHandler
  else window##.onload := genericHandler Search.onload
(** Entry point. Looks up for page type and calls [genericHandler] with specific to this page handler as argument. *)

let () = main ()
