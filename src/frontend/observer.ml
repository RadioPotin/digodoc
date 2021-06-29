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
open IntersectionObserver
open Global

let observer : intersectionObserver t=
  let f : intersectionObserverEntry t js_array t -> intersectionObserver t -> unit = 
    (fun entries _ ->
        let entry = unoptdef @@ array_get entries 0 in
        if entry##.isIntersecting = _true then begin
          Dom.removeChild (get_main_div ()) load_div;
          let added = Requests.sendRequest () in
          if added then begin
            state.last_id <- state.last_id + 50;
            Dom.appendChild (get_main_div ()) load_div
          end
        end;
    )
  and options : intersectionObserverOptions t = empty_intersection_observer_options () in
  let a : float js_array t= new%js array_empty  in
  array_set a 0 1.0;
  options##.threshold := a;
  new%js intersectionObserver (wrap_callback f) options
