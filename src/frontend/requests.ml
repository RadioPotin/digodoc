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
open Data_types
open Lwt.Infix
open Globals

(** Module [Requests] contains all front-end side requests to search-api with auxiliary types/functions. *)

(** {1 Auxiliary} *)

type 'res response = ('res, server_error_type) result
(** Polymorphic response type from search-api. *)

let handle_error err =
    Lwt.return @@
    match err with
    | EzReq_lwt_S.UnknownError _ ->  Error Unknown
    | EzReq_lwt_S.KnownError {error;_} -> Error error
(** Handler that converts [ez_api] error structure to [response] *)

let handle_response (resp:'res) : 'res response Lwt.t = 
    Lwt.return @@
        Ok resp
(** Handler that converts [ez_api] response structure to [response] *)

let get0 ?post ?headers ?params ?msg ~host service  =
    EzReq_lwt.get0 host service ?msg ?post ?headers ?params
(** Generic GET request to server [host] that exposes service [service] without arguments. *)
let get1 ?post ?headers ?params  ?msg ~host service  arg =
    EzReq_lwt.get1 host service ?msg ?post ?headers  ?params arg  
(** Same as [get0] but service waits for 1 argument instead. *)
let get2 ?post ?headers ?params ?msg ~host service  arg1 arg2 =
    EzReq_lwt.get2 host service ?msg ?post ?headers ?params arg1 arg2 
(** Same as [get0] but service waits for 2 arguments instead. *)

let get_web_host () =
    (* Get url of the current window *)
    let url () =
        match Url.url_of_string (Js.to_string window##.location##.href) with
        | Some url -> url
        | None -> raise @@ web_app_error "url: page has invalid url"
    in 
        EzAPI.BASE
            begin 
                match url() with
                | Url.Http hu ->
                    Printf.sprintf "http://%s:%d" hu.Url.hu_host hu.Url.hu_port
                | Url.Https hu ->
                    Printf.sprintf "https://%s:%d" hu.Url.hu_host hu.Url.hu_port
                | _ -> "http://localhost:8888"
            end
(** Get web server host name.
    Raises [Web_app_error] if url is invalid. *)

let api_host = ref None
(** Variable that stores a search-api host name. Initialised by sending [api_host] request. *)

let get_api_host () =
    match !api_host with
    | Some api -> api
    | None -> raise @@ web_app_error "get_api_host: service api_host hasn't been called yet"
(** Get content of [api_host] variable. 
    Raises [Web_app_error] if variable isn't initialised yet *)

module Service = struct

    open EzAPI

    let info_encoding =
        let open Json_encoding in
        conv
            (fun {www_apis} -> www_apis)
            (fun www_apis -> {www_apis}) @@
            obj1
                (req "apis" (list string))
    (** Encoding for 'info.json' file *)

    let info : (www_server_info, exn, no_security) service0 =
    service
      ~name:"info"
      ~descr:"Server info"
      ~output: info_encoding
      Path.(root // "info.json" )
    (** Service [info] allows to get search-api info from 'info.json' file, 
        that lists all the URLs that could be used to communicate with search-api. 
     *)
end
(** Module [Service] defines services exposed by front-end. *)

let default_error_handler err =
    begin 
        match err with
        | Invalid_regex -> warn "Search-api : Invalid regex"
        | Unknown -> warn "Search-api : Unknown error"
    end;
    Lwt.return_unit
(** Default error handler *)

let send_generic_request : 'res. 
        request:(unit -> 'res response Lwt.t) ->
        callback:('res -> unit Lwt.t) ->
        ?error:(server_error_type ->  unit Lwt.t) ->
        unit ->
        unit Lwt.t
    = fun
        ~request 
        ~callback
        ?(error = default_error_handler) 
        () 
    ->
        let%lwt resp = request () in
        match resp with
        | Ok res -> callback res
        | Error err -> error err
(** [send_generic_request ~request ~callback ~error] executes function that sends request [request] to search-api
    and execute continuations according to the request result. When request is succesfull [callback] is applied 
    on response. Otherwise, [error] is applied on an [Data_types.server_error]. If no [error] argument was specified -
    [default_error_handler] is used.*)

let api_host () =
  let host = get_web_host () in
  get0 ~host Service.info >>= function
     | Error _ -> 
        raise @@ web_app_error {|api_host: couldn't get the API info|} 
     | Ok {www_apis} ->
       (* Choose random api URL *)
       let api = List.nth www_apis (Random.int @@ List.length www_apis) in
       (* Init global var *)
       api_host := Some (EzAPI.BASE api);
       handle_response ()
(** Sends request to web-host service [Service.info] that sends the list of search-api URLs.
    URL that will be used to send other requests is choosen randomly from the list.
    Should be called before other requests.
    Raises [api_host] when couldn't get data from 'info.json' files.   *)

let getEntries entry_info () =
    get1 ~host:(get_api_host ()) Services.entries entry_info >>= function
        | Error err -> handle_error err
        | Ok entries -> (match entries with Lib _ -> logs "request Lib" | Src _ -> logs "request Src"| _ -> ());  handle_response entries
(** [getEntries entry_info ()] sends request [Services.entries] to search-api with [entry_info] passed 
    as argument of type [Data_types.entry_info]. Server returns 50 first entries that respect constraints 
    mentioned in [entry_info]. *)

let getElements element_info () = 
    get1 ~host:(get_api_host ()) Services.elements element_info >>= function
        | Error err -> handle_error err
        | Ok elements -> handle_response elements
(** Same as [getEntries] but instead deals with OCaml syntax elements (val,type,class, etc.). 
    Takes an argument [element_info] of type [Data_types.element_info]. *)

let search pattern () =
    get1 ~host:(get_api_host ()) Services.search pattern >>= function
        | Error err -> handle_error err
        | Ok search_result -> handle_response search_result
            (*Insertion.insert_search_result (Object.search_result_to_jsoo search_result);*)
(** [search pattern ()] sends request [Services.search] to search-api with [pattern] in argument.
    Returns search result of type [Data_types.search_result] that contains totaly 10 entries : 
    - 3 or less first packages
    - 3 or less first libraries
    - rest of first modules *)

let getNumber info () = 
    get2 ~host:(get_api_host ()) Services.exec_command Count info >>= function
        | Error err -> handle_error err
        | Ok {result} -> handle_response result
(** [getNumber info ()] sends request [Services.exec_command] to search-api with [info] passed 
    as argument of type [Data_types.info]. Server returns total number of entries that respects constraints 
    mentioned in [info]. *)

(*(* if there are no entries *)
            if Utils.empty_entries entries 
            then  handle_response false
            else begin
                (* continuation *)
                handler entries;
                (*Insertion.insert_packages (Object.packages_to_jsoo packages);*)
                handle_response true
            end*)






            (*(* if there are no elements *)
            if Utils.empty_elements elements 
            then  handle_response false
            else begin
                handler elements;
                (*Insertion.insert_packages (Object.packages_to_jsoo packages);*)
                handle_response true
            end*)
    (*end
    | MOD ->  begin
        get1 ~host:(get_api_host ()) Services.module_entries entry_info >>= function
            | Error err -> handle_error err
            | Ok modules -> 
                if not (modules = []) 
                then begin
                    handler modules;
                    (*Insertion.insert_modules (Object.modules_to_jsoo modules);*)
                    handle_response true
                end
                else handle_response false
    end
    | LIB -> begin
        get1 ~host:(get_api_host ()) Services.library_entries entry_info >>= function
            | Error err -> handle_error err
            | Ok libraries -> 
                if not (libraries = []) 
                then begin
                    handler libraries;
                    (*Insertion.insert_libraries (Object.libraries_to_jsoo libraries);*)
                    handle_response true
                end
                else handle_response false
    end
    | META -> begin
        get1 ~host:(get_api_host ()) Services.meta_entries entry_info >>= function
            | Error err -> handle_error err
            | Ok metas -> 
                if not (metas = []) 
                then begin
                    handler metas;
                    (*Insertion.insert_metas (Object.metas_to_jsoo metas);*)
                    handle_response true
                end
                else handle_response false
    end
    | SRC -> begin
        get1 ~host:(get_api_host ()) Services.source_entries entry_info >>= function
            | Error err -> handle_error err
            | Ok sources -> 
                if not (sources = []) 
                then begin
                    Insertion.insert_sources (Object.sources_to_jsoo sources);
                    handle_response true
                end
                else handle_response false
            end
        | _ -> assert false
    end*)



(*
let sendAdvancedSearchRequest entry entry_info () =
    match entry with
    | "packages" -> begin
            (get1 ~host:(get_api_host ()) Services.package_entries entry_info >>= function
                | Error err ->handle_error err
                | Ok packages -> 
                    if not (packages = []) 
                    then begin
                        Insertion.insert_search_packages (Object.packages_to_jsoo packages);
                        handle_response true
                    end
                    else handle_response false);
        end
    | "libraries" -> begin
            get1 ~host:(get_api_host ()) Services.library_entries entry_info >>= function
                | Error err -> handle_error err
                | Ok libraries -> 
                    if not (libraries = []) 
                    then begin
                        Insertion.insert_search_libraries (Object.libraries_to_jsoo libraries);
                        handle_response true
                    end
                    else handle_response false
        end
    | "modules" ->  begin
            get1 ~host:(get_api_host ()) Services.module_entries entry_info >>= function
                | Error err -> handle_error err
                | Ok modules -> 
                    if not (modules = []) 
                    then begin
                        Insertion.insert_search_modules (Object.modules_to_jsoo modules);
                        handle_response true
                    end
                    else handle_response false
        end
    | "metas" -> begin
            get1 ~host:(get_api_host ()) Services.meta_entries entry_info >>= function
                | Error err -> handle_error err
                | Ok metas -> 
                    if not (metas = []) 
                    then begin
                        Insertion.insert_search_metas (Object.metas_to_jsoo metas);
                        handle_response true
                    end
                    else handle_response false
        end
    | "sources" -> begin
            get1 ~host:(get_api_host ()) Services.source_entries entry_info >>= function
                | Error err -> handle_error err
                | Ok sources -> 
                    if not (sources = []) 
                    then begin
                        Insertion.insert_search_sources (Object.sources_to_jsoo sources);
                        handle_response true
                    end
                    else handle_response false
        end
    | "vals" -> begin
            get1 ~host:(get_api_host ()) Services.val_entries entry_info >>= function
                | Error err -> handle_error err
                | Ok vals -> 
                    if not (vals = []) 
                    then begin
                        Insertion.insert_search_vals (Object.vals_to_jsoo vals);
                        handle_response true
                    end
                    else handle_response false
        end
    | _ -> assert false
*)

