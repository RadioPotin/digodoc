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

(* Server side possible errors *)


(* Response type *)
type 'res response = ('res, error) result

(* Polymorphic GET requests *)
let get0 ?post ?headers ?params ?msg ~host service  =
  EzReq_lwt.get0 host service ?msg ?post ?headers ?params 
let get1 ?post ?headers ?params  ?msg ~host service  arg =
  EzReq_lwt.get1 host service ?msg ?post ?headers  ?params arg  
let get2 ?post ?headers ?params ?msg ~host service  arg1 arg2 =
  EzReq_lwt.get2 host service ?msg ?post ?headers ?params arg1 arg2 


let url () =
  match Url.url_of_string (Js.to_string window##.location##.href) with
  | None -> assert false
  | Some url -> url

let web_host =
  EzAPI.BASE
    begin 
        match url() with
        | Url.Http hu ->
            Printf.sprintf "http://%s:%d" hu.Url.hu_host hu.Url.hu_port
        | Url.Https hu ->
            Printf.sprintf "https://%s:%d" hu.Url.hu_host hu.Url.hu_port
        | _ -> "http://localhost:8888"
    end 

let api_host = ref None

let get_api_host () =
    match !api_host with
    | Some api -> api
    | None -> assert false


module Service = struct

    open EzAPI

    let info : (www_server_info, exn, no_security) service0 =
    service
      ~name:"info"
      ~descr:"Server info"
      ~output: Encoding.info_encoding
      EzAPI.Path.(root // "info.json" )
 
end


let entry_info_of_state () =
    {
        last_id = Int64.of_int state.last_id;
        pattern = encode_path state.pattern;
        starts_with = state.starts_with
    }

let handle_error err =
    Lwt.return @@
    match err with
    | EzReq_lwt_S.KnownError {code;_} when code = 500 ->
        Error InvalidRegex
    | _ -> Error Unknown

let handle_response (resp:'res) : 'res response Lwt.t = 
    Lwt.return @@
    Ok resp

let default_error_handler err =
    begin 
        match err with
        | InvalidRegex -> warn "Invalid regex"
        | Unknown -> warn "Unknown error"
    end;
    Lwt.return_unit

let send_generic_request 
        ~(request: unit -> 'res response Lwt.t) 
        ~(callback: 'res -> unit Lwt.t)
        ?(error: error ->  unit Lwt.t = default_error_handler) 
        ()
        : unit Lwt.t =
    let%lwt resp = request () in
    match resp with
    | Ok res -> callback res
    | Error err -> error err

let api_host () =
  get0 ~host:web_host Service.info >>= function
     | Error err -> handle_error err
     | Ok {www_apis} ->
       let api = List.nth www_apis (Random.int @@ List.length www_apis) in
       api_host := Some (EzAPI.BASE api);
       handle_response ()

let getEntriesNumber ?entry_info entry () = 
    let api = get_api_host () in
    let entry_info = 
        match entry_info with
        | Some ei -> ei
        | None -> entry_info_of_state ()
    and command = Utils.command_of_string @@ "count+" ^ entry in
        get2 ~host:api Services.exec_command command entry_info >>= function
            | Error err -> handle_error err
            | Ok {result} -> handle_response result


let sendRequest () = 
    let entry_info = entry_info_of_state ()  in
    begin 
        match filename with
        | "packages.html" -> begin
                get1 ~host:(get_api_host ()) Services.package_entries entry_info >>= function
                    | Error err -> handle_error err
                    | Ok packages -> 
                        if not (packages = []) 
                        then begin
                            Insertion.insert_packages (Object.packages_to_jsoo packages);
                            handle_response true
                        end
                        else handle_response false
            end
        | "modules.html" ->  begin
                get1 ~host:(get_api_host ()) Services.module_entries entry_info >>= function
                    | Error err -> handle_error err
                    | Ok modules -> 
                        if not (modules = []) 
                        then begin
                            Insertion.insert_modules (Object.modules_to_jsoo modules);
                            handle_response true
                        end
                        else handle_response false
            end
        | "libraries.html" -> begin
                get1 ~host:(get_api_host ()) Services.library_entries entry_info >>= function
                    | Error err -> handle_error err
                    | Ok libraries -> 
                        if not (libraries = []) 
                        then begin
                            Insertion.insert_libraries (Object.libraries_to_jsoo libraries);
                            handle_response true
                        end
                        else handle_response false
            end
        | "metas.html" -> begin
                get1 ~host:(get_api_host ()) Services.meta_entries entry_info >>= function
                    | Error err -> handle_error err
                    | Ok metas -> 
                        if not (metas = []) 
                        then begin
                            Insertion.insert_metas (Object.metas_to_jsoo metas);
                            handle_response true
                        end
                        else handle_response false
            end
        | "sources.html" -> begin
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
    end

let sendSearchRequest pattern () =
    get1 ~host:(get_api_host ()) Services.search pattern >>= function
        | Error err -> handle_error err
        | Ok search_result -> 
            Insertion.insert_search_result (Object.search_result_to_jsoo search_result);
            handle_response ()

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