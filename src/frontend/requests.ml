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

open Global
open Js_of_ocaml
open Data_types
open Lwt.Infix

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

let wrap_res ?error f = function
  | Ok x -> f x
  | Error exn -> let s = Printexc.to_string exn in match error with
    | None ->  ()
    | Some e -> e 500 (Some s)

let get0 ?post ?headers ?params ?msg ~host service  =
  EzReq_lwt.get0 host service ?msg ?post ?headers ?params 
let get1 ?post ?headers ?params  ?msg ~host service  arg =
  EzReq_lwt.get1 host service ?msg ?post ?headers  ?params arg  
let get2 ?post ?headers ?params ?msg ~host service  arg1 arg2 =
  EzReq_lwt.get2 host service ?msg ?post ?headers ?params arg1 arg2 


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
        pattern = state.pattern;
        starts_with = state.starts_with
    }

let api_host () =
  get0 ~host:web_host Service.info >>= function
     | Error _ -> Lwt.return_unit
     | Ok {www_apis} ->
       let api = List.nth www_apis (Random.int @@ List.length www_apis) in
       api_host := Some (EzAPI.BASE api);
       Lwt.return_unit




let getEntriesNumber () = 
    let api = get_api_host () in
    let entry = 
        match filename with
        | "packages.html" -> "packages"
        | "modules.html" -> "modules"
        | "libraries.html" -> "libraries"
        | "metas.html" -> "metas"
        | "sources.html" -> "sources"
        | _ -> assert false
    in
    let entry_info = entry_info_of_state ()
    and command = Utils.command_of_string @@ "count+" ^ entry in
        get2 ~host:api Services.exec_command command entry_info >>= function
            | Error _ ->Lwt.return_unit
            | Ok {result} -> 
                let indicator =  unopt @@ document##getElementById  (js "item-number") in
                indicator##.innerHTML := js (result ^ " " ^ entry);
                Lwt.return_unit


let sendRequest () = 
    let entry_info = entry_info_of_state ()  in
    begin 
        match filename with
        | "packages.html" -> begin
                (get1 ~host:(get_api_host ()) Services.package_entries entry_info >>= function
                    | Error _ ->Lwt.return_false
                    | Ok packages -> 
                        if not (packages = []) 
                        then begin
                            Insertion.insert_packages (Object.packages_to_jsoo packages);
                            Lwt.return_true
                        end
                        else Lwt.return_false);
            end
        | "modules.html" ->  begin
                get1 ~host:(get_api_host ()) Services.module_entries entry_info >>= function
                    | Error _ -> Lwt.return_false
                    | Ok modules -> 
                        if not (modules = []) 
                        then begin
                            Insertion.insert_modules (Object.modules_to_jsoo modules);
                            Lwt.return_true
                        end
                        else Lwt.return_false
            end
        | "libraries.html" -> begin
                get1 ~host:(get_api_host ()) Services.library_entries entry_info >>= function
                    | Error _ -> Lwt.return_false
                    | Ok libraries -> 
                        if not (libraries = []) 
                        then begin
                            Insertion.insert_libraries (Object.libraries_to_jsoo libraries);
                            Lwt.return_true
                        end
                        else Lwt.return_false
            end
        | "metas.html" -> begin
                get1 ~host:(get_api_host ()) Services.meta_entries entry_info >>= function
                    | Error _ -> Lwt.return_false
                    | Ok metas -> 
                        if not (metas = []) 
                        then begin
                            Insertion.insert_metas (Object.metas_to_jsoo metas);
                            Lwt.return_true
                        end
                        else Lwt.return_false
            end
        | "sources.html" -> begin
                get1 ~host:(get_api_host ()) Services.source_entries entry_info >>= function
                    | Error _ -> Lwt.return_false
                    | Ok sources -> 
                        if not (sources = []) 
                        then begin
                            Insertion.insert_sources (Object.sources_to_jsoo sources);
                            Lwt.return_true
                        end
                        else Lwt.return_false
            end
        | _ -> assert false
    end;