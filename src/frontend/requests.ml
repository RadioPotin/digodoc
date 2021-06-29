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
        | _ -> "https://docs.ocaml.pro"
    end 
   
let api_host = ref None

let get_api_host () =
    match !api_host with
    | Some api -> api
    | None -> assert false

let logs s = Firebug.console##log (Js.string s)

let wrap_res ?error f = function
  | Ok x -> f x
  | Error exn -> let s = Printexc.to_string exn in match error with
    | None -> logs s
    | Some e -> e 500 (Some s)

let get0 ?post ?headers ?params ?error ?msg ~host service f =
  EzReq.get0 host service ?msg ?post ?headers ?error ?params (wrap_res ?error f)
let get1 ?post ?headers ?params ?error ?msg ~host service f arg =
  EzRequest.ANY.get1 host service ?msg ?post ?headers ?error ?params arg (wrap_res ?error f) 
let get2 ?post ?headers ?params ?error ?msg ~host service f arg1 arg2 =
  EzRequest.ANY.get2 host service ?msg ?post ?headers ?error ?params arg1 arg2 (wrap_res ?error f) 


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
  get0 
    ~host:web_host 
    Service.info
    ~error:(fun code content ->
        let s = match content with
          | None -> "network error"
          | Some content -> "network error: " ^ string_of_int code ^ " -> " ^ content in
        logs s)
    (fun ({www_apis}) ->
       let api = List.nth www_apis (Random.int @@ List.length www_apis) in
       api_host := Some (EzAPI.BASE api);)


let getEntriesNumber () = 
    let entry = 
        match filename with
        | "index.html" -> "packages"
        | "modules.html" -> "modules"
        | "libraries.html" -> "libraries"
        | "metas.html" -> "metas"
        | "sources.html" -> "sources"
        | _ -> assert false
    in
    let entry_info = entry_info_of_state ()
    and command = Utils.command_of_string @@ "count+" ^ entry in
    get2 
        ~host:(get_api_host ())
        Services.exec_command 
        (fun {result} -> 
            let indicator =  unopt @@ document##getElementById  (js "item-number") in
            indicator##.innerHTML := js (result ^ " " ^ entry)) 
        command 
        entry_info 

let sendRequest () =
    let entry_info = entry_info_of_state () 
    and added = ref false in 
    begin 
        match filename with
        | "index.html" -> 
            get1
                ~host:(get_api_host ())
                Services.package_entries
                (fun packages -> 
                    if not (packages = []) 
                    then begin
                        added := true;
                        Insertion.insert_packages (Object.packages_to_jsoo packages)
                    end)
                entry_info;
        
        | "modules.html" -> 
            get1
                ~host:(get_api_host ())
                Services.module_entries
                (fun modules -> 
                    if not (modules = []) 
                    then begin
                        added := true;
                        Insertion.insert_modules (Object.modules_to_jsoo modules)
                    end)
                entry_info;
        | "libraries.html" -> 
            get1
                ~host:(get_api_host ())
                Services.library_entries
                (fun libraries -> 
                    if not (libraries = []) 
                    then begin
                        added := true;
                        Insertion.insert_libraries (Object.libraries_to_jsoo libraries)
                    end)
                entry_info;
        | "metas.html" -> 
            get1
                ~host:(get_api_host ())
                Services.meta_entries
                (fun metas -> 
                    if not (metas = []) 
                    then begin
                        added := true;
                        Insertion.insert_metas (Object.metas_to_jsoo metas)
                    end)
                entry_info;
        | "sources.html" -> 
            get1
                ~host:(get_api_host ())
                Services.source_entries
                (fun sources -> 
                    if not (sources = []) 
                    then begin
                        added := true;
                        Insertion.insert_sources (Object.sources_to_jsoo sources)
                    end)
                entry_info;
        | _ -> assert false;
    end;
    !added
