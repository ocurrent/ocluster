open! Lwt.Infix
open Capnp_rpc_lwt
module R = Ocluster_api.Raw

let process_out stdout_push stderr_push complete_u =
  let module P = R.Service.ProcessOut in
  P.local
  @@ object
       inherit P.service

       method stdout_impl params release_param_caps =
         let open P.Stdout in
         let buf = Params.chunk_get params in
         release_param_caps ();
         stdout_push (Some buf);
         Service.return @@ Service.Response.create_empty ()

       method stderr_impl params release_param_caps =
         let open P.Stderr in
         let buf = Params.chunk_get params in
         release_param_caps ();
         stderr_push (Some buf);
         Service.return @@ Service.Response.create_empty ()

       method complete_impl params release_param_caps =
         let open P.Complete in
         let exit_code = Params.exit_code_get params in
         release_param_caps ();
         stdout_push None;
         stderr_push None;
         Lwt.wakeup complete_u exit_code;
         Service.return @@ Service.Response.create_empty ()
     end

let process_in stdin_push cancel_t =
  let module P = R.Service.ProcessIn in
  P.local
  @@ object
       inherit P.service

       method stdin_impl params release_param_caps =
         let open P.Stdin in
         let buf = Params.chunk_get params in
         release_param_caps ();
         stdin_push (Some buf);
         Service.return_empty ()

       method cancel_impl _params release_param_caps =
         let open! P.Cancel in
         release_param_caps ();
         stdin_push None;
         cancel_t ();
         Service.return_empty ()
     end

let agent =
  let module Agent = R.Service.Agent in
  Agent.local
  @@ object
       inherit Agent.service

       method exec_impl params release_param_caps =
         let open Agent.Exec in
         let command = Params.cmd_get params in
         release_param_caps ();
         let binary = R.Reader.Command.binary_get command in
         let args = R.Reader.Command.args_get_array command in
         Service.return_lwt @@ fun () ->
         Agents.exec (binary, args) >|= function
         | Error (`Msg msg) -> Service.fail msg
         | Ok exit_code ->
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.exit_code_set results exit_code;
             Ok response

       method spawn_impl params release_param_caps =
         let open Agent.Spawn in
         let command = Params.cmd_get params in
         let pout = Params.pout_get params in
         release_param_caps ();
         match pout with
         | None -> Service.fail "must specify callback process"
         | Some pout ->
             let binary = R.Reader.Command.binary_get command in
             let args = R.Reader.Command.args_get_array command in
             let stdin_stream, stdin_push = Lwt_stream.create () in
             let cancel_t = Agents.spawn (binary, args) pout stdin_stream in
             let pin = process_in stdin_push cancel_t in
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.pin_set results (Some pin);
             Service.return response
     end

let cluster t =
  let module Cluster = R.Service.Cluster in
  Cluster.local
  @@ object
       inherit Cluster.service

       method register_impl params release_param_caps =
         let open Cluster.Register in
         let hostname = Params.hostname_get params in
         let callback = Params.callback_get params in
         release_param_caps ();
         match callback with
         | None -> Service.fail "no callback specified"
         | Some callback -> (
             Logs.info (fun l -> l "Registered %s" hostname);
             Service.return_lwt @@ fun () ->
             Capability.inc_ref callback;
             Agents.register ~hostname callback t >>= function
             | Ok () -> Lwt.return_ok @@ Service.Response.create_empty ()
             | Error (`Msg msg) ->
                 Capability.dec_ref callback;
                 (* TODO add agent unregister to decr cap *)
                 Service.fail msg )

       method find_impl params release_param_caps =
         let open Cluster.Find in
         let hostname = Params.hostname_get params in
         release_param_caps ();
         match hostname with
         | "" -> Service.fail "must specify a hostname"
         | hostname -> (
             match Agents.find ~hostname t with
             | None -> Service.fail "hostname not found"
             | Some agent ->
                 let response, results =
                   Service.Response.create Results.init_pointer
                 in
                 Results.callback_set results (Some agent.agent_cap);
                 Service.return response )

       method list_impl _params release_param_caps =
         let open Cluster.List in
         release_param_caps ();
         let _response, _results =
           Service.Response.create Results.init_pointer
         in
         let _agent_caps =
           Agents.list t |> List.map (fun a -> a.Agents.agent_cap)
         in
         (* let _ = Results.agents_set_list results agent_caps in *)
         Service.fail "TODO"
     end
