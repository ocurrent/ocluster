open Capnp_rpc_lwt

module Logger = struct
  type t = Raw.Client.Logger.t Capability.t

  let log ~msg t =
    let open Raw.Client.Logger.Log in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.msg_set params msg;
    Capability.call_for_unit t method_id request
end

module Cluster = struct
  type t = Raw.Client.Cluster.t Capability.t

  let register ~hostname ~callback t =
    let open Raw.Client.Cluster.Register in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.hostname_set params hostname;
    Params.callback_set params (Some callback);
    Capability.call_for_caps t method_id request Results.reply_get_pipelined
end

module Agent = struct
  type t = Raw.Client.Agent.t Capability.t

  let exec ~cmd t =
    let open Raw.Client.Agent.Exec in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.cmd_set params cmd; (* TODO this should be a command array *)
    Capability.call_for_value t method_id request |> Lwt_result.map Results.exit_code_get
end