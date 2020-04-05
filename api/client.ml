open Capnp_rpc_lwt

module Process = struct
  module Out = struct
    type t = Raw.Client.ProcessOut.t Capability.t

    let stdout ~chunk t =
      let open Raw.Client.ProcessOut.Stdout in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.chunk_set params chunk;
      Capability.call_for_unit t method_id request

    let stderr ~chunk t =
      let open Raw.Client.ProcessOut.Stderr in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.chunk_set params chunk;
      Capability.call_for_unit t method_id request

    let complete ~exit_code t =
      let open Raw.Client.ProcessOut.Complete in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.exit_code_set params exit_code;
      Capability.call_for_unit t method_id request
  end

  module In = struct
    type t = Raw.Client.ProcessIn.t Capability.t

    let stdin ~chunk t =
      let open Raw.Client.ProcessIn.Stdin in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.chunk_set params chunk;
      Capability.call_for_unit t method_id request

    let cancel t =
      let open Raw.Client.ProcessIn.Cancel in
      let request, _params = Capability.Request.create Params.init_pointer in
      Capability.call_for_unit t method_id request
  end
end

module ClusterMember = struct
  type t = Raw.Client.ClusterMember.t Capability.t

  let register ~hostname ~callback t =
    let open Raw.Client.ClusterMember.Register in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.hostname_set params hostname;
    Params.callback_set params (Some callback);
    Capability.call_for_unit_exn t method_id request
end

module ClusterUser = struct
  type t = Raw.Client.ClusterUser.t Capability.t

  let find ~hostname t =
    let open Raw.Client.ClusterUser.Find in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.hostname_set params hostname;
    Capability.call_for_caps t method_id request Results.callback_get_pipelined
end

module Agent = struct
  type t = Raw.Client.Agent.t Capability.t

  type command = string * string array

  let exec ~cmd t =
    let open Raw.Client.Agent.Exec in
    let request, params = Capability.Request.create Params.init_pointer in
    let cmd_params = Params.cmd_init params in
    let binary, args = cmd in
    Raw.Builder.Command.binary_set cmd_params binary;
    let _ = Raw.Builder.Command.args_set_array cmd_params args in
    Capability.call_for_value t method_id request
    |> Lwt_result.map Results.exit_code_get

  let spawn cmd pout t =
    let open Raw.Client.Agent.Spawn in
    let request, params = Capability.Request.create Params.init_pointer in
    let cmd_params = Params.cmd_init params in
    let binary, args = cmd in
    Raw.Builder.Command.binary_set cmd_params binary;
    let _ = Raw.Builder.Command.args_set_array cmd_params args in
    Params.pout_set params (Some pout);
    Capability.call_for_caps t method_id request Results.pin_get_pipelined
end
