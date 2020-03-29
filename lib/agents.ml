open Ocluster_api
(** Manage a cluster of agents registering *)

open! Lwt.Infix

type agent = { hostname : string; agent_cap : Client.Agent.t }

type cluster = { mutable agents : agent list }

let init () =
  let agents = [] in
  let cluster = { agents } in
  Logs.info (fun l -> l "Initialised cluster state");
  Lwt.return cluster

let register ~hostname agent_cap t =
  let agent = { hostname; agent_cap } in
  (* TODO check dups *)
  t.agents <- agent :: t.agents;
  Lwt.return_ok ()

let find ~hostname t = List.find_opt (fun t -> t.hostname = hostname) t.agents

let list t = t.agents

let spawn cmd pout stdin_stream =
  let proc = Lwt_process.open_process_full cmd in
  let proc_stdout = proc#stdout in
  let proc_stdin = proc#stdin in
  let rec send_stdout () =
    Lwt_io.read proc_stdout >>= function
    | "" -> Lwt.return_unit
    | chunk -> (
        Client.Process.Out.stdout ~chunk pout >>= function
        | Error (`Capnp msg) ->
            (* TODO retry, or buffer? *)
            Logs.info (fun l ->
                l "Error sending stdout, terminating stdout stream: %a"
                  Capnp_rpc.Error.pp msg);
            Lwt.return_unit
        | Ok () -> send_stdout () )
  in
  let stdin_t =
    Lwt_stream.iter_s (fun chunk -> Lwt_io.write proc_stdin chunk) stdin_stream
  in
  let stdout_t = send_stdout () in
  let cancel_t () =
    proc#terminate;
    Lwt.cancel stdout_t;
    Lwt.cancel stdin_t
  in
  Lwt.async (fun () -> stdout_t);
  Lwt.async (fun () -> stdin_t);
  cancel_t

let exec (bin, args) =
  Logs.info (fun l ->
      l "exec %s %s" bin (String.concat " " @@ Array.to_list args));
  let exit_code = 1l in
  let stdout = "dummy stdout" in
  let stderr = "dummy stderr" in
  Lwt.return_ok { Client.Agent.exit_code; stdout; stderr }
