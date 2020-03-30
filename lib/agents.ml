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

(* TODO turn this into an enum / union on the wire *)
let exit_code_of_status = function
  | Unix.WEXITED c -> Int32.of_int c
  | Unix.WSIGNALED _ -> -1l (* TODO expose signal/stopped info? *)
  | Unix.WSTOPPED _ -> -2l

let spawn cmd pout stdin_stream =
  let proc = Lwt_process.open_process_full cmd in
  let proc_stdin = proc#stdin in
  Logs.info (fun l ->
      let bin, args = cmd in
      l "spawn %s %s" bin (String.concat " " @@ Array.to_list args));
  let rec send_channel ic fn =
    Lwt_io.read ic >>= function
    | "" -> Lwt.return_unit
    | chunk -> (
        fn ~chunk pout >>= function
        | Error (`Capnp msg) ->
            (* TODO retry, or buffer? *)
            Logs.debug (fun l ->
                l "Error in stream stream: %a" Capnp_rpc.Error.pp msg);
            Lwt.return_unit
        | Ok () -> send_channel ic fn )
  in
  let stdin_t =
    Lwt_stream.iter_s (fun chunk -> Lwt_io.write proc_stdin chunk) stdin_stream
  in
  let stdout_t = send_channel proc#stdout Client.Process.Out.stdout in
  let stderr_t = send_channel proc#stderr Client.Process.Out.stderr in
  let on_termination_t =
    proc#status >>= fun status ->
    stdout_t >>= fun _ ->
    (* flush stdout *)
    stderr_t >>= fun _ ->
    (* flush stderr *)
    let exit_code = exit_code_of_status status in
    Client.Process.Out.complete ~exit_code pout >>= fun _ ->
    (* best effort *)
    Lwt.return_unit
  in
  let cancel_t () =
    proc#terminate;
    Lwt.cancel stdout_t;
    Lwt.cancel stdin_t
  in
  Lwt.async (fun () -> stdout_t);
  Lwt.async (fun () -> stdin_t);
  Lwt.async (fun () -> on_termination_t);
  cancel_t

let exec (bin, args) =
  Logs.info (fun l ->
      l "exec %s %s" bin (String.concat " " @@ Array.to_list args));
  Lwt_process.exec (bin, args) >>= fun status ->
  (* TODO catch lwt exception and turn to result code *)
  Lwt.return_ok (exit_code_of_status status)
