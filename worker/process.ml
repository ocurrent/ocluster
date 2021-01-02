open Lwt.Infix

type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

let send_to ch contents =
  Lwt.try_bind
    (fun () ->
       Lwt_io.write ch contents >>= fun () ->
       Lwt_io.close ch
    )
    (fun () -> Lwt.return (Ok ()))
    (fun ex -> Lwt.return (Error (`Msg (Printexc.to_string ex))))

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x
    
let process_result ~label ~stdin_result ~switch = function
  | _ when not (Lwt_switch.is_on switch) -> Error `Cancelled
  | Unix.WEXITED 0 ->
    begin match stdin_result with
      | Ok () -> Ok ()
      | Error (`Msg msg) -> Error (`Msg (Fmt.strf "Failed sending input to %s: %s" label msg))
    end
  | Unix.WEXITED n -> Error (`Exit_code n)
  | Unix.WSIGNALED x -> Error (`Msg (Fmt.strf "%s failed with signal %d" label x))
  | Unix.WSTOPPED x -> Error (`Msg (Fmt.strf "%s stopped with signal %a" label pp_signal x))
  
let add_process_hook ~label ~switch proc =
  Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
      if Lwt.state proc#status = Lwt.Sleep then (
        Log.info (fun f -> f "Cancelling %s job..." label);
        proc#terminate;
      );
      Lwt.return_unit
    )

let exec ~label ~log ~switch ?env ?(stdin="") ?(stderr=`FD_copy Unix.stdout) cmd =
  Log.info (fun f -> f "Exec(%s): %a" label Fmt.(list ~sep:sp (quote string)) cmd);
  let cmd = "", Array.of_list cmd in
  let proc = Lwt_process.open_process ?env ~stderr cmd in
  add_process_hook ~label ~switch proc
  >>= fun () ->
  let copy_thread = Log_data.copy_from_stream log proc#stdout in
  send_to proc#stdin stdin >>= fun stdin_result ->
  copy_thread >>= fun () -> (* Ensure all data has been copied before returning *)
  proc#status >|= process_result ~label ~stdin_result ~switch

let exec_output ~label ~log ~switch ?env ?(stdin="") cmd =
  Log.info (fun f -> f "Exec(%s): %a" label Fmt.(list ~sep:sp (quote string)) cmd);
  let cmd = "", Array.of_list cmd in
  let proc = Lwt_process.open_process_full ?env cmd in
  add_process_hook ~label ~switch proc
  >>= fun () ->
  let copy_thread = Log_data.copy_from_stream log proc#stderr in
  let reader = Lwt_io.read proc#stdout in
  send_to proc#stdin stdin >>= fun stdin_result ->
  copy_thread >>= fun () -> (* Ensure all data has been copied before returning *)
  reader >>= fun stdout ->
  proc#status >|= process_result ~label ~stdin_result ~switch
  |> Lwt_result.map @@ fun () -> String.trim stdout

let check_call ~label ~log ~switch ?env ?stdin ?stderr cmd =
  exec ~label ~log ~switch ?env ?stdin ?stderr cmd >|= function
  | Ok () -> Ok ()
  | Error `Cancelled -> Error `Cancelled
  | Error (`Exit_code n) -> Error (`Msg (Fmt.strf "%s failed with exit-code %d" label n))
  | Error (`Msg _) as e -> e

let check_output ~label ~log ~switch ?env ?stdin cmd =
  exec_output ~label ~log ~switch ?env ?stdin cmd >|= function
  | Ok output -> Ok output
  | Error `Cancelled -> Error `Cancelled
  | Error (`Exit_code n) -> Error (`Msg (Fmt.strf "%s failed with exit-code %d" label n))
  | Error (`Msg _) as e -> e
