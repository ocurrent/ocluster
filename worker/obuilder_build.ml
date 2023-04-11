open Lwt.Infix

let prune_margin = 600.0        (* Don't prune anything used less than 10 minutes ago *)

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

module Config = struct
  type t = {
    store : Obuilder.Store_spec.store Lwt.t;
    sandbox_config : [ `Native of Obuilder.Native_sandbox.config
                     | `Docker of Obuilder.Docker_sandbox.config ]
  }

  let v sandbox_config store = { store; sandbox_config }
end

type t = {
  builder : builder;
  root : string;
  mutable pruning : bool;
  cond : unit Lwt_condition.t;          (* Fires when we finish pruning *)
  prune_threshold : float option;
}

let ( / ) = Filename.concat

let pp_timestamp f x =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = Unix.gmtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d"
    (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let log_to log_data tag msg =
  match tag with
  | `Heading -> Log_data.info log_data "\n\027[01;34m%s\027[0m" msg
  | `Note -> Log_data.info log_data "\027[01;2m\027[01;35m%a %s\027[0m" pp_timestamp (Unix.gettimeofday ()) msg
  | `Output -> Log_data.write log_data msg

let create ?prune_threshold config =
  let { Config.store; sandbox_config } = config in
  store >>= fun (Obuilder.Store_spec.Store ((module Store), store)) ->
  begin match sandbox_config with
  | `Native conf ->
     let module Builder = Obuilder.Builder (Store) (Obuilder.Native_sandbox) (Obuilder.Docker_extract) in
     Obuilder.Native_sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf >|= fun sandbox ->
     let builder = Builder.v ~store ~sandbox in
     Builder ((module Builder), builder)
  | `Docker conf ->
     let module Builder = Obuilder.Docker_builder (Store) in
     Obuilder.Docker_sandbox.create conf >|= fun sandbox ->
     let builder = Builder.v ~store ~sandbox in
     Builder ((module Builder), builder)
  end
  >>= fun (Builder ((module Builder), builder)) ->
  Log.info (fun f -> f "Performing OBuilder self-test…");
  Builder.healthcheck builder >|= function
  | Error (`Msg m) -> Fmt.failwith "Initial OBuilder healthcheck failed: %s" m
  | Ok () ->
    Log.info (fun f -> f "OBuilder self-test passed");
    {
      builder = Builder ((module Builder), builder);
      root = Store.root store;
      pruning = false;
      prune_threshold;
      cond = Lwt_condition.create ();
    }

(* Prune [t] until [path]'s free space rises above [prune_threshold]. *)
let do_prune ~path ~prune_threshold t =
  let Builder ((module Builder), builder) = t.builder in
  let rec aux () =
    let stop = Unix.gettimeofday () -. prune_margin |> Unix.gmtime in
    let limit = 100 in
    Builder.prune builder ~before:stop limit >>= fun n ->
    let free = Df.free_space_percent path in
    Log.info (fun f -> f "OBuilder partition: %.0f%% free after pruning %d items" free n);
    if free > prune_threshold then Lwt.return_unit      (* Space problem is fixed! *)
    else if n < limit then (
      Log.warn (fun f -> f "Out of space, but nothing left to prune! (will wait and then retry)");
      Lwt_unix.sleep 600.0 >>= aux
    ) else (
      (* Continue pruning *)
      aux ()
    )
  in
  aux ()

(* Check the free space in [t]'s store.
   If less than [t.prune_threshold], spawn a prune operation (if not already running).
   If less than half that is remaining, also wait for it to finish.
   Returns once there is enough free space to proceed. *)
let check_free_space t =
  match t.prune_threshold with
  | None -> Lwt.return_unit
  | Some prune_threshold ->
    let path = t.root in
    let rec aux () =
      let free = Df.free_space_percent path in
      Log.info (fun f -> f "OBuilder partition: %.0f%% free" free);
      (* If we're low on space, spawn a pruning thread. *)
      if free < prune_threshold && t.pruning = false then (
        t.pruning <- true;
        Lwt.async (fun () ->
            Lwt.finalize
              (fun () -> do_prune ~path ~prune_threshold t)
              (fun () ->
                 Lwt.pause () >|= fun () ->
                 t.pruning <- false;
                 Lwt_condition.broadcast t.cond ()
              )
          );
      );
      if free < prune_threshold /. 2.0 then (
        assert (t.pruning);
        Log.info (fun f -> f "OBuilder space very low. Waiting for prune to finish…");
        Lwt_condition.wait t.cond >>= aux
      ) else (
        Lwt.return_unit
      )
    in
    aux ()

let build t ~switch ~log ~spec ~src_dir ~secrets =
  check_free_space t >>= fun () ->
  let log = log_to log in
  let context = Obuilder.Context.v ~switch ~log ~src_dir ~secrets () in
  let Builder ((module Builder), builder) = t.builder in
  Builder.build builder context spec

let healthcheck t =
  let Builder ((module Builder), builder) = t.builder in
  Builder.healthcheck builder
