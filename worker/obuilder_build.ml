open Lwt.Infix

let prune_margin = 600.0        (* Don't prune anything used less than 10 minutes ago *)

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

type t = {
  builder : builder;
  spec : Obuilder.Store_spec.t;
  mutable pruning : bool;
  cond : unit Lwt_condition.t;          (* Fires when we finish pruning *)
  prune_threshold : float option;
}

module Sandbox = Obuilder.Runc_sandbox

let ( / ) = Filename.concat

let log_to log_data tag msg =
  match tag with
  | `Heading -> Log_data.info log_data "%s" msg
  | `Note -> Log_data.info log_data "%s" msg
  | `Output -> Log_data.write log_data msg

let create ?prune_threshold spec =
  Obuilder.Store_spec.to_store spec >|= fun (Store ((module Store), store)) ->
  let sandbox = Sandbox.create ~runc_state_dir:(Store.state_dir store / "runc") in
  let module Builder = Obuilder.Builder(Store)(Sandbox) in
  let builder = Builder ((module Builder), Builder.v ~store ~sandbox) in
  { builder; pruning = false; prune_threshold; spec; cond = Lwt_condition.create () }

(* Prune [t] until [path]'s free space rises above [prune_threshold]. *)
let do_prune ~path ~prune_threshold t =
  let Builder ((module Builder), builder) = t.builder in
  let rec aux () =
    let stop = Unix.gettimeofday () -. prune_margin |> Unix.gmtime in
    let limit = 100 in
    Builder.prune builder ~before:stop limit >>= fun n ->
    Df.free_space_percent path >>= fun free ->
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

let store_path t =
  match t.spec with
  | `Btrfs path -> path
  | `Zfs pool -> "/" ^ pool

(* Check the free space in [t]'s store.
   If less than [t.prune_threshold], spawn a prune operation (if not already running).
   If less than half that is remaining, also wait for it to finish.
   Returns once there is enough free space to proceed. *)
let check_free_space t =
  match t.prune_threshold with
  | None -> Lwt.return_unit
  | Some prune_threshold ->
    let path = store_path t in
    let rec aux () =
      Df.free_space_percent path >>= fun free ->
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
        Log.info (fun f -> f "OBuilder space very low. Waiting for prune to finish...");
        Lwt_condition.wait t.cond >>= aux
      ) else (
        Lwt.return_unit
      )
    in
    aux ()

let build t ~switch ~log ~spec ~src_dir =
  check_free_space t >>= fun () ->
  let log = log_to log in
  let context = Obuilder.Context.v ~switch ~log ~src_dir () in
  let Builder ((module Builder), builder) = t.builder in
  Builder.build builder context spec
