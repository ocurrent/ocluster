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
  mutable pruning : bool;
  cond : unit Lwt_condition.t;          (* Fires when we finish pruning *)
  prune_threshold : float option;
  prune_item_threshold : int64 option;  (* Threshold number of items to hold in obuilder store *)
  prune_limit : int option;             (* Number of items to prune from obuilder when threshold is reached *)
  sandbox_config : [ `Native of Obuilder.Native_sandbox.config
                   | `Docker of Obuilder.Docker_sandbox.config ]
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

let create ?prune_threshold ?prune_item_threshold ?prune_limit config =
  let { Config.store; sandbox_config } = config in
  store >>= fun (Obuilder.Store_spec.Store ((module Store), store)) ->
  begin match sandbox_config with
  | `Native conf ->
     let module Builder = Obuilder.Builder (Store) (Obuilder.Native_sandbox) (Fetcher) in
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
      pruning = false;
      prune_threshold;
      prune_item_threshold;
      prune_limit;
      cond = Lwt_condition.create ();
      sandbox_config;
    }

(* Prune [t] until free space rises above [prune_threshold]
   or number of items falls below [prune_item_threshold]. *)
let do_prune ~prune_threshold ~prune_item_threshold ~prune_limit t =
  let Builder ((module Builder), builder) = t.builder in
  let rec aux () =
    let stop = Unix.gettimeofday () -. prune_margin |> Unix.gmtime in
    Builder.prune builder ~before:stop prune_limit >>= fun n ->
    Builder.df builder >>= fun free ->
    let count = Builder.count builder in
    Log.info (fun f -> f "OBuilder partition: %.0f%% free, %Li items after pruning %d items" free count n);
    if free > prune_threshold && count < prune_item_threshold
    then Lwt.return_unit      (* Space problem is fixed! *)
    else if n < prune_limit then (
      Log.warn (fun f -> f "Out of space, but nothing left to prune! (will wait and then retry)");
      Lwt_unix.sleep 600.0 >>= aux
    ) else (
      (* Continue pruning *)
      aux ()
    )
  in
  aux ()

(* Check the free space and/or number of items in [t]'s store.
   If less than [t.prune_threshold] or items > [t.prune_item_threshold], spawn a prune operation (if not already running).
   If less than half that is remaining, also wait for it to finish.
   Returns once there is enough free space to proceed. *)
let check_free_space t =
  let prune_limit = Option.value t.prune_limit ~default:100 in
  let prune_threshold = Option.value t.prune_threshold ~default:0. in
  let prune_item_threshold = Option.value t.prune_item_threshold ~default:Int64.max_int in
  if prune_threshold = 0. && prune_item_threshold = Int64.max_int then
    Lwt.return_unit (* No limits have been set *)
  else
    let Builder ((module Builder), builder) = t.builder in
    let rec aux () =
      Builder.df builder >>= fun free ->
      let count = Builder.count builder in
      Log.info (fun f -> f "OBuilder partition: %.0f%% free, %Li items" free count);
      (* If we're low on space, or over the threshold number of items spawn a pruning thread. *)
      if ((prune_threshold > 0. && free < prune_threshold) ||
          (prune_item_threshold < Int64.max_int && count > prune_item_threshold)) && not t.pruning then (
        t.pruning <- true;
        Lwt.async (fun () ->
            Lwt.finalize
              (fun () -> do_prune ~prune_threshold ~prune_item_threshold ~prune_limit t)
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

let cache_stats t =
  let Builder ((module Builder), builder) = t.builder in
  Builder.cache_stats builder

let purge t =
  let Builder ((module Builder), builder) = t.builder in
  let before = Unix.gettimeofday () +. prune_margin |> Unix.gmtime in
  (* set a future time and a big number to ensure everything is deleted *)
  Builder.prune builder ~before Int.max_int >>= fun n ->
  Lwt.return n

let backend t =
  t.sandbox_config
