open Lwt.Infix

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

module Config = struct
  type t = {
    store : Obuilder.Store_spec.store Lwt.t;
    sandbox_config : [ `Native of Obuilder.Native_sandbox.config
                     | `Qemu of Obuilder.Qemu_sandbox.config
                     | `Docker of Obuilder.Docker_sandbox.config ]
  }

  let v sandbox_config store = { store; sandbox_config }
end

type t = {
  builder : builder;
  mutable pruning : bool;
  cond : unit Lwt_condition.t;          (* Fires when we finish pruning *)
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

let create ?(prune_threshold = 30.0) ?(prune_limit = 100) config =
  let { Config.store; sandbox_config } = config in
  store >>= fun (Obuilder.Store_spec.Store ((module Store), store)) ->
  begin match sandbox_config with
  | `Native conf ->
     let module Builder = Obuilder.Builder (Store) (Obuilder.Native_sandbox) (Fetcher) in
     Obuilder.Native_sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf >|= fun sandbox ->
     let builder = Builder.v ~store ~sandbox in
     Builder ((module Builder), builder)
  | `Qemu conf ->
     let module Builder = Obuilder.Builder (Store) (Obuilder.Qemu_sandbox) (Obuilder.Qemu_snapshot) in
     Obuilder.Qemu_sandbox.create conf >|= fun sandbox ->
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
    let r =
    {
      builder = Builder ((module Builder), builder);
      pruning = false;
      cond = Lwt_condition.create ();
    } in
    Lwt.async (fun () ->
      let rec loop () =
        Builder.df builder >>= fun free ->
        let count = Builder.count builder in
        Log.info (fun f -> f "OBuilder partition: %.0f%% free, %Li items" free count);
        Prometheus.Gauge.set Metrics.obuilder_space_free free;
        if free > prune_threshold then (
          r.pruning <- false;
          Lwt_condition.signal r.cond (); (* release one waiting process *)
          Lwt_unix.sleep 30.0 >>= fun () -> loop ()
        ) else (
          r.pruning <- true;
          let stop = Unix.gettimeofday () |> Unix.gmtime in
          Builder.prune builder ~before:stop prune_limit >>= fun n ->
          Log.info (fun f -> f "Pruned %i items" n);
          (if n = 0 then Lwt_unix.sleep 30.0
          else Lwt.return_unit )>>= fun () -> loop ()
        )
      in loop ()
    ); r

let build t ~switch ~log ~spec ~src_dir ~secrets =
  (if t.pruning then Lwt_condition.wait t.cond
  else Lwt.return ()) >>= fun () ->
  let log = log_to log in
  let Builder ((module Builder), builder) = t.builder in
  let shell = Builder.shell in
  let context = Obuilder.Context.v ~switch ~log ~src_dir ?shell ~secrets () in
  Builder.build builder context spec

let healthcheck t =
  let Builder ((module Builder), builder) = t.builder in
  Builder.healthcheck builder

let cache_stats t =
  let Builder ((module Builder), builder) = t.builder in
  Builder.cache_stats builder
