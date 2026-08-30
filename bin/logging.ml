let pp_exit_status f n =
  if Sys.win32 && n < 0 then
    Fmt.pf f "0x%08lx" (Int32.of_int n)
  else
    Fmt.int f n

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stderr ("%a %a %a @[" ^^ fmt ^^ "@]@.")
      pp_timestamp (Unix.gettimeofday ())
      Fmt.(styled `Magenta string) (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let init style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  (* Logs.Src.set_level Capnp_rpc.Debug.src (Some Debug); *)
  Logs.set_reporter reporter

let cmdliner =
  let open Cmdliner in
  let docs = Manpage.s_common_options in
  Term.(const init $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())
