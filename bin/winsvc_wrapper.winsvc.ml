let dir_exists d =
  match Unix.lstat d with
  | Unix.{ st_kind = S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> false

let ensure_dir path =
  if not (dir_exists path) then Unix.mkdir path 0o700

let run name state_dir (main:?formatter:Format.formatter -> unit -> unit) =
  let stop_notification = Lwt_unix.make_notification ~once:true (fun () -> exit 0) in
  let module Svc = Winsvc.Make
    (struct
      let name = name
      let display = ""
      let text = ""
      let arguments = []
      let stop () = Lwt_unix.send_notification stop_notification
    end)
  in
  try
    let name = Printf.sprintf "%s-%d.log" (Sys.executable_name |> Filename.basename |> Filename.remove_extension) (Random.bits ()) in
    let f = Filename.concat state_dir name in
    ensure_dir state_dir;
    let formatter = Format.formatter_of_out_channel (open_out_bin f) in
    Svc.run (main ~formatter)
  with Failure _ -> main ()

let install name display text arguments =
  let module Svc = Winsvc.Make
    (struct
      let name = name
      let display = display
      let text = text
      let arguments = arguments
      let stop () = ()
    end)
  in
  Svc.install ()

let remove name =
  let module Svc = Winsvc.Make
    (struct
      let name = name
      let display = ""
      let text = ""
      let arguments = []
      let stop () = ()
    end)
  in
  Svc.remove ()
