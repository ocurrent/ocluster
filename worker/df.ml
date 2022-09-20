let normalise_path root_dir =
  if Sys.win32 then
    let vol, _ = Fpath.(v root_dir |> split_volume) in
    vol ^ "\\"
  else
    root_dir

let free_space_percent root_dir =
  let vfs = ExtUnix.All.statvfs (normalise_path root_dir) in
  let used = Int64.sub vfs.f_blocks vfs.f_bfree in
  100. -. 100. *. (Int64.to_float used) /. (Int64.to_float (Int64.add used vfs.f_bavail))

let free_files_percent root_dir =
  let vfs = ExtUnix.All.statvfs (normalise_path root_dir) in
  let used = Int64.sub vfs.f_files vfs.f_ffree in
  100. -. 100. *. (Int64.to_float used) /. (Int64.to_float (Int64.add used vfs.f_favail))

