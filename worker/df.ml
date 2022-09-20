let free_space_percent root_dir =
  let root_dir =
    if Sys.win32 then
      let vol, _ = Fpath.(v root_dir |> split_volume) in
      vol ^ "\\"
    else
      root_dir
  in
  let vfs = ExtUnix.All.statvfs root_dir in
  let used_blocks = Int64.sub vfs.f_blocks vfs.f_bfree in
  let used_files = Int64.sub vfs.f_files vfs.f_ffree in
  let percent_free_blocks = 100. -. 100. *. (Int64.to_float used_blocks) /. (Int64.to_float (Int64.add used_blocks vfs.f_bavail)) in
  let percent_free_files = 100. -. 100. *. (Int64.to_float used_files) /. (Int64.to_float (Int64.add used_files vfs.f_favail)) in
  min percent_free_blocks percent_free_files
