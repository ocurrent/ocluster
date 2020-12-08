open Lwt.Infix

let free_space_percent_unix path =
  Lwt_process.pread ("", [| "df"; path; "--output=pcent" |]) >|= fun lines ->
  match String.split_on_char '\n' (String.trim lines) with
  | [_; result] ->
    let used =
      try Scanf.sscanf result " %f%%" Fun.id
      with _ -> Fmt.failwith "Expected %S, got %S" "xx%" result
    in
    100. -. used
  | _ ->
    Fmt.failwith "Expected two lines from df, but got:@,%S" lines

external get_disk_free_space_ex_w: string -> (int64 * int64) = "stub_GetDiskFreeSpaceExW"
let free_space_percent_win32 path =
  let free, total = get_disk_free_space_ex_w path in
  Int64.to_float free /. Int64.to_float total *. 100.0 |> Lwt.return

let free_space_percent path =
  if Sys.os_type = "Win32" then
    free_space_percent_win32 path
  else
    free_space_percent_unix path
