open Lwt.Infix

let free_space_percent path =
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
