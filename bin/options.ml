(* "-c CAP cmd args" -> "cmd -c CAP args"
   "-c CAP" -> "" (to avoid confusing error message when getting the list of commands) *)
let regen connect_args = function
  | cmd :: args -> [cmd] @ connect_args @ args
  | [] -> []

(* cmdliner can't cope with arguments coming first, but it's really inconvenient to have the cap after the command,
   so manually move it around in that case. *)
let move_connect_arg = function
  | prog :: ("-c" | "--connect" as arg) :: cap :: args -> prog :: regen [arg; cap] args
  | prog :: arg_cap :: args when Astring.String.is_prefix ~affix:"--connect=" arg_cap -> prog :: regen [arg_cap] args
  | x -> x

let argv =
  Sys.argv |> Array.to_list |> move_connect_arg |> Array.of_list
