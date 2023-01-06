let run _name _state_dir ?style_renderer (main:?formatter:Format.formatter -> unit -> unit) =
  ignore style_renderer;
  main ()

let install _name _display _text _arguments = failwith "Cannot install a service on non-Windows systems."

let remove _name = failwith "Cannot remove a service on non-Windows systems."
