let run _name _state_dir (main:?formatter:Format.formatter -> unit -> unit) = main ()

let install _name _display _text _arguments = failwith "Cannot install a service on non-Windows systems."

let remove _name = failwith "Cannot remove a service on non-Windows systems."
