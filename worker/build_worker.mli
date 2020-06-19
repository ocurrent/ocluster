val run :
  ?switch:Lwt_switch.t ->
  ?docker_build:(switch:Lwt_switch.t ->
                 log:Log_data.t ->
                 src:string ->
                 string -> (unit, Process.error) Lwt_result.t) ->
  capacity:int ->
  Api.Registration.t ->
  unit Lwt.t
(** [run ~capacity registry] runs a builder that connects to registry and runs up to [capacity] jobs at once.
    @param switch Turning this off causes the builder to exit (for unit-tests)
    @param docker_build Used to override the default build action (for unit-tests) *)

module Process = Process
module Log_data = Log_data
