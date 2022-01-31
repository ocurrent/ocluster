val solve_to_custom : Solver_service_api.Worker.Solve_request.t -> Cluster_api.Custom.payload
(** [solve_to_custom req] converts the solver request to a custom job specification. *)

val solve_of_custom : Cluster_api.Custom.t -> (Solver_service_api.Worker.Solve_request.t, string) result
(** [solve_of_custom c] tries to read the custom job specifcation as a solver request. *)

val solve : 
  solver:Solver_service_api.Solver.X.t Capnp_rpc_lwt.Capability.t ->
  switch:'a ->
  log:Cluster_worker.Log_data.t ->
  Cluster_api.Custom.t ->
  (string, 'b) result Lwt.t
(** [solve ~solver ~switch ~log c] interprets [c] as a solver request and solves it using [solver]. *)

val spawn_local : ?solver_dir:string -> unit -> Solver_service_api.Solver.t
(** [spawn_local ()] forks a process running a [solver-service] that communicates over standard input/output. *)