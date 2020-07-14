module Image_id : sig
  type t
  (** A Docker image name and tag (e.g. "foo/bar:latest") *)

  val v : repo:string -> tag:string -> t
  val v_opt : repo:string -> tag:string -> (t, [> `Msg of string]) result
  val of_string : string -> (t, [> `Msg of string]) result
  val to_string : t -> string
  val repo : t -> string
  val tag : t -> string
  val pp : t Fmt.t
end

module Spec : sig
  type push = {
    target : Image_id.t;
    user : string;
    password : string;
  }

  type options = {
    build_args : string list;  (** "--build-arg" arguments. *)
    squash : bool;
    buildkit: bool;
    include_git : bool;
  } [@@deriving yojson]

  type t = {
    dockerfile : [`Contents of string | `Path of string];
    options : options;
    push_to : push option;     (** Where to upload the resulting image. *)
  }

  val defaults : options

  val init : Raw.Builder.DockerBuild.t -> t -> unit
  (** [init builder t] initialises a fresh builder with the values from [t]. *)

  val read : Raw.Reader.DockerBuild.t -> t
  (** [read reader] reads from a Cap'n Proto buffer (as initialised using [init]). *)
end
