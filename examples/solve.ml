open Lwt.Syntax

(* A small analysis step to get some opam files that the user specifies. 
   This is a more manual and simple version of OCaml-CI's anaylsis step. *)
let pool = Current.Pool.create ~label:"analyse" 2

module Raw = struct
  type t = No_context

  module Key = struct
    type t = { 
      packages : (string * Fpath.t) list;
      commit : Current_git.Commit.t;
    }

    let digest { packages; commit } =
      let package p = `Assoc [ "name", `String (fst p); "path", `String (Fpath.to_string @@ snd p) ] in
      let json = `Assoc [
          "packages", `List (List.map package packages);
          "commit", `String (Current_git.Commit.hash commit);
        ]
      in
      Yojson.Safe.to_string json
  end

  module Value = Current.Unit
  module Outcome = struct
    type t = (string * string) list

    let to_yojson t = `Assoc (List.map (fun (p, f) -> p, `String f) t)  
    let of_yojson = function
      | `Assoc assoc -> List.map (function (p, `String f) -> p, f | _ -> failwith "Expected files") assoc
      | _ -> [] 

    let digest t = Yojson.Safe.to_string (to_yojson t)
    let pp = Fmt.list (Fmt.pair Fmt.string Fmt.string)
    let marshal t = to_yojson t |> Yojson.Safe.to_string
    let unmarshal t = Yojson.Safe.from_string t |> of_yojson
    let equal = Stdlib.( = )
  end

  let read_file path =
    Lwt_io.with_file ~mode:Lwt_io.input path
      (fun ch ->
          let* len = Lwt_io.length ch in
          let len = Int64.to_int len in
          let buf = Bytes.create len in
          let+ () = Lwt_io.read_into_exactly ch buf 0 len in
          Bytes.to_string buf
      )

  let id = "ci-opam-file"

  let run No_context job Key.{ packages; commit } () =
    let* () = Current.Job.start_with job ~pool ~level:Current.Level.Harmless in
    Current_git.with_checkout ~job commit @@ fun src ->
    let opamfile (pkg, path) =
      let+ file = read_file Fpath.(to_string @@ src // path) in 
      Current.Job.write job file;
      (pkg, file)
    in
    let+ files = 
      Lwt_list.map_p opamfile packages
    in
    Ok files

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = true
  let latched = true
end

module Opamfile = Current_cache.Generic (Raw)

let get_opamfile ~packages commit = 
  let open Current.Syntax in
  Current.component "opam file" |>
  let> commit = Current_git.fetch @@ Current.map List.hd commit in
  Opamfile.run No_context Raw.Key.{ packages; commit } ()

let opam_template arch =
  let arch = Option.value ~default:"%{arch}%" arch in
  Fmt.str
    {|
  {
    "arch": "%s",
    "os": "%%{os}%%",
    "os_family": "%%{os-family}%%",
    "os_distribution": "%%{os-distribution}%%",
    "os_version": "%%{os-version}%%",
    "opam_version": "%%{opam-version}%%"
  }
|}
    arch

let get_vars ~ocaml_package_name ~ocaml_version ?arch () =
  let+ vars =
    Lwt_process.pread
      ("", [| "opam"; "config"; "expand"; opam_template arch |])
  in
  let json =
    match Yojson.Safe.from_string vars with
    | `Assoc items ->
        `Assoc
          (("ocaml_package", `String ocaml_package_name)
          :: ("ocaml_version", `String ocaml_version)
          :: items)
    | json ->
        Fmt.failwith "Unexpected JSON: %a"
          Yojson.Safe.(pretty_print ~std:true)
          json
  in
  Result.get_ok @@ Solver_service_api.Worker.Vars.of_yojson json

let solve_to_custom req =
  let open Cluster_api.Raw in
  let params =
    Yojson.Safe.to_string
    @@ Solver_service_api.Worker.Solve_request.to_yojson req
  in
  let custom = Builder.Custom.init_root () in
  let builder = Builder.Custom.payload_get custom in
  let request =
    Solver_service_api.Raw.Builder.Solver.Solve.Params.init_pointer builder
  in
  Solver_service_api.Raw.Builder.Solver.Solve.Params.request_set request params;
  let r = Reader.Custom.of_builder custom in
  Reader.Custom.payload_get r

let solve_of_custom c =
  let open Solver_service_api.Raw in
  let payload = Cluster_api.Custom.payload c in
  let request =
    Reader.Solver.Solve.Params.request_get @@ Reader.of_pointer payload
  in
  Solver_service_api.Worker.Solve_request.of_yojson
  @@ Yojson.Safe.from_string request