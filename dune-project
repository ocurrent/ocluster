(lang dune 3.3)
(name ocluster)
(formatting disabled)
(generate_opam_files true)
(source (github ocurrent/ocluster))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(package
 (name ocluster-api)
 (synopsis "Cap'n Proto API for OCluster")
 (description "OCaml bindings for the OCluster Cap'n Proto API.")
 (depends
  ppx_deriving
  (lwt (>= 5.6.1))
  (capnp-rpc-lwt (>= 1.2))
  fmt
  ppx_deriving_yojson
  (ocaml (>= 4.12.0))))
(package
 (name ocluster)
 (synopsis "Distribute build jobs to workers")
 (description
  "OCluster manages a pool of build workers.\nA build scheduler service accepts build jobs from clients and distributes them to worker machines using Cap'n Proto.\nWorkers register themselves by connecting to the scheduler (and workers do not need to be able to accept incoming network connections).\n\nThe scheduler can manage multiple pools (e.g. `linux-x86_64` and `linux-arm32`).\nClients say which pool should handle their requests.\nAt the moment, two build types are provided: building a Dockerfile, or building an OBuilder spec.\nIn either case, the build may done in the context of some Git commit.\nThe scheduler tries to schedule similar builds on the same machine, to benefit from caching.")
 (depends
  (ppx_expect (>= v0.14.1))
  prometheus
  ppx_sexp_conv
  dune-build-info
  (ocluster-api (= :version))
  (lwt (>= 5.6.1))
  capnp-rpc-lwt
  capnp-rpc-net
  (capnp-rpc-unix (>= 1.2))
  (extunix (>= 0.3.2))
  (winsvc (and (>= 1.0.1) (= :os "win32")))
  (cmdliner (>= 1.1.0))
  logs
  fmt
  (conf-libev (<> :os "win32"))
  (digestif (>= 0.8))
  fpath
  lwt-dllist
  (prometheus-app (>= 1.2))
  cohttp-lwt-unix
  sqlite3
  obuilder
  psq
  (mirage-crypto (>= 0.8.5))
  (ocaml (>= 4.12.0))
  (current_ocluster (and (= :version) :with-test))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))))
(package
 (name current_ocluster)
 (synopsis "OCurrent plugin for OCluster builds")
 (description
  "Creates a stage in an OCurrent pipeline for submitting jobs to OCluster.")
 (depends
  ppx_deriving
  (ocluster-api (= :version))
  (lwt (>= 5.6.1))
  (current (>= 0.3))
  (current_git (>= 0.3))
  (current_web (and (>= 0.3) :with-test))
  (current_github (and (>= 0.3) :with-test))
  (capnp-rpc-unix (>= 1.2))
  duration
  logs
  fmt
  prometheus
  ppx_deriving_yojson
  (ocaml (>= 4.12.0))))
