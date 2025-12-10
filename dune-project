(lang dune 3.7)
(name ocluster)
(formatting disabled)
(generate_opam_files true)

(source (github ocurrent/ocluster))
(documentation "https://ocurrent.github.io/ocluster/")
(authors
 "Antonin Décimo <antonin@tarides.com>"
 "David Allsopp <david.allsopp@metastack.com>"
 "Kate <kit.ty.kate@disroot.org>"
 "Lucas Pluvinage <lucas@tarides.com>"
 "Mark Elvers <mark.elvers@tunbury.org>"
 "Patrick Ferris <patrick@sirref.org>"
 "Thomas Leonard <talex5@gmail.com>"
 "Tim McGilchrist <timmcgil@gmail.com>")
(maintainers "Mark Elvers <mark.elvers@tunbury.org>")
(license "Apache-2.0")

(package
 (name ocluster-api)
 (synopsis "Cap'n Proto API for OCluster")
 (description "OCaml bindings for the OCluster Cap'n Proto API.")
 (depends
  (ocaml (>= 4.14.1))
  (capnp-rpc-lwt (>= 1.2.3))
  fmt
  (lwt (>= 5.6.1))
  ppx_deriving
  ppx_deriving_yojson))

(package
 (name ocluster-worker)
 (synopsis "OCluster library for defining workers")
 (description "OCluster library for defining workers")
 (depends
  (ocluster-api (= :version))
  (ocaml (>= 4.14.1))
  (capnp-rpc-lwt (>= 1.2.3))
  (cohttp-lwt-unix (>= 4.0))
  (digestif (>= 0.8))
  (extunix (>= 0.4.1))
  fpath
  logs
  (lwt (>= 5.6.1))
  (obuilder (>= 0.5.1))
  (prometheus-app (>= 1.2))))

(package
 (name ocluster)
 (synopsis "Distribute build jobs to workers")
 (description "OCluster manages a pool of build workers.\nA build scheduler service accepts build jobs from clients and distributes them to worker machines using Cap'n Proto.\nWorkers register themselves by connecting to the scheduler (and workers do not need to be able to accept incoming network connections).\n\nThe scheduler can manage multiple pools (e.g. `linux-x86_64` and `linux-arm32`).\nClients say which pool should handle their requests.\nAt the moment, two build types are provided: building a Dockerfile, or building an OBuilder spec.\nIn either case, the build may done in the context of some Git commit.\nThe scheduler tries to schedule similar builds on the same machine, to benefit from caching.")
 (depends
  (ocluster-api (= :version))
  (ocluster-worker (= :version))
  (ocaml (>= 4.14.1))
  (capnp-rpc-lwt (>= 1.2.3))
  (capnp-rpc-net (>= 1.2.3))
  (capnp-rpc-unix (>= 1.2.3))
  (cmdliner (>= 1.2.0))
  (conf-libev (<> :os "win32"))
  (digestif (>= 0.8))
  dune-build-info
  fmt
  fpath
  logs
  (lwt (>= 5.6.1))
  lwt-dllist
  (mirage-crypto (>= 0.8.5))
  (obuilder (>= 0.5.1))
  (ppx_expect (>= v0.14.1))
  ppx_sexp_conv
  prometheus
  (prometheus-app (>= 1.2))
  (psq (>= 0.2.1))
  sqlite3
  (winsvc (and (>= 1.0.1) (= :os "win32")))
  (current_ocluster (and (= :version) :with-test))
  (alcotest (and (>= 1.7.0) :with-test))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  (mirage-crypto-rng (and (>= 0.11.0) :with-test))
  (mirage-crypto-rng-lwt (and (>= 0.11.0) :with-test))))

(package
 (name current_ocluster)
 (synopsis "OCurrent plugin for OCluster builds")
 (description
  "Creates a stage in an OCurrent pipeline for submitting jobs to OCluster.")
 (depends
  (ocluster-api (= :version))
  (ocaml (>= 4.14.1))
  (capnp-rpc-unix (>= 1.2.3))
  (current (>= 0.6.4))
  (current_git (>= 0.6.4))
  duration
  fmt
  logs
  (lwt (>= 5.6.1))
  ppx_deriving
  ppx_deriving_yojson
  (prometheus (>= 1.2))
  (current_github (and (>= 0.6.4) :with-test))
  (current_web (and (>= 0.6.4) :with-test))))
