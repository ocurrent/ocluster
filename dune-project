(lang dune 2.5)
(name ocluster)
(formatting disabled)
(generate_opam_files true)
(source (github ocurrent/ocluster))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(package
 (name ocluster-api)
 (synopsis "Cap'n Proto API for OCluster")
 (depends
  ppx_deriving
  lwt
  (capnp-rpc-lwt (>= 0.9.0))
  fmt
  ppx_deriving_yojson
  (ocaml (>= 4.10.0))))
(package
 (name ocluster)
 (synopsis "Distribute build jobs to workers")
 (depends
  prometheus
  ppx_sexp_conv
  dune-build-info
  (ocluster-api (= :version))
  lwt
  capnp-rpc-lwt
  capnp-rpc-net
  (capnp-rpc-unix (>= 0.9.0))
  logs
  fmt
  (conf-libev (<> :os "win32"))
  digestif
  fpath
  lwt-dllist
  (prometheus-app (>= 1.0))
  cohttp-lwt-unix
  sqlite3
  obuilder
  psq
  (mirage-crypto (>= 0.8.5))
  (ocaml (>= 4.10.0))
  (current_ocluster (and (= :version) :with-test))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))))
(package
 (name current_ocluster)
 (synopsis "OCurrent plugin for OCluster builds")
 (depends
  ppx_deriving
  (ocluster-api (= :version))
  lwt
  (current (>= 0.3))
  (current_git (>= 0.3))
  (capnp-rpc-unix (>= 0.9.0))
  duration
  logs
  fmt
  prometheus
  ppx_deriving_yojson
  (ocaml (>= 4.10.0))))
