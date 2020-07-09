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
  lwt
  (capnp-rpc-lwt (>= 0.7.0))
  fmt
  (ocaml (>= 4.10.0))
))

(package
 (name ocluster)
 (synopsis "Distribute build jobs to workers")
 (depends
  (ocluster-api (= :version))
  lwt
  (capnp-rpc-unix (>= 0.7.0))
  logs
  fmt
  conf-libev
  git-unix
  lwt-dllist
  prometheus-app
  cohttp-lwt-unix
  sqlite3
  (ocaml (>= 4.10.0))
  (current_ocluster (and (= :version) :with-test))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))
))

(package
 (name current_ocluster)
 (synopsis "OCurrent plugin for OCluster builds")
 (depends
  (ocluster-api (= :version))
  lwt
  (current (>= 0.3))
  (current_git (>= 0.3))
  (capnp-rpc-unix (>= 0.7.0))
  duration
  logs
  fmt
  ppx_deriving_yojson
  (ocaml (>= 4.10.0))
))
