(lang dune 2.5)
(name build-scheduler)
(formatting disabled)

(generate_opam_files true)

(source (github ocurrent/build-scheduler))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name build-scheduler-api)
 (synopsis "Cap'n Proto API for build-scheduler")
 (depends
  lwt
  (capnp-rpc-lwt (>= 0.7.0))
  fmt
  (ocaml (>= 4.08.0))
))

(package
 (name build-scheduler)
 (synopsis "Distribute build jobs to workers")
 (depends
  (build-scheduler-api (= :version))
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
  (ocaml (>= 4.08.0))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))
))
