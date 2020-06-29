(lang dune 2.5)
(name build-scheduler)
(formatting disabled)

(generate_opam_files true)

(source (github ocurrent/build-scheduler))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name build-scheduler)
 (synopsis "Distribute build jobs to workers")
 (depends
  lwt
  (capnp-rpc-unix (>= 0.7.0))
  logs
  fmt
  conf-libev
  git-unix
  lwt-dllist
  prometheus-app
  sqlite3
  (ocaml (>= 4.08.0))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))
))
