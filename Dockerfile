FROM ocurrent/opam:debian-10-ocaml-4.10@sha256:144b282ab4c04887bc883e4991a6d0c25660dde88e2cbec4588fe06bcc17a5e8 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard ebf6df3cbe74a468d1e55f41082a3f7bed38d455 && opam update
COPY --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam config exec -- dune build ./_build/install/default/bin/ocluster-scheduler

FROM debian:10
RUN apt-get update && apt-get install libev4 libsqlite3-0 -y --no-install-recommends
WORKDIR /var/lib/ocluster-scheduler
ENTRYPOINT ["/usr/local/bin/ocluster-scheduler"]
COPY --from=build /src/_build/install/default/bin/ocluster-scheduler /usr/local/bin/
