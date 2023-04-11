FROM ocaml/opam:debian-11-ocaml-4.14@sha256:18fcbeb356957c58cf8f37bc43adcca5683d50163a120d9b322b173428281e61 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto libcapnp-dev m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 15b381eeae1aa1c8b67b214ce1739344717aae89 && opam update
COPY --chown=opam ocluster-api.opam ocluster-worker.opam ocluster.opam /src/
COPY --chown=opam obuilder/obuilder.opam obuilder/obuilder-spec.opam /src/obuilder/
RUN opam pin -yn /src/obuilder/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune subst
RUN opam exec -- dune build \
  ./_build/install/default/bin/ocluster-scheduler \
  ./_build/install/default/bin/ocluster-admin

FROM debian:11
RUN apt-get update && apt-get install libev4 libsqlite3-0 -y --no-install-recommends
RUN apt-get install ca-certificates -y  # https://github.com/mirage/ocaml-conduit/issues/388
WORKDIR /var/lib/ocluster-scheduler
ENTRYPOINT ["/usr/local/bin/ocluster-scheduler"]
COPY --from=build \
     /src/_build/install/default/bin/ocluster-scheduler \
     /src/_build/install/default/bin/ocluster-admin \
     /usr/local/bin/
