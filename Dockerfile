FROM ocaml/opam:debian-12-ocaml-4.14@sha256:a41e1b0df0a727efab8dfe7860c19c4f4da63bbee905c3f0e975c3f77536ef29 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto libcapnp-dev m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 5d3f0d1d655199e596a1e785e69fae8fad78cad3 && opam update
COPY --chown=opam ocluster-api.opam ocluster-worker.opam ocluster.opam /src/
COPY --chown=opam obuilder/obuilder.opam obuilder/obuilder-spec.opam /src/obuilder/
RUN opam pin -yn /src/obuilder/
WORKDIR /src
RUN echo '(lang dune 3.0)' | \
    tee obuilder/dune-project | \
    tee ./dune-project
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune subst
RUN opam exec -- dune build \
  ./_build/install/default/bin/ocluster-scheduler \
  ./_build/install/default/bin/ocluster-admin

FROM debian:12
RUN apt-get update && apt-get install libev4 libsqlite3-0 -y --no-install-recommends
RUN apt-get install ca-certificates -y  # https://github.com/mirage/ocaml-conduit/issues/388
WORKDIR /var/lib/ocluster-scheduler
ENTRYPOINT ["/usr/local/bin/ocluster-scheduler"]
COPY --from=build \
     /src/_build/install/default/bin/ocluster-scheduler \
     /src/_build/install/default/bin/ocluster-admin \
     /usr/local/bin/
