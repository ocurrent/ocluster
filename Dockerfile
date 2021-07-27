FROM ocaml/opam:debian-10-ocaml-4.12@sha256:9ce3fcbb65b4a23c56445d5c15e5009a37e904f0306586bbee57f8791a99f58c AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard 9935f57dd3f9722e6ca803aeb232b49d7058abf8 && opam update
COPY --chown=opam ocluster-api.opam ocluster.opam /src/
COPY --chown=opam obuilder/obuilder.opam obuilder/obuilder-spec.opam /src/obuilder/
RUN opam pin -yn /src/obuilder/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam config exec -- dune build \
  ./_build/install/default/bin/ocluster-scheduler \
  ./_build/install/default/bin/ocluster-admin

FROM debian:10
RUN apt-get update && apt-get install libev4 libsqlite3-0 -y --no-install-recommends
RUN apt-get install ca-certificates -y  # https://github.com/mirage/ocaml-conduit/issues/388
WORKDIR /var/lib/ocluster-scheduler
ENTRYPOINT ["/usr/local/bin/ocluster-scheduler"]
COPY --from=build \
     /src/_build/install/default/bin/ocluster-scheduler \
     /src/_build/install/default/bin/ocluster-admin \
     /usr/local/bin/
