FROM ocurrent/opam:ubuntu-20.04-ocaml-4.10@sha256:be90dc531fa6693060f65576f587cd38a031370d8e8655144f5cf6f9433ba3c6 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard a5e373ef1d13748cb092ede3c5b74ce6b6c03349 && opam update
COPY --chown=opam ocluster-api.opam ocluster.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam config exec -- dune build ./_build/install/default/bin/ocluster-scheduler

FROM ubuntu:20.04
RUN apt-get update && apt-get install libev4 libsqlite3-0 -y --no-install-recommends
WORKDIR /var/lib/ocluster-scheduler
ENTRYPOINT ["/usr/local/bin/ocluster-scheduler"]
COPY --from=build /src/_build/install/default/bin/ocluster-scheduler /usr/local/bin/
