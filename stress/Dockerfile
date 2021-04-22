FROM debian:10
RUN apt-get update && apt-get install libev4 libsqlite3-0 -y --no-install-recommends
RUN apt-get install ca-certificates -y  # https://github.com/mirage/ocaml-conduit/issues/388
