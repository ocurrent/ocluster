all:
	dune build
	dune runtest --no-buffer -j 1
