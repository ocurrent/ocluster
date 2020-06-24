CONTEXT := ci.ocamllabs.io

all:
	dune build
	dune runtest --no-buffer -j 1

deploy-stack:
	docker --context $(CONTEXT) stack deploy --prune -c stack.yml build-scheduler
