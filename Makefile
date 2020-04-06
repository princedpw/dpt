.PHONY: test clean

default:
	dune build src/bin/main.exe
	cp _build/default/src/bin/main.exe dpt

#install: default
#	cp _build/default/src/bin/main.exe dpt

test: default
	dune runtest -f --no-buffer

doc:
	dune build @doc

clean:
	dune clean
	rm -f dpt
