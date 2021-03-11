.PHONY: build doc test clean

build:
	dune build @install
	ln -sf _build/install/default/bin bin
	ln -sf _build/install/default/lib lib

doc:
	dune build @doc
	ln -sf _build/default/_doc/_html doc

test:
	dune test

clean:
	dune clean
	rm -f bin lib doc
