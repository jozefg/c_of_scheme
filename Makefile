build:
	mkdir -p ../.c_of_scheme
	cp c-bits/* ${HOME}/.c_of_scheme
	cabal build
	cp dist/build/c_of_scheme/c_of_scheme .

test:

.PHONY: build test
