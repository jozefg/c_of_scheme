build: scheme2c
	cp c-bits/* ${HOME}/.scheme2c

scheme2c:
	cabal build
	cp dist/build/scheme2c/scheme2c .

test:

.phony: build test
