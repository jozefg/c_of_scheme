build:
	cp c-bits/* ${HOME}/.scheme2c
	cabal build
	cp dist/build/scheme2c/scheme2c .

test:

.PHONY: build test
