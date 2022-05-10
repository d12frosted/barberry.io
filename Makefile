build:
	cabal build
	cabal run site -- clean
	cabal run site -- build

watch: build
	cabal run site -- watch
