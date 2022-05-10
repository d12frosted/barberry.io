build:
	cabal build
	cabal run site -- clean
	cabal run site -- build
	cp -r assets/site.webmanifest _site/site.webmanifest

watch: build
	cabal run site -- watch
