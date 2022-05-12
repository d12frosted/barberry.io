build:
	stack build
	stack run site -- clean
	stack run site -- build
	cp -r assets/site.webmanifest _site/site.webmanifest

watch: build
	stack run site -- watch
