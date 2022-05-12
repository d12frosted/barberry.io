build:
	stack build
	stack run site -- clean
	stack run site -- build

watch: build
	stack run site -- watch
