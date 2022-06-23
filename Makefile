notes:
	emacs --batch --load ${HOME}/.config/emacs/init.el --load build-rules.el --eval '(porg-run "barberry.io")'

build:
	yarn install
	stack build
	stack run site -- clean
	stack run site -- build
	cp -r assets/CNAME _site/CNAME

watch: build
	stack run site -- watch
