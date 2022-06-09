notes:
	emacs --batch --load ${HOME}/.config/emacs/init.el --load lisp/publicatorg.el

build:
	stack build
	stack run site -- clean
	stack run site -- build
	cp -r assets/CNAME _site/CNAME

watch: build
	stack run site -- watch
