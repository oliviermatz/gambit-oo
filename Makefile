include config.mk

.PHONY: repl install clean

LIB_DEST := $(shell ${GSI} -e '(pretty-print (path-expand "~~/lib/oo/"))')

oo.o1: oo.scm oo\#.scm
	${GSC} -o oo.o1 oo.scm

install: oo.o1 ${LIB_DEST}
	cp oo.o1 oo#.scm ${LIB_DEST}

${LIB_DEST}:
	mkdir -p ${LIB_DEST}

clean:
	rm oo.o1

repl:
	gsi -:d
