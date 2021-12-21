include config.mk

.PHONY: repl install clean

LIB_DEST := $(shell ${GSI} -e '(pretty-print (path-expand "~~/lib/oo/"))')

GSC_OPTS := -debug -debug-location -debug-source -debug-environments -track-scheme -keep-c

oo.o1: oo.scm oo\#.scm
	${GSC} ${GSC_OPTS} -o oo.o1 oo.scm

examples.o1: examples.scm oo\#.scm
	${GSC} ${GSC_OPTS} -o examples.o1 examples.scm

install: oo.o1 examples.o1 ${LIB_DEST}
	cp oo.o1 oo#.scm ${LIB_DEST}

${LIB_DEST}:
	mkdir -p ${LIB_DEST}

clean:
	rm oo.o1 examples.o1

repl: oo.o1 examples.o1 install
	gsi -:d
