CC=gcc
CFLAGS+=-fPIC -shared
LIBS+=-lc

CFLAGS+=$(shell if test `uname` = SunOS ; then echo '-Xlinker -B -Xlinker symbolic -Xlinker -s -Xlinker -R/usr/local/lib' ; fi)

CPPFLAGS+=$(shell if test `uname` = SunOS ; then echo -DSUNOS ; elif test `uname` = HP-UX ; then echo -DHPUX ; fi)

SRC=../../core/alsp_src/generic

all: gethostid.psl test Makefile

gethostid.psl : gethostid.c $(SRC)/alspi_slib.c Makefile
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LIBS) -I $(SRC) -o gethostid.psl gethostid.c $(SRC)/alspi_slib.c

test: gethostid.psl Makefile
	alspro -q -b gethostid.psl -g 'gethostid(X), atom(X), write(X), nl.'

clean:
	rm -f gethostid.psl
