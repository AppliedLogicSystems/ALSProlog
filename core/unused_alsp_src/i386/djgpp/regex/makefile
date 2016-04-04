# You probably want to take -DREDEBUG out of CFLAGS, and put something like
# -O in, *after* testing (-DREDEBUG strengthens testing by enabling a lot of
# internal assertion checking and some debugging facilities).
# Put -Dconst= in for a pre-ANSI compiler.
# Do not take -DPOSIX_MISTAKE out.
# REGCFLAGS isn't important to you (it's for my use in some special contexts).
CFLAGS=-I. -I.. -DPOSIX_MISTAKE -DREDEBUG $(REGCFLAGS) -O2

# Flags for linking but not compiling, if any.
LDFLAGS=

# Extra libraries for linking, if any.
LIBS=

# Internal stuff, should not need changing.
OBJPRODN=regcomp.o regexec.o regerror.o regfree.o
OBJS=$(OBJPRODN) split.o debug.o main.o
H=cclass.h cname.h regex2.h utils.h
REGSRC=regcomp.c regerror.c regexec.c regfree.c
ALLSRC=$(REGSRC) engine.c debug.c main.c split.c

# Stuff that matters only if you're trying to lint the package.
LINTFLAGS=-I. -Dstatic= -Dconst= -DREDEBUG
LINTC=regcomp.c regexec.c regerror.c regfree.c debug.c main.c
JUNKLINT=possible pointer alignment|null effect

default:	lib

lib:	$(OBJPRODN)
	rm -f ../libregex.a
	ar crv ../libregex.a $(OBJPRODN)

# dependencies
$(OBJPRODN) debug.o:	utils.h regex.h regex2.h
regcomp.o:	cclass.h cname.h regcomp.ih
regexec.o:	engine.c engine.ih
regerror.o:	regerror.ih
debug.o:	debug.ih
main.o:	main.ih

# tester
re:	$(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

# regression test
r:	re tests
	go32 re <tests
	go32 re -el <tests
	go32 re -er <tests

# 57 variants, and other stuff, for development use -- not useful to you
ra:	re tests
	-go32 re <tests
	-go32 re -el <tests
	-go32 re -er <tests

rx:	re tests
	go32 re -x <tests
	go32 re -x -el <tests
	go32 re -x -er <tests

t:	re tests
	-time go32 re <tests
	-time go32 re -cs <tests
	-time go32 re -el <tests
	-time go32 re -cs -el <tests

l:	$(LINTC)
	lint $(LINTFLAGS) -h $(LINTC) 2>&1 | egrep -v '$(JUNKLINT)' | tee lint

fullprint:
	ti README WHATSNEW notes todo | list
	ti *.h | list
	list *.c
	list regex.3 regex.7

print:
	ti README WHATSNEW notes todo | list
	ti *.h | list
	list reg*.c engine.c


mf.tmp:	Makefile
	sed '/^REGEXH=/s/=.*/=regex.h/' Makefile | sed '/#DEL$$/d' >$@

DTRH=cclass.h cname.h regex2.h utils.h
PRE=COPYRIGHT README WHATSNEW
POST=regex.3 regex.7 tests $(DTRH) $(ALLSRC) fake/*.[ch]
FILES=$(PRE) Makefile $(POST)
DTR=$(PRE) Makefile=mf.tmp $(POST)
dtr:	$(FILES) mf.tmp
	makedtr $(DTR) >$@
	rm mf.tmp

cio:	$(FILES)
	cio $(FILES)

rdf:	$(FILES)
	rcsdiff -c $(FILES) 2>&1 | p

# various forms of cleanup
tidy:
	rm -f junk* core core.* *.core dtr *.tmp lint

clean:	tidy
	rm -f *.o *.s re libregex.a
