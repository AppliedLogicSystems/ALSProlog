# HP-UX build using gcc
export SRCDIR = /apache/alsp_src

export CINCLUDES = 
export CC = gcc
export LINK = -shared -fPIC -Xlinker -s
export LIBS = /usr/local/lib/iodbc-2.12.sl
export LIBEXT = sl

all:
	make -f ../src/makefile.unix

superclean:
	make -f ../src/makefile.unix superclean
