# Solaris 2.4 build using gcc
export SRCDIR = /apache/alsp_src

export CINCLUDES = 
export CC = gcc
export LINK = -shared -Xlinker -z -Xlinker defs -Xlinker -s
export LIBS = /usr/local/lib/iodbc-2.12.so -lc
export LIBEXT = so

all:
	make -f ../src/makefile.unix

superclean:
	make -f ../src/makefile.unix superclean
