# Solaris 2.4 build using gcc
export SRCDIR = /apache/alsp_src

export CINCLUDES = 
export CC = gcc -fPIC
export LINK = -shared
export LIBS = /usr/local/lib/iodbc-2.12.so -lc
export LIBEXT = so

all:
	make -f ../unix/Makefile

superclean:
	make -f ../unix/Makefile superclean
