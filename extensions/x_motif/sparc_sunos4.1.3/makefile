# SunOS build using gcc and native ld
# gcc does not support shared libraries on SunOS 4.1.3, so this
# makefile is customized
# Currently I can't make motifintf.so

export SRCDIR = /apache/alsp_src

export C2PINCLUDES = -I /usr/include  -I /usr/include/X11R5 \
	-I /usr/include/Xm 
export C2PDEFINES = -DXTSTRINGDEFINES -DXMSTRINGDEFINES
CC = gcc
CINCLUDES = -I /usr/include/X11R5
LINK = ld
LIBS = -lX11
#export LIBS =	-lc -lm -L/usr/lib/X11  \
#	-lXm -lXt -lX11



all: interface xintf.so

interface:
	make -f ../common/interface_make WIN=x 
	make -f ../common/interface_make WIN=motif

xincludes = -I ../x_source -I $(SRCDIR)/generic $(CINCLUDES)
xsource = \
	 $(SRCDIR)/generic/alspi_slib.c \
	 x0.c \
	 x1.c \
	 x2.c \
	 ../x_source/pi_init.c \
	 ../x_source/xaux.c \
	 ../x_source/xtaux.c

xintf.so: $(xsource)
	$(CC) -c $(xincludes) $(xsource)
	$(LINK) alspi_slib.o x0.o x1.o x2.o pi_init.o xaux.o xtaux.o -o xintf.so $(LIBS)


motifincludes = -I ../motif_source -I $(SRCDIR)/generic $(CINCLUDES)
motifsource = \
	 $(SRCDIR)/generic/alspi_slib.c \
	 motif0.c \
	 motif1.c \
	 motif2.c \
	 motif3.c \
	 ../motif_source/pi_init.c \
	 ../motif_source/motifaux.c \
	 ../motif_source/xtaux.c

motifintf.so: $(motifsource)
	$(CC) -c $(motifincludes) $(motifsource)
	$(LINK) alspi_slib.o motif0.o motif1.o motif2.o  motif3.o pi_init.o motifaux.o xtaux.o -o motifintf.so $(LIBS)

clean:
	make -f ../common/interface_make WIN=x clean
	make -f ../common/interface_make WIN=motif clean
	-rm -f alspi_slib.o x0.o x1.o x2.o pi_init.o xaux.o xtaux.o
	-rm -f alspi_slib.o motif0.o motif1.o motif2.o motif3.o pi_init.o motifaux.o xtaux.o

superclean: clean
	make -f ../common/interface_make WIN=x superclean
	make -f ../common/interface_make WIN=motif superclean
	-rm -f xintf.so motifintf.so