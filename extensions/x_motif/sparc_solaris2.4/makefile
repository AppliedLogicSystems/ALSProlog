# Solaris2.4 build using gcc
export SRCDIR = /apache/alsp_src

export C2PINCLUDES = -I /usr/include -I /usr/include/X11 -I /usr/dt/include
export C2PDEFINES = -DXTSTRINGDEFINES -DXMSTRINGDEFINES

export CINCLUDES = -I /usr/dt/include
export CC = gcc
export LINK = -shared -Xlinker -z -Xlinker defs -Xlinker -s
export LIBS =	-L/usr/dt/lib \
		-lc -lX11 -lXm -lXt -lMrm

all:
	make -f ../common/x_motif_make

clean:
	make -f ../common/x_motif_make clean

superclean:
	make -f ../common/x_motif_make superclean
