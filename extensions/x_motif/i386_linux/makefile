# Linux build using gcc
export SRCDIR = /apache/alsp_src

export C2PINCLUDES = -I /usr/include -I /usr/include/X11 -I /usr/dt/include
export C2PDEFINES = -DXTSTRINGDEFINES -DXMSTRINGDEFINES

export CINCLUDES = -I /usr/dt/include
export CC = gcc -fPIC
export LINK = -shared
#export LIBS =	-L/usr/dt/lib -lc -lX11 -lXm -lXt -lMrm
export LIBS =	
export LIBEXT = so

all:
	make -f ../common/x_motif_make

clean:
	make -f ../common/x_motif_make clean

superclean:
	make -f ../common/x_motif_make superclean
