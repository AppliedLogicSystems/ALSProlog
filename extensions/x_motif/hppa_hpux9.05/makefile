# HP-UX build using gcc
export SRCDIR = /apache/alsp_src

export C2PINCLUDES = -I /usr/include -I /usr/include/X11R6 \
		     -I /usr/include/Motif1.2
export C2PDEFINES = -DXTSTRINGDEFINES -DXMSTRINGDEFINES

export CINCLUDES = -I /usr/include/X11R6 -I /usr/include/Motif1.2
export CC = gcc
export LINK = -shared -fPIC -Xlinker -s
export LIBS =	-L/usr/lib/X11R6  -L/usr/lib/Motif1.2 \
		-lX11 -lXm -lXt
export LIBEXT = sl

all:
	make -f ../common/x_motif_make

clean:
	make -f ../common/x_motif_make clean

superclean:
	make -f ../common/x_motif_make superclean
