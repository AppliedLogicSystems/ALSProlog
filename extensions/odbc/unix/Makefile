SDK_DIR = ../../../core/alsp_src

VPATH=../common:../src:../../../core/alsp_src/generic

CPPFLAGS= -I- -I /home/als_build_support/unix/include -I ../common -I ../src -I ../../../core/alsp_src/generic

CFLAGS=-fpic -MD

SOURCE= alspi_slib.c pi_init.c odbc0.c odbc1.c odbc2.c odbc3.c odbcaux.c

OBJECTS = $(SOURCE:.c=.o)

odbc.psl: $(OBJECTS)
	gcc -fpic -MD -shared $(OBJECTS) -L/home/als_build_support/unix/linux/lib -lodbc -lc -o odbc.psl

clean:
	rm -f *.o *.d odbc.psl

-include *.d
