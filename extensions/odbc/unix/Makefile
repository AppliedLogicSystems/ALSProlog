SDK_DIR = ../../../core/alsp_src

VPATH=../common/save_V20:../src:../../../core/alsp_src/generic

CPPFLAGS= -I- -I /apache/als_build_support/unix/include -I ../common -I ../src -I ../../../core/alsp_src/generic

CFLAGS=-fpic -MD

SOURCE= alspi_slib.c pi_init.c odbc0.c odbc1.c odbc2.c odbcaux.c

OBJECTS = $(SOURCE:.c=.o)

odbc.psl: $(OBJECTS)
	gcc -fpic -MD -shared $(OBJECTS) -liodbc -lc -o odbc.psl

clean:
	rm -f *.o *.d odbc.psl

-include *.d