SDK_DIR=../../core/alsp_src/generic

all: python.psl

python.psl: python_interface.c
	gcc -fpic -shared -o python.psl -I$(SDK_DIR) -I/usr/include/python1.5 \
	    python_interface.c \
	    $(SDK_DIR)/alspi_slib.c \
	    $(SDK_DIR)/new_alspi.c \
	     -lc -L /usr/lib/python1.5/config -lpython1.5 -lpthread

clean:
	rm -f python.psl
