SDK_DIR = ../../alsp_src/generic

VPATH=../common

all: security.psl

security.psl: ../common/sysinfo_security.c Makefile
	gcc -fpic -shared $(PSL_LINK) \
	-o security.psl -I$(SDK_DIR) \
	    ../common/sysinfo_security.c $(SDK_DIR)/alspi_slib.c

clean:
	rm -f security.psl
