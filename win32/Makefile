all: standard 

core:
	$(MAKE) -C ../core/win32 all docs
	cd ../foreign_sdk/win32 ; ./make.bat

odbc:
	cd ../extensions/odbc/i386_mswin32 ; ./build.bat

python:
	cd ../extensions/python/i386_mswin32 ; ./build.bat

#standard: core odbc python
standard: core
	./build_dist.sh standard

all: standard

test: core
	$(MAKE) -C ../core/win32 test

distclean:
	$(MAKE) -C ../core/win32 distclean
	sh superclean.sh
