all: standard

core:
	$(MAKE) -C ../core/unix
	cd ../foreign_sdk/unix ; ./build

odbc:
	cd ../extensions/odbc/i386_mswin32 ; ./build.bat
	  
standard: core
	./build_dist.sh standard

# If you want to capture test output to a file, just execute
#	make test 2>&1 | cat > testrun.log
# It takes 30 seconds on a 2.6 GHz Macbook Pro.
# Don't try to use unix tee, because it misses some shell error output.

test: core
	$(MAKE) -C ../core/unix test

