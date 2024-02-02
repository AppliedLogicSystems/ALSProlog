all: standard

core:
	$(MAKE) -C ../core/unix all
	cd ../foreign_sdk/unix ; ./build.sh

doc:
	$(MAKE) -C ../docs all

odbc:
	cd ../extensions/odbc/i386_mswin32 ; ./build.bat
	  
standard: core doc
	./build_dist.sh standard

standard-nodoc: core
	./build_dist.sh standard

# If you want to just capture test output to a file, execute:
#	make test 2>&1 | cat > testrun.log
# To capture test output to a file, while simultaneously viewing output on the console, execute:
#	make test 2>&1 | tee testrun.log
# It takes 30 seconds on a 2.6 GHz Macbook Pro.

test: core
	$(MAKE) -C ../core/unix test

testdocs:
	$(MAKE) -C ../docs test

distclean:
	$(MAKE) -C ../core/unix distclean
	$(MAKE) -C ../docs clean
	sh superclean.sh
