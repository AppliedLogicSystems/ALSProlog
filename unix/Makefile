all: standard

core:
	$(MAKE) -C ../core/unix
	cd ../foreign_sdk/unix ; ./build

odbc:
	cd ../extensions/odbc/i386_mswin32 ; ./build.bat
	  
standard: core
	./build_dist.sh standard

# Advice: don't attempt to capture output using "make test | tee <file>"
# Some tests produce shell error output which is not caught by the tee
# To get a complete log file: Start with a completely new terminal window.
# Navigate to the directory containing this file, and execute "make test"
# Then select the entire window (on Mac, ^A), copy it, open an editor,
# and paste the selection into the editor, and save.
# Note that autotest.log is still produced in core/unix/$ARCH

test: core
        $(MAKE) -C ../core/unix test

