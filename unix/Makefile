all: student standard demo

core:
	$(MAKE) -C ../core/unix
	cd ../foreign_sdk/unix ; ./build

odbc:
	cd ../extensions/odbc/i386_mswin32 ; ./build.bat
	  
student: core
	./build_dist.sh student

standard: core
	./build_dist.sh standard

demo: core
	./build_dist.sh demo
