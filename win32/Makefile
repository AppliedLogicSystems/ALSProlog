all: student standard demo

core:
	cd ../core/win32 ; ./build.bat
	cd ../foreign_sdk/win32 ; ./make.bat

odbc:
	cd ../extensions/odbc/i386_mswin32 ; ./build.bat
	
student: core
	build_dist.sh student

standard: core odbc
	build_dist.sh standard

demo: core odbc
	build_dist.sh demo
