all: student standard demo

core:
	cd ../core/win32 ; build.bat
	cd ../foreign_sdk/win32 ; make.bat
	
student: core
	build_dist.sh student

standard: core
	build_dist.sh standard

demo: core
	build_dist.sh demo
