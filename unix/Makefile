all: student standard

core:
	$(MAKE) -C ../core/unix
	cd ../foreign_sdk/unix ; build

student: core
	build_dist.sh student

standard: core
	build_dist.sh standard