# Makefile to create appropriate alsdir in ~als_dev/alsdev
# for building either the win32 or mac versions of alsdev
# 

SOURCE_DIR="../../alsp_src"
SD2=../../$(SOURCE_DIR)
TTKVer=1.03
TTKDir=/dakota/ALS_Software/ALS_Packages/Tcl-Tk_Interface/Tcl-Tk_Interface_$(TTKVer)

mac:
	echo Using source path = $(SOURCE_DIR)

	mkdir -p alsdir/builtins
	rm -f alsdir/builtins/*.pro alsdir/builtins/*.obp
	cd alsdir/builtins ; ln -s $(SD2)/builtins/*.pro . ; cd ../..

	mkdir -p alsdir/library
	rm -f alsdir/library/*.pro alsdir/library/*.alb alsdir/library/*.obp
	cd alsdir/library ; ln -s $(SD2)/library/*.pro . ; cd ../..
	cd alsdir/library ; ln -s $(SD2)/library/*.alb . ; cd ../..

	mkdir -p alsdir/images
	rm -f alsdir/images/*.*
	cd alsdir/images ; ln -s ../../images/*.* . ; cd ../..

	mkdir -p alsdir/shared
	rm -f alsdir/shared/*.*
	cd alsdir/shared ; ln -s ../../*.tcl . ; cd ../..
	./hide_tclintf_consult ../../tcltk_interface/common


win32:
	echo Using source path = $(SOURCE_DIR)

	mkdir -p alsdir/builtins
	rm -f alsdir/builtins/*.pro
	cd alsdir/builtins ; ln -s $(SD2)/builtins/*.pro . ; cd ../..

	mkdir -p alsdir/library
	rm -f alsdir/library/*.pro alsdir/library/*.alb
	cd alsdir/library ; ln -s $(SD2)/library/*.pro . ; cd ../..
	cd alsdir/library ; ln -s $(SD2)/library/*.alb . ; cd ../..

	mkdir -p alsdir/images
	rm -f alsdir/images/*.*
	cd alsdir/images ; ln -s ../../images/*.* . ; cd ../..

	mkdir -p alsdir/shared
	rm -f alsdir/shared/*.*
	cd alsdir/shared ; ln -s ../../*.tcl . ; cd ../..
	rm tcltk.pro
	cd alsdir/shared ; cp $(TTKDir)/win32/*.* . ; cd ../..



