mkdir -p alsdir
mkdir -p alsdir/builtins
mkdir -p alsdir/library
mkdir -p alsdir/images
mkdir -p alsdir/shared
cd alsdir/builtins ; ln -s ../../../../alsp_src/builtins/*.pro . ; cd ../..
cd alsdir/library ; ln -s ../../../../alsp_src/library/*.pro . ; cd ../..
cd alsdir/library ; ln -s ../../../../alsp_src/library/*.alb . ; cd ../..
cd alsdir/images ; ln -s ../../images/*.* . ; cd ../..
cd alsdir/shared ; ln -s ../../*.tcl . ; cd ../..
