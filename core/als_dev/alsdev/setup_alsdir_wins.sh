mkdir alsdir
cd alsdir
cvs -d :pserver:ken@hilbert.als.com:/apache/reposit checkout als_prolog/core/alsp_src/builtins
cvs -d :pserver:ken@hilbert.als.com:/apache/reposit checkout als_prolog/core/alsp_src/library
cd als_prolog/core/alsp_src
mv CVS ../../..
mv builtins ../../..
mv library ../../..
cd ../../..
mkdir images
cd images
cp ../../images/*.* .
cd ..
mkdir shared
cd shared
cp ../../*.tcl .
cp G:/ALS_Software/ALS_Packages/Tcl-Tk_Interface/Tcl-Tk_Interface_1.03/win32/*.* .
cd ..
rm -r als_prolog
cd ..
