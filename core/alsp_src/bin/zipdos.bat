cd alsp_dos.src
pkzip src.zip -rm
mv src.zip ..
cd ..\builds\i386_dos
pkzip bld.zip -rm
mv bld.zip ../..
cd ..\..
