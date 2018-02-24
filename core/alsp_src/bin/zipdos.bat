cd alsp_dos.src
pkzip src.zip -rp
mv src.zip ..
cd ..\builds\i386_dos
pkzip bld.zip -rp
mv bld.zip ../..
cd ..\..
