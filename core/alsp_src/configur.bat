@echo off
echo Starting....
		rem  configur.bat
		rem  Configuration program for DOS
		rem
		rem  Usage: <build_dir> = the (current) dir, in which build will
		rem         take place. <build dir> and als source dir must be on
		rem         same drive.
		rem
		rem  <alsp_src_DOS> = absolute path to als source dir,
		rem                   in DOS format (e.g., \foo\bar\alsp_src )
		rem  <alsp_src_UNIX> = absolute path to als source dir,
		rem                    in UNIX format (e.g., /foo/bar/alsp_src)
		rem
		rem  In <build_dir>, invoke:    
		rem      \<alsp_src_DOS>\configur  <alsp_src_DOS> <alsp_src_UNIX>

@echo off

set dossrc=%1
set dosi386=%dossrc%\i386
set dosdjgpp=%dosi386%\djgpp
echo using dos source path: dossrc=%dossrc%
echo using dosi386=%dosi386%    dosdjgpp=%dosdjgpp%

echo s@pathsrc@%dossrc%@g>pd1.tmp
sed -f %dosdjgpp%\d2qux.sed pd1.tmp > pd2.tmp
sed -f %dosdjgpp%\at2ds.sed pd2.tmp > pathdir.sed
rm *.tmp

		rem setup mt.sed for top level makefile:
set sedmt=%dosi386%\djgpp\maketop.sed
echo sedmt=%sedmt%
sed -f pathdir.sed %sedmt% > m
update m mt.sed
del m
echo mt.sed done

		rem apply mt.sed for top level makefile:
copy %dossrc%\makefile.in mf1.tmp
sed -f mt.sed mf1.tmp > m
update m makefile
del m
del *.tmp

		rem create local modified copy of %dossrc%\generic\generic.mkf
set generdir=%dossrc%\generic
echo source generic dir = %generdir%

		rem setup (modify) generic.mkf:
set sedmtg=%dosdjgpp%\mh-gener.sed
copy %generdir%\generic.mkf generic.mkf
sed -f %sedmtg% generic.mkf > m
update m generic.mkf
del m

copy %dosdjgpp%\config.h config.h

		rem create bld-port
echo Setting up bld-port
if not exist bld-port\makefile mkdir bld-port

copy %generdir%\pi_cfg.in bld-port\pi_cfg.h
copy %generdir%\pi_init.c bld-port\pi_init.c

		rem setup mt.sed for bld-port/makefile:
echo s@pathsrc@%dossrc%\port@g>pd1.tmp
sed -f %dosdjgpp%\d2qux.sed pd1.tmp > pd2.tmp
sed -f %dosdjgpp%\at2ds.sed pd2.tmp > pathdir.sed
rm *.tmp

sed -f pathdir.sed %sedmt% > m
update m mtbld.sed
del m

		rem apply mtbld.sed for bld-port/makefile:
copy %dossrc%\bld-port\makefile.in mf1.tmp
sed -f mtbld.sed mf1.tmp > m
update m bld-port/makefile
del m
del *.tmp

		rem setup tconfig.h in bld-port:
echo /* tconfig.h */ > m
echo #include "dfltsys.h" >> m
update m bld-port/tconfig.h
del m

cd bld-port
mkdir alsdir
cd alsdir
mkdir builtins
mkdir library
copy %dossrc%\builtins\*.pro builtins
copy %dossrc%\library\*.pro library
cd ..
cd ..
echo Finished setting up bld-port

rem Remove this goto when ready to handle native code again:
goto :end

		rem create bld-natv
echo Setting up bld-natv
if not exist bld-natv\makefile mkdir bld-natv

		rem setup mt.sed for bld-natv/makefile:
echo s@pathsrc@%dossrc%\port@g>pd1.tmp
sed -f %dosdjgpp%\d2qux.sed pd1.tmp > pd2.tmp
sed -f %dosdjgpp%\at2ds.sed pd2.tmp > pathdir.sed
rm *.tmp
sed -f pathdir.sed %sedmt% > m
update m mtbld.sed
del m

		rem apply mtbld.sed for bld-natv/makefile:
copy %dossrc%\bld-port\makefile.in mf1.tmp
sed -f mtbld.sed mf1.tmp > m
update m bld-natv/makefile
del m

		rem setup tconfig.h in bld-natv:
echo /* tconfig.h */ > m
echo #include "dfltsys.h" >> m
update m bld-natv/tconfig.h
del m

cd bld-natv
mkdir alsdir
cd alsdir
mkdir builtins
mkdir library
copy %dossrc%\builtins\*.pro builtins
copy %dossrc%\library\*.pro library
cd ..
cd ..

echo Finished setting up bld-port


goto :end

:end
echo Cleaning up...
del *.sed
del *.tmp

:exit

