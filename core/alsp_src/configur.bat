REM /*   configure.bat for . */
REM /* Usage:  <build_dir> and <alsp_src> at top-levels on some drive */
REM /* In <build_dir>, invoke:    \<alsp_src>\configur  <alsp_src>    */
REM 

@echo off

if "%2" == "" goto :nodrive
goto :hasdrive
:spset
set dosi386=%dossrc%\i386
echo using dos i386 source path=%dosi386%

		REM setup mt.sed for top level makefile:
set sedmt=%dosi386%\djgpp\maketop.sed
echo sedmt=%sedmt%
echo s/pathsrc/%sedpth%/g>pathdir.sed
sed -f pathdir.sed %sedmt% > m
update m mt.sed
del m

		REM apply mt.sed for top level makefile:
sed -f mt.sed %unixsrc%/makefile.in > m
update m makefile
del m

		REM create bld-port
echo Setting up bld-port
if not exist bld-port\makefile mkdir bld-port
set unixbport=%unixsrc%/bld-port

		REM setup mt.sed for bld-port/makefile:
echo s/pathsrc/..\\\/%sedpth%\\\/port/g>pathdir.sed
sed -f pathdir.sed %sedmt% > m
update m mtbld.sed
del m

		REM apply mtbld.sed for bld-port/makefile:
sed -f mtbld.sed %unixbport%/makefile.in > m
update m bld-port/makefile
del m

		REM setup tconfig.h in bld-port:
cp %unixbport%/tconfig.in > bld-port/tconfig.h

REM Remove this goto when ready to handle native code again:
goto :gmkf

		REM create bld-natv
echo Setting up bld-natv
if not exist bld-natv\makefile mkdir bld-natv
set unixbnatv=%unixsrc%/bld-natv

		REM setup mt.sed for bld-natv/makefile:
echo s/pathsrc/..\\\/%sedpth%\\\/bld-natv/g>pathdir.sed
sed -f pathdir.sed %sedmt% > m
update m mtbld.sed
del m

		REM apply mtbld.sed for bld-natv/makefile:
sed -f mtbld.sed %unixbnatv%/makefile.in > m
update m bld-natv/makefile
del m

		REM setup tconfig.h in bld-natv:
cp %unixbport%/tconfig.in > bld-port/tconfig.h


:gmkf

		REM create local modified copy of ...../generic/generic.mkf
set generdir=%dossrc%\generic
echo source generic dir = %generdir%

		REM setup (modify) generic.mkf:
set sedmtg=..\%1\i386\djgpp\mh-gener.sed
copy %generdir%\generic.mkf generic.mkf
sed -f %sedmtg% generic.mkf > m
update m generic.mkf
del m

copy %generdir%\pi_cfg.in bld-port\pi_cfg.h
copy %generdir%\pi_init.c bld-port\pi_init.c
update %unixsrc%/i386/djgpp/config.h config.h

cd bld-port
mkdir alsdir
cd alsdir
mkdir builtins
mkdir library
copy ..\..\%dossrc%\builtins\*.pro builtins
copy ..\..\%dossrc%\library\*.pro library
cd ..
cd ..

REM Remove this goto when ready to handle native code again:
goto :end

cd bld-natv
mkdir alsdir
cd alsdir
mkdir builtins
mkdir library
copy ..\..\%dossrc%\builtins\*.pro builtins
copy ..\..\%dossrc%\library\*.pro library
cd ..
cd ..

goto :end

:nodrive
set unixsrc=../%1
set dossrc=..\%1
	echo using source paths: unix=%unixsrc% dos=%dossrc%
set sedpth=..\\\/%1
	echo sedpth = %sedpth%
goto :spset

:hasdrive
set unixsrc=%2:/%1
set dossrc=%2:\%1
	echo using source paths: unix=%unixsrc%  dos=%dossrc%
set sedpth=%2\\:\\\/%1
	echo sedpth = %sedpth%
goto :spset


:end
echo Cleaning up...
del *.sed

:exit

