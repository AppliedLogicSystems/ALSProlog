set CWPATH=\Program Files\Metrowerks\CodeWarrior
set TCLTKPATH=%CWPATH%\ALS Build Support\Tcl-Tk Support
copy setcwd.bat temp.bat
cd >> temp.bat
call temp.bat
del temp.bat
echo %CWD%
"%CWPATH%\bin\ide.exe" /b /t "Build All" "%CWD%\CWP4_win32_alsdev.mcp"
copy "ALS Prolog Stub.exe" "ALS Prolog.exe"
copy "%TCLTKPATH%\bin\tcl80.dll" .
copy "%TCLTKPATH%\bin\tk80.dll" .
xcopy /e /i "%TCLTKPATH%\lib" lib
mkdir alsdir
mkdir alsdir\shared
copy ..\als_dev\alsdev\*.tcl alsdir\shared
xcopy /e /i ..\als_dev\alsdev\images alsdir\images
set DEV_ALSDIR=..\alsp_src
alspro_b -b -g "(consult('..\\als_dev\\alsdev\\ldr_dvsh'), consult('..\\tcltk_interface\\common\\tcltk_util'), attach_image('ALS Prolog.exe'))"
set DEV_ALSDIR=