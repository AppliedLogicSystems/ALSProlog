setlocal
set CWPATH=C:\Program Files\Metrowerks\CodeWarrior
set TCLTKPATH=%CWPATH%\ALS Build Support\Tcl-Tk Support
copy setcwd.bat temp.bat
cd >> temp.bat
call temp.bat
del temp.bat
"%CWPATH%\bin\ide.exe" /b /t "Build All" "%CWD%\CWP4_win32_alsdev.mcp"
copy "ALS Prolog Stub.exe" "ALS Prolog.exe"
copy "ALS Prolog Stub.exe" "ALS Student Prolog.exe"
copy "%TCLTKPATH%\bin\msvcrt.dll" .
copy "%TCLTKPATH%\bin\tclpip80.dll" .
copy "%TCLTKPATH%\bin\tcl80.dll" .
copy "%TCLTKPATH%\bin\tk80.dll" .
copy "%TCLTKPATH%\bin\itcl30.dll" .
copy "%TCLTKPATH%\bin\itk30.dll" .
xcopy /e /i "%TCLTKPATH%\lib\tcl8.0" lib\tcl8.0
mkdir lib\tk8.0
copy  "%TCLTKPATH%\lib\tk8.0\*" lib\tk8.0
xcopy /e /i "%TCLTKPATH%\lib\itcl3.0" lib\itcl3.0
xcopy /e /i "%TCLTKPATH%\lib\itk3.0" lib\itk3.0
xcopy /e /i "%TCLTKPATH%\lib\iwidgets3.0" lib\iwidgets3.0
mkdir alsdir
mkdir alsdir\shared
copy ..\als_dev\alsdev\*.tcl alsdir\shared
mkdir alsdir\images
xcopy ..\als_dev\alsdev\images\*.gif alsdir\images
set DEV_ALSDIR=..\alsp_src
alspro_b -b -g "(consult('..\\als_dev\\alsdev\\ldr_dvsh'),consult('..\\tcltk_interface\\common\\tcltk_util'), attach_image('ALS Prolog.exe'))"
alspro_b -b -g "(consult('..\\als_dev\\alsdev\\ldr_dvsh'),consult('..\\tcltk_interface\\common\\tcltk_util'),builtins:abolish(save_image,1),builtins:abolish(save_image,2),sio:abolish(open_socket_stream,4),attach_image('ALS Student Prolog.exe'))"
alspro_b -b -g "save_image(alspro)"
endlocal
