rmdir /q /s "ALS Prolog"
mkdir "ALS Prolog"
copy "..\core\win32\ALS Prolog.exe" "ALS Prolog"
copy ..\core\win32\msvcrt.dll "ALS Prolog"
copy ..\core\win32\tcl80.dll "ALS Prolog"
copy ..\core\win32\tk80.dll "ALS Prolog"
copy ..\core\win32\tclpip80.dll "ALS Prolog"
xcopy /e /i ..\core\win32\alsdir "ALS Prolog\alsdir"
xcopy /e /i ..\core\win32\lib "ALS Prolog\lib"

xcopy /i ..\examples\als\*.pro "ALS Prolog\examples\als"
xcopy /i ..\examples\pxs\*.pro "ALS Prolog\examples\pxs"

copy ..\manual\student_man.pdf "ALS Prolog"

xcopy /i ..\core\alsp_src\library\*.pro "ALS Prolog\alsdir\library"
xcopy /i ..\core\alsp_src\library\*.alb "ALS Prolog\alsdir\library"


