mkdir "ALS Prolog"
copy "..\core\win32\ALS Prolog.exe" "ALS Prolog"
copy ..\core\win32\msvcrt.dll "ALS Prolog"
copy ..\core\win32\tcl80.dll "ALS Prolog"
copy ..\core\win32\tk80.dll "ALS Prolog"
copy ..\core\win32\tclpip80.dll "ALS Prolog"
xcopy /e /i ..\core\win32\alsdir "ALS Prolog\alsdir"
xcopy /e /i ..\core\win32\lib "ALS Prolog\lib"

xcopy /e /i ..\examples "ALS Prolog\examples"

mkdir "ALS Prolog\manual"

xcopy /e /i ..\core\alsp_src\library "ALS Prolog\alsdir\library"


