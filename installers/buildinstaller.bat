rmdir /q /s "ALS Student Prolog"
mkdir "ALS Student Prolog"
copy "..\core\win32\ALS Student Prolog.exe" "ALS Student Prolog"
copy ..\core\win32\msvcrt.dll "ALS Student Prolog"
copy ..\core\win32\tcl80.dll "ALS Student Prolog"
copy ..\core\win32\tk80.dll "ALS Student Prolog"
copy ..\core\win32\tclpip80.dll "ALS Student Prolog"
xcopy /e /i ..\core\win32\alsdir "ALS Student Prolog\alsdir"
xcopy /e /i ..\core\win32\lib "ALS Student Prolog\lib"

xcopy /i ..\examples\als\*.pro "ALS Student Prolog\examples\als"
xcopy /i ..\examples\pxs\*.pro "ALS Student Prolog\examples\pxs"

copy ..\manual\welcome_student.als "ALS Student Prolog\Welcome.txt"
copy ..\manual\copying.als "ALS Student Prolog\Copying ALS.txt"
copy ..\manual\student_man.pdf "ALS Student Prolog\Student Manual.pdf"

xcopy /i ..\core\alsp_src\library\*.pro "ALS Student Prolog\alsdir\library"
xcopy /i ..\core\alsp_src\library\*.alb "ALS Student Prolog\alsdir\library"


