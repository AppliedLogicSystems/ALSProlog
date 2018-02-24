set ALS_OPTIONS=heap_size:30000,stack_size:16192
set C=%ALSP_SRC%\cinterf\c2pro\
copy %C%c2p.pro + %C%c2pro + %C%cexp + %C%cfiles + %C%cmacro + %C%cout + %C%cparse + %C%ctoken c2prolcl.pro
set C=
..\..\bld-port\alspro -b -g c2pro c2prolcl.pro -p -win32funcmacros -os mswin32 -d 1 -I C:\CW8_Gold_Headers\ -I C:\CW8_Gold_Headers\ANSI -I C:\CW8_Gold_Headers\Win32_SDK -I C:\CW8_Gold_Headers\Win32_SDK\WINNT -I C:\CW8_Gold_Headers\MFC_Extras %1 -srcpath %ALSP_SRC%\wins\src\mswin32\ -filterFile %ALSP_SRC%\wins\src\mswin32\%1.filter 
set ALS_OPTIONS=
