set ALS_OPTIONS=heap_size:20000,stack_size:15192
set C=%ALSP_SRC%\cinterf\c2pro\
copy %C%c2p.pro + %C%c2pro + %C%cexp + %C%cfiles + %C%cmacro + %C%cout + %C%cparse + %C%ctoken c2prolcl.pro
set C=-b -ind c2p_cmd.lst
..\..\bld-port\alspro %C% %1 -srcpath %ALSP_SRC%\wins\src\mswin32\ -filterFile %ALSP_SRC%\wins\src\mswin32\%1.filter 
set C=
set ALS_OPTIONS=
rem %ALSP_SRC%\wins\src\mswin32\
