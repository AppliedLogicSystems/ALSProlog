set ALS_OPTIONS=heap_size:20000,stack_size:15192
set C=%ALSP_SRC%\cinterf\c2pro
copy %C%c2p.pro + %C%c2pro + %C%cexp + %C%cfiles + %C%cmacro + %C%cout + %C%cparse + %C%ctoken c2prolcl.pro
set C=
..\bld-port\alspro -ind c2procmd.lst %1 -srcpath %ALSP_SRC% -filterFile %ALSP_SRC%\wins\src\mswin32\%1.filter 
set ALS_OPTIONS=