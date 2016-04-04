set ALS_OPTIONS=heap_size:30000,stack_size:16192
set C=%ALSP_SRC%\cinterf\pro2intf\
copy %C%p2i.pro + %C%pro2intf + %C%intfout + %C%mytrans p2intlcl.pro
set C=
..\..\bld-port\alspro -b p2intlcl.pro -g pro2intf -p -Ddebug -t mswin32_trans %1 -fpre %1
set ALS_OPTIONS=
