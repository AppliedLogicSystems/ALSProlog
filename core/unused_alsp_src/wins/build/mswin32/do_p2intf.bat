set ALS_OPTIONS=heap_size:24000,stack_size:16192
set C=%ALSP_SRC%\cinterf\pro2intf\
copy %C%p2i.pro + %C%pro2intf + %C%intfout + %C%mytrans p2intlcl.pro
..\..\bld-port\alspro -b -ind p2i_cmd.lst %1 -fpre %1
set ALS_OPTIONS=