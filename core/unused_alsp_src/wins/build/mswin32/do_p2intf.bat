set ALS_OPTIONS=heap_size:24000,stack_size:16192
cd i:\alsp_src\cinterf\pro2intf\
copy p2i.pro + pro2intf + intfout + mytrans i:\bldtests\mswin32\bld-win\p2intlcl.pro
cd i:\bldtests\mswin32\bld-win\
..\bld-port\alspro_b -ind p2intf_cmd.lst %1 -fpre %1
