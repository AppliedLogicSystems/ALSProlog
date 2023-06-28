#!/bin/bash
#
#1-x_libblt.sh
alspro doctools.pro -g file_doc -p 
echo ">>>>>>>>>>>>>>>"

#2-x_libblt.sh
alspro doctools.pro -g file_doc -p xqty789
echo ">>>>>>>>>>>>>>>"

#3-x_libblt.sh
#
alspro doctools.pro -g file_doc -p xqty789.pro
echo ">>>>>>>>>>>>>>>"

#4-x_libblt.sh
alspro doctools.pro -g file_doc -p xqty789.kgf
echo ">>>>>>>>>>>>>>>"

#5-x_libblt.sh
cp lib_blt_xamps/x_sample_lib_f.pro ../../core/alsp_src/library
alspro doctools.pro -g file_doc -p x_sample_lib_f.pro -lib -ow
echo ">>>>>>>>>>>>>>>"

#6-x_libblt.sh
cp lib_blt_xamps/x_sample_lib_f.pro ../../core/alsp_src/library
alspro doctools.pro -g file_doc -p x_sample_lib_f.pro -lib
echo ">>>>>>>>>>>>>>>"

#7-x_libblt.sh
cp lib_blt_xamps/x_sample_blt_f.pro ../../core/alsp_src/builtins
alspro doctools.pro -g file_doc -p x_sample_blt_f.pro -blt -ow
echo ">>>>>>>>>>>>>>>"

#8-x_libblt.sh
cp lib_blt_xamps/x_sample_blt_f.pro ../../core/alsp_src/builtins
alspro doctools.pro -g file_doc -p x_sample_blt_f.pro -blt
echo ">>>>>>>>>>>>>>>"

