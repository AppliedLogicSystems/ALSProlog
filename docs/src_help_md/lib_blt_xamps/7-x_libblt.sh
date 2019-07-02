#!/bin/bash
#
cp lib_blt_xamps/x_sample_blt_f.pro ../../core/alsp_src/builtins
#alspro doctools.pro -p x_sample_blt_f.pro -blt -ow
alspro doctools.pro -g file_doc -p x_sample_blt_f.pro -blt -ow

