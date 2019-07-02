#!/bin/bash
#
cp lib_blt_xamps/x_sample_lib_f.pro ../../core/alsp_src/library
#alspro doctools.pro -p x_sample_lib_f.pro -lib
alspro doctools.pro -g file_doc -p x_sample_lib_f.pro -lib -ow

