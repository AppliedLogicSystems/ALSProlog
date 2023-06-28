#!/bin/sh
# Called from blts_xprt_udoc.pro to obtain raw lines
# from builtins/....pro files starting with 'export'

Tgt=./TmpDir/exps_blts.txt
BltPath=/Users/ken/ALS/GitHub/ALSProlog/core/alsp_src/builtins

for ff in $BltPath/*.pro; do
	ff=${ff##*/}
echo "bix_file='$ff'" >> $Tgt

echo "`grep '^export' $BltPath/$ff`\n" >> $Tgt
done
