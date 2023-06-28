#!/bin/bash
#
echo "++++++++++++"
# t1.sh
alspro doctools.pro -g do_np -p xamps_np/x_np-1.np 
#
echo "++++++++++++"
# t2.sh
alspro doctools.pro -g do_np -p xamps_np/x_np-2.np 
#
echo "++++++++++++"
# t3.sh
alspro doctools.pro -g do_np -p xamps_np/x_np-3.np 
#
echo "++++++++++++"
# t4.sh
alspro doctools.pro -g do_np -p xamps_np/x_np-4.np 
#
echo "++++++++++++"
# t-ok.sh
rm ../docs/ref/x_sample.md
alspro doctools.pro -g do_np -p xamps_np/x_np-ok.np 
#
#t-all.sh
rm ../docs/ref/x_sample.md
alspro doctools.pro -g do_np -p xamps_np/x_np-1.np xamps_np/x_np-2.np xamps_np/x_np-3.np xamps_np/x_np-4.np xamps_np/x_np-ok.np

