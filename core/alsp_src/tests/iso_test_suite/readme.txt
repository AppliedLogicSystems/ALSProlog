This is the README file for the validation suite for conformance
to the ISO/IEC Prolog Standard (1995). 

The suite has  been written by J.P.E. Hodgson, John Hallat and 
Joseph Pedano of Saint Joseph's University, with the support of
Ken Bowen of ALS. 

Files required to run the suite:

Files containing prolog code:

  All the files that contain tests of the standard are named
secxxx.pro. So that section8.10 is tested in sec801.pro
and section 9.1 is tested in sec9.1. 

There is a support file direct2.pro that is required in the tests
of section 7.4 (directives).

There are two versions of the utilities one:

utils_so.pro
is used by valid_so.pro and writes its output
to the standard output. It should be noted that you will not get all
the output sent to a file from this version if you use tell because the 
tests of set_output reset the output and it doesn't then go back to the
file you selected. If however you remove the section 8.11 tests and
run the rest everything should work. 

maxint.pro contains tests that require that the flag max_integer is defined.
 

utils_lg.pro 
is used by the valid_lg.pro and write the results to a log
file called 'validn.txt'. You can change the name by changing the predicate
open_log in utils_lg.pro to open another file.  


In any case to run the suite load either of the valid_xx.pro files
and when everything is  loaded type the goal run_all_tests.

Non prolog files that are required.
The IO predicates use some additional files. These are
 
charfile.txt
chario.tmp
codeio.tmp
opfile.tmp
outchar.txt
peekbyte.bin
peekchar.txt
peekfile.txt
termfile.txt
termio.tmp
valid.out

Some of them are only written to, but all are supplied.


Finally in order to get the suite to run with ALS all the multifile
directvies has to be commented out. There is one in each secxxx.pro file
and in maxint.pro.


