echo off

rem Check for proper function of command line arguments and general
rem command shell functionality.

echo "Starting Command Line Tests"

set prolog=F:\choupt\builds\win32\bld-port\alspro
rem prolog=alspro

rem test success/fail return value for -b -g options

echo "%prolog% -q -b -g true"
%prolog% -q -b -g true
if not errorlevel 0 goto failed
echo "Test Successful."

echo "%prolog% -q -b -g fail"
%prolog% -q -b -g fail
if not errorlevel 1 goto failed
"Test Successful."

rem currently only tests error handling for -heap and -stack options

echo "%prolog% -heap > null"
%prolog% -heap > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -stack > null"
%prolog% -stack > /dev/null
if not errorlevel 2 goto failed
"Test Successful."

echo "%prolog% -heap -stack > null"
%prolog% -heap -stack > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -stack -heap > null"
%prolog% -stack -heap > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -heap xxx > null"
%prolog% -heap xxx > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -stack xxx > null"
%prolog% -stack xxx > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -heap xxx -stack 1000 > null"
%prolog% -heap xxx -stack 1000 > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -heap 10000 -stack xxx > null"
%prolog% -heap 10000 -stack xxx > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -stack 1000 -heap xxx > null"
%prolog% -stack 1000 -heap xxx > null
if not errorlevel 2 goto failed
echo "Test Successful."

echo "%prolog% -stack xxx -heap 1000 > null"
%prolog% -stack xxx -heap 1000 > null
if not errorlevel 2 goto failed
echo "Test Successful."

rem Check error handling of out-of-range -heap and -stack values

set extreme_values='-10000000 -1000000 -100000 -10000 -1000 -100 -10 -1 0
		1 2 3 4 5 6 7 8 9 10 5000000 10000000'

echo %extreme_values$
for %%i in %extreme_values%
do
echo "%prolog% -heap $i  > null"
%prolog% -heap $i > null
if not errorlevel 2 goto failed
echo "Test Successful."
done

for i in $extreme_values
do
echo "%prolog% -stack $i  > null"
%prolog% -stack $i  > null
if not errorlevel 2 goto failed
echo "Test Successful."
done

for i in $extreme_values
do
for j in $extreme_values
do
echo "%prolog% -heap $i -stack $j  > null"
%prolog% -heap $i -stack $j > null
if not errorlevel 2 goto failed
echo "Test Successful."
done
done

rem Check the correct fuctioning of -heap and -stack

echo "%prolog% -heap 1000 -b -q -g 'statistics([_,_,heap(_,_,_,_,1024000),_])'"
%prolog% -heap 1000 -b -q -g 'statistics([_,_,heap(_,_,_,_,1024000),_])'
if not errorlevel 0 goto failed
echo "Test Successful."

echo "%prolog% -heap 2000 -b -q -g 'statistics([_,_,heap(_,_,_,_,2048000),_])'"
%prolog% -heap 2000 -b -q -g 'statistics([_,_,heap(_,_,_,_,2048000),_])'
if not errorlevel 0 goto failed
echo "Test Successful."

echo "%prolog% -stack 1000 -b -q -g 'statistics([_,stack(_,_,1024000),_,_])'"
%prolog% -stack 1000 -b -q -g 'statistics([_,stack(_,_,1024000),_,_])'
if not errorlevel 0 goto failed
echo "Test Successful."

echo "%prolog% -stack 2000 -b -q -g 'statistics([_,stack(_,_,2048000),_,_])'"
%prolog% -stack 2000 -b -q -g 'statistics([_,stack(_,_,2048000),_,_])'
if not errorlevel 0 goto failed
echo "Test Successful."

echo "%prolog% -heap 2000 -stack 1000 -b -q -g 'statistics([_,stack(_,_,1024000),heap(_,_,_,_,2048000),_])'"
%prolog% -heap 2000 -stack 1000 -b -q -g 'statistics([_,stack(_,_,1024000),heap(_,_,_,_,2048000),_])'
if not errorlevel 0 goto failed
echo "Test Successful."

echo "%prolog% -heap 1000 -stack 2000 -b -q -g 'statistics([_,stack(_,_,2048000),heap(_,_,_,_,1024000),_])'"
%prolog% -heap 1000 -stack 2000 -b -q -g 'statistics([_,stack(_,_,2048000),heap(_,_,_,_,1024000),_])'
if not errorlevel 0 goto failed
echo "Test Successful."

goto success

failed:
echo "Command Line Test Error: unexpected exit status"
goto end:

success:
echo "Finished Command Line Tests"

end:



