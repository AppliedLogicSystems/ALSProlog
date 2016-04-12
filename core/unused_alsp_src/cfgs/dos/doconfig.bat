REM Final DJGPP build directory configuration
REM %1 should = absolute path to source dir, 
REM     in Unix/DGJPP slashes(/)
echo Using source directory = %1

echo s^@srcdir@^%1%^g>sed.tmp
sed -f sed.tmp makefile > m
update m makefile
rm m

cd bld-port
sed -f ../sed.tmp makefile > m
update m makefile
rm m

cd ..
rm sed.tmp
