set tgtdrive=I:
copy alsdir\*.* %tgtdrive%\als_dist\i386_dos\alsdir
copy alsdir\builtins\*.pro %tgtdrive%\als_dist\i386_dos\alsdir\builtins
copy alsdir\library\*.pro %tgtdrive%\als_dist\i386_dos\alsdir\library

copy alspro*.* %tgtdrive%\als_dist\i386_dos\threaded
copy \djgpp\bin\go32.exe %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\bin\stub*.* %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\bin\setdjgpp.bat %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\bin\coff2exe.exe %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\bin\gunzip.exe %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\bin\uncompre.exe %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\docs\djgpp\copying %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\docs\djgpp\copying.* %tgtdrive%\als_dist\i386_dos\bin
copy \djgpp\docs\djgpp\djgpp.faq %tgtdrive%\als_dist\i386_dos\bin
