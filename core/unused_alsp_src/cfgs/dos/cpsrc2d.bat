REM Copies appropriate portions of the source tree
REM from drive %1:\alsp_src  to drive%2:\alsp_src
copy %1:\alsp_src\*.* %2:\alsp_src
xcopy %1:\alsp_src\bld-port %2:\alsp_src\bld-port /s /e
xcopy %1:\alsp_src\bld-natv %2:\alsp_src\bld-natv /s /e
xcopy %1:\alsp_src\builtins %2:\alsp_src\builtins /s /e
xcopy %1:\alsp_src\cfgs %2:\alsp_src\cfgs /s /e
xcopy %1:\alsp_src\cinterf %2:\alsp_src\cinterf /s /e
xcopy %1:\alsp_src\generate %2:\alsp_src\generate /s /e
xcopy %1:\alsp_src\generic %2:\alsp_src\generic /s /e
xcopy %1:\alsp_src\i386 %2:\alsp_src\i386 /s /e
xcopy %1:\alsp_src\library %2:\alsp_src\library /s /e
xcopy %1:\alsp_src\pconfig %2:\alsp_src\pconfig /s /e
xcopy %1:\alsp_src\port %2:\alsp_src\port /s /e
xcopy %1:\alsp_src\tests %2:\alsp_src\tests /s /e
xcopy %1:\alsp_src\utils %2:\alsp_src\utils /s /e
