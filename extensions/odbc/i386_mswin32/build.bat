setlocal
set CWPATH=C:\Program Files\Metrowerks\CodeWarrior
copy setcwd.bat temp.bat
cd >> temp.bat
call temp.bat
del temp.bat
"%CWPATH%\bin\ide.exe" /b /t "ODBC Prolog Interface" "%CWD%\odbcintf.dll.mcp"
endlocal
