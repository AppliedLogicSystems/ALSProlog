pushd core\win32
call build.bat
popd
pushd installers
call buildinstaller.bat
popd
