#!/bin/sh

set -eu

echo "Doing superclean"
case `uname -rs` in
    "SunOS 4"*)	ARCH=sunos ;;
    "SunOS 5"*)	ARCH=solaris ;;
    Linux*) 	ARCH=linux ;;
    "HP-UX"*)	ARCH=hpux ;;
    "IRIX"*)	ARCH=irix ;;
    *"_NT"*) 	ARCH=win32 ;;
    "Darwin"*)	ARCH=darwin ;;
    *) 		echo "Unknown machine type..."; exit 1 ;;
esac
	rm -rf *.tgz *.zip $ARCH
	rm -rf ../foreign_sdk/*/ALS_Prolog_Foreign_SDK

