#!/bin/sh

set -eu

echo "Doing superclean"
case `uname -rs` in
    "SunOS 4"*)	ARCH=sunos ;;
    "SunOS 5"*)	ARCH=solaris ;;
    Linux*) 	ARCH=linux ;;
    "HP-UX"*)	ARCH=hpux ;;
    "IRIX"*)	ARCH=irix ;;
    "CYGWIN"*) 	ARCH=cygwin32 ;;
    "Darwin"*)	ARCH=darwin ;;
    *) 		echo "Unknown machine type..."; exit 1 ;;
esac
	rm -rf *.tgz $ARCH


