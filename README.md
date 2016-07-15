Welcome to the ALS Prolog Source Tree
=====================================

[![Build Status](https://travis-ci.org/AppliedLogicSystems/ALSProlog.svg?branch=master)](https://travis-ci.org/AppliedLogicSystems/ALSProlog)

The ALS Prolog source tree is divided into core and peripheral
directories.  The core directory contains the source for the Prolog
compiler, runtime, and IDE. The peripheral directories contain manuals, examples, extensions, etc. The tree is hosted on GitHub at:

	https://github.com/AppliedLogicSystems/ALSProlog

Build Instructions
------------------

Use `git clone https://github.com/AppliedLogicSystems/ALSProlog.git` to obtain the tree.

**Linux, Unix-like systems, including Mac OS X and Cygwin:**

Locate yourself in the toplevel of the tree, and execute:

	cmake .
	make
	make package
	   

 When the package build completes, you will have a packaged tar.gz file.

**Windows:**

Locate yourself in the toplevel 'win32' directory in the tree, and execute 'make'.  When the build completes, you will find a folder  

	win32/als-prolog	

Build Dependencies
------------------

### Linux

Generally, a 32-bit GNU/Linux with standard build tools (Make, GCC 4), Git, and Tcl/Tk. Packages for popular distributions:

Debian/Ubuntu: `sudo apt-get install build-essential git tk-dev cmake`

CentOS/Fedora: `sudo yum groupinstall 'Development Tools' && sudo yum install tk-devel cmake`

### Mac OS X

[Xcode](https://developer.apple.com/xcode/) command line tools.  
[CMake](https://cmake.org) build system. Available from [Hombrew](http://brewformulas.org/Cmake).  

### Windows

[Cygwin](https://cygwin.com) 32-bit is used to build both Win32 and Cygwin, with packages:

- Devel (git cmake make gcc-core mingw64-i686-gcc-core libcrypt-devel mingw64-i686-libgcrypt)
- Tcl-Tk (tcl-tk-devel mingw64-i686-tcl mingw64-i686-tk)
- Archive (zip)
- X11 (optional to run X11 alsdev)
