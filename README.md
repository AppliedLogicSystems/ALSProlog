Welcome to the ALS Prolog Source Tree
=====================================

[![Travis-CI Build Status](https://travis-ci.org/AppliedLogicSystems/ALSProlog.svg?branch=master)](https://travis-ci.org/AppliedLogicSystems/ALSProlog)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/786esihnss6ksk7k/branch/master?svg=true)](https://ci.appveyor.com/project/AppliedLogicSystems/alsprolog/branch/master)

The ALS Prolog source tree is divided into core and peripheral
directories.  The core directory contains the source for the Prolog
compiler, runtime, and IDE. The peripheral directories contain manuals, examples, extensions, etc. The tree is hosted on GitHub at:

	https://github.com/AppliedLogicSystems/ALSProlog

Build Instructions
------------------

Use `git clone https://github.com/AppliedLogicSystems/ALSProlog.git` to obtain the tree.

**Linux, Unix-like systems, including Mac OS X and Cygwin:**

Locate yourself in the toplevel 'unix' directory in the tree, and execute 'make'.  When the build completes, you will find a folder  

	unix/linux/als-prolog	
		or 	
	unix/darwin/als-prolog
		or 	
	unix/cygwin/als-prolog
		or possibly
	unix/<flavor>/als-prolog
where darwin is the Mac OS X flavor of Unix, and &lt;flavor&gt; is possibly some other flavor of unix detected by the build process.

**Windows:**

Locate yourself in the toplevel 'win32' directory in the tree, and execute 'make'.  When the build completes, you will find a folder  

	win32/als-prolog	

Build Dependencies
------------------

### Linux

Generally, a GNU/Linux OS with standard build tools (Make, GCC with 32-bit support), Git, Libcurl, and Tcl/Tk. Packages for popular distributions:

Debian/Ubuntu: `sudo dpkg --add-architecture i386 && sudo apt-get update && sudo apt-get install build-essential git gcc-multilib libcurl4-openssl-dev:i386 tk-dev:i386`

CentOS/Fedora: `sudo yum groupinstall 'Development Tools' && sudo yum install glibc-devel.i686 libgcc.i686 libcurl.i686 tcl-devel.i686 tcl.i686 tk-devel.i686 tk.686`

### Mac OS X

[Xcode](https://developer.apple.com/xcode/) command line tools.

### Windows

[Cygwin](https://cygwin.com) 32-bit is used to build both Win32 and Cygwin, with packages:

- Devel (git make gcc-core mingw64-i686-gcc-core libcrypt-devel mingw64-i686-libgcrypt)
- Tcl-Tk (tcl-tk-devel mingw64-i686-tcl mingw64-i686-tk)
- Libcurl (mingw64-i686-curl php procps)
- Archive (zip)
- X11 (optional to run X11 alsdev)
