Welcome to the ALS Prolog Source Tree
=====================================

[![Build & Test](https://github.com/AppliedLogicSystems/ALSProlog/actions/workflows/ci.yml/badge.svg)](https://github.com/AppliedLogicSystems/ALSProlog/actions/workflows/ci.yml)
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

Generally, a GNU/Linux OS with standard build tools (Make, GCC with 32-bit support), Git, Ruby, Libcurl, and Tcl/Tk. Packages for popular distributions:

Debian/Ubuntu: `sudo dpkg --add-architecture i386 && sudo apt-get update && sudo apt-get install build-essential git gcc-multilib ruby ruby-dev php libcurl4-openssl-dev:i386 tk-dev:i386`

CentOS/Fedora: `sudo yum groupinstall 'Development Tools' && sudo yum install ruby ruby-dev php glibc-devel.i686 libgcc.i686 libcurl.i686 tcl-devel.i686 tcl.i686 tk-devel.i686 tk.686`

### Mac OS X

[Xcode](https://developer.apple.com/xcode/) command line tools. For version before High Sierra (10.13), a recent version of Ruby is required. [Homebrew Ruby](https://jekyllrb.com/docs/installation/macos/#homebrew) is recommended.

### Windows

[MSYS2](https://www.msys2.org) tool-chain is used to build for Win32, with packages:

    pacman --sync git base-devel mingw-w64-i686-toolchain mingw-w64-i686-curl zip procps

[Ruby](https://www.ruby-lang.org/en/) installed via [RubyInstaller](https://rubyinstaller.org) (use recommended Ruby+DevKit version).

[PHP](https://www.php.net) installed via [PHP Download](https://windows.php.net/download/) or elsewhere.
