Welcome to the ALS Prolog Source Tree
=====================================

The ALS Prolog source tree is divided into core and peripheral
directories.  The core directory contains the source for the Prolog
compiler, runtime, and IDE.  The peripheral directories contain manuals,
examples, extensions, etc.   The tree is hosted on GitHub at

	https://github.com/AppliedLogicSystems/ALSProlog

Build Instructions
------------------

Use `git clone https://github.com/AppliedLogicSystems/ALSProlog.git` to obtain the tree.

**Linux, Linux-like systems, including Mac OS X and Cygwin:**

Locate yourself in the toplevel 'unix' directory in the tree, and execute 'make'.  When the build completes, you will find a folder  

	unix/linux/als-prolog	
		or 	
	unix/darwin/als-prolog
		or 	
	unix/cygwin/als-prolog
		or possibly
	unix/<flavor>/als-prolog
where darwin is the Mac OS X flavor of linux, and &lt;flavor&gt; is possibly some other flavor of unix detected by the build process.

**Windows:**

Locate yourself in the toplevel 'win32' directory in the tree, and execute 'make'.  When the build completes, you will find a folder  

	win32/als-prolog	
Please note that as of 2015-11-5, the win32 build is broken.

