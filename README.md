Welcome to the ALS Prolog Source Tree
=====================================
Test chang345 -- more

The ALS Prolog source tree is divided into core and peripheral
directories.  The core directory contains the source for the Prolog
compiler, runtime, and IDE.  The peripheral directories contain manuals,
examples, extensions, etc. 

Checkout Instructions
---------------------

**Unix:**

Use Unix CVS to checkout als_prolog.  This insures correct line endings.

**Macintosh:**

Use MacCVS Pro and the "ALS Prolog CVS Stationary" (in
/home/als_build_support/macos)  to checkout als_prolog.  This insures
that line endings and file types are correct. 

**Windows:**

Use Windows CVS to checkout als_prolog. This insures correct line endings.

Note:

For public release builds, the CVS information must be removed from the 
build tree.

On Unix and Windows, use the cvs export command to create a clean tree. 

On the Mac, use MacCVS Pro's Action->Orphan Files command to clean the
tree. 

Build Instructions
------------------

**Unix:**

In the als_prolog/unix directory, run GNU Make (make or gmake, as appropriate)

**Macintosh:**

In the als_prolog folder, run the AppleScript Build.

**Windows:**

In the als_prolog\win32 directory, run GNU Make (use bash/cygwin)

Open each installer template (standard_installer.wse and
student_installer.wse)  with Wise and view it in the "Installation Expert" 
mode.

Edit the properties of "Installation Files" under the "Application Files" 
attribute. 

Add the contents of "ALS Prolog" or "ALS Student Prolog" to the
Applications folder.  Click on the Distribute button to create the
installer. 

