#include <Types.r>
#include <SysTypes.r>

resource 'vers' (1) {
	2, 2,
	final, 0x00, verUS,
	"2.2",
	"tkTable 2.2 by Jeffrey Hobbs\n"
	"Macintosh Port by Chuck Houpt"
};

resource 'vers' (2) {
	2, 2,
	final, 0x00, verUS,
	"2.2",
	"tkTable 2.2 © 1997,1998"
};

/*
 * The -16397 string will be displayed by Finder when a user
 * tries to open the shared library. The string should
 * give the user a little detail about the library's capabilities
 * and enough information to install the library in the correct location.  
 * A similar string should be placed in all shared libraries.
 */
resource 'STR ' (-16397, purgeable) {
	"tkTable Library\n\n"
	"This library provides the ability to create tables "
	" from Tcl/Tk programs.  To work properly, it "
	"should be placed in the ‘Tool Command Language’ folder "
	"within the Extensions folder."
};

read 'TEXT' (3000, "tkTable", purgeable, preload) "tkTable.tcl";

/* 
 * We now load the Tk library into the resource fork of the library.
 */

data 'TEXT' (4000,"pkgIndex",purgeable, preload) {
	"package ifneeded Tktable 2.2 "
	"\"package require Tk; [list load [file join $dir Tktable.shlb] Tktable]\""
};
