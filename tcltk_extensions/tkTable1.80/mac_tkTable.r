#include <Types.r>
#include <SysTypes.r>

resource 'vers' (1) {
	1, 8,
	final, 0x00, verUS,
	"1.80",
	"Tktable 1.80 by Jeffrey Hobbs"
};

/*
 * The -16397 string will be displayed by Finder when a user
 * tries to open the shared library. The string should
 * give the user a little detail about the library's capabilities
 * and enough information to install the library in the correct location.  
 * A similar string should be placed in all shared libraries.
 */
resource 'STR ' (-16397, purgeable) {
	"Tktable Library\n\n"
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
	"package ifneeded Tktable 1.80 "
	"\"package require Tk; [list load [file join $dir Tktable.shlb] Tktable]\""
};
