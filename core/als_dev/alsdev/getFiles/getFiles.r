#include <Types.r>
#include <SysTypes.r>

#include "getFiles_version.h"

resource 'vers' (1) {
	VERSION_MAJOR, VERSION_MINOR << 4 | VERSION_PATCH,
	final, 0x00, verUS,
	VERSION_STRING,
	VERSION_STRING " © Applied Logic Systems, Inc. 1998"
};

resource 'vers' (2) {
	VERSION_MAJOR, VERSION_MINOR << 4 | VERSION_PATCH,
	final, 0x00, verUS,
	VERSION_STRING,
	"getFiles " VERSION_STRING " © 1998"
};

resource 'STR ' (-16397, purgeable) {
	"getFiles Library\n\n"
	"This library provides the ability to select multiple files "
	"from Tcl/Tk programs.  To work properly, it "
	"should be placed in the ÔTool Command LanguageÕ folder "
	"within the Extensions folder."
};

/* 
 * We now load the Tk library into the resource fork of the library.
 */

data 'TEXT' (4000,"pkgIndex",purgeable, preload) {
	"package ifneeded getFiles " VERSION_STRING " "
	"\"package require Tk 8.0; [list load [file join $dir getFiles.shlb] getFiles]\""
};
