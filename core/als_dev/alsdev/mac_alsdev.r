#include <Types.r>
#include <SysTypes.r>

/* Tcl/Tk resources */
include "Tcl8.0.shlb" 'TEXT';
include "Tk8.0.shlb" 'TEXT';
include "Tk8.0.shlb" 'DLOG';
include "Tk8.0.shlb" 'DITL';
include "Tk8.0.shlb" 'CNTL';
include "Tk8.0.shlb" 'crsr';
include "Tk8.0.shlb" 'CURS';
include "Tk8.0.shlb" 'MDEF';
include "Tk8.0.shlb" 'MENU';
include "Tk8.0.shlb" 'PICT';
include "Tk8.0.shlb" 'SICN';

/* Itcl/Itk/Iwidgets resources */

include "itcl30.shlb" 'TEXT';
include "itk30.shlb" 'TEXT';
include "iwidgets30.rsrc";

/* Tcl source files */

/* Loaded in bld_dvsh.pro */
read 'TEXT' (200, "alsdev", purgeable, preload) "alsdev.tcl";
read 'TEXT' (201, "als_splash", purgeable, preload) "als_splash.tcl";

/* Loaded in alsdev.tcl */
read 'TEXT' (202, "alsdev_main", purgeable, preload) "alsdev_main.tcl";
read 'TEXT' (203, "als_settings", purgeable, preload) "als_settings.tcl";
read 'TEXT' (204, "debugwin", purgeable, preload) "debugwin.tcl";
read 'TEXT' (205, "defstr", purgeable, preload) "defstr.tcl";
read 'TEXT' (206, "als_menu", purgeable, preload) "als_menu.tcl";
read 'TEXT' (207, "als_document", purgeable, preload) "als_document.tcl";
read 'TEXT' (208, "als_projects", purgeable, preload) "als_projects.tcl";

read 'TEXT' (209, "als_tklib", purgeable, preload) "als_tklib.tcl";


/* Tcl images in base64 format */
read 'GIFf' (200, "turnstile_splash", purgeable, preload) "turnstile_splash.b64";
read 'GIFf' (201, "down-arrow-blue", purgeable, preload) "down-arrow-blue.b64";
read 'GIFf' (202, "left-arrow-blue", purgeable, preload) "left-arrow-blue.b64";
read 'GIFf' (203, "right-arrow-blue", purgeable, preload) "right-arrow-blue.b64";
read 'GIFf' (204, "up-arrow-blue", purgeable, preload) "up-arrow-blue.b64";
read 'GIFf' (205, "closed_ptr", purgeable, preload) "closed_ptr.b64";
read 'GIFf' (206, "open_ptr", purgeable, preload) "open_ptr.b64";
read 'GIFf' (207, "closed_mac", purgeable, preload) "closed_mac.b64";
read 'GIFf' (208, "open_mac", purgeable, preload) "open_mac.b64";

