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

/* Tcl source files */
read 'TEXT' (200, "alsdev", purgeable, preload) "alsdev.tcl";
read 'TEXT' (201, "alsdev_main", purgeable, preload) "alsdev_main.tcl";
read 'TEXT' (202, "als_settings", purgeable, preload) "als_settings.tcl";
read 'TEXT' (203, "als_tklib", purgeable, preload) "als_tklib.tcl";
read 'TEXT' (204, "debugwin", purgeable, preload) "debugwin.tcl";
read 'TEXT' (205, "defstr", purgeable, preload) "defstr.tcl";
read 'TEXT' (206, "als_document", purgeable, preload) "als_document.tcl";
read 'TEXT' (207, "als_menu", purgeable, preload) "als_menu.tcl";
read 'TEXT' (208, "als_splash", purgeable, preload) "als_splash.tcl";
read 'TEXT' (209, "als_tkfbox", purgeable, preload) "als_tkfbox.tcl";
read 'TEXT' (210, "als_projects", purgeable, preload) "als_projects.tcl";

/* Tcl images in base64 format */
read 'GIFf' (200, "turnstile_splash", purgeable, preload) "turnstile_splash.b64";
read 'GIFf' (201, "down-arrow-blue", purgeable, preload) "down-arrow-blue.b64";
read 'GIFf' (202, "left-arrow-blue", purgeable, preload) "left-arrow-blue.b64";
read 'GIFf' (203, "right-arrow-blue", purgeable, preload) "right-arrow-blue.b64";
read 'GIFf' (204, "up-arrow-blue", purgeable, preload) "up-arrow-blue.b64";
read 'GIFf' (205, "closed_ptr", purgeable, preload) "closed_ptr.b64";
read 'GIFf' (206, "open_ptr", purgeable, preload) "open_ptr.b64";

