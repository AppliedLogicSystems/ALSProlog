#include <Types.r>
#include <SysTypes.r>


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


read 'OBPT' (128, "blt_als.obp") "blt_als.obp";
read 'OBPT' (129, "blt_atom.obp") "blt_atom.obp";
read 'OBPT' (130, "blt_brk.obp") "blt_brk.obp";
read 'OBPT' (131, "blt_cslt.obp") "blt_cslt.obp";
read 'OBPT' (132, "blt_ctl.obp") "blt_ctl.obp";
read 'OBPT' (133, "blt_db.obp") "blt_db.obp";
read 'OBPT' (134, "blt_dvsh.obp") "blt_dvsh.obp";
read 'OBPT' (135, "blt_evt.obp") "blt_evt.obp";
read 'OBPT' (136, "blt_flgs.obp") "blt_flgs.obp";
read 'OBPT' (137, "blt_frez.obp") "blt_frez.obp";
read 'OBPT' (138, "blt_shlr.obp") "blt_shlr.obp";
read 'OBPT' (139, "blt_io.obp") "blt_io.obp";
read 'OBPT' (140, "blt_misc.obp") "blt_misc.obp";
read 'OBPT' (141, "blt_msg.obp") "blt_msg.obp";
read 'OBPT' (142, "blt_pckg.obp") "blt_pckg.obp";
read 'OBPT' (143, "blt_shl.obp") "blt_shl.obp";
read 'OBPT' (144, "blt_shlr.obp") "blt_shlr.obp";
read 'OBPT' (145, "blt_std.obp") "blt_std.obp";
read 'OBPT' (146, "blt_stk.obp") "blt_stk.obp";
read 'OBPT' (147, "blt_sys.obp") "blt_sys.obp";
read 'OBPT' (148, "blt_term.obp") "blt_term.obp";
read 'OBPT' (149, "builtins.obp") "builtins.obp";
read 'OBPT' (150, "cutils.obp") "cutils.obp";
read 'OBPT' (151, "dcgs.obp") "dcgs.obp";
read 'OBPT' (152, "debugger.obp") "debugger.obp";
read 'OBPT' (153, "filepath.obp") "filepath.obp";
read 'OBPT' (154, "fsmac.obp") "fsmac.obp";
read 'OBPT' (155, "fs_cmn.obp") "fs_cmn.obp";
read 'OBPT' (156, "simplio.obp") "simplio.obp";
read 'OBPT' (157, "sio.obp") "sio.obp";
read 'OBPT' (158, "sio_d10.obp") "sio_d10.obp";
read 'OBPT' (159, "sio_rt.obp") "sio_rt.obp";
read 'OBPT' (160, "sio_wt.obp") "sio_wt.obp";
read 'OBPT' (161, "xconsult.obp") "xconsult.obp";


read 'OBPT' (200, "strctutl.obp") "strctutl.obp";
read 'OBPT' (201, "strings.obp") "strings.obp";
read 'OBPT' (202, "tk_alslib.obp") "tk_alslib.obp";
read 'OBPT' (203, "miscterm.obp") "miscterm.obp";
read 'OBPT' (204, "tcl_sppt.obp") "tcl_sppt.obp";
read 'OBPT' (205, "listutl1.obp") "listutl1.obp";
read 'OBPT' (206, "msc_ioin.obp") "msc_ioin.obp";
read 'OBPT' (207, "mscioout.obp") "mscioout.obp";

read 'OBPT' (300, "tcltk.obp") "tcltk.obp";

resource 'STR#' (128) {
	{
	"debugger", "blt_dvsh",
	"miscterm","strctutl","strings","tcl_sppt","tcltk",
	"tk_alslib", "listutl1", "msc_ioin", "mscioout"
	}
};
