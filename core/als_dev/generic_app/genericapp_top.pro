/*====================================================================*
 |               genericapp_top.pro
 |
 |            Prolog Top Level of: genericapp
 *====================================================================*/


%:-['cmn_utils.pro'].

use tcltk.
use tk_alslib.

this_app_name(genericapp).

this_app_name(genericapp.exe).

this_app_name('GENERICAPP').

this_app_name('GENERICAPP'.exe).

export coldstart_genericapp/0.
coldstart_genericapp :-
    catch(real_coldstart_genericapp,_,halt).

real_coldstart_genericapp :-
    start_genericapp,
    tk_main_loop.


export warmstart_genericapp/0.
warmstart_genericapp :-
	start_genericapp.
start_genericapp :-
    % resize_memory(2000000, 3000000),
    SPLASHFILE = 'genericapp.gif',
    basic_app_init,
    get_cwd(CurDir),
    negotiate_path(TclPath),
    stock_splash(TclPath, SPLASHFILE, tcli, RemoveSplashCmd),
    tcl_call(tcli,[source,'genericapp_gui.tcl'],_),
    tcl_call(tcli,[after,800],_),
    call(RemoveSplashCmd),
    (A1Path = '' -> true ; change_cwd(A1Path) ).


