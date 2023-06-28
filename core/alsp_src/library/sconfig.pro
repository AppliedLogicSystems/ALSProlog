/*===================================================================
 |		sconfig.pro
 |	Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |	General system configuration information
 |
 |	Author: Ken Bowen
 |	Date: 8 Aug 94
 *==================================================================*/

module builtins.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Generic classification of operating systems:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export general_os/3.
export general_os/4.
export determine_default_ws/1.
export known_ws/1.
export winsystems_for/2.
export winsystems_for/3.
export specif_winsystems_for/3.
export ws_vars/3.
export ws_vars/5.
export system_dir_root/2.
export incl_req/3.
export characteristic_files/3.
export possible_dir_for/4.
export flatten_ws_lists/2.
export flatten_to_atom/2.

/*!-----------------------------------------------------------------------
 |	general_os/3
 |	general_os(Arch, String, GOS) 
 |	general_os(+, +, -) 
 |
 |	- determine the GeneralOS of String
 *-----------------------------------------------------------------------*/
general_os(Arch, String, GOS) 
	:-
	general_os(Arch, String, _, GOS) .

/*!-----------------------------------------------------------------------
 |	general_os/4
 |	general_os(Arch, String, OS, GOS) 
 |	general_os(Arch, String, OS, GOS) 
 |
 |	- determine OS and GeneralOS of String
 *-----------------------------------------------------------------------*/
	%%
	%% 3-char names: aix,bsd,osf,ptx,sco
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,0,3,_,OS), 
	dmember(OS,[aix,bsd,osf,ptx,sco]), 
	!.

general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,0,3,_,OS), 
	dmember(OS,[dj1,dj2]), 
	!.

	%%
	%% 4-char names: bosx,dgux,hpux,irix,mach,sysv
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,0,4,_,OS), 
	dmember(OS,[bosx,dgux,hpux,irix,mach,sysv]), 
	!.

	%%
	%% 5-char names: dynix,linux,sunos
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,0,5,_,OS), 
	dmember(OS,[dynix,linux,sunos,djgpp]), 
	!.

general_os(Arch, String, OS, macos) 
	:-
	sub_atom(String,0,5,_,OS), 
	dmember(OS,[macos]), 
	!.

	%%
	%% 6-char names: hiuxwe,lynxos,netbsd,newsos,riscix,riscos,ultrix,unicos
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,0,6,_,OS),
	dmember(OS,[hiuxwe,lynxos,netbsd,newsos,riscix,riscos,ultrix,unicos]), 
	!.

	%%
	%% 7-char names: solaris
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,0,7,_,OS), 
	dmember(OS,[solaris]), 
	!.

general_os(Arch, String, OS, mswin32) 
	:-
	sub_atom(String,0,7,_,OS), 
	dmember(OS,[mswin32]), 
	!.

	%%
	%% 8-char names: nextstep
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,0,8,_,OS), 
	dmember(OS,[nextstep]), 
	!.

lib_extension( unix, a ) :-!.
lib_extension( OS, LExt )
	:-
	general_os(_,OS,GOS),
	lib_extension(GOS,LExt).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Window system information
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!-----------------------------------------------------------------------
 |	determine_default_ws/1
 |	determine_default_ws(WS)
 |	determine_default_ws(-)
 |
 |	- determine the current default window system
 *-----------------------------------------------------------------------*/
determine_default_ws(WS)
	:-
	current_prolog_flag(windows_system, WS).

/*!-----------------------------------------------------------------------
 |	known_ws/1
 |	known_ws(WS)
 |	known_ws(?)
 |
 |	- holds for known window systems
 *-----------------------------------------------------------------------*/
known_ws(motif).
known_ws(wxwin).
known_ws(macos).
known_ws(nextstep).
known_ws(dvx).

/*!-----------------------------------------------------------------------
 |	winsystems_for/2
 |	winsystems_for(Arch_OS, WSL)
 |	winsystems_for(+, -)
 |
 |	- determine the window systems for an architecture/os combination
 *-----------------------------------------------------------------------*/
winsystems_for(Arch_OS, WSL)
	:-
	asplit(Arch_OS, 0'_, Arch, OS),
	winsystems_for(Arch, OS, WSL).

/*!-----------------------------------------------------------------------
 |	winsystems_for/3.
 |	winsystems_for(Arch, OSString, WSL)
 |	winsystems_for(+, +, -)
 |
 |	- determine the window systems for an architecture/os combination
 *-----------------------------------------------------------------------*/

	%% What window systems do we support/make for
	%% a given ARCH/OS pair:

winsystems_for(Arch, OSString, WSL)
	:-
	general_os(Arch, OSString, OS, GOS), 
	(specif_winsystems_for(Arch, OS, WSL) ->
		true
		;
		gen_winsystems(GOS, WSL)
	).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%     !!!!!! WARNING   !!!!!
		%%%%  Keep the window systems on the
		%%%% lists below in the order of
		%%%% any dependencies;  e.g., 
		%%%% x __must__ be processed before
		%%%% motif, since motif processing
		%%%% loads the x libraries;
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_winsystems(unix, [x, motif, wxwin]).

/*!-----------------------------------------------------------------------
 |	specif_winsystems_for/3
 |	specif_winsystems_for(Arch, OS, WSList)
 |	specif_winsystems_for(+, +, -)
 |
 |	- determine specific window systems for an architecture/os combination
 *-----------------------------------------------------------------------*/
specif_winsystems_for(i386, mswin32, [mswin32]).
specif_winsystems_for(_, macos, [macos]).
specif_winsystems_for(_, nextstep, [nextstep]).
specif_winsystems_for(_, dvx, [x, motif]).

	%% Window system-specific information which is to
	%% be written into the top of the Makefile for that
	%% system:

/*!-----------------------------------------------------------------------
 |	ws_vars/3
 |	ws_vars(WS, ARCH_OS, WSHeaderLines)
 |	ws_vars(+, +, -)
 |
 |	- list makefile window system vars/header lines for an arch/os combination
 *-----------------------------------------------------------------------*/
ws_vars(WS, ARCH_OS, WSHeaderLines)
	:-
	asplit(ARCH_OS, 0'_, Arch, OS),
	ws_vars(WS, ARCH, OS, [], WSHeaderLines).

/*!-----------------------------------------------------------------------
 |	ws_vars/5
 |	ws_vars(WS, ARCH, OS, SwitchInfo, WSHeaderLines)
 |	ws_vars(+, +, +, +, -)
 |
 |	- determine window system makefile vars info
 *-----------------------------------------------------------------------*/
ws_vars(x, ARCH, OS, SwitchInfo, WSHeaderLines)
	:-
	WSHeaderLines =
	[
		'WIN'		= x ,
		'C2PFILT'	= all,
		'ADDL_CS' 	= [] ,
		'CFLAGS'	= '' ,
		'$(WIN)LIBS' 	= '-lX11' ,
		'ADDL_LIBS' 	= [] ,
		'CFG' 		= ['#define WIN_STR X_WIN_STR'],
		'ADDL_INITS' 	= [],
		'ADDL_PROFS' 	= []
		| VARIABLE_LINES ],
	(dmember(tgtws=TGTWS, SwitchInfo) -> 
		true ; TGTWS = unix),
	ws_vars_variable(TGTWS,x,ARCH,OS,VARIABLE_LINES).

ws_vars_variable(dvx,_,ARCH,OS,VARIABLE_LINES)
	:-!,
	VARIABLE_LINES =
	[
		'XINCLUDES' = '-I djgpp/include'
	].
ws_vars_variable(unix,_,mips,irix,VARIABLE_LINES)
	:-
	VARIABLE_LINES =
	[
		'XINCLUDES' = '-I /usr/include',
		'XDEFINES' = '-D_LANGUAGE_C -D_MIPS_SZINT=32 -D_MIPS_SZLONG=32 -DMips -Dsgi -D__sgi'
	].
ws_vars_variable(unix,_,ARCH,OS,VARIABLE_LINES)
	:-
	VARIABLE_LINES =
	[
		'XINCLUDES' = '-I /usr/include'
	].

ws_vars(motif, ARCH, OS, SwitchInfo, WSHeaderLines)
	:-
	WSHeaderLines =
	[
		'WIN'		= motif ,
		'C2PFILT'	= all,
		'ADDL_CS' 	= ['xtaux.c'] ,
		'CFLAGS'	= '-D_NO_PROTO' ,
		'$(WIN)LIBS' 	= '-lMrm -lXm -lXt -lX11' ,
		'ADDL_LIBS' 	= ['../x/xinterf.a'] ,
		'CFG' 		= ['#define WIN_STR MOTIF_WIN_STR'],
		'ADDL_INITS' 	= ['x_init();', 'xtaux_init();'] ,
		'ADDL_PROFS' 	= ['../x/*.pro'],
		'XDEFINES' = '-DXTSTRINGDEFINES -DXMSTRINGDEFINES'
		| VARIABLE_LINES ],
	(dmember(tgtws=TGTWS, SwitchInfo) -> 
		true ; TGTWS = unix),
	ws_vars_variable(TGTWS,motif,ARCH,OS,VARIABLE_LINES).

ws_vars(wxwin, ARCH, OS, SwitchInfo, WSHeaderLines)
	:-
	WSHeaderLines =
	[
		'WIN'		= wxwin ,
		'C2PFILT'	= all,
		'ADDL_CS' 	= '' ,
		'CFLAGS'	= '-x c++ -Dwx_motif -DSYSV -DHAVE_STDLIB_H',
		'$(WIN)LIBS' 	= '-lwxextend_motif -lwx_motif -lMrm -lXm -lXt -lX11 -lg++ -lgcc' ,
		'ADDL_LIBS' 	= '',
		'CFG' 		= ['#define WIN_STR WXWIN_WIN_STR'],
		'ADDL_INITS' 	= '' ,
		'ADDL_PROFS' 	= '',
		'XDEFINES' = '-Dwx_motif'
		| VARIABLE_LINES ],
	(dmember(tgtws=TGTWS, SwitchInfo) -> 
		true ; TGTWS = unix),
	ws_vars_variable(TGTWS,motif,ARCH,OS,VARIABLE_LINES).

ws_vars(nextstep, ARCH, OS, SwitchInfo, WSHeaderLines)
	:-
	WSHeaderLines =
	[
		'WIN'		= nextstep,
		'C2PFILT'	= all,
		'XINCLUDES'	= '-I/usr/include/ansi -I/usr/include/bsd -I/usr/include/appkit',
		'ADDL_CS' 	= [] ,
		'CFLAGS'	= '-D_NO_PROTO' ,
		'$(WIN)LIBS' 	= '-lXm -lMrm -lXt -lX11' ,
		'ADDL_LIBS' 	= [] ,
		'CFG' 		= ['#define WIN_STR NEXTSTEP_WIN_STR'],
		'ADDL_INITS' 	= [] ,
		'ADDL_PROFS' 	= []
		| VARIABLE_LINES ],
	(dmember(tgtws=TGTWS, SwitchInfo) -> 
		true ; TGTWS = unix),
	ws_vars_variable(TGTWS,motif,ARCH,OS,VARIABLE_LINES).

/*!-----------------------------------------------------------------------
 |	system_dir_root/2
 |	system_dir_root(WS, RootDirName)
 |	system_dir_root(+, -)
 |
 |	- determine directory root name for WS
 *-----------------------------------------------------------------------*/
system_dir_root(x,'X').
system_dir_root(motif,'Motif').
system_dir_root(wxwin,'wxwin').


/*!-----------------------------------------------------------------------
 |	incl_req/3
 |	incl_req(WS, OS, ReqWS)
 |	incl_req(+, +, -)
 |
 |	- holds for required included window systems for WS and OS
 *-----------------------------------------------------------------------*/
incl_req(motif, _, x).
incl_req(wxwin, OS, x)
	:-
	general_os(_, OS, unix).
incl_req(wxwin, OS, motif)
	:-
	general_os(_, OS, unix).

/*!-----------------------------------------------------------------------
 |	characteristic_files/3
 |	characteristic_files(WS, Kind, FileList)
 |	characteristic_files(+, +, -)
 |
 |	- holds for files characterizing a Type of directory for a WS
 *-----------------------------------------------------------------------*/
characteristic_files(x, include, ['X11/X.h']).

characteristic_files(motif, include, ['Xm/Xm.h']).
characteristic_files(motif, lib, ['libXm.*']).

characteristic_files(wxwin, include, ['wx.h', 'wx_win.h', 'wxextend.h']).
characteristic_files(wxwin, lib, ['libwx_motif.a', 'libwxextend_motif.a']).

/*!-----------------------------------------------------------------------
 |	possible_dir_for/4
 |	possible_dir_for(WS, Type, OS, Dir)
 |	possible_dir_for(+, +, +, -)
 |
 |	- determine a possible Type dir location for WS and OS
 *-----------------------------------------------------------------------*/

possible_dir_for(motif,lib,_,'/usr/lib').
possible_dir_for(motif,lib,_,'/usr/lib/X11R6').
possible_dir_for(motif,lib,solaris,'/usr/dt/lib').

possible_dir_for(wxwin,include,_,'/usr2/wxwins/include/base').
possible_dir_for(wxwin,include,_,'/usr2/wxwins/include/x').
possible_dir_for(wxwin,include,_,'/usr2/wxwins/utils/wxextend/src').
possible_dir_for(wxwin,lib,_,'/usr2/wxwins/lib').
possible_dir_for(wxwin,lib,_,'/usr2/wxwins/utils/wxextend/lib').
	
/*!-----------------------------------------------------------------------
 |	flatten_ws_lists/2
 |	flatten_ws_lists(InList, OutList)
 |	flatten_ws_lists(+, -)
 |
 |	- flatten Value expressions in Tag=Value pairs on InList
 *-----------------------------------------------------------------------*/
flatten_ws_lists([], []).
flatten_ws_lists([Tag=Value | WSHeaderItems], [Tag = FlatValue | WSHeaderLines])
	:-
	Value = [_|_],
	!,
	flatten_to_atom(Value, FlatValue),
	flatten_ws_lists(WSHeaderItems, WSHeaderLines).
flatten_ws_lists([Tag=[] | WSHeaderItems], [Tag = '' | WSHeaderLines])
	:-!,
	flatten_ws_lists(WSHeaderItems, WSHeaderLines).
flatten_ws_lists([Tag=Value | WSHeaderItems], [Tag = Value | WSHeaderLines])
	:-
	flatten_ws_lists(WSHeaderItems, WSHeaderLines).

/*!-----------------------------------------------------------------------
 |	flatten_to_atom/2
 |	flatten_to_atom(List, ListFlatValue)
 |	flatten_to_atom(+, -)
 |
 |	- convert list of atoms to single atom with nl separating atom entries
 *-----------------------------------------------------------------------*/
flatten_to_atom([], '').
flatten_to_atom([Atom], Atom)
	:-!,
	atom(Atom).
flatten_to_atom([Atom | List], FlatValue)
	:-
	flatten_to_atom(List, ListFlatValue),
	catenate([Atom,' \\\n',ListFlatValue], FlatValue).


endmod.
