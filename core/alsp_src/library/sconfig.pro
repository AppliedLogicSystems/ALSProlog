/*===================================================================
 |		sconfig.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
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

general_os(Arch, String, GOS) 
	:-
	general_os(Arch, String, _, GOS) .

	%%
	%% 3-char names: aix,bsd,osf,ptx,sco
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,1,3,OS), 
	dmember(OS,[aix,bsd,osf,ptx,sco]), 
	!.

	%%
	%% 4-char names: bosx,dgux,hpux,irix,mach,sysv
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,1,4,OS), 
	dmember(OS,[bosx,dgux,hpux,irix,mach,sysv]), 
	!.

	%%
	%% 5-char names: dynix,linux,sunos
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,1,5,OS), 
	dmember(OS,[dynix,linux,sunos,djgpp]), 
	!.

	%%
	%% 6-char names: hiuxwe,lynxos,netbsd,newsos,riscix,riscos,ultrix,unicos
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,1,6,OS),
	dmember(OS,[hiuxwe,lynxos,netbsd,newsos,riscix,riscos,ultrix,unicos]), 
	!.

	%%
	%% 7-char names: solaris
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,1,7,OS), 
	dmember(OS,[solaris]), 
	!.

	%%
	%% 8-char names: nextstep
	%%
general_os(Arch, String, OS, unix) 
	:-
	sub_atom(String,1,8,OS), 
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

export determine_default_ws/1.
determine_default_ws(WS)
	:-
	builtins:als_system(SysProps),
	dmember(wins=WS,SysProps).

export known_ws/1.

known_ws(motif).
known_ws(macos).
known_ws(nextstep).
known_ws(dvx).

export winsystems_for/2.
winsystems_for(Arch_OS, WSL)
	:-
	asplit(Arch_OS, 0'_, Arch, OS),
	winsystems_for(Arch, OS, WSL).

export winsystems_for/3.

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
gen_winsystems(unix, [x, motif]).

export specif_winsystems_for/3.
specif_winsystems_for(_, nextstep, [nextstep]).
specif_winsystems_for(_, dvx, [x, motif]).

	%% Window system-specific information which is to
	%% be written into the top of the Makefile for that
	%% system:

export ws_vars/3.
ws_vars(WS, ARCH_OS, WSHeaderLines)
	:-
	asplit(ARCH_OS, 0'_, Arch, OS),
	ws_vars(WS, ARCH, OS, [], WSHeaderLines).

export ws_vars/5.

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

export system_dir_root/2.
system_dir_root(x,'X').
system_dir_root(motif,'Motif').



export characteristic_files/3.
characteristic_files(motif, include, ['Xm/Xm.h']).
characteristic_files(motif, lib, 
					 [ID^files(ID,'libXm.*',[_|_])]).

export possible_dir_for/4.
possible_dir_for(motif,include,_,'/usr/include').
possible_dir_for(motif,include,solaris,'/usr/dt/include').
possible_dir_for(motif,lib,_,'/usr/lib').
possible_dir_for(motif,lib,solaris,'/usr/dt/lib').
	

export flatten_ws_lists/2.
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

export flatten_to_atom/2.
flatten_to_atom([], '').
flatten_to_atom([Atom], Atom)
	:-!,
	atom(Atom).
flatten_to_atom([Atom | List], FlatValue)
	:-
	flatten_to_atom(List, ListFlatValue),
	catenate([Atom,' \\\n',ListFlatValue], FlatValue).

export cat_together_seplines/2.
cat_together_seplines([], '').
cat_together_seplines([Item | Rest], Result)
	:-
	cat_together_seplines(Rest, RestResult),
	catenate([Item, '\n', RestResult], Result).

export cat_together_spaced/2.
cat_together_spaced([], '').
cat_together_spaced([Item | Rest], Result)
	:-
	cat_together_spaced(Rest, RestResult),
	catenate([Item, ' ', RestResult], Result).

export prefix_dir/3.
prefix_dir([], _, []).
prefix_dir([Item | List], WSDir, [XItem | XList])
	:-
	extendPath(WSDir, Item, XItem),
	prefix_dir(List, WSDir, XList).

export prefix_to/3.
prefix_to([], _, []).
prefix_to([Item | List], Atom, [XItem | XList])
	:-
	catenate(Atom, Item, XItem),
	prefix_to(List, Atom, XList).

endmod.
