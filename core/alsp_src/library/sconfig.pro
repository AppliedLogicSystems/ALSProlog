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
	dmember(OS,[dynix,linux,sunos]), 
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

gen_winsystems(unix, [x, motif]).

specif_winsystems_for(_, nextstep, [nextstep]).

	%% Window system-specific information which is to
	%% be written into the top of the Makefile for that
	%% system:

export ws_vars/3.
ws_vars(WS, ARCH_OS, WSHeaderLines)
	:-
	asplit(ARCH_OS, 0'_, Arch, OS),
	ws_vars(WS, ARCH, OS, WSHeaderLines).

export ws_vars/4.

ws_vars(x, ARCH, OS, WSHeaderLines)
	:-
	WSHeaderLines =
	[
		'WIN'		= x ,
		'ADDL_CS' 	= [] ,
		'CFLAGS'	= '' ,
		'$(WIN)LIBS' 	= '-lX11' ,
		'ADDL_LIBS' 	= [] ,
		'CFG' 		= ['#define WIN_STR X_WIN_STR'],
		'ADDL_INITS' 	= [],
		'ADDL_PROFS' 	= []
	].

ws_vars(motif, ARCH, OS, WSHeaderLines)
	:-
	WSHeaderLines =
	[
		'WIN'		= motif ,
		'ADDL_CS' 	= ['xtaux.c'] ,
		'CFLAGS'	= '-D_NO_PROTO' ,
		'$(WIN)LIBS' 	= '-lXm -lMrm -lXt -lX11' ,
		'ADDL_LIBS' 	= ['../x/xinterf.a'] ,
		'CFG' 		= ['#define WIN_STR MOTIF_WIN_STR'],
		'ADDL_INITS' 	= ['x_init();', 'xtaux_init();'] ,
		'ADDL_PROFS' 	= ['../x/*.pro']
	].

export flatten_ws_lists/2.
flatten_ws_lists([], []).
flatten_ws_lists([Tag=Value | WSHeaderItems], [Tag = FlatValue | WSHeaderLines])
	:-
	Value = [_|_],
	!,
	flatten_to_atom(Value, FlatValue),
	flatten_ws_lists(WSHeaderItems, WSHeaderLines).
flatten_ws_lists([Tag=Value | WSHeaderItems], [Tag = Value | WSHeaderLines])
	:-
	flatten_ws_lists(WSHeaderItems, WSHeaderLines).

export flatten_to_atom/2.
flatten_to_atom([], '').
flatten_to_atom([Atom | List], FlatValue)
	:-
	flatten_to_atom(List, ListFlatValue),
	catenate([Atom,' \\\n',ListFlatValue], FlatValue).

endmod.
