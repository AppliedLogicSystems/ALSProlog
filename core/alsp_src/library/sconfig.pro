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

	%% Generic classification of operating systems:

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


endmod.
