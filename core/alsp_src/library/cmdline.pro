/*=====================================================================
 | 			cmdline.pro		
 |	Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Utilities for dealing with command line lists
 |
 |	Note that the "prolog portion" of the command line (the portion
 |	treated here) is that part rightwards of the first -p switch.
 *====================================================================*/

module builtins.

export get_cmdline_vals/1.
export cmdline_vals/2.
export eat_cmd_line/3.
export pull_out_nullswitches/3.

/*!-----------------------------------------------------------------------*
 |	get_cmdline_vals/1
 |	get_cmdline_vals(SwitchVals)
 |	get_cmdline_vals(-)
 |
 |	- obtains the list of command line arguments
 |
 |	get_cmdline_vals/1 calls builtins:command_line(CmdLine), and parses
 |	CmdLine into a list SwitchVals of sublists of one of the forms
 |		[SwitchName, SwitchVal] or [SwitchName]
 |	for each expression -S or V following -p on the command line,
 |	as follows:
 |	i)   if -S is followed by a -T, the expression for -S is ['-S'];
 |	ii)  if -S is followed by V which is NOT of the form -T, then
 |		 the expression for the pair -S V is ['-S','V'];
 |	iii) if V is NOT preceeded by an expression of the form -S, then
 |		 the expression corresponding to V is ['-null_switch', 'V'] 
 |	For example, given the invocation
 |		alspro -p foo -gg a b c -f -h -k e3 e4
 |
 |	?- get_cmdline_vals(X).
 |
 |	X = [['-null_switch',foo],['-gg',a],['-null_switch',b],['-null_switch',c],
 |        ['-f'],['-h'],['-k',e3],['-null_switch',e4]]
 *!-----------------------------------------------------------------------*/
get_cmdline_vals(SwitchVals)
	:-
	command_line(CmdLine),
	cmdline_vals(CmdLine, SwitchVals).

cmdline_vals([], []).

cmdline_vals([Item | RestCmdLine], SwitchVals)
	:-
	start_cmdline_vals(Item, RestCmdLine, SwitchVals).

start_cmdline_vals(Item, RestCmdLine, SwitchVals)
	:-
	sub_atom(Item,0,1,_,'-'),
	!,
	xtr_cmdline_vals([Item | RestCmdLine], SwitchVals).
	
start_cmdline_vals(Item, RestCmdLine, SwitchVals)
	:-
	xtr_cmdline_vals(['-null_switch',Item | RestCmdLine], SwitchVals).

xtr_cmdline_vals([], []).
	%% Invariant: Item will be a switch: '-....'
xtr_cmdline_vals([Item | RestCmdLine], [[Item | ItemArgList] | RestSwitchVals])
	:-
	cmd_item_arg(RestCmdLine, ItemArgList, TailCmdLine),
	cmdline_vals(TailCmdLine, RestSwitchVals).
	

cmd_item_arg([], [], []).
cmd_item_arg([Arg | TailCmdLine], [Arg], TailCmdLine)
	:-
	not(sub_atom(Arg,0,1,_,'-')),
	!.
cmd_item_arg(RestCmdLine, [], RestCmdLine).

/*!-----------------------------------------------------------------------*
 |	eat_cmd_line/3
 |	eat_cmd_line(CmdLine, Items, CmdLineTail)
 |	eat_cmd_line(+, -, -)
 |	
 |	- splits CmdLine into an initial null switch segment and remainder
 |
 |	CmdLine  -  a (possibly tail of) a command line, as produced by 
 |				get_cmdline_vals/1;
 |	Items  - 	the longest list of Atoms corresponding, in order, to
 |				the longest initial segment (possibly empty) of CmdLine 
 |				of entries of the form  ['-null_switch Atom];
 |	CmdLineTail - the tail of CmdLine following the segment generating Items.
 *!-----------------------------------------------------------------------*/
eat_cmd_line([],[], []).
eat_cmd_line([['-null_switch',Item] | RestCommandLine], 
				[Item | RestItems], CmdLineTail)
	:-!,
	eat_cmd_line(RestCommandLine, RestItems, CmdLineTail).
eat_cmd_line(CmdLineTail, [], CmdLineTail).

/*!-----------------------------------------------------------------------*
 |	pull_out_nullswitches/3
 |	pull_out_nullswitches(SwitchVals, Items, ReducedSwitchVals)
 |	pull_out_nullswitches(+, -, -)
 |	
 |	- extracts all items from cmd line not preceeded by a switch
 |
 |	SwitchVals 		  - a list produced by get_cmdline_vals
 |	Items 			  = bagof(X, member(X, ['-null_switch',X], SwitchVals)
 |	ReducedSwitchVals = The "rest of" switch vals
 *!-----------------------------------------------------------------------*/

pull_out_nullswitches([], [], []).
pull_out_nullswitches([['-null_switch' , Item] | SwitchVals], 
			[Item | Items], ReducedSwitchVals)
	:-
	pull_out_nullswitches(SwitchVals, Items, ReducedSwitchVals).

pull_out_nullswitches([Switch | SwitchVals], Items, [Switch | ReducedSwitchVals])
	:-
	pull_out_nullswitches(SwitchVals, Items, ReducedSwitchVals).

endmod.
