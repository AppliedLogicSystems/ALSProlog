/*=====================================================================
 | 			cmdline.pro		
 |		Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |		Utilities for dealing with command line lists
 |
 |	Note that the "prolog portion" of the command line (the portion
 |	treated here) is that part rightwards of the first -p switch.
 *====================================================================*/

module builtins.

export get_cmdline_vals/1.
export cmdline_vals/2.

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
	sub_atom(Item,1,1,'-'),
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
	not(sub_atom(Arg,1,1,'-')),
	!.
cmd_item_arg(RestCmdLine, [], RestCmdLine).


endmod.
