/*=====================================================================
 | 		blt_brk.pro 	
 | 	Copyright (c) 1987-95 Applied Logic Systems, Inc.
 |
 |	Basic break handler for ^C or ^Break interrupts
 |
 | Author: Ken Bowen (based on code from Kevin Buettner)
 | Date  : 7/29/91
 |
 | Revised 4/1/93 - Ken Bowen 
 |   -- changed to library menu routines
 |   -- changed to stream I/O routines
 |     - removed see/tell info from breakpoint record
 *===================================================================*/
module builtins.
use windows.

:- make_gv('BreakLevel').
:- setBreakLevel([b(0,user,true)]).

breakhandler(M,G) 
	:- 
	getBreakLevel(BreakList),
	BreakList = [b(OldLevel,_,_) | _],
	NewLevel is OldLevel+1,
	setBreakLevel([b(NewLevel,M,G) | BreakList]),
	catch(breakhandler0(M,G),
		  breakhandler(NewM,NewG),
		  breakhandler0(NewM,NewG) ), !.

breakhandler(M,G) 
	:- 
	getBreakLevel(BreakList),
	BreakList = [_ | BreakListTail],
	setBreakLevel(BreakListTail),
	!,
	fail.
	

listOfCodes([ a, b, c, d, e, f, p, s, t, ?]).

choiceItems( [
		 'Abort Computation',
		 'Break shell',
		 'Continue',
		 'Debug',
		 'Exit Prolog',
		 'Fail',
		 'Return to Previous Break Level',
		 'Show goal broken at',
		 'Stack trace',
		 'This message'] ).

responses( M, G, [
		 abort,
		 break_shell,
		 continue(M,G),
		 debug(M,G),
		 exit,
		 fail,
		 previous,
		 show(M,G),
		 stack_trace,
		 help ] ).

options(ListOfCodes, Responses, Prompt,
			[codes		= ListOfCodes,
			 responses	= Responses,
			 default	= a,
			 title		= 'Break Handler',
			 indent		= '    ',
			 prompt		= Prompt ] ).

breakhandler0(M,G) 
	:- 
	getBreakLevel([b(Level,_,_)|_]),
	catenate(['Break(',Level,') >'],Prompt),
	listOfCodes(ListOfCodes),
	choiceItems(ChoiceItems),
	responses(M, G, Responses),
	options(ListOfCodes, Responses, Prompt, Options0),
	Options = [io_streams=(warning_input,warning_output) | Options0],
		%% Note: menu/4 defined in Library in iolayer.pro:
	menu(break_window,ChoiceItems,Response,Options),
	break_handler(Response,M,G).

break_handler(abort,M,G) 
	:-!, 
	setBreakLevel([b(0,user,true)]),
		%% abort_ctlc: "Aborting from Control-C or Control-Break.\n"
	prolog_system_error(abort_ctlc, []),
	abort.
break_handler(break_shell,M,G) 
	:-!,
	prolog_shell,
	breakhandler0(M,G).
break_handler(continue(M,G),M,G) 
	:-!,
	getBreakLevel([_ | PrevList]),
	setBreakLevel(PrevList),
	(obtain_alarm_interval(Intrv) -> 
%		write(breakhandler_resetting_alarm(Intrv,Intrv)),nl,flush_output,
		alarm(Intrv, Intrv) 
		; 
		true),
	M:G.
break_handler(debug(M,G),M,G) 
	:-!,
	catch(trace(M,G),
		  breakhandler_debug(NewM,NewG),
		  breakhandler0(NewM,NewG) ).
break_handler(exit,M,G) 
	:-!, 
		%% exit_ctlc: "Exiting Prolog from Control-C or Control-Break.\n"
	prolog_system_error(exit_ctlc, []),
	halt.
break_handler(fail,M,G) 
	:-!,
	fail.
break_handler(previous,M,G) 
	:-!, 
	getBreakLevel([_ | PrevList]),
	setBreakLevel(PrevList),
	PrevList = [b(PrevLevel, PrevM, PrevG) | _],
	(   PrevLevel < 1 ->  
			%% no_prev_lev: "No previous level!!\n"
		prolog_system_error(no_prev_lev, []),
	    abort
		;
	    throw(breakhandler(PrevM,PrevG))
	).
break_handler(show(M,G),M,G) 
	:-!,
	als_advise('Break at: %t:%t\n',[M,G]),
	breakhandler0(M,G).
break_handler(stack_trace,M,G) 
	:-!,
	stack_trace,
	breakhandler0(M,G).

break_handler(Otherwise,M,G) 
	:- 
	als_advise('\n    Bad input! Please re-enter.\n\n',[]),
	!,
	breakhandler0(M,G).

stack_trace 
	:-
	stack_trace(1).

stack_trace(30) :- !.
stack_trace(N) 
	:-
	frame_info(N,FI),
	!,
	printf(debugger_output,'(%d) %t\n',[N,FI],[quoted(true),maxdepth(8)]),
	NN is N+1,
	stack_trace(NN).
stack_trace(_).

endmod.
