/*=====================================================================*
 |			freeze.pro
 |		Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |		Freeze/delay - related tests
 |
 |	Author: Ken Bowen
 |	Date: May 7, 1995
 *=====================================================================*/

	/*-------------------------------------
	 | Simple interactive producer-consumer
	 *------------------------------------*/

int_pc :- freeze(TS,produce(0,TS)), consume(TS).

produce(N, [N | T])
	:-
	M is N+1,
	freeze(T, produce(M,T)).

consume([N | T])
	:-
	write(n=N),nl,
	write('>'),read(X),
	disp(X,T).

disp(quit, _) :-!.
disp(gc, T) 
	:-!,
	printf('Before: '), flush_output,cptx,
	gc,
	printf('After: '), flush_output,cptx,
	consume(T).
	
disp(gv, T) 
	:-!,
	builtins:'get_$delay_terms'(X),
	pbi_write(X),pbi_nl,
	consume(T).

disp(heap(N), T) 
	:-!,
	M is -N,
	display_heap(0,M),
	consume(T).

disp(ct, T) 
	:-!,
	builtins:collect_thawed(X),
	pbi_write(X),pbi_nl,
	consume(T).

disp(thaw, T) 
	:-!,
	builtins:'get_$delay_terms'(XB),
	pbi_write(before=XB),pbi_nl,pbi_ttyflush,
%	builtins:free_thawed,
	builtins:'get_$delay_terms'(XA),
	pbi_write(after=XA),pbi_nl,pbi_ttyflush,
	consume(T).

disp(clean, T) 
	:-!,
	builtins:'get_$delay_terms'(XB),
	printf('Before: '), flush_output,cptx,
	pbi_write(before=XB),pbi_nl,pbi_ttyflush,
%	builtins:free_thawed,
	gc,
	printf('After: '), flush_output,cptx,
	builtins:'get_$delay_terms'(XA),
	pbi_write(after=XA),pbi_nl,pbi_ttyflush,
	consume(T).

disp(_,T) 
	:-
	consume(T).

	/*-------------------------------------
	 | Simple non-interactive producer-
	 | consumer, with cutoff; displays some info
	 *------------------------------------*/

pc2 :- 
	cptx,
	freeze(S, produce2(0,S)), consume2(S).

produce2(N, [N | T])
	:-
	M is N+1,
	write('-p-'),
	freeze(T, produce2(M,T)).

consume2([N | T])
	:-
	write(n=N),nl,
%	free_thawed,
	((N > 3, 0 is N mod 3) -> gc; true),
	cptx,
	(N < 800 ->
		consume2(T) ; true).
