/*
 * Icode buffer Tests
 */


/*
 * Blow up icode buffer
 */
icodebuf_test1(N) :-
	create_list(N,L),
	nl, write('Trying to assert p1(List) where List has '), write(N),
		write(' integers.'), !,
	assert(p1(L)), !,
	nl, write('Test is successful.').

create_list(0,[]) :- !.
create_list(N,[N|L]) :- M is N - 1, create_list(M,L).


/*
 * Howard Arnold's example blowing up icode buffer (32K)
 */
icodebuf_test2 :-
	assert(screen(ploc1,17,77,
					[
						box(0,0,17,77,2),
						text(0,2,"Parts @ Locations"),
						text(2,2,"Part ID:"),
						text(2,11,"Class:"),
						text(2,16,"Subclass:"),
						text(4,2,"Unit of Measure:"),
						text(6,2,"Description:"),
						text(8,2,"Location ID:"),
						text(10,3,"Bin Number:"),
						text(12,4,"Low Limit:"),
						text(12,18,"High Limit:"),
						text(12,32,"Reorder:"),
						text(14,5,"On Hand:"),
						text(14,16,"Pend In:"),
						text(14,25,"Pend Out: "),
						efield(p_bin,10,15,bin,t,f,"Bin"),
						efield(p_class,2,37,class,f,f,"Class"),
						efield(p_highlimit,12,41,quan,t,f,"High Limit"),
						efield(p_location,8,15,location,t,f,"Location"),
						efield(p_lowlimit,12,15,quan,t,f,"Low Limit"),
						efield(p_partdesc,6,15,chr30,f,f,"Description"),
						efield(p_partid,2,11,pid,t,t,"PartID."),
						efield(p_unom,4,19,unom,f,f,"Unit of Measure"),
						efield(p_reorder,12,63,quan,t,f,"Reorder"),
						efield(p_subclass,2,56,subclass,f,f,"SubClass"),
						efield(p_oh,14,15,quan,f,f,"Amount on Hand"),
						efield(p_pi,14,41,quan,f,f,"Pending Inventory"),
						efield(p_po,14,63,quan,f,f,"Pending Out")
					]
				)).


/*
 * This test causes a lot of errors:
 * 		N = 50  ==> Error: Parser area exhausted
 * 		N = 10 	==> Error: Icode Buffer Overflow
 */
icodebuf_test3(N) :-
	create_nested_term(N,T),
	nl, write('Trying to assert p3(Term) where Term has '), write(N),
		write(' nested terms.'), !,
	assert(p(T)), !,
	nl, write('Test is successful.').

create_nested_term(1,1) :- !.
create_nested_term(N,f(T,T)) :- M is N - 1, create_nested_term(M,T).


/*
 * To blow up macroarea
 * (macroarea is dynamic in 386 system, it is automatically incremented
 *  when there is overflow)
 */
icodebuf_test4(N) :-
	create_nested_termplus(N,T),
	nl, write('Trying to assert (p4(N,R) :- R is Term) where Term has '),
		write(N), write(' nested terms.'), !,
	assert((p4(N,R) :- R is T)), !,
	nl, write('Test is successful.').

create_nested_termplus(1,1) :- !.
create_nested_termplus(N,+(N,T)) :- M is N - 1, create_nested_termplus(M,T).


/*
 * To blow up callinfo
 */
icodebuf_test5(N) :-
	create_nested_termcomma(N,T),
	nl, write('Trying to assert (p5(N) :- Term) where Term has '),
		write(N), write(' subgoals.'), !,
	assert((p5(N) :- T)), !,
	nl, write('Test is successful.').

create_nested_termcomma(1,q) :- !.
create_nested_termcomma(N,','(q,T)) :- 
	M is N - 1, 
	create_nested_termcomma(M,T).



