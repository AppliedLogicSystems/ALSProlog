/*=====================================================================*
 | 			clptest.pro
 |		Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |			-- basic tests for interval constraints
 |
 | Author: Ken Bowen (with thanks to Bill Older & Andre Vellino)
 *=====================================================================*/

/*---------------------------------------------------------------------*
 |	Tests are to be defined by clauses of the form:
 |
 | 			clpt(N,VarList,NameList) :- Body
 |
 |	where VarList and NameList are of the same length, VarList is the
 |	list of variables which occur in Body and which you want to show
 |	in the output, and NameList is a corresponding list of names for the
 |	variables.  I use integers for N, but N can be any identifier. 
 |	Example:
 |		clpt(1,[X,Y,Z],['X','Y','Z'])
 |			:-
 |			X::real(-3,7),Y::real(2,12), Z::real,{Z == X+Y}.
 *---------------------------------------------------------------------*/

/*---------------------------------------------------------------------*
 |	clptest/0
 |
 |	Runs all the tests which have been defined (ie, included in this
 |	file), and presents output for each test.
 *---------------------------------------------------------------------*/

module clptest.

export clptest/0.

clptest
	:-
	setof(N, [V,H,J,B]^
				(clause(clpt(N,V,H),B) ;
					clause(clpt(N,V,H,J),B)),NList),
	current_output(Stream),
	run_clp_tests(NList,Stream).

run_clp_tests([],Stream).

run_clp_tests([N | NList],Stream)
	:-
	clpt_run(N,Stream),
	!,
	run_clp_tests(NList,Stream).

	%% Test failed:
run_clp_tests([N | NList],Stream)
	:-
	printf(Stream,'\n#%t: --!!!-- TEST FAILED !!!',[N]),
	printf(Stream,'\n----------------------------------------------------\n\n',[]),
	run_clp_tests(NList,Stream).

/*---------------------------------------------------------------------*
 |	clpt/1.
 |	clpt(N)
 |	clpt(+)
 |
 |	-- run & report a single test
 *---------------------------------------------------------------------*/

export clpt/1.
clpt(N)
	:-
	current_output(Stream),
	clpt_run(N,Stream).

clpt_run(N,Stream)
	:-
	(clpt(N,VarsList,Names,ExpVals) ->
		true
		;
		clpt(N,VarsList,Names),
		ExpVals = not_avail
	),
	printf(Stream,'\n#%t: ',[N]),
	show_the_clause(N,Stream),
	printf(Stream,'\n--Computed:---------------Expected--------------------\n',[]),
	(ExpVals = not_avail ->
		length(Names, Len),
		n_of(Len, '?', ExpVals0)
		;
		ExpVals0 = ExpVals
	),
	out_subsx(Names,VarsList,ExpVals0,Stream),
	printf(Stream,'=============================================================\n',[]).

show_the_clause(N,Stream)
	:-
	(clause(clpt(N,Vars,Names,_),Body) -> true ; clause(clpt(N,Vars,Names),Body) ),
	Vars = Names,
	rev_clp_braces(Body,Body0),
	printf(Stream,'%t',[Body0]).

/*---------------------------------------------------------------------*
 |	rev_clp_braces/2
 |	rev_clp_braces(In, Out)
 |	rev_clp_braces(+, -)
 |
 |	Replaces all occurrences of clp(...) with '{}'(...)
 *---------------------------------------------------------------------*/

rev_clp_braces((B0,C0),(B,C))
	:-!,
	rev_clp_braces(B0, B),
	rev_clp_braces(C0, C).

rev_clp_braces(clp(B), '{}'(B))
	:-!.

rev_clp_braces(B, B).

/*---------------------------------------------------------------------*
 |	out_subsx/4
 |	out_subsx(Names,VarsList,ExpVals,Stream)
 |	out_subsx(+,+,+,+)
 |	
 |	out_sub_pair/4
 |	out_sub_pair(N,V,EV,Stream)
 |	out_sub_pair(+,+,+,+)
 |	
 |	- output var name, computed value, & expected value
 |
 |	Adapted from wr_subs2 and wr_subs3 in builtins/sio_wt.pro
 *---------------------------------------------------------------------*/

out_subsx([],_,_,_).
out_subsx([N|Names],[V|VarsList],[EV|ExpVals],Stream)
	:-
	out_sub_pair(N,V,EV,Stream),
	out_subsx(Names,VarsList,ExpVals,Stream).

out_sub_pair(N,V,EV,Stream)
	:-
	'$is_delay_var'(V),
	rel_arith:'$domain_term'(V, DomainTerm),
	(rel_arith:valid_domain(DomainTerm, Type, LArg, UArg) ->
		builtins:epsilon_show(Eps),
		Width is abs(UArg - LArg),
		(Width < Eps ->
			SPrt is (UArg + LArg)/2
			;
			SPrt = [LArg, UArg]
		),
		printf(Stream,'%t=%t\t\t%t\n',['%lettervar%'(N),SPrt,EV])
		;
		nl(Stream),
		write_term(Stream,'%lettervar%'(N),[]),
		arg(1,Var_DelayTerm, DelayVar),
		builtins:sio_var_to_atom(DelayVar,DelayVarAtom),
		put_char(Stream,'['),
		put_atom(Stream, DelayVarAtom),
		put_char(Stream,']'),
		write_term(Stream,'->',[]),
		builtins:write_delay_term(Stream,Var_DelayTerm, DelayVarAtom)
	).

out_sub_pair(N,V,EV,Stream)
	:-
	printf(Stream,'\n%lettervar%(%t)=%t\t\t%t\n',[N,V,EV]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% THE TESTS %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%%% INTEGER ARITHMETIC %%%%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clpt(1,[X,Y,Z],['X','Y','Z'],[[1,7],[20,32],[21,39]])
	:-
	X::real(1,7),Y::real(20,32), Z::real,{Z == X+Y}.

clpt(2,[X,Y,Z],['X','Y','Z'],[[1,7],[14,38],[21,39]])
	:-
	X::real(1,7),Y::real, Z::real(21,39),{Z == X+Y}.

clpt(3,[X,Y,Z],['X','Y','Z'],[[-3,7],[2,12],[-1,19]])
	:-
	X::real(-3,7),Y::real(2,12), Z::real,{Z == X+Y}.

clpt(4,[X,Y,Z],['X','Y','Z'],[7,2,9])
	:-
	X::real(7,7),Y::real(2,2), Z::real,{Z == X+Y}.

clpt(5,[X,Y,Z],['X','Y','Z'],[[7,12],[-4,8],[8,15]])
	:-
	X::real(7,12),Y::real, Z::real(8,15), {Z == X+Y}.

clpt(6,[X,Y,Z],['X','Y','Z'],[[-13,17],[2,12],[-1,19]])
	:-
	X::real,Y::real(2,12), Z::real(-1,19), {Z == X+Y}.

clpt(7,[X,Y,Z],['X','Y','Z'])
	:-
	X::real(1,7),Y::real(20,32), Z::real,{Z == X-Y}.

clpt(8,[X,Y,Z],['X','Y','Z'])
	:-
	X::real(1,7),Y::real, Z::real(21,39),{Z == X-Y}.

clpt(9,[X,Y,Z],['X','Y','Z'])
	:-
	X::real(7,7),Y::real(2,2), Z::real,{Z == X-Y}.

clpt(10,[X,Y,Z],['X','Y','Z'],[[1,7],[20,32],[20,224]])
	:-
	X::real(1,7),Y::real(20,32), Z::real,{Z == X*Y}.

clpt(11,[X,Y,Z],['X','Y','Z'])
	:-
	X::real(1,7),Y::real, Z::real(20,224),{Z == X*Y}.

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%%%% REAL ARITHMETIC %%%%%%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clpt(101,[X,Y,Z],['X','Y','Z'])
	:-
	X::real(1.1,7.34),Y::real(20.21,32.0), Z::real,{Z == X+Y}.

clpt(102,[X,Y,Z],['X','Y','Z'])
	:-
	X::real(1.1,7.34),Y::real, Z::real(21.31,39.34),{Z == X+Y}.



clpt(111,[X,Y],['X','Y'],[[1.0000, 3.0001],[-1.7321, 1.7321]])
	:-
	X::real(1,3), Y::real, {Y**2==X}.






clpt(201,[X,Y],['X','Y'], [[0.14285, 0.14286],[0.42857, 0.42858]])
	:- 
	[X,Y]::real, {1==X + 2*Y, Y - 3*X==0}. 




		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%%%% TRANSCENDENTALS %%%%%%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clpt(301,[X,Y],['X','Y'], [[1.0966, 1.0967], [1.9486, 1.9487]])
	:- 
	X::real,Y::real,
	{X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }.










endmod.
