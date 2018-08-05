/*============================================================*
 |	dc.pro
 |	Copyright (c) 1986-2015 by Applied Logic Systems

 |	A Desk Calculator
 |
 | Author:  Kevin A. Buettner
 | Creation:  12/03/86
 *============================================================*/

/*------------------------------------------------------------*
 *	Description:
 *
 *	A moderately sophisticated desk calculator is defined.  In addition
 *	to the usual arithmetic operations provided by most calculators,
 *	this calculator allows constants and simple numeric functions to be 
 *	defined.  For example, the constant two is defined as follows:
 *		two := 2
 *	The factorial function may be defined as
 *		fact(n) := if n==0 then 1 else n*fact(n-1)
 *
 *
 *	The desk calcualor is invoked from prolog by consulting this file
 *	and then executing the following goal.
 *		?- dc.
 *	The desc calculator manages is own input, so you should NOT
 *	terminate input with a period; simply type input expressions
 *	as you would on a handheld calculator, then hit return.
 *------------------------------------------------------------*/

module desk_calc.
   export dc/0.

   /*
    * Procedure:	dc
    * Normal Usage:	dc
    * Description:	Calls the readin predicate, lexical analysis, parser,
    *			and evaluator.
    */

   dc :-
	nl,write('Simple shell for a desk calculator'),nl,
	write('    By Kevin Buettner'),nl,nl,
	write('Type numeric expressions (no final . needed) plus return'),nl,
	write('Type exit to exit from the calculator to Prolog'),nl,nl,
	dc0.
   dc0 :- write('dc: '), 
         readin(Line), 
	 lexan(Line,Tokens), 
	 parse(Tokens,Tree), 
         eval(Tree,Answer),
	 write(Answer), nl, 
	!, 
	dc_again(Answer).
    dc0 :- write('??'), nl, dc0.

   dc_again(exit) :- !, seen.
   dc_again(_) :- dc0.

   source(File) :-
      seeing(OldF),
      see(File),
      readfile,
      seen,
      see(OldF).

   readfile :-
      readin(Line),
      lexan(Line,Tokens),
      parse(Tokens,Tree),
      eval(Tree,Answer),
      !,
      readmorefile(Answer).
   readfile :-
      write('Error in source.'),
      nl,
      readfile.

   readmorefile(exit) :- !.
   readmorefile(_) :- readfile.






   /*
    * Procedure: 	readin(List)
    * Normal Usage:	readin(out)
    * Description:	Reads a line from the terminal and places the
    *			Characters in List.
    */
   
   readin(L) :- readin0(Line), !, write(Line), nl, atom_codes(Line, L).    
   readin("stop").
   
   readin0(FullLine) :-
      get_line(Line),
      continue_line(Line, FullLine).
   continue_line(end_of_file, stop).
   continue_line(PartialLine, FullLine) :-
      sub_atom(PartialLine, _, _, 0, '\\'),
      readin0(LineTail),
      sub_atom(PartialLine, 0, _, 1, LineHead),
      atom_concat(LineHead, LineTail, FullLine).
   continue_line(Line,Line).
   
   /*
    * Procedure:	lexan(InList,OutList)
    * Normal Usage:	lexan(in,out)
    * Description:	Takes the list of characters InList and transforms it
    *			into a list of tokens OutList.
    * Data Structures:
    *			Characters representing numbers are transformed to 
    *			numbers; function names and special symbols are
    *			transformed into their equivalent symbol
    *			representations.
    */

   lexan([],[]) :- !.
   lexan(".",[]) :- !.
   lexan(InL,[Tok | Toks]) :- 
      classify(InL,RestL,Tok),
      lexan(RestL,Toks).

   classify([N | Ns], RestL, Number) :-
      digit(N),
      !,
      read_number([N | Ns], RestL, Number).
   classify([A | As], RestL, Token) :-
      alphabetic(A),
      !,
      read_alphas([A | As], RestL, Token).
   classify(Special, RestL, Token) :-
      read_special(Special,RestL,Token),
      !.
   classify([W | T], RestL, Token) :-
      white_space(W),
      !,
      classify(T,RestL,Token).

   digit(C) :- 0'0 =< C, C =< 0'9.

   alphabetic(C) :- 0'A =< C, C =< 0'Z, !.
   alphabetic(C) :- 0'a =< C, C =< 0'z, !.
   alphabetic(0'$) :- !.
   alphabetic(0'_).

   white_space(0' ) :- !.		/* space */
   white_space(0'\t) :- !.		/* tab */
   white_space(0'\r) :- !.		/* carriage return */
   white_space(0'\n) :- !.		/* line feed */

   read_number(L,RestL,Number) :-
      read_integer(L,IntRest,Int),
      read_frac(IntRest,RestL,Frac),
      Number is Int+Frac.

   read_integer(L,RestL,Num) :-
      read_integer(L,RestL,0,Num).

   read_integer([D|T],RestL,Acc,Num) :-
      digit(D),
      !,
      NewAcc is Acc*10+D-0'0,
      read_integer(T,RestL,NewAcc,Num).
   read_integer(RestL,RestL,Num,Num).

   read_frac([0'. | T],RestL,Num) :-
      !,
      read_frac(T,RestL,0,10,Num).
   read_frac(RestL,RestL,0).

   read_frac([D|T],RestL,Acc,Div,Num) :-
      digit(D),
      !,
      NewAcc is Acc+(D-0'0)/Div,
      NewDiv is Div*10,
      read_frac(T,RestL,NewAcc,NewDiv,Num).
   read_frac(RestL,RestL,Num,_,Num).

   read_alphas(L, RestL, Token) :-
      read_alphastring(L, RestL, TokString),
      name(Token,TokString).

   read_alphastring([A|T], RestL, [A|NT]) :-
      alphabetic(A),
      !,
      read_alphastring(T,RestL,NT).
   read_alphastring(RestL, RestL, []).

   read_special([0':,0'= | R],R,':=') :- !.
   read_special([0'&, 0'& | R], R, '&&') :- !.
   read_special([0'|, 0'| | R], R, '||') :- !.
   read_special([0'!, 0'= | R], R, '!=') :- !.
   read_special([0'=, 0'= | R], R, '==') :- !.
   read_special([0'<, 0'= | R], R, '<=') :- !.
   read_special([0'>, 0'= | R], R, '>=') :- !.
   read_special([0'=, 0'< | R], R, '<=') :- !.
   read_special([0'+ | R],R,'+') :- !.
   read_special([0'- | R],R,'-') :- !.
   read_special([0'* | R],R,'*') :- !.
   read_special([0'/ | R],R,'/') :- !.
   read_special([0'& | R],R,'&') :- !.
   read_special([0'| | R],R,'|') :- !.
   read_special([0'^ | R],R,'^') :- !.
   read_special([0'( | R],R,'(') :- !.
   read_special([0') | R],R,')') :- !.
   read_special([0', | R],R,',') :- !.
   read_special([0'< | R],R,'<') :- !.
   read_special([0'> | R],R,'>') :- !.
   read_special([0'! | R],R,'!') :- !.
   read_special([0'? | R],R,'?') :- !.
   read_special([0': | R],R,':') :- !.
   read_special([0'^ | R],R,'^') :- !.

   /*
    * Procedure:	parse(InList,Structure)
    * Normal Usage:	parse(in,out)
    * Description:	parse takes the list InList produced by the lexical
    *			analyzer and builds a Prolog structure which represents
    *			the tokenized list.
    *
    * Grammar:		DCG's are used to implement the parser.  We'd like
    *			the grammar to look something like the following
    *			(without the attributes):
    *
    *		statement	--> definition
    *		                  | expression
    *
    *		definition	--> header ':=' expression
    *
    *		header		--> identifier hparams
    *
    *		hparams		--> lambda
    *		                  | '('hparam-list  ')'
    *
    *		hparam-list 	--> variable-name
    *				  | variable-name ',' hparam-list
    *
    *		variable-name	--> identifier
    *
    *		expression	--> e1
    *
    *		e1		--> 'if' e1 then e1 else e1
    *				  | e2 '?' e1 ':' e1
    *				  | e2
    *
    *		e2		--> e2 '||' e3
    *				  | e3
    *
    *		e3		--> e3 '&&' e4
    *				  | e4
    *
    *		e4		--> e5 '<=' e5
    *				  | e5 '>=' e5
    *				  | e5 '<' e5
    *				  | e5 '>' e5
    *				  | e5 '==' e5
    *				  | e5 '!=' e5
    *				  | e5
    *
    *		e5		--> e5 '+' e6
    *				  | e5 '-' e6
    *				  | e6
    *
    *		e6		--> e6 '*' e7
    *				  | e6 '/' e7
    *				  | e7
    *
    *		e7		--> e8 '^' e7
    *				  | e8
    *
    *		e8		--> number
    *				  | function
    *				  | '-' e8
    *				  | '!' e8
    *				  | '(' e1 ')'
    *
    *		function	--> identifer params
    *
    *		params		--> lambda		(empty production)
    *				  | '(' expression-list ')'
    *
    *		expression-list	--> expression
    *				  | expression ',' expression-list
    *
    *
    *		The problem with making the grammar look like this is the
    *		presence of left recursion.  Left recursion is not a problem
    *		with LR or LALR parsers, but DCGS are essentially LL parsers
    *		and as everyone knows, LL parsers don't handle left recursion
    *		especially well.  So the grammar is tranformed using standard
    *		techniques to eliminate the left recursion.
    */

   parse(TokList,Structure) :-
      statement(Structure,TokList,[]), !.

   lambda(X,X).

   statement(S)		--> expression(S).
   statement(S)		--> definition(S).

   expression(S)	--> e1(S).

   definition(':='(H,E)) --> header(H), [':='], expression(E).

   header(H)		--> identifier(Ident), hparams(ParamList),
   			    {H =.. [Ident | ParamList]}.

   hparams(PL)		--> ['('], hparam_list(PL), [')'].
   hparams([])		--> lambda.

   hparam_list([P1|Ps])	--> identifier(P1), hparam_list1(Ps).

   hparam_list1([P1|Ps]) --> [','], identifier(P1), hparam_list1(Ps).
   hparam_list1([])	--> lambda.

   e1(if_then_else(C,T,F)) --> [if], e1(C), [then], e1(T), [else], e1(F).
   e1(S)		--> e2(U), e1prime(S,U).

   e1prime(if_then_else(C,T,F),C)
   			--> ['?'], e1(T), [':'], e1(F).
   e1prime(S,S)		--> lambda.

   e2(S)		--> e3(T), e2prime(S,T).

   e2prime(S,U)		--> ['||'], e3(V), e2prime(S,'||'(U,V)).
   e2prime(S,S)		--> lambda.

   e3(S)		--> e4(T), e3prime(S,T).

   e3prime(S,U) 	--> ['&&'], e4(V), e3prime(S,'&&'(U,V)).
   e3prime(S,S)		--> lambda.

   e4(S)		--> e5(U), e4prime(S,U).

   e4prime('<='(U,V),U)	--> ['<='], e5(V).
   e4prime('>='(U,V),U)	--> ['>='], e5(V).
   e4prime('<'(U,V),U)	--> ['<'], e5(V).
   e4prime('>'(U,V),U)	--> ['>'], e5(V).
   e4prime('=='(U,V),U)	--> ['=='], e5(V).
   e4prime('!='(U,V),U) --> ['!='], e5(V).
   e4prime(S,S)		--> lambda.

   e5(S)		--> e6(T), e5prime(S,T).

   e5prime(S,U)		--> ['+'], e6(V), e5prime(S,'+'(U,V)).
   e5prime(S,U)  	--> ['-'], e6(V), e5prime(S,'-'(U,V)).
   e5prime(S,S)		--> lambda.

   e6(S)		--> e7(T), e6prime(S,T).

   e6prime(S,U)		--> ['*'], e7(V), e6prime(S,'*'(U,V)).
   e6prime(S,U) 	--> ['/'], e7(V), e6prime(S,'/'(U,V)).
   e6prime(S,S)		--> lambda.

   e7(S)		--> e8(U), e7prime(S,U).
   
   e7prime(^(U,V),U)	--> ['^'], e7(V).
   e7prime(S,S)		--> lambda.

   e8('-'(S))		--> ['-'], e8(S).
   e8('!'(S))		--> ['!'], e8(S).
   e8(S)		--> ['('], e1(S), [')'].
   e8(Number)		--> [Number], {number(Number)}.
   e8(Function)		--> function(Function).

   function(Function)	--> identifier(Ident), params(ParamList),
   			    {Function =.. [Ident | ParamList]}.

   params(PL)		--> ['('], param_list(PL), [')'].
   params([])		--> lambda.

   param_list([P1|Rest]) --> e1(P1), param_list1(Rest).

   param_list1([P1|Rest]) --> [','], e1(P1), param_list1(Rest).
   param_list1([])	--> lambda.

   identifier(Ident)	--> [Ident], {not_special(Ident)}.

   not_special(ID) :- read_special(_,_,ID), !, fail.
   not_special(ID).

   /*
    * Procedure:	eval(Expression,Answer)
    * Normal Usage:	eval(in,out)
    * Description:	Evaluates Expression and returns the result in
    *			Answer.  Notice that for function and constant
    *			definitions, this eval relation is extended with
    *			assertz.
    */

   eval(stop,exit)     :- !.
   eval(exit,exit)     :- !.
   eval(halt,exit)     :- !.
   eval(Number,Number) :- number(Number), !.
   eval( X+Y,  Result) :- !, eval(X,XR), eval(Y,YR), Result is XR+YR.
   eval( X*Y,  Result) :- !, eval(X,XR), eval(Y,YR), Result is XR*YR.
   eval( X/Y,  Result) :- !, eval(X,XR), eval(Y,YR), Result is XR/YR.
   eval( X-Y,  Result) :- !, eval(X,XR), eval(Y,YR), Result is XR-YR.
   eval( X^Y,  Result) :- !, eval(X,XR), eval(Y,YR), Result is XR^YR.
   eval(div(X,Y),Result) :- !, eval(X,XR), eval(Y,YR), Result is XR div YR.
   eval(mod(X,Y),Result) :- !, eval(X,XR), eval(Y,YR), Result is XR mod YR.
   eval('&&'(X,Y),Result) :- !, eval(X,XR), bool_and(XR,Y,Result).
   eval('||'(X,Y),Result) :- !, eval(X,XR), bool_or(XR,Y,Result).
   eval(if_then_else(C,T,F),R) :- !, eval(C,CR), if_then_eval(CR,T,F,R).
   eval('<'(X,Y),Result) :- !, eval(X,XR), eval(Y,YR), eval_cond(XR<YR,Result).
   eval('>'(X,Y),Result) :- !, eval(X,XR), eval(Y,YR), eval_cond(XR>YR,Result).
   eval('<='(X,Y),Result) :- !, eval(X,XR), eval(Y,YR), 
                             eval_cond(XR=<YR,Result).
   eval('>='(X,Y),Result) :- !, eval(X,XR), eval(Y,YR),
                             eval_cond(XR>=YR,Result).
   eval('=='(X,Y),Result) :- !, eval(X,XR), eval(Y,YR),
                             eval_cond(XR=:=YR,Result).
   eval('!='(X,Y),Result) :- !, eval(X,XR), eval(Y,YR), 
                             eval_cond(XR=\=YR,Result).
   eval('!'(X), Result)   :- !, eval(X,XR), bool_not(XR,Result).
   eval(':='(H,E),Result) :- !, make_definition(H,E,Result).
   eval(source(File),ok)  :- !, source(File).


   bool_and(0,_,0) :- !.
   bool_and(_,Y,R) :- eval(Y,YR), make_bool(YR,R).

   bool_or(XR,_,1) :- make_bool(XR,1), !.
   bool_or(_,Y,R)  :- eval(Y,YR), make_bool(YR,R).

   bool_not(0,1) :- !.
   bool_not(_,0).

   make_bool(0,R) :- !, R=0.
   make_bool(_,1).

   if_then_eval(0,_,F,R) :- !, eval(F,R).
   if_then_eval(_,T,_,R) :- !, eval(T,R).

   eval_cond(Call,1) :- call(Call),!.
   eval_cond(_,0).

   make_definition(H,E,F) :- 
      H =.. [F|PL],			/* Get the function and parameters */
      make_vlist(PL,VL1),		/* Make up a vlist for matching    */
      make_vlist(PL,VL2),		/* Make up another for the results */
      Match_Structure =.. [F|VL1],	/* Got the Match_Structure	   */
      nuke_defn(Match_Structure),	/* Nuke any previous definitions   */
      make_param_goals(VL1,VL2,ParamGoals),
      subst(E,PL,VL2,NewE),		/* substitute real variables	   */
      goal_concat(!,ParamGoals,BG),
      assertz((eval(Match_Structure,Res) :- BG,eval(NewE,Res))).

   make_vlist([],[]).
   make_vlist([_|T],[_|NT]) :- make_vlist(T,NT).

   nuke_defn(MS) :- retract((eval(MS,_) :- !,_)), fail.
   nuke_defn(_).			/* slimey isn't it? */

   make_param_goals([],[],true).
   make_param_goals([P|Ps],[R|Rs],G) :-
      make_param_goals(Ps,Rs,Gs),
      goal_concat(eval(P,R),Gs,G).

   goal_concat(true,G,G) :- !.
   goal_concat(G,true,G) :- !.
   goal_concat(G1,G2,(G1,G2)).

   subst(Number,_,_,Number) :- number(Number),!.
   subst(Param,PL,VL,Var)   :- atom(Param), vassoc(Param,PL,VL,Var), !.
   subst(Atom,PL,VL,Atom)   :- atom(Atom), !.
   subst(Func,PL,VL,Res)    :-
      functor(Func,F,A),
      functor(Res,F,A),
      subst_args(A,Func,PL,VL,Res).

   subst_args(0,_,_,_,_) :- !.
   subst_args(A,InS,PL,VL,OutS) :-
      arg(A,InS,InArg),
      arg(A,OutS,OutArg),
      subst(InArg,PL,VL,OutArg),
      A1 is A-1,
      subst_args(A1,InS,PL,VL,OutS).

   vassoc(P,[P|_],[V|_],V) :- !.
   vassoc(P,[_|Ps],[_|Vs],V) :- vassoc(P,Ps,Vs,V).

endmod.
