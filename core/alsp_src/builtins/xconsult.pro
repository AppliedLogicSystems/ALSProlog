/*=================================================================*
 | 			xconsult.pro
 |	Copyright (c) 1992-96, Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Transforms prolog programs into a form which is
 |		somewhat efficient to run under ALS-Prolog.  The primary
 |		effect is transformation of -> and ; which are not
 |		directly compiled.
 |
 | Author:	Kevin A. Buettner
 | Modifications(minor): Ken Bowen
 | Mods to support {} clp usage: Ken Bowen - 05/09/95
 *=================================================================*/

module xconsult.

export xconsult/2.

:-	op(1200, fx, module),
	op(1200, fx, export),
	op(1200, fx, declare),
	op(1200, fx, use).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Source-level debugging hooks
	%%    -- the primary "flag" ,
	%% source_level_debugging/1, is being 
	%% removed in favor of the offical prolog_flag
	%% "debug" (with values on/off)
	%% The definitions below are temporary until
	%% a complete sweep of everything can be
	%% performed:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export source_level_debugging/1.
	%% Start up with source_level_debugging off here; 
	%% Debugger does direct re-consultd of files it has
	%% to work on.
source_level_debugging(Value)
	:-
	current_prolog_flag(debug, Value).


export change_source_level_debugging/1.
export change_source_level_debugging/2.

change_source_level_debugging(Value)
	:-
	set_prolog_flag(debug, Value).
change_source_level_debugging(Value,Prev)
	:-
	current_prolog_flag(debug, Prev),
	set_prolog_flag(debug, Value).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% xconsult/2
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export xxconsult/5.
xxconsult(Stream, File, SDMode, CGFlag, FinalErrs)
	:- 
	readFile(Stream, File, SDMode, CGFlag, [], [], [], InitFinalErrs),
	dreverse(InitFinalErrs, FinalErrs).

export xconsult/2.
xconsult(File,NErrs)
	:- 
	xconsult(File,NErrs, _).

export xconsult/3.
xconsult(File,NErrs, FinalErrs)
	:- 
	nonvar(File),
	( exists_file(File),
	    open(File,read,Stream)
		;   File = user,
	    	current_alias(user_input,Stream) ),
	!,
	(source_level_debugging(on) -> SDMode = debugging ; SDMode = no_debugging),
		%% repair if needed:
	CGFlag = true,
	readFile(Stream, File, SDMode, CGFlag, [], [], [], InitFinalErrs),
	dreverse(InitFinalErrs, FinalErrs),
	sio:stream_syntax_errors(Stream,NErrs),	%% get the number of errors
	close(Stream).

xconsult(File,0) 
	:-
		%% xconsult_er: "Error (x)consulting file %t.\n"
	prolog_system_error(xconsult_er, [File]).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%		MAIN LOOP			%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
readFile(Stream, File, SDMode, CGFlag, PredStack, ModStack, CurErrs, FinalErrs) 
	:-
	catch( ( readvnv(Stream,SDMode, Term,Names,Vars), Flag = ok),
		   Ball,
		   Flag = error(Ball) ),
	!,
%	statistics,
%	pbi_debug(Term),
%	gc,

	disp_process(Flag, Term,Names,Vars,Stream, File, SDMode, CGFlag, PredStack, ModStack, CurErrs, FinalErrs).

/**********************
xc_process_list([], Stream, File, SDMode, ModStack, ModStack, CurErrs, CurErrs).

xc_process_list([Term | List], Stream, File, SDMode, ModStack, FinalStack, CurErrs, FinalErrs)
	:-
	process(Term,[],[],Stream, File, ModStack, NewModStack, CurErrs, NewErrs, Flag), !,
	xc_process_list(List, Stream, File, SDMode, NewModStack, FinalStack, NewCurErrs, FinalErrs).
**********************/


disp_process(ok, end_of_file, Names,Vars,Stream, File, SDMode, CGFlag, PredStack, ModStack, CurErrs, FinalErrs)
	:-
	(ModStack = [] ->
		FinalErrs = CurErrs
		;
		FinalErrs = [ prolog_system_error(mods_open,[File,ModStack]) | CurErrs]
	).

	%% Accumulate syntax errors:
disp_process(error(error(syntax_error,EL)), 
				Term,Names,Vars,Stream, File, SDMode, CGFlag, PredStack, ModStack, CurErrs, FinalErrs)
	:-!,
	readFile(Stream, File, SDMode, CGFlag, PredStack, ModStack, [error(syntax_error,EL) | CurErrs], FinalErrs).

	%% Quit on any other type of error:
disp_process(error(Error), Term,Names,Vars,Stream, File, SDMode, CGFlag, PredStack, ModStack, 
					CurErrs, [error(Error) | CurErrs])
	:-!.

disp_process(ok, Term,Names,Vars,Stream, File, SDMode, CGFlag, PredStack, ModStack, CurErrs, FinalErrs)
	:-
	process(Term,Names,Vars,Stream, File, CGFlag, PredStack, NewPredStack, 
							ModStack, NewModStack, CurErrs, NewErrs, Flag), 
	!,
	fin_process(Flag, Stream, File, SDMode, CGFlag, NewPredStack, NewModStack, NewErrs, FinalErrs).

fin_process(ok, Stream, File, SDMode, CGFlag, NewPredStack, NewModStack, NewErrs, FinalErrs)
	:- !,
	readFile(Stream, File, SDMode, CGFlag, NewPredStack, NewModStack, NewErrs, FinalErrs).

fin_process(warning, Stream, File, SDMode, CGFlag, NewPredStack, NewModStack, NewErrs, FinalErrs)
	:- !,
	readFile(Stream, File, SDMode, CGFlag, NewPredStack, NewModStack, NewErrs, FinalErrs).

fin_process(error, Stream, File, SDMode, CGFlag, NewPredStack, NewModStack, FinalErrs, FinalErrs).

readvnv(Stream, SDMode, Term,Names,Vars) 
	:-
	read_term(Stream,Term0,[vars_and_names(Vars,Names), SDMode]),
	(SDMode = debugging ->
		top_clausegroup(CID),
		sio:pp_xform_clause(Term0,CID,Term)
		;
		Term = Term0
	).

top_clausegroup(CID) 
	:-
	pop_clausegroup(CID),		%% This is kind of clumsy, but
	push_clausegroup(CID).		%% it works.

/*-------------------------------------------------------------------*
 |	process/8
 |	process(Item, Names, Vars,Stream, File, ModStack, Errs, NewErrs)
 |	process(+, +, +,+, +, +, +, -)
 *-------------------------------------------------------------------*/

process('?-'(Command),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, Errs, NewErrs, Flag)
	:- 
	topmod(Module),
	!,
	execute_command_or_query(Stream,qf,Module,Command, Errs, NewErrs, Flag).

/**********************
process(':-'(defineClass(DMod,Spec)),
			Names,Vars,Stream, File, ModStack, NewModStack, CurErrs, NewErrs, ok)
	:- !, 
	process(defineClass(DMod,Spec),
			Names,Vars,Stream, File, ModStack, NewModStack, CurErrs, NewErrs, ok).

process(defineClass(DMod,Spec),
			Names,Vars,Stream, File, ModStack, NewModStack, CurErrs, NewErrs, ok)
	:- !, 
write(defineClass(DMod,Spec)),nl,
	builtins:defineClass(DMod,Spec,Code),
write(code=Code),nl,flush_output,
	xc_process_list(Code, Stream, File, SDMode, ModStack, NewModStack, CurErrs, NewErrs).
**********************/


process((':-'(Command) :- '$dbg_aph'(_,_,_)),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, 
				Errs, NewErrs, Flag)
	:- 
	topmod(Module),
	!,
	execute_command_or_query(Stream,cf,Module,Command, Errs, NewErrs, Flag).

process((':-'(Command) :- '$dbg_apf'(_,_,_)),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, 
					Errs, NewErrs, Flag)
	:- 
	topmod(Module),
	!,
	execute_command_or_query(Stream,cf,Module,Command, Errs, NewErrs, Flag).

process(':-'(Command),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, Errs, NewErrs, Flag)
	:- 
	topmod(Module),
	!,
	execute_command_or_query(Stream,cf,Module,Command, Errs, NewErrs, Flag).

process((module M),Names,Vars,Stream, File, CGF,PS,PS, ModStack, [M | ModStack], Errs, Errs, ok)
	:- !,
	pushmod(M).


process(endmod,Names,Vars,Stream, File, CGF,PS,PS, [], [], 
					Errs, [prolog_system_error(s(endmods,Stream),[]) | Errs], warning)
	:- !.
		%% endmods: "Too many endmods.\n"

process(endmod,Names,Vars,Stream, File, CGF,PS,PS1, [_ | ModStack], ModStack, Errs, Errs, ok)
	:- !,
	killvars,
	popmod.

process((export ExportList),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, Errs, Errs, ok)
	:- !,
	doexport(ExportList).

process((use UseList),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, Errs, Errs, ok)
	:- !,
	douse(UseList).

process((declare DeclareList),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, Errs, Errs, ok)
	:- !,
	Names=Vars,
	dodeclare(DeclareList).

process((H --> B), Names, Vars,Stream, File, CGF,PS,NPS, ModStack, NewStack, Errs, NewErrs, Flag)
	:-
	builtins:dcg_expand((H-->B),OutClause),
	!,
	process(OutClause,Names,Vars,Stream, File, CGF,PS,NPS, ModStack, NewStack, Errs, NewErrs, Flag).

process((Head :- _),Names,Vars,Stream, File, CGF,PS,PS, ModStack, ModStack, 
			Errs, [prolog_system_error(s(ErrCode,Stream),[LineNumber]) | Errs], warning)
	:-
	functor(Head, F, A),
	illegal_dfn(F, A, ErrCode),
	!,
	sio_linenumber(Stream,LineNumber).

process((Head :- Body),Names,Vars,Stream, File,  CGF,PS,NPS, ModStack, ModStack, Errs, Errs, ok)
	:- !, 
	(CGF ->
		NPS = PS
		;
		functor(Head,HF,HA),
		(dmember(HF/HA, PS) ->
			NPS = PS
			;
			(ModStack = [MM|_]; MM=user),
			MM:abolish(HF,HA),
			NPS = [HF/HA | PS]
		)
	),
	xform(Head,Body,NewBody,Names,Vars), 
	addrule(Head,NewBody).

process( Fact, Names,Vars,Stream, File,  CGF,PS,PS, ModStack, ModStack, 
			Errs, [prolog_system_error(s(ErrCode,Stream),[LineNumber]) | Errs], warning)
	:-
	functor(Fact, F, A),
	illegal_dfn(F, A, ErrCode),
	!,
	sio_linenumber(Stream,LineNumber).

process(Fact, Names, Vars,Stream, File,  CGF,PS,NPS, ModStack, ModStack, Errs, Errs, ok)
	:-
	(CGF ->
		NPS = PS
		;
		functor(Fact,HF,HA),
		(dmember(HF/HA, PS) ->
			NPS = PS
			;
			(ModStack = [MM|_] ; MM=user),
			MM:abolish(HF,HA),
			NPS = [HF/HA | PS]
		)
	),
	xform(Fact,true,NewBody,Names,Vars),
	addrule(Fact,NewBody).

execute_command_or_query(Stream_or_alias,ErrTag,Module,CommandOrQuery, Errs, NewErrs, Flag)
	:-
	sio:is_stream(Stream_or_alias, Stream),
	sio:is_input_stream(Stream),
	!,
	sio_linenumber(Stream,LineNumber),
	sio:stream_name(Stream,StreamName),
	xform_command_or_query(CommandOrQuery,XCommandOrQuery),
	fin_xcmd_q(XCommandOrQuery,ErrTag,Module,LineNumber,Stream,StreamName,Errs,NewErrs, Flag).

fin_xcmd_q(XCommandOrQuery,ErrTag,Module,LineNumber,Stream,StreamName,Errs,NewErrs, Flag)
	:-
	catch(
	  execcommand(
		catch( Module:XCommandOrQuery,
			   error(W,L),
			   ( prolog_system_error(error(W,L),[]), throw(error(W,L)) )
			  )  ),
		OuterBall,
		true),
	!,
	fin_Exec(OuterBall, Errs, NewErrs, Flag).

fin_xcmd_q(XCommandOrQuery,ErrTag,Module,LineNumber,Stream,StreamName,Errs,NewErrs, error)
	:-
	Error = error(qc_failed(ErrTag,Name,LineNumber),[]),
	NewErrs = [Error | Errs].

fin_Exec(OuterBall, Errs, Errs, ok)
	:-
	var(OuterBall),!.
fin_Exec(OuterBall, Errs, [OuterBall | Errs], error).

illegal_dfn(',', 2, rdef_comma).
illegal_dfn(';', 2, rdef_semi).
illegal_dfn('->', 2, rdef_arrow).
illegal_dfn('!', 0, rdef_cut).

/*---------------------------*
 | module stack
 *---------------------------*/

topmod(user).

pushmod(M)
	:-
	atom(M),
	!,
	functor(MM,M,0),		/* intern it */
	'$icode'(-10,MM,0,0,0),		/* new module */
	asserta_at_load_time(topmod(MM)).

pushmod(M)
	:-
	write(error_stream,'Invalid module name ``'), 
	write(error_stream,M), 
	write(error_stream,'''''. Using user instead.'),
	nl(error_stream),
	pushmod(user).

popmod
	:-
	'$icode'(-9,0,0,0,0),		/* end module */
	retract(topmod(_)),
	!,
	makenonempty.

makenonempty
	:-
	topmod(_), !.
makenonempty
	:-
	asserta(topmod(user)).

/*
 * module exports & use declarations -- moved to blt_db.pro
 */

/*
 * variable declarations
 */

dodeclare((D1,D2))
	:-
	dodeclare(D1),
	dodeclare(D2).
dodeclare(Var)
	:-
	atom(Var),
	!,
	topmod(M),
	gv_alloc(N),
	gv_set(N,_),
	asserta(gvar(Var,N,M)).
dodeclare(Huh)
	:-
	write(error_stream,'Invalid Variable Declaration.  Ignoring it.'),
	nl(error_stream).



/*
 * kill certain global variables
 */

killvars
	:-
	topmod(M),
	killvars(M).

killvars(M)
	:-
	retract(gvar(_,_,M)),
	!,
	killvars(M).
killvars(_).



/*
 * addrule
 */

addrule(Head,Body)
	:-
	topmod(M),
	addrule(Body,Head,M).

addrule(V,Head,M)
	:-
	var(V),
	!,
	addclause(M,(Head:-call(V))).
addrule(true,Head,M)
	:- !,
	addclause(M,Head).
addrule(Body,Head,M)
	:-
	addclause(M,(Head:-Body)).

addclauses(Cs, M)
	:-
	do_addclauses(Cs, M, []).

do_addclauses([], M, MStack).

do_addclauses([(module NewM) | Cs], M, MStack) 
	:-!,
	do_addclauses(Cs, NewM, [M | MStack]).

do_addclauses([endmod | Cs], _, MStack) 
	:-!,
	(MStack = [M | NewMStack] -> true ; M = user),
	do_addclauses(Cs, M, NewMStack).

do_addclauses([C | Cs], M, MStack) 
	:-
	xaddclause(C, M),
	do_addclauses(Cs, M, MStack).

xaddclause((export ExportList), M)
	:-!,
	doexport(ExportList).
xaddclause((use UseList), M)
	:-!,
	douse(UseList).
xaddclause(':-'(Command), Module)
	:-
	execute_command_or_query(Stream,cf,Module,Command),
	!.
xaddclause(':-'(Command), Module)
	:-!,
	prolog_system_error(s(cf,Stream),[]).
xaddclause(dynamic(P,A), M)
	:-
	builtins:dynamic0(P/A,M).
xaddclause(C, M)
	:-
	addclause(M, C).



/*-------------------------------------------------------------------*
 | xform/5
 | xform(Head, Body, NewBody, VNames, Vars)
 | xform(+, +, -, -, -)
 *-------------------------------------------------------------------*/

xform(Head, Body, NewBody, VNames, Vars)
	:-
	gvextend(Body,Body1,VNames,Vars),
	conjunctList(Body1,!,ConjunctList,MustExpand),
	xform2(MustExpand,Head,ConjunctList,NewBody,cutneeded(_)).

/*-----------------------------*
 | gvextend/4
 *-----------------------------*/

/* -- removed until we have a need for it --
gvextend(Body,NewBody,VNames,Vars)
	:-
	gvar(_,_,_),
	!,
	gvx(VNames,Vars,Body,NewBody).
-- removed until we have a need for it -- */

gvextend(Body,Body,_,_).

gvx([],[],Body,Body)
	:- !.
gvx([Name|RestN],[Var|RestV],Body, (gv_get(Num,Var),Body1) )
	:-
	gvar(Name,Num,_),
	!,
	gvx(RestN,RestV,Body,Body1).
gvx([_|RestN],[_|RestV],Body,Body1)
	:-
	gvx(RestN,RestV,Body,Body1).

/*-------------------------------------------------------------------------*
 | conjunctList/4
 | conjunctList(Body,CutGoal,List,MustExpand)
 | conjunctList(+,+,-,-)
 |
 | Takes a clause body (Body) and produces a list, each element of which
 | is a goal in the Body.  If a disjunction or arrow shows up in the list
 | then MustExpand will be set to yes.  If a cut is encountered, then
 | it will be replaced by CutGoal.
 *-------------------------------------------------------------------------*/

conjunctList(Body,CutGoal,List,MustExpand)
	:-
	conjunctList(Body,CutGoal,List,[],MustExpand),
	mustExpandFill(MustExpand).

conjunctList(IsVar,CutGoal,[call(IsVar) | Hole],Hole,ME)
	:-
	var(IsVar),
	!.
conjunctList(!,CutGoal,[CutGoal | Hole],Hole,ME)
	:- !.
conjunctList((G1,G2),CutGoal,L,Hole,ME)
	:- !,
	conjunctList(G1,CutGoal,L,L1,ME),
	conjunctList(G2,CutGoal,L1,Hole,ME).
conjunctList(G,CutGoal,[G|Hole],Hole,MustExpand)
	:-
	functor(G,P,A),
	mustExpand(P,A,MustExpand),
	!.

mustExpand(';',2,yes).
mustExpand('->',2,yes).

mustExpand('{}',1,yes).

mustExpand(_,_,_).


mustExpandFill(no)
	:- !.
mustExpandFill(yes).

mustExpandOr(yes,_,yes)
	:- !.
mustExpandOr(_,yes,yes)
	:- !.
mustExpandOr(no,no,no).

/*-----------------------------------------------------------------*
 | xform2/5
 | xform2(MustExpand,Head,ConjunctList,NewBody,CutVarInfo)
 | xform2(+,+,+,-,+)
 *-----------------------------------------------------------------*/

xform2(no,Head,CJL,NewBody,_)
	:- !, 		/* no further transformation needed */
	listToConjuncts(CJL,NewBody).
xform2(yes,Head,CJL,NewBody,CVI)
	:-
	vcollect([Head|CJL],VL1),
	cutVarInfo(CVI,CJL,VL1,CVO,VL2),
	vreorganize(Head,CJL,VL2,VL3),
	xsemi(CJL,1,VL3,NewBody,CVO,_).

/*-----------------------------------------------------------------------*
 | listToConjuncts(L,C)
 |
 | Converts a list produced by conjunctList back to the comma form.  This
 | is used by xform2 when no expansion is really necessary.  conjunctList
 | occasionally does useful work for us (such as replacing ! with $cut/1)
 | which we don't want to throw away.
 *-----------------------------------------------------------------------*/

listToConjuncts([],true)
	:- !.
listToConjuncts([H|T],Conjuncts)
	:-
	listToConjuncts0(T,H,Conjuncts).

listToConjuncts0([],Goal,Goal)
	:- !.
listToConjuncts0([H|T],Goal,(Goal,Conjuncts))
	:-
	listToConjuncts0(T,H,Conjuncts).

/*----------------------------------------------------------------------*
 | vcollect/2
 | vcollect(TermList,VList) 
 | vcollect(+,-) 
 |
 | Calls vcollect0/3 which recurses down TermList collecting 
 | (via vcollect/3) information about the variables in TermList, and
 | then terminates the dlist which vcollect0/3 utilized for collection.
 | The terms in termlist are numbered with the first element being 0.
 *----------------------------------------------------------------------*/

vcollect(Term,VList)
	:-
	vcollect0(Term,VList,0),
	vfill(VList).

vcollect0([],_,_).
vcollect0([H|T],VList,Id)
	:-
	vcollect(H,VList,Id),
	IdN is Id+1,
	vcollect0(T,VList,IdN).

/*-----------------------------------------------------------------------*
 | vcollect/3
 | vcollect(Term,VarList,Id) 
 | vcollect(+,-,+) 
 |
 | collects in VarList all of the variables in Term.  
 | 
 | Term		= an arbitrary Prolog term, typically a goal from a clause;
 | VarList	= difference list containing terms of the following form:
 | 					v(Var,IdList)
 | Id 		= integer uniquely identifying Term in left-to-right sweep 
 |				of source clause;
 |
 | In the term v(Var, IdList), Var is a variable occurring in Term,
 | and IdList is the list of Id's of all terms in which this variable
 | occurs.
 |
 | Neither VarList nor any of the IdLists will be [] terminated.  This
 | can be done with vfill/1.
 *-----------------------------------------------------------------------*/

vcollect(Atomic,_,_)
	:-
	atomic(Atomic),
	!.
vcollect(Var,VL,Id)
	:-
	var(Var),
	!,
	addtoVL(Var,VL,Id).
vcollect(Term,VL,Id)
	:-
	functor(Term,_,Arity),
	vcollect(Arity,Term,VL,Id).

vcollect(0,_,_,Id)
	:- !.
vcollect(N,S,VL,Id)
	:-
	arg(N,S,Arg),
	vcollect(Arg,VL,Id),
	NP is N-1,
	vcollect(NP,S,VL,Id).

addtoVL(Var,VL,Id)
	:-
	var(VL),
	!,
	VL=[v(Var,[Id|_]) | _].
addtoVL(Var,[v(V,IDL)|_],Id)
	:-
	Var == V,
	member(Id,IDL),
	!.
addtoVL(Var,[_|Rest],Id)
	:-
	addtoVL(Var,Rest,Id).

vfill([])
	:- !.
vfill([v(_,IDL)|Rest])
	:-
	terminateList(IDL),vfill(Rest).

terminateList([])
	:- !.
terminateList([_|Rest])
	:-
	terminateList(Rest).

/*---------------------------------*
 | vmember(V,VL,E)
 |
 *---------------------------------*/

vmember(V1,[v(V2,L)|_],v(V2,L))
	:-
	V1 == V2, !.
vmember(V1,[_|T],E)
	:-
	vmember(V1,T,E).

/*---------------------------------------------------*
 | lastelem(L,E,Count)
 |
 | Retrieves the last element E from list L.  
 | Count is the length of the list.
 *---------------------------------------------------*/

lastelem([H|T],E,Count)
	:-
	lastelem(T,H,E,1,Count).

lastelem([],E,E,Count,Count)
	:- !.
lastelem([H|T],_,E,InCount,OutCount)
	:-
	NextCount is InCount+1,
	lastelem(T,H,E,NextCount,OutCount).

/*---------------------------------*
 | vremove(V,InVL,OutVL,E)
 *---------------------------------*/

vremove(V1,[v(V2,L) | Rest],Rest,v(V2,L))
	:-
	V1==V2, !.
vremove(V,[H|T1],[H|T2],E)
	:-
	vremove(V,T1,T2,E).
	
/*-----------------------------------------------------------------------*
 | vreorganize(Head,ConjunctList,InVL,OutVL)
 |
 | Reorganizes InVL producing OutVL so that head variable positions will 
 | match up with resultant body positions for the final goal.
 *-----------------------------------------------------------------------*/

vreorganize(Head,ConjunctList,InVL,OutVL)
	:-
	lastelem(ConjunctList,LastGoal,GoalPosition),
	arrow_or_semi(LastGoal),
	!,
	functor(Head,_,NArgs),
	vrsplit(InVL,GoalPosition,GVL,OutVL,HVL),
	vr0(NArgs,Head,GVL,HVL).
vreorganize(_,_,VL,VL).

vrsplit([],Pos,[],T,T)
	:- !.
vrsplit([v(V,L)|More],Pos,VLP,VLNP,VLH)
	:-
	vrsplit_decide(L,Pos,v(V,L),VLP,VLPT,VLNP,VLNPT),
	vrsplit(More,Pos,VLPT,VLNPT,VLH).

vrsplit_decide([_],_,_,VLP,VLP,VLNP,VLNP)
	:- !.
vrsplit_decide(L,Pos,E,[E|VLPT],VLPT,VLNP,VLNP)
	:-
	member(Pos,L),
	!.
vrsplit_decide(L,Pos,E,VLP,VLP,[E|VLNP],VLNP).

vr0(NArgs,Head,VL,OutVL)
	:-
	length(VL,VLLen),
	VLLen =< NArgs,
	StopAt is NArgs-VLLen,
	!,
	vr1(StopAt,NArgs,Head,VL,[],OutVL).
vr0(NArgs,Head,VL,OutVL)
	:-
	vr1(0,NArgs,Head,VL,[],OutVL).

vr1(StopAt,StopAt,_,VL,SoFar,OutVL)
	:-
	vrfill(SoFar,VL,VLR),
	append(VLR,SoFar,OutVL),
	!.
vr1(StopAt,N,Head,VL,SoFar,OutVL)
	:-
	arg(N,Head,Arg),
	NP is N-1,
	vr2(Arg,StopAt,NP,Head,VL,SoFar,OutVL).

vr2(NV,StopAt,N,Head,VL,SoFar,OutVL)
	:-
	nonvar(NV),
	!,
	vr1(StopAt,N,Head,VL,[_|SoFar],OutVL).
vr2(V,StopAt,N,Head,VL,SoFar,OutVL)
	:-
	vremove(V,VL,VL1,E),
	!,
	vr1(StopAt,N,Head,VL1,[E|SoFar],OutVL).
vr2(V,StopAt,N,Head,VL,SoFar,OutVL)
	:-
	vr1(StopAt,N,Head,VL,[_|SoFar],OutVL).


vrfill([],R,R)
	:- !.
vrfill([V|T1],[E|T2],T2)
	:- 
	var(V),
	!,
	V=E.
vrfill([_|T],VL,VLR)
	:-
	vrfill(T,VL,VLR).

/*-----------------------------------------------------------------*
 | cutVarInfo/5
 | cutVarInfo(CutInfIn,ConjunctList,VLIn,CutInfOut,VLOut)
 | cutVarInfo(CutInfIn,ConjunctList,VLIn,CutInfOut,VLOut)
 *-----------------------------------------------------------------*/

cutVarInfo(nocutneeded,_,VL,nocutneeded,VL)
	:- !.
cutVarInfo(CVI,ConjunctList,VLIn,CVO,VLOut)
	:-
	getCutVar(CVI,CV),
	vremove(CV,VLIn,VL1,v(_,P1)),
	append(P1,_,P2),
	member(0,P2),
	!,
	cvi(P2,CV,CVI,ConjunctList,VL1,CVO,VLOut).
/*
	member(0,P2,P3),
	!,
	cvi(P3,CV,CVI,ConjunctList,VLIn,CVO,VLOut).
*/
cutVarInfo(CVI,ConjunctList,VLIn,CVO,VLOut)
	:-
	getCutVar(CVI,CV),
	cvi([0|_],CV,CVI,ConjunctList,VLIn,CVO,VLOut).

cvi(Placements,CV,CVI,ConjunctList,VL,CVI,[v(CV,Placements) | VL])
	:-
	cutPlacements(ConjunctList,Placements),
	Placements \= [0],
	!.
cvi(_,_,_,_,VL,nocutneeded,VL).

getCutVar(nocutneeded,_)
	:- !.		/* makes life easier */
getCutVar(cutneeded(CV),CV)
	:- !.
getCutVar(cutvar(CV),CV).

/*-------------------------------------------------------------------*
 | xsemi/6
 | xsemi(Goals,GN,VL,O,InCV,OutCV),
 | xsemi(+,+,+,-,+,-),
 *-------------------------------------------------------------------*/

xsemi([],_,_,true,CV,CV)
	:- !.
xsemi([Goal|RestGoals],GN,VL,O,InCV,OutCV)
	:-
	xgoal(Goal,GN,VL,O1,InCV,ICV),
	NGN is GN+1,
	xsemi(RestGoals,NGN,VL,O2,ICV,OutCV),
	makeConjunct(O1,O2,O).

/*-------------------------------------------------------------------*
 | xgoal/6
 | xgoal(InGoal,GN,VL,SemiGoal,InCV,NCV)
 | xgoal(+,+,+,-,+,-)
 *-------------------------------------------------------------------*/
xgoal(InGoal,GN,VL,SemiGoal,InCV,NCV)
	:-
	arrow_or_semi(InGoal),
	!,
	cutpt(InCV,Head,SemiGoal,NCV),
	semihead(VL,GN,Head),
	builddisjuncts(InGoal,Head,NCV).

xgoal(!,_,_,'$cut'(CV),CV,CV)
	:-
	var(CV),
	!.

xgoal(!,_,_,!,CV,CV).

xgoal('{}'(Braced),GN,VL,Goal,InCV,NCV)
	:-
	xbrace(Braced,GN,VL,Goal,InCV,NCV).

xgoal(G,_,_,G,CV,CV).

/*---------------------------------------------------------------------------*
 | semihead/3	-- build head/goal for semicolon
 |
 | semihead(VarLis,GoalNum,Head)
 *---------------------------------------------------------------------------*/

arrow_or_semi((_->_)).
arrow_or_semi((_;_)).

semihead(VarLis,GoalNum,Head)
	:-
	semihead0(VarLis,GoalNum,OutLis),
	gensym(semi,HeadName),
	Head =.. [HeadName | OutLis].

semihead0([],_,[])
	:- !.
semihead0([v(V,GL)|Rest],GN,[V|RestV])
	:-
%	GL \= [_],
	GL \= [],
	member(GN,GL),
	!,
	semihead0(Rest,GN,RestV).
semihead0([_|Rest],GN,RestV)
	:-
	semihead0(Rest,GN,RestV).

cutpt(cutvar(CV),G,G,cutvar(CV))
	:- !.
cutpt(nocutneeded,G,G,nocutneeded)
	:- !.
cutpt(cutneeded(CV),G,'als$cd'(0,G,CV),cutvar(CV)).


/*
 * builddisjuncts/3
 *
 * builddisjuncts(Disjuncts,Head,CutVar)
 */

builddisjuncts(IsVar,Head,CV)
	:-
	var(IsVar),
	!,
	builddisjuncts(call(IsVar),Head,CV).
builddisjuncts((I1;I2),Head,CV)
	:-
	nonvar(I1),	/* process left-nested if-then-else separately */
	I1 = (Arrow ; _),
	nonvar(Arrow),
	Arrow = (_ -> _),
	!,
	getCutVar(CV,CutVar),
	conjunctList(I1,'$cut'(CutVar),CJL,ME),
	xform2(ME,Head,CJL,Body,CV),
	addrule(Head,Body),
	builddisjuncts(I2,Head,CV).
builddisjuncts((I1;I2),Head,CV)
	:- !,
	builddisjuncts(I1,Head,CV),
	builddisjuncts(I2,Head,CV).
builddisjuncts((I1->I2),Head,CV)
	:- !,
	getCutVar(CV,CutVar),
	conjunctList(I1,'$cut'(CutVar),CJL1,ME1),
	conjunctList(I2,'$cut'(CutVar),CJL2,ME2),
	append(CJL1,[!|CJL2],CJL),
	mustExpandOr(ME1,ME2,ME),
	xform2(ME,Head,CJL,ArrowBody,CV),
	addrule(Head,ArrowBody).
builddisjuncts(Goals,Head,CV)
	:-
	getCutVar(CV,CutVar),
	conjunctList(Goals,'$cut'(CutVar),ConjunctList,ME),
	xform2(ME,Head,ConjunctList,OutGoals,CV),
	addrule(Head,OutGoals).




/*
 * makeConjunct/3	-- make a conjunction
 */

makeConjunct(G1,G2,C)
	:-
	makeconj0(G1,G1O),
	makeconj0(G2,G2O),
	makeconj(G1O,G2O,C).

makeconj0(V,call(V))
	:-
	var(V), !.
makeconj0(G,G).

makeconj(true,G,G)
	:- !.
makeconj(G,true,G)
	:- !.
makeconj(G1,G2,(G1,G2)).





/*
 * isCutMacro/1 enumerates the goals which we consider to be cutmacros
 */

isCutMacro(X)
	:-
	var(X).		/* This will expand to call */
isCutMacro(!).
isCutMacro(call(_)).
isCutMacro(_:_).

/*
 * cutPlacements/2 is given a list produced by conjunctList and will
 * produce a list of the places where cuts occur within arrows or
 * disjunctions.  Top level cuts are excluded.
 */

cutPlacements(CJL,CL)
	:-
	cutPlacements(CJL,1,CL),
	terminateList(CL).

cutPlacements([],_,_)
	:- !.
cutPlacements([H|T],N,CL)
	:-
	NN is N+1,
	cP0(H,N,CL),
	cutPlacements(T,NN,CL).

cP0(IsVar,_,_)
	:-
	var(IsVar), !.
cP0((D1;D2),N,CL)
	:- !,
	cP1(D1,N,CL),
	cP1(D2,N,CL).
cP0((A1->A2),N,CL)
	:- !,
	cP1(A1,N,CL),
	cP1(A2,N,CL).
cP0(_,_,_).

cP1(IsCutMacro,N,CL)
	:-
	isCutMacro(IsCutMacro),
	member(N,CL),
	!.
cP1((C1,C2),N,CL)
	:- !,
	cP1(C1,N,CL),
	cP1(C2,N,CL).
cP1(G,N,CL)
	:-
	cP0(G,N,CL).

/*--------------------------------------------------------------------*
 | xbrace/6
 | xbrace(Braced,GN,VL,Goal,InCV,NCV)
 | xbrace(+,+,+,-,+,-)
 *--------------------------------------------------------------------*/

xbrace(Braced,GN,VL,Goal,InCV,NCV)
	:-
	NCV = InCV,
	brace_clp_goal(Braced, Goal),
	!.

xbrace(Braced,GN,VL,Goal,InCV,NCV)
	:-
	NCV = InCV,
	vars_in_this_goal(VL,GN,InGoalVs),
	Goal = freeze_list_ground(InGoalVs, Braced).

brace_clp_goal(Braced, clp(Braced)).

vars_in_this_goal([], GN, []).
vars_in_this_goal([v(Var,GL) | VL], GN, [Var | InGoalVs])
	:-
	dmember(GN,GL),
	!,
	vars_in_this_goal(VL, GN, InGoalVs).

vars_in_this_goal([_ | VL], GN, InGoalVs)
	:-
	vars_in_this_goal(VL, GN, InGoalVs).

/*---------------------------------------------------------------------------
 * listing fixup
 *
 * The procedure goalFix/2 is defined in builtins.pro.  It is used by
 * the source extracter (listing, clause, and retract) and by the debugger
 * to put certain goals in a more appropriate external form.  This is necessary
 * because certain goals contain a cut point as an additional argument.
 * goalFix/2 is used in such circumstances to return a form which does
 * not have the additional argument.  
 *
 * Semicolon and arrow expansion causes similar problems, though probably
 * much more severe.  Instead of a single argument causing a problem,
 * an entire control structure has been replaced with a procedure with
 * a funny name.
 *
 * The strategy here is to attach a new clause for goalFix/2 which will
 * detect such procedures with funny names (semi___ as a prefix) and
 * get all of the clauses which form this procedure, unify the heads of
 * all clauses together with the original goal which was to be fixed up,
 * then build semicolon structures with the clause bodies as the elements
 * of the structures.  Along the way, the clause bodies need to be
 * transformed as they will contain a ! instead of an -> and $cut/1
 * instead of cut.
 *---------------------------------------------------------------------------*/


semiFix(SemiGoal,P,A,SemiFixed,Mode)
	:-
	procedures(M,P,A,FirstRef),
	!,
	semiclause(FirstRef,SemiGoal,FirstBody,Mode),
	sf0(FirstRef,SemiGoal,FirstBody,SemiFixed,Mode).

%% === sf0 recursively gets the all clauses after the first and
%%     separates them with semicolons
sf0(Ref,Head,Body,(Body;Semis),Mode)
	:-
	'$clauseinfo'(Ref,NextRef,_,_),
	NextRef \= 0,
	!,
	semiclause(NextRef,Head,NextBody,Mode),
	sf0(NextRef,Head,NextBody,Semis,Mode).
sf0(_,_,Body,Body,Mode).

%% === semiclause is given a clause reference and a head (which
%%     will be unified with all the heads seen so far) and returns
%%     the clause body after being fixed up so that it will contain
%%     arrow instead of ! and ! instead of $cut.
semiclause(Ref,Head,FixedBody,Mode)
	:-
	clause(Head,Body,Ref),
	!,
	fixBody(Body,FixedBody,Mode).


%% === fixBody/2 is responsible for fixing the body of a clause for
%%     semiclause/3
fixBody(Body,FixedBody,Mode)
	:-
	fixBody(Body,FixedBody,T,T,Mode).

%% === fixBody(InGoal,Top,Goals,Trans,Mode)
%%		InGoal 		-- the goal we are about to look at
%%		Top		-- the variable which represents the top level
%%				   return value;  Such a mechanism is necessary
%%				   since the trees for the input structure
%%				   and output structures are dramatically
%%				   different.
%%		Goals		-- Variable representing the goals that we
%%				   have encountered before this point in
%%				   our traversal of the comma list
%%		Trans		-- The transformed InGoal
%%     This procedure is responsible for traversing a comma list and
%%     passing each goal encounted in the comma list to fixBody1 where
%%     the goals will be subjected to further scrutiny.  Note that the
%%     basis clauses come after the recursive clause and are responsible
%%	   for dealing with the various end-of-comma-list cases.
fixBody((A,B),Top,Goals,Trans,Mode)
	:- !,
	fixBody1(A,B,Top,Goals,TA,TB,Mode),
	makeConjunct(TA,TB,Trans).
fixBody(!,(Goals->true),Goals,true,Mode)
	:- !.
fixBody('$cut'(_),Top,Top,!,Mode)
	:- !.
fixBody(G,Top,Top,G,Mode).


%% === fixBody1(Goal,Rest,Top,Goals,TGoal,TRest)
%%		Goal		-- the Goal which we are about to transform
%%		Rest		-- the rest of the goals in the comma list
%%		Top		-- the variable which represents the top level
%%				   return value;  see above
%%		Goals		-- Variable representing the transformed goals
%%				   that we have encountered before this point
%%				   in our traversal of the comma list
%%		TGoal		-- the transformed Goal
%%		TRest		-- the transformed Rest
%%     This procedure transforms one goal and recursively calls some
%%     variant of fixBody to transform the rest.
fixBody1(!,Rest,(Goals->TRest),Goals,true,true,Mode)
	:- !,
	fixBody(Rest,TRest,Mode).
fixBody1('$cut'(_),Rest,Top,Goal,!,TRest,Mode)
	:- !,
	fixBody(Rest,Top,Goal,TRest,Mode).
fixBody1(Goal,Rest,Top,Goals,Goal,TRest,Mode)
	:-
	fixBody(Rest,Top,Goals,TRest,Mode).



endmod.

module pgm_info.
dummy.
endmod.

