/*=========================================================================*
 |			gprim_c.pro
 |		Copyright (c) 1995 Applied Logic Systems, Inc.
 |		Copyright (c) 1995 Bell Northern Research, Ltd.
 |
 |	Compiler for interval operations pseudo code: emits C
 |	code suitable for interface to ALS Prolog
 |
 |	Original Author(s): Bill Older, + ??
 |	Date: 1992
 |
 |	Revision: Ken Bowen (port to ALS Prolog)
 |	Date: May/August, 1995
 |
 |	Language:
 |		Registers:   XL,XH,YL,YH,ZL,ZH, WL, WH
 |		Operations:  
 |         <-  assignment
 |         =,=<,<,>,>=, /=
 |         A?B ->[LT,EQ,GT]  three-way branch
 |         ub(_),lb(_)
 |         +,-,*,/, etc.
 |         Label: Expression
 |
 | Input is a clause of form  
 |
 |		op(XL,XH,YL,YH,...):- RHS
 |
 | where RHS is written in the above language:
 |     Begin : Exp  ,...
 |-------------------------------------------------------------
 |  First version revisions:
 |  may 7 92  - allow: 'goto label' where label is global 
 |				( i.e. symbol instead of var)
 |  jun 2 92  - case table hard coded into driver file / eliminates 
 |				use of text processing
 |  july 28 92 - C version 
 |  sept  17   -  special case transcendental and sqrt 
 |  oct   9    - unspecial case sqrt
 |  may 3 1995 - support for round (to nearest longint)
 *=======================================================================*/



	  %%  operators for input file:
:-op(600,xfx,'?').        % label
:-op(600,xfx,'<-').       % assignment
:-op(600,xfx,'<->').      % interchange
:-op(600,fx, goto).       % unconditional branch

	  %%  comparison operators for output file:
:-op(600,xfx,'LT').       
:-op(600,xfx,'LE').       
:-op(600,xfx,'GT').       
:-op(600,xfx,'GE').       
:-op(600,xfx,'EQ').       
:-op(600,xfx,'NE').       

module pseudocode.

	/*-------------------------------------------*
	 |	NORMAL STARTUP/EXECUTION
	 *-------------------------------------------*/
export make_c/0.
make_c 
	:- 
	make_c( 'pseudoc.ode',  'intrv_pr.h', 'intrv.c').

export make_c/3.
make_c( SrcFile, OutFileH, OutFileC)
	:-
	open(SrcFile,read,InS,[]),
	read_pseudo(InS, IntervalOps, PseudoCode1),
	close(InS),
	adjust_names(PseudoCode1, PseudoCode),
	bagof(SOP, sop(SOP), SOPs),

	open(OutFileH, write, OutHS, []), 
	open(OutFileC, write, OutCS, []),
	headers(OutHS, OutFileH, SrcFile),
	headers(OutCS, OutFileC, SrcFile),
	printf(OutCS, '\n#include "intrv.h"\n', []),
	printf(OutCS, '\n#if defined(INTCONSTR)\n\n',[]),
	printf(OutCS, '\n\nextern void extract_bds PARAMS((PWord *, fp *, fp *));\n\n',[]),
	
	compile_s_primitives( SOPs, OutHS, OutCS),
	compile_primitives( IntervalOps, PseudoCode, OutHS, OutCS),
	!,
	close(OutHS), 
	printf(OutCS, '\n#endif /* defined(INTCONSTR) */\n\n',[]),
	close(OutCS).

	/*------------------------------------------------*
	 | Read input file, and break items into groups
	 | of special facts, standard control facts, and
	 | standard pseudocode terms; assert special control
	 | facts and special (macro) code.
	 *------------------------------------------------*/
read_pseudo(Stream, IntervalOps, PseudoCode)
	:-
	read_term(Stream, Next_Term, [ vars_and_names(Vars,Names) ]),
	disp_rd_pseudo(Next_Term, Vars, Names, IntervalOps, PseudoCode, Stream).

disp_rd_pseudo(end_of_file, _, _, [], [], Stream)
	:-!.

disp_rd_pseudo(special_interval_operation(Op), _, _, IntervalOps, PseudoCode, Stream)
	:-!,
	assert(sop(Op)),
	functor(Op, FOp,_),
	assert(special_interval_operation(FOp)),
	read_pseudo(Stream, IntervalOps, PseudoCode).

disp_rd_pseudo(interval_operation(Op), _, _, [Op | IntervalOps], PseudoCode, Stream)
	:-!,
	read_pseudo(Stream, IntervalOps, PseudoCode).

disp_rd_pseudo(Term, Vars, Names, IntervalOps, PseudoCode, Stream)
	:-
	Term = (Head :- Tail),
	functor(Head, Op, _),
	special_interval_operation(Op),
	!,
		%% Remove vars occurring in Head from
		%% list of of vars (and corresp values)
		%% for clause as a whole:
	Head =.. [P | Args],
	remove_head_vars(Args, Vars, Names, SVars, SNames),
		%% Instantiate remaing vars (i.e., label
		%% names) to their atomic (quoted) names:
	SVars=SNames,
	assert(Term),
	read_pseudo(Stream, IntervalOps, PseudoCode).

disp_rd_pseudo(Term, Vars, Names, 
		IntervalOps, [Term | PseudoCode], Stream)
	:-
	Vars = Names,
	!,
	read_pseudo(Stream, IntervalOps, PseudoCode).

remove_head_vars([], Vars, Names, Vars, Names).
remove_head_vars([Arg | Args], Vars, Names, SVars, SNames)
	:-	
	nonvar(Arg),
	!,
	remove_head_vars(Args, Vars, Names, SVars, SNames).
remove_head_vars([Arg | Args], Vars, Names, SVars, SNames)
	:-	
	rem_v(Vars, Names, Arg, IVars, INames),
	remove_head_vars(Args, IVars, INames, SVars, SNames).

rem_v([], [], Arg, [], []).
rem_v([V | IVars], [N | INames], Arg, IVars, INames)
	:-
	V == Arg,
	!.
rem_v([V | Vars], [N | Names], Arg, [V | IVars], [N | INames])
	:-
	rem_v(Vars, Names, Arg, IVars, INames).

cia_version( 'May 3 95 Version of make_C_interval_engine').

	%% Headers for the output files:
headers(OutS, OutFile, SrcFile)
	:-
    cia_version(Version), 
	Addl = '\t\t  -- by genC_ie(make_C)\n\n\
    Interval Primitives: %t\n',
	gen_file_header(OutS, SrcFile, OutFile,printf(OutS,Addl,[Version]) ).

adjust_names([], []).

adjust_names([(Head :- Body) | InitPseudoCode], [Name - (Head :- Body) | PseudoCode])
	:-
	functor(Head, Name, _),
	adjust_names(InitPseudoCode, PseudoCode).

adjust_names([Head | InitPseudoCode], [Name-Head | PseudoCode])
	:-
	functor(Head, Name, _),
	adjust_names(InitPseudoCode, PseudoCode).

pc_error(Message, Args)
	:-
	als_advise(Message, Args).


%-----------------------------------------------------
%           generic compiler implementation
%-----------------------------------------------------   

compile_primitives( [], _, _, _).
compile_primitives( [Op | IntervalOps], PseudoCode, OutHS, OutCS)
	:-
	compile_primitive( Op , PseudoCode, OutHS, OutCS),
	compile_primitives( IntervalOps, PseudoCode, OutHS, OutCS).

compile_s_primitives( [], _, _).
compile_s_primitives( [SOP | SOPs], OutHS, OutCS)
	:-
	compile_s_prim( SOP, OutHS, OutCS),
	compile_s_primitives( SOPs, OutHS, OutCS).
/*----------------------------------------------------*
 |	compile_primitive/3
 |	compile_primitive( Name, PseudoCode, Stream)
 |	compile_primitive( +, +, +)
 |
 |  compiling  a primitive
 *----------------------------------------------------*/

compile_primitive( Name, PseudoCode, OutHS, OutCS)
	:-
	catenate('i_', Name, IName),
	cmt_for(OutCS, IName),
	dmember(Name - Entry, PseudoCode),
	(Entry = (Head :- Body) -> 
		true 
		; 
		Body = true, 
		Head = Entry),
	Head =.. [Name | Args],
	label_pfx(Name, LabPfx), 
	printf(OutCS, 'int  %t  PARAMS( (void) );\n',[IName]),
	procheader( IName, Args, OutCS),
	compile_body( Body, standard, LabPfx, OutCS ),
	!,
	procend( IName, 4, OutCS),
	ansi_proto(IName, OutHS),
		%% notify user we finished a primitive:
	printf(user_output, '-%t\n', [Name]),
	flush_output(user_output).

compile_primitive( Name, _, _) 
	:- 
	pc_error('IOp %t failed to compile\n', [Name]).

compile_s_prim( SOP, OutHS, OutCS)
	:-
	printf('-%t',[SOP]),
	SOP =.. [Name | Args],
	catenate('i_', Name, IName),
	cmt_for(OutCS, IName),
	clause(SOP, Body),
	printf(OutCS, 'int  %t  PARAMS( (void) );\n',[IName]),
	procheader2( IName, Args, OutCS),
	label_pfx(Name, LabPfx), 
	printf(OutCS,'\t\t/* Macro version of call: %t  */\n',[SOP]),
	compile_body( Body, special, LabPfx, OutCS ),
	!,
	procend( IName, 3, OutCS),
	ansi_proto(IName, OutHS),
		%% notify user we finished a primitive:
	printf(user_output, '-%t\n', [Name]),
	flush_output(user_output).

compile_s_prim( SOP, OutHS, OutCS)
	:-
	pc_error('IOp %t failed to compile\n', [SOP]).

/*----------------------------------------------------*
 |	compile_body/2
 |	compile_body( Goals, Stream)
 |	compile_body( +, +)
 |
 |  compiling the body of a primitive
 *----------------------------------------------------*/
compile_body( true , Flag, LabPfx, S) :-!.

compile_body( ( (goto Lab), (L:Op), Ops ), Flag, LabPfx, S)
	:-
	Lab == L, 							% suppress unneeded branch
	!,
	compile_body( ( (L:Op), Ops ), Flag, LabPfx, S).

compile_body( ( (Lab:Op), Ops),  Flag, LabPfx, S)
	:-
	map_label(Lab,LabPfx,Lab1), 				% emit local label        
	!,
	printf(S, '   %t:\n', [Lab1]),
	compile_body( ( Op,Ops ), Flag, LabPfx, S).

compile_body( ( (X ->L), (Lab:Op), Ops ), Flag, LabPfx, S)
	:-                  				% branch constructs
	map( (X -> L), Cmd, LabPfx,Lab), 
	out(Cmd,LabPfx,S),
	!,
	compile_body( ( (Lab:Op), Ops ), Flag, LabPfx, S).

	%% General recusion clause:
compile_body( (Op, Ops ), Flag, LabPfx, S)
	:-                                	% other operations
	compile_op(Op, Flag, LabPfx,S),
	!,
	compile_body( Ops, Flag, LabPfx, S).

compile_body( Op, Flag, LabPfx, S)
	:- 
	Op \= true,
	Op \= (_,true),
	compile_body( (Op,true), Flag, LabPfx, S),
	!.

compile_body( X, Flag, LabPfx, S)
	:- 
	pc_error('Failed to compile body: %t\n', [X]),
	fail.


compile_op( ( (goto Lab), Ops ), Flag, LabPfx,S)
	:-
	map( goto Lab, LabPfx,Cmd ), 
	out(Cmd,LabPfx,S).

compile_op( success, _, LabPfx,S)
	:-!.

compile_op( Op, Flag, LabPfx,S)
	:-                                	% other operations
	map( Op, LabPfx,Cmd ), 
	out(Cmd,LabPfx,S).

label_pfx(Name, LabPfx)
	:-
	atom_chars(Name, [K | _]),			% note that K is an atom
	abolish(cur_prim_key, 1),
	catenate(K,'_',LabPfx).

procheader(Name, Stream)
	:-
	printf(Stream, '\n\ninterval_op(%t)\n', [Name]).     

procheader(Name, Args, Stream)
	:-
	printf(Stream, '\nint\n%t()\n', [Name]),
	printf(Stream, '{\n', []),

	args_PWord_dec(3, [z,x,y], Stream, InPWordArgs, InternalArgs),
	args_types_dec(3, [z,x,y], Stream, InTypeArgs),
	real_int_end_type(RIET),
	aux_args(Name,AuxArgs),
	append(AuxArgs, InternalArgs, I2DeclareArgs),
	internal_args_dec(I2DeclareArgs, Stream, RIET),
	printf(Stream, '\tint status = 0;\n', []),
	printf(Stream, '\tPWord stat_var;\n', []),
	printf(Stream, '\tint stat_var_t;\n', []),
	nl(Stream),
	get_args(InPWordArgs, InTypeArgs, 1, Stream),
	nl(Stream),
	setup_args(InPWordArgs, InTypeArgs, InternalArgs, RIET, Stream).

real_int_end_type(double).

procheader2(Name, Args, Stream)
	:-
	printf(Stream, '\nint\n%t()\n', [Name]),
	printf(Stream, '{\n', []),

	args_PWord_dec(2, [z,x], Stream, InPWordArgs, InternalArgs),
	args_types_dec(2, [z,x], Stream, InTypeArgs),
	real_int_end_type(RIET),
	aux_args(Name,AuxArgs),
	append(AuxArgs, InternalArgs, I2DeclareArgs),
	internal_args_dec(I2DeclareArgs, Stream, RIET),
	printf(Stream, '\tint status = 0;\n', []),
	printf(Stream, '\tPWord stat_var;\n', []),
	printf(Stream, '\tint stat_var_t;\n', []),
	nl(Stream),
	get_args(InPWordArgs, InTypeArgs, 1, Stream),
	nl(Stream),
	setup_args(InPWordArgs, InTypeArgs, InternalArgs, RIET, Stream).

special_procheader( IName, Args, OutCS)
	:-
	real_int_end_type(RIET),
	printf(OutCS, '\nint\n%t(zl,zh, xl, xh, status)\n', [IName]),
	printf(OutCS, '\t%t zl,zh, xl, xh;\n', [RIET]),
	printf(OutCS, '\tint status;\n', []),
	printf(OutCS, '{\n', []).

aux_args(i_begin_tog,[]) :-!.
aux_args(i_finish_tog,[]) :-!.
aux_args(i_equal,[]) :-!.
aux_args(i_unequal,[]) :-!.
aux_args(i_greatereq,[]) :-!.
aux_args(i_higher,[]) :-!.
aux_args(i_j_less,[]) :-!.
aux_args(i_k_equal,[]) :-!.
aux_args(i_narrower,[]) :-!.
aux_args(i_or,[]) :-!.

aux_args(i_cos,[vl, vh, ul, uh]) :-!.
aux_args(i_sin,[vl, vh, ul, uh]) :-!.

aux_args(i_tan,[ul, uh]) :-!.
aux_args(i_rootsquare,[ul, uh]) :-!.
aux_args(i_qpow_even,[vl, vh, uh]) :-!.
aux_args(i_xp,[ul, uh]) :-!.
aux_args(i_inf,[vl]).
aux_args(i_lub,[vh]).
aux_args(i_vabs,[ul]) :-!.
aux_args(i_wrap,[vl, vh, ul]) :-!.

aux_args(_,[vl, vh]).

procend( Name, N, S)
	:-
	(N = 3 ->
		consis_and_change2(S)
		;
		consis_and_change(S)
	),
	printf(S, '\tw_get_An(&stat_var, &stat_var_t, %d);\n', [N]),
	printf(S, '\tif(w_unify(stat_var, stat_var_t, status, WTP_INTEGER))\n',[]),
	printf(S, '\t\tSUCCEED; \n\telse \n\t\tFAIL;\n',[]),
	printf(S, '\n}\n\n', []).     


args_PWord_dec(M, VNList, Stream, InPWordArgs, InternalArgs)
	:-
	printf(Stream, '\tPWord ',[]),
	args_PWord_dec0(M, VNList, Stream, InPWordArgs, InternalArgs).

args_PWord_dec0(1, [VName | _], Stream, [VName], [VL, VH ] )
	:-!,
	catenate(VName, l, VL),
	catenate(VName, h, VH),
	printf(Stream, '%t;\n', [VName]).

args_PWord_dec0(N, [VName | VNList], Stream, 
				[VName | InPWordArgs], [VL, VH | InternalArgs])
	:-
	catenate(VName, l, VL),
	catenate(VName, h, VH),
	printf(Stream, '%t, ', [VName]),
	M is N-1,
	args_PWord_dec0(M, VNList, Stream, InPWordArgs, InternalArgs).

args_types_dec(M, VNList, Stream, InTypeArgs)
	:-
	printf(Stream, '\tint ',[]),
	args_types_dec0(M, VNList, Stream, InTypeArgs).

args_types_dec0(1, [VName | _], Stream, [VT ])
	:-!,
	catenate(VName, '_t', VT),
	printf(Stream, '%t;\n', [VT]).

args_types_dec0(N, [VName | VNList], Stream, [VT | InTypeArgs])
	:-
	catenate(VName, '_t', VT),
	printf(Stream, '%t, ',[VT]),
	M is N-1,
	args_types_dec0(M, VNList, Stream, InTypeArgs).

internal_args_dec(InternalArgs, Stream, RIET)
	:-
	printf(Stream, '\t%t ', [RIET]),
	internal_args_dec0(InternalArgs, Stream).

internal_args_dec0([A], Stream)
	:-!,
	printf(Stream, '%t;\n',[A]).

internal_args_dec0([A | InternalArgs], Stream)
	:-
	printf(Stream, '%t, ', [A]),
	internal_args_dec0(InternalArgs, Stream).

get_args([], [], _, _).

get_args([PWV | InPWordArgs], [TPV | InTypeArgs], N, Stream)
	:-
	printf(Stream, '\tw_get_An(&%t, &%t, %t);\n', [PWV, TPV, N]),
	M is N+1,
	get_args(InPWordArgs, InTypeArgs, M, Stream).


setup_args([], [], _, _, _).
setup_args([PWV | InPWordArgs], [TPV | InTypeArgs], 
			[VL, VH | RestVNs], RIET, Stream)
	:-
	printf(Stream, '\tif(%t == WTP_INTEGER)\n', [TPV]),
	printf(Stream, '\t\t{\t%t = (%t)%t;\n\t\t\t%t = (%t)%t; }\n', 
				[VL, RIET, PWV, VH, RIET, PWV]),
	printf(Stream, '\telse if(%t == WTP_UNBOUND)\n', [TPV]),
	printf(Stream, '\t  extract_bds(%t, &%t, &%t);\n',[PWV, VL, VH]),
	printf(Stream, '\telse  FAIL;\n\n', []),
	setup_args(InPWordArgs, InTypeArgs, RestVNs, RIET, Stream).

ansi_proto(Name, Stream)
	:-
	printf(Stream, 'extern int\t%t\t\tPARAMS( (void) );\n',[Name]).

cmt_for(OutCS, IName)
	:-
	printf(OutCS, '     /*----------------*\n', []),
	printf(OutCS, '      |   %t \n', [IName]),
	printf(OutCS, '      *----------------*/\n\n', []).

consis_and_change(S)
	:-
	printf(S, '\tif ( zl > zh ) FAIL;\n',[]),
	printf(S, '\telse if ( xl > xh ) FAIL;\n',[]),
	printf(S, '\telse if ( yl > yh ) FAIL;\n',[]),

	printf(S, '\tif (zlchange & status) change_bound(z, &zl, LOWER_BOUND);\n',[]),
	printf(S, '\tif (zhchange & status) change_bound(z, &zh, UPPER_BOUND);\n',[]),
	printf(S, '\tif (xlchange & status) change_bound(x, &xl, LOWER_BOUND);\n',[]),
	printf(S, '\tif (xhchange & status) change_bound(x, &xh, UPPER_BOUND);\n',[]),
	printf(S, '\tif (ylchange & status) change_bound(y, &yl, LOWER_BOUND);\n',[]),
	printf(S, '\tif (yhchange & status) change_bound(y, &yh, UPPER_BOUND);\n\n',[]).

consis_and_change2(S)
	:-
	printf(S, '\tif ( zl > zh ) FAIL;\n',[]),
	printf(S, '\telse if ( xl > xh ) FAIL;\n',[]),

	printf(S, '\tif (zlchange & status) change_bound(z, &zl, LOWER_BOUND);\n',[]),
	printf(S, '\tif (zhchange & status) change_bound(z, &zh, UPPER_BOUND);\n',[]),
	printf(S, '\tif (xlchange & status) change_bound(x, &xl, LOWER_BOUND);\n',[]),
	printf(S, '\tif (xhchange & status) change_bound(x, &xh, UPPER_BOUND);\n\n',[]).

/*--------------------------------------------------------------*
 |	out/2
 |	out( F,S)
 |	out( +,+)
 |
 |	output formatting 
 *--------------------------------------------------------------*/
out( true, LabPfx,S) :-!.

out( (F, Fs), LabPfx,S)
	:- 
	!,
	out(F, LabPfx,S),
	out(Fs, LabPfx,S).

out( (Lab:F), LabPfx,S )
	:- 
	map_label(Lab,LabPfx,Lab1), 
	printf(S,'\n   %t:',[Lab1]),
	!, 
	out(F, LabPfx,S).

out( F, LabPfx,S)
	:- 
	format(F,LabPfx,S),
	!.

out( F,LabPfx,S)
	:- 
	printf(user_output,'Cannot format %t\n',[F]), 
	fail.

format( true, LabPfx,S)
	:-!, 
	printf(S,';\n').

format( if( Exp, (goto Lab) ), LabPfx, S)
	:-
	special_interval_operation(Lab),
	!,
	FcnCall =.. [Lab, zl,zh, xl, xh],
	printf(S,'\tif (%t) {\n', [Exp]),
	printf(S,'\t\t/* Macro version of call: %t  */\n',[FcnCall]),
	clause(FcnCall, CallBody),
	
	label_pfx(Lab, LabPfx2), 
	catenate(LabPfx2,LabPfx,LabLabPfx),

	compile_body( CallBody, special, LabLabPfx, S ),
	printf(S,'\t}\n', []).

format( if( Exp, (goto L) ), LabPfx, S)
	:-!, 
	fmt_goto( L, LabPfx, G), 
	printf(S,'\tif (%t)  %t;\n', [Exp,G]).

format( if( Exp, Then, Else ), LabPfx, S)
	:-!, 
	format( if(Exp, Then),LabPfx, S),
	format( Else, LabPfx, S). 

format( X=Y,    LabPfx, S)
	:-!, 
	printf(S,'\t%t = %t;\n',[X,Y]).

format( goto L, LabPfx, S)
	:-!, 
	printf(S,'\tgoto %t;\n', [L]).

format( F ,     LabPfx, S)
	:-  
	printf(S,'\t%t;\n',[F]).

fmt_goto( iaerror, LabPfx, 'iaerror()')
	:-!.

fmt_goto(  L, LabPfx, Str)
	:- 
	sprintf( atom(Str),'goto %t',[L]).

map_cons(C,C)
	:- 
	number(C),
	!.

map_cons(pi,'pi()')
	:-!.

map_cons(2**0,1)
	:-!.

map_cons(2**1,2).

map_reg(R,R)
	:- 
	valid_reg(R).

map_reg(x,xl,xh, 'xflipped()')
	:-!.
map_reg(y,yl,yh, 'yflipped()')
	:-!.
map_reg(z,zl,zh, 'zflipped()')
	:-!.

valid_reg(xl).
valid_reg(xh).
valid_reg(yl).
valid_reg(yh).
valid_reg(zl).
valid_reg(zh).
valid_reg(vl).
valid_reg(vh).
valid_reg(ul).
valid_reg(uh).
valid_reg(temp).

exchange_reg(temp).

change( xl, 'xlchng()') :-!.
change( xh, 'xhchng()') :-!.
change( yl, 'ylchng()') :-!.
change( yh, 'yhchng()') :-!.
change( zl, 'zlchng()') :-!.
change( zh, 'zhchng()') :-!.
change( _,  true).

%-------------------------------------------------------------
% map varname to local label
%  ( all labels are local except for entry points )
%-------------------------------------------------------------

map_label( L, LabPfx,Label )
	:- 
	atom(L),
	sub_atom(L,1,1,C1A),
	name(C1A, C1C),
	C1C >= 0'A, C1C =< 0'Z,
	!,
%	cur_prim_key(K),
	sprintf(atom(Label),'%t%t', [LabPfx,L]).

map_label( L, LabPfx,L)
	:- 
	atom(L).

%-------------------------------------------------------
%  map arithmetic operations
%  map_exp( Expr, Code, Targetreg)
%-------------------------------------------------------

map_exp( lb(E), lowerbd(Expr,D), D)
	:- 
	map_expression(E, Expr),
	!.  

map_exp( ub(E), upperbd(Expr,D), D)
	:- 
	map_expression(E, Expr),
	!.  

map_exp( lb(E), lowerbd2(Expr,D), D)
	:- !, 
	map_trans_expression(E, Expr).  

map_exp( ub(E), upperbd2(Expr,D), D)
	:- !, 
	map_trans_expression(E, Expr).  

%map_exp( F(E1,E2), D=Code, D)
map_exp( InExpr, D=Code, D)
	:-
	functor(InExpr, _, 2),
	!,
	arity2(InExpr, Code).  

map_exp( next(X), ( D=X1,next(D) ), D)
	:-!, 
	map_reg(X,X1).

map_exp( prev(X), ( D=X1,prev(D) ), D)
	:-!, 
	map_reg(X,X1).

%map_exp( F(E), D=Code, D)
map_exp( InExpr, D=Code, D)
	:- 
	functor(InExpr,_,1),
	!,
%	arity1(F(E),Code).  
	arity1(InExpr, Code).  

map_exp( X, D=X1, D)
	:-!, 
	map_reg(X,X1).

map_expression( E, E1)
	:- 
	map_reg(E,E1),
	!.

map_expression( E, E1)
	:- 
	arity2(E,E1),
	!.

map_expression( E, E1)
	:- 
	arity1(E,E1),
	!.

map_trans_expression( E, E1)
	:- 
	map_reg(E,E1),
	!.

%map_trans_expression( F(X),  F1(X1) )
map_trans_expression( InExpr,  OutExpr )
	:- 
	InExpr =.. [F, X],
	map_tfunc(F,F1), 
	map_reg(X,X1),
	OutExpr =.. [F1, X1].

%arity2( F(X,Y),  F1(X1,Y1) )
arity2( InExpr,  OutExpr )
	:- 
	InExpr =.. [F, X, Y],
	map_func2(F,F1), 
	map_reg(X,X1), 
	map_reg(Y,Y1),
	OutExpr =.. [F1, X1, Y1].

map_func2( + , + ).
map_func2( * , * ).
map_func2( - , - ).
map_func2( / , / ).

%arity1( F(X),  F1(X1) )
arity1( InExpr,  OutExpr )
	:- 
	InExpr =.. [F, X],
	map_func(F,F1), 
	map_reg(X,X1),
	OutExpr =.. [F1, X1].

map_func( -,   -  ).
map_func( floor,floor).
map_func( ceiling,ceiling).
map_func( sqrt, sqrt ).
map_func( round, round).

%-------------------------------------------------------
%   approximate relations 
%-------------------------------------------------------

map_tfunc( tan, tan).
map_tfunc( atan,atan).
map_tfunc( sin, sin).
map_tfunc( asin,asin).
map_tfunc( cos, cos).
map_tfunc( acos,acos).
map_tfunc( exp, exp).
map_tfunc( ln, ln).

%-------------------------------------------------------
% map operators
%-------------------------------------------------------

%map(  A <- F(X..),      [Code,Upd..])
map( A <- Expr, LabPfx, ( Code, Upd ) )
	:- 
	map_reg(A,A1),
	map_exp(Expr, Code, A1),
	change(A,Upd),
	!.

%map(  A <- B, [A1=B1,  Upd..])
map( A <- B, LabPfx, ( A1=B1, Upd ) )
	:- 
	map_reg(A,A1), 
	map_reg(B,B1),
	!, 
	change(A,Upd).

map( A <- C, LabPfx, ( A1=C1, Upd ) )
	:- 
	map_reg(A,A1), 
	map_cons(C,C1), 
	change(A,Upd),
	!.

map( (goto Lab), LabPfx, (goto Lab1) )
	:- 
	map_label(Lab,LabPfx,Lab1).

/*
map( success,    'success()' ).
map( fail,       'fail()'    ).
%map( success,    'SUCCEED' ).
*/

map( fail,       LabPfx, 'FAIL'    ).
map( persistent, LabPfx, 'deact()'   ).

map( (A <-> B),  LabPfx, ( X=A1, A1= B1, B1=X ) )
	:- 
	map_reg(A,A1), 
	map_reg(B,B1), 
	exchange_reg(X).

map( flip(A),  LabPfx, swap(A) )
	:-
	A=x ; A=y ; A=z,
	!.

map( schedy,   LabPfx, 'schedy()' ).

%-------------------------------------------------------
% map compare and branch instructions
%-------------------------------------------------------

map( (C -> [L,E,R]), Code, LabPfx, Lab )
	:- 
	L == Lab,
	!,
	map_cmp(C,A1,B1),
	map_label(L,LabPfx,L1), 
	map_label(R,LabPfx,R1), 
	map_label(E,LabPfx,E1),
	branches( [fbgt(R1),fbeq(E1),fblt(L1)], Branches),
	ifstatement( Branches, A1,B1, Code, L1).

map( (C -> [L,E,R]), Code, LabPfx, Lab )
	:- 
	R == Lab,
	!,
	map_cmp(C,A1,B1),
	map_label(L,LabPfx,L1), 
	map_label(R,LabPfx,R1), 
	map_label(E,LabPfx,E1),
	branches( [fblt(L1),fbeq(E1),fbgt(R1)], Branches),
	ifstatement( Branches, A1,B1, Code, R1).

map( (C -> [L,E,R]), Code, LabPfx, Lab )
	:- 
	E == Lab,
	!,
	map_cmp(C,A1,B1),
	map_label(L,LabPfx,L1), 
	map_label(R,LabPfx,R1), 
	map_label(E,LabPfx,E1),
	branches( [fbgt(R1),fblt(L1),fbeq(E1)], Branches),
	ifstatement( Branches, A1,B1, Code, E1).

map( (C -> [L,E,R]), Code, LabPfx, _ )
	:- 
	map_cmp(C,A1,B1),
	map_label(L,LabPfx,L1), 
	map_label(R,LabPfx,R1),
	map_label(E,LabPfx,E1),
	branches( [fblt(L1),fbgt(R1),fbeq(E1)], Branches),
	ifstatement( Branches, A1,B1, Code, []).

map_cmp( ( A ? B ), A1, B1)
	:- 
	map_reg(A,A1),
	( map_reg(B,B1) ; map_cons(B,B1) ).

ifstatement( [],_,_, [],_).

%ifstatement( [B1(L1),B2(L2)], X,Y,  if( Op(X,Y), goto L1 ), L2)
ifstatement( [Br1, Br2], X,Y,  if( IfExpr, goto L1 ), L2)
	:-
	Br1 =.. [B1, L1],
	Br2 =.. [_, L2],
	!,
	comparison(B1,Op),
	IfExpr =.. [Op, X, Y].

%ifstatement( [B1(L1),B2(L2)],X,Y,  if( Op(X,Y), goto L1, goto L2),_)
ifstatement( [Br1, Br2], X,Y,  if( IfExpr, (goto L1), (goto L2) ), _)
	:-
	Br1 =.. [B1, L1],
	Br2 =.. [_, L2],
	!,
	comparison(B1,Op),
	IfExpr =.. [Op, X, Y].

%ifstatement( [B(L),Bs..], X,Y, if( Op( X, Y ), goto L, Rest ),Lab )
ifstatement( [Br | Bs], X, Y, if( IfExpr, goto L, Rest ), Lab )
	:- 
	Br =.. [B, L],
	comparison(B,Op),
	!,
	IfExpr =.. [Op, X, Y],
	ifstatement( Bs, X,Y, Rest, Lab).

%-------------------------------------------------------
% auxiliary functions for special casing branches
%-------------------------------------------------------

%branches( [ B1(L), B2(L) ],[B3(L)] )
branches( [ Br1, Br2 ], [Br3] )
	:-
	Br1 =.. [B1, L],
	Br2 =.. [B2, L],
	!, 
	merge(B1,B2,B3),
	Br3 =.. [B3, L].

%branches( [ B1(L), B2(L), B3(L)], _)
branches( [ Br1, Br2, Br3], _ )
	:- 
	Br1 =.. [B1, L],
	Br2 =.. [B2, L],
	Br3 =.. [B3, L],
	printf(user_output,' branch error \n'), 
%	failexit(compile).
	fail.

%branches( [ B1(L), B2(L), B3(L3)], [B12(L),B3(L3)])
branches( [ Br1, Br2, Br3], [Br12, Br3] )
	:- 
	Br1 =.. [B1, L],
	Br2 =.. [B2, L],
	Br3 =.. [B3, L3],
	!,
	merge(B1,B2,B12),
	Br12 =.. [B12, L].

%branches( [ B1(L), B2(L2), B3(L)], [B13(L),B2(L2)])
branches( [ Br1, Br2, Br3], [Br13, Br2] )
	:-
	Br1 =.. [B1, L],
	Br2 =.. [B2, L2],
	Br3 =.. [B3, L],
	!, 
	merge(B1,B3,B13),
	Br13 =.. [B13, L].
 
%branches( [ B1(L1), B2(L), B3(L)], [B1(L1),B23(L)])
branches( [ Br1, Br2, Br3], [Br1, Br23] )
	:-
	Br1 =.. [B1, L1],
	Br2 =.. [B2, L],
	Br3 =.. [B3, L],
	!, 
	merge(B2,B3,B23),
	Br23 =.. [B23, L].

branches( X, X ). 

merge( fblt, fbgt, fbne) :-!.
merge( fblt, fbeq, fble) :-!.
merge( fbgt, fbeq, fbge) :-!.
merge( B1, B2, B3)
	:- 
	merge( B2, B1, B3).

comparison( fblt, 'LT' ).
comparison( fble, 'LE').
comparison( fbgt, 'GT' ).
comparison( fbge, 'GE').
comparison( fbeq, 'EQ').
comparison( fbne, 'NE').

endmod.
