/*===============================================================*
 |		abs2make.pro
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |		Transformer from abstract makefiles to
 |		OS+Compiler - specific Makefile templates
 |
 | Author:	Ken Bowen
 | Date:	06/06/95
 *===============================================================*/
:-op(690, yfx, qual).
:-op(400, xfy, depends_on).
:-op(400, xfy, appnd).
:-op(120, fx, '&').
:-op(120, fx, '~').

module abs2make.

:- dynamic(terminal_output/0).

%%---------Tests------------------
t :-
	abs2make('generic.abs','generic.unix',unix).

tj :-
	abs2make('generic.abs','generic.djgpp',djgpp).

tw :-
	abs2make('generic.abs','generic.msw31',msw31).

tm :-
	abs2make('generic.abs','generic.mac',mac).

bpu :-
	abs2make('bld-port.abs','bld-port.in',unix).
%%-------------------------------

/*-----------------------------------------------
 |	Command-line switches:
 |
 |	-src: The source abstract file to process:
 |			Arg format:  <file>.abs
 |
 |	-tgt: The target output file:
 |			Arg format:  <file>.<ext>
 |	
 |	-ctx: The context to use:
 |			unix, djgpp, os2, mac, msw31, ...
 |
 |	-termout: Ignore -tgt and output to terminal
 *----------------------------------------------*/
export abs2make_cl/0.

abs2make_cl
	:-
	get_cmdline_vals(SwitchVals),
	get_the_args(SwitchVals, SrcFile,TgtFile,ContextName),
	!,
	abs2make(SrcFile,TgtFile,ContextName).
abs2make_cl.

get_the_args(SwitchVals, SrcFile,TgtFile,ContextName)
	:-
	dmember(['-src', SrcFile], SwitchVals),
	dmember(['-tgt', TgtFile], SwitchVals),
	dmember(['-ctx', ContextName], SwitchVals),
	(dmember(['-termout'], SwitchVals) ->
		assert(terminal_output) ; true ),
	!,
	check_switches(SrcFile, TgtFile, ContextName).

check_switches(SrcFile, TgtFile, ContextName)
	:-
	(var(SrcFile) -> 
		Flag = fail,
		printf('Missing source file!\n', []) ; true),
	(var(TgtFile) -> 
		Flag = fail,
		printf('Missing target file!\n', []) ; true),
	(var(ContextName) -> 
		Flag = fail,
		printf('Missing context name!\n', []) ; true),
	!,
	(var(Flag) -> true ; fail).

export abs2make/3.

abs2make(SrcFile,TgtFile,ContextName)
	:-
	open(SrcFile, read, Src, []),
	read_terms(Src,SrcTerms),
	close(Src),
	dmember(schema(Scheme), SrcTerms),
	(terminal_output ->
		Out = user_output
		;
		open(TgtFile,write,Out,[])
	),
	context(ContextName, Context),
	abs_m(Scheme,SrcTerms,Context,Out),
	(terminal_output ->
		true ; close(Out) ),
	write(done),nl.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Main Loop/Recursion
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abs_m([], _, Context, _).

abs_m([ noop | Scheme], SrcTerms, Context, Out)
	:-!,
	abs_m(Scheme, SrcTerms, Context, Out).

abs_m([ Left=Right | Scheme], SrcTerms, Context, Out)
	:-!,
	abs_eval(Left, SrcTerms, Context, LeftVal),
	xabs_eval(Right, SrcTerms, Context, RightVal, Quals),
	printf(Out, '%t =  ', [LeftVal]),
	list_body(RightVal, Quals, Context, Out),
	abs_m(Scheme, SrcTerms, Context, Out).

abs_m([ (Left := Right) | Scheme], SrcTerms, Context, Out)
	:-!,
	abs_eval(Left, SrcTerms, Context, LeftVal),
	xabs_eval(Right, SrcTerms, Context, RightVal, Quals),
	printf(Out, '%t :=  ', [LeftVal]),
	list_body(RightVal, Quals, Context, Out),
	abs_m(Scheme, SrcTerms, Context, Out).

abs_m([ (Left:Right) | Scheme], SrcTerms, Context, Out)
	:-!,
	xabs_eval(Left, SrcTerms, Context, LeftVal, LeftQuals),
	xabs_eval(Right, SrcTerms, Context, RightVal, RightQuals),
	(dmember(context_name=mac, Context) ->
		DepSep = ' \xC4 '
		;
		DepSep = ':'
	),
	(RightVal = [_|_] -> RVL = [DepSep | RightVal] ; RVL = [DepSep, RightVal]),
	(LeftVal = [_|_] -> LVL = LeftVal ; LVL = [LeftVal]),
	append(LVL, RVL, LIST),
	list_body(LIST, [single_line], Context, Out),
	nl(Out), nl(Out),
	abs_m(Scheme, SrcTerms, Context, Out).

abs_m([ case(Cond,Actions) | Scheme], SrcTerms, Context, Out)
	:-!,
	abs_eval(Cond, SrcTerms, Context, CondVal),
	dmember( (CondVal:Action), Actions),
	(Action = [_|_] -> 
		append(Action, Scheme, AScheme) ; AScheme = [Action | Scheme] ),
	abs_m(AScheme, SrcTerms, Context, Out).



abs_m([ depends_list(EXT) | Scheme], SrcTerms, Context, Out)
	:-!,
	lookup( depends_list(EXTAA)=DependsTerm, SrcTerms, Context),
	not(not(EXT = EXTAA)),
	copy_term(DependsTerm, DependsTermCopy),
	DependsTermCopy = depends_list(EXT, List),
	append(List, Scheme, AScheme),
	abs_m(AScheme, SrcTerms, Context, Out).


abs_m([include_raw(Path) | Scheme], SrcTerms, Context, Out)
	:-
	exists_file(Path),
	!,
	open(Path, read, InS, []),
	copy_stream_nl(InS, Out, unix),
	close(InS),
	abs_m(Scheme, SrcTerms, Context, Out).

abs_m([include_raw(Path) | Scheme], SrcTerms, Context, Out)
	:-
	printf('Warning!: File %t does not exist!\n',[Path]),
	abs_m(Scheme, SrcTerms, Context, Out).


abs_m([shell(Entry) | Scheme], SrcTerms, Context, Out)
	:-
	(Entry = [E1 | Es] ->
		abs_m([shell(E1) , shell(Es)], SrcTerms, Context, Out)
		;
		(Entry = [] ->
			nl(Out)
			;
			abs_eval(Entry, SrcTerms, Context, EntryVal),
			printf(Out, '%t\n', [EntryVal])
		)
	),
	abs_m(Scheme, SrcTerms, Context, Out).

abs_m([Entry | Scheme], SrcTerms, Context, Out)
	:-
	schema_abs_eval(Entry, SrcTerms, Context, EntryVal),
	abs_w(EntryVal, SrcTerms, Context, Out),
	!,
	abs_m(Scheme, SrcTerms, Context, Out).

abs_m([Entry | Scheme], SrcTerms, Context, Out)
	:-
	printf('Error: %t\n',[Entry]),
	abs_m(Scheme, SrcTerms, Context, Out).

/*---------------------------------------------
 |	abs_w/4
 |	abs_w(EntryVal, SrcTerms, Context, Out)
 |	abs_w(+, +, +, +)
 *--------------------------------------------*/

	%%%%%%%%%%%%%%%%%%%%%
	%% Comments
	%%%%%%%%%%%%%%%%%%%%%

abs_w(cmnt(ID), SrcTerms, Context,Out)
	:-!,
	dmember(cmnt(ID) = CLSrc, SrcTerms),
	abs_eval(CLSrc, SrcTerms, Context, CL),
	printf(Out,'# \n',[]),
	cmnt_out(CL, Out),
	printf(Out,'# \n\n',[]).

cmnt_out([], _).
cmnt_out([CLine | CL], Out)
	:-
	printf(Out,'#\t%s\n',[CLine]),
	cmnt_out(CL, Out).

	%%%%%%%%%%%%%%%%%%%%%
	%% Implicit Rule
	%%%%%%%%%%%%%%%%%%%%%

abs_w(irule(ID,Tgt), SrcTerms, Context, Out)
	:-
	lookup( (irule(ID,Tgt) = irule(Src,Tgt,Action)), SrcTerms, Context),
	abs_eval(Src, SrcTerms, Context, RuleSrc),
	abs_eval(Tgt, SrcTerms, Context, RuleTgt),
	xabs_eval(Action, SrcTerms, Context, RuleAction, ActionQuals),
	(dmember(context_name=mac, Context),
		printf(Out, '.%t %c .%t\n', [RuleTgt,0xC4,RuleSrc])
		;
		printf(Out, '%%.%t: %%.%t\n', [RuleTgt,RuleSrc])
	),
	list_body(RuleAction, [rule_body | ActionQuals], Context, Out).

abs_w(irule(ID), SrcTerms, Context,Out)
	:-!,
	lookup( (irule(ID) = irule(Src,Tgt,Action)), SrcTerms, Context),
	abs_eval(Src, SrcTerms, Context, RuleSrc),
	abs_eval(Tgt, SrcTerms, Context, RuleTgt),
	xabs_eval(Action, SrcTerms, Context, RuleAction, ActionQuals),
	printf(Out, '%%.%t:%%.%t\n', [RuleTgt,RuleSrc]),
	list_body(RuleAction, [rule_body | ActionQuals], Context, Out).

	%%%%%%%%%%%%%%%%%%%%%
	%% Explicit Rule
	%%%%%%%%%%%%%%%%%%%%%
abs_w(rule(ID), SrcTerms, Context,Out)
	:-
	(lookup( (rule(ID) = rule(Tgt,Dep,Action)), SrcTerms, Context) ->
		true 
		;
		lookup( rule(ID,Dep,Action), SrcTerms, Context),
		Tgt = ID
	),
	abs_eval(Tgt, SrcTerms, Context, RuleTgt),
	xabs_eval(Dep, SrcTerms, Context, RuleDep, DepQuals),
	xabs_eval(Action, SrcTerms, Context, RuleAction, ActionQuals),
	(dmember(context_name=mac, Context) ->
		printf(Out, '%t \xC4 ', [RuleTgt])
		;
		printf(Out, '%t: ', [RuleTgt])
	),
	list_body(RuleDep, [one_final | DepQuals], Context, Out),
	list_body(RuleAction, [rule_body | ActionQuals], Context, Out).

	%%%%%%%%%%%%%%%%%%%%%
	%% Explicit pure dep
	%%%%%%%%%%%%%%%%%%%%%

abs_w(dep(ID), SrcTerms, Context, Out)
	:-!,
	(lookup( (ID depends_on Right), SrcTerms, Context) -> 
		LeftVal = ID
		;
		lookup( (dep(ID) = (Left depends_on Right)), SrcTerms, Context),
		abs_eval(Left, SrcTerms, Context, LeftVal)
		),
	xabs_eval(Right, SrcTerms, Context, RightVal,Quals),
	(dmember(context_name=mac, Context) ->
		printf(Out, '%t \xC4 ', [LeftVal])
		;
		printf(Out, '%t: ', [LeftVal])
	),
	list_body(RightVal, Quals, Context, Out).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Explicit = : both sides already evaluated
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
abs_w( (Exp1  = Expr2), SrcTerms, Context,Out)
	:-!,
	printf(Out, '%t =  ', [Expr1]),
	list_body(List, Quals, Context, Out).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% First Default: Implicit dependency: Expr depends_on List
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abs_w(Expr, SrcTerms, Context,Out)
	:-
	abs_w(dep(Expr), SrcTerms, Context,Out),
	!.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Second DEFAULT: Item = list
	%% -- possibly with qualifiers/modifiers
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
abs_w(Expr, SrcTerms, Context,Out)
	:-
	lookup( (Expr = ExprRight), SrcTerms, Context),
	xabs_eval(ExprRight, SrcTerms, Context, ExprRightVal, Quals),
	printf(Out, '%t =  ', [Expr]),
	list_body(ExprRightVal, Quals, Context, Out).

lookup( What, SrcTerms, Context)
	:-
	dmember(What, SrcTerms),
	!.
lookup( What, SrcTerms, Context)
	:-
	dmember(What, Context).

list_body(List, Quals, Context, Out)
	:-
	dmember(single_line, Quals),
	!,
	one_line_list(List, Quals, Context, Out).

list_body(List, Quals, Context, Out)
	:-
	prefix_suffix(Quals, Context, Prefix, Suffix, LastSuffix),
	determine_print_opts(Quals, Opts),
	list_out(List, Prefix, Suffix, LastSuffix, Out, Opts),
	(dmember(one_final, Quals) ->
		nl(Out) ;
		nl(Out),nl(Out)
	).

determine_print_opts(Quals, [line_length(500),line_end(false)])
	:-
	dmember(single_line, Quals),
	!.
determine_print_opts(Quals, []).

list_out([], _, _, _, _, _).
list_out([Item], Prefix, Suffix, LastSuffix, Out, Opts)
	:-!,
	printf(Out, '%t%t%t', [Prefix,Item,LastSuffix], Opts).
list_out([Item | List], Prefix, Suffix, LastSuffix, Out, Opts)
	:-!,
	printf(Out, '%t%t%t', [Prefix,Item,Suffix], Opts),
	list_out(List, Prefix, Suffix, LastSuffix, Out, Opts).
list_out(Item, Prefix, Suffix, LastSuffix, Out, Opts)
	:-
	printf(Out, '%t%t%t', [Prefix,Item,LastSuffix], Opts).

prefix_suffix(Quals, Context, '\t', ' \xB6\n', '\n')
	:-
	dmember(context_name=mac, Context),
	dmember(one_per_line, Quals),
	!.
prefix_suffix(Quals, _, '\t', ' \\\n', '\n')
	:-
	dmember(one_per_line, Quals),
	!.
prefix_suffix(Quals, _, '\t', '\n','\n')
	:-
	dmember(rule_body, Quals),
	!.
prefix_suffix(_, _, ' ', '','').

one_line_list(List, Quals, Context, Out)
	:-
	is_stream(Out, OutStream),
%	stream_wt_line_length(OutStream, LineLength),
LineLength=1000,
	oll(List, 0, Quals, LineLength,OutStream).

oll([], Used, Quals, LL, Out).
oll([Item | List], Used, Quals, LL, Out)
	:-
	item_length(Item, ItemAtom, ItemLen),
	NewUsed is Used + ItemLen + 1,
	(NewUsed >= LL ->
		put_byte(Out, 0'\\),
		nl(Out),
		put_byte(Out,0'\t),
		put_atom(Out, ItemAtom), 
		put_byte(Out,0' ),
		StartLen is ItemLen + 9,
		oll(List, StartLen, Quals, LL, Out)
		;
		put_atom(Out, ItemAtom),
		put_byte(Out,0' ),
		oll(List, NewUsed, Quals, LL, Out)
	).
	
item_length(Item, ItemAtom, ItemLen)
	:-
	sprintf(atom(ItemAtom), '%t', [Item]),
	atom_length(ItemAtom, ItemLen).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Expression evaluation
	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%

schema_abs_eval(Entry, SrcTerms, Context, Entry)
	:-
	atomic(Entry),
	!.

schema_abs_eval(Entry, SrcTerms, Context, EntryVal)
	:-
	abs_eval(Entry, SrcTerms, Context, EntryVal).

xabs_eval( (A qual InQuals), SrcTerms, Context, AVal, Quals)
	:-
	(InQuals = [_|_] -> Quals = InQuals ; Quals = [InQuals]),
	abs_eval(A, SrcTerms, Context, AVal),
	!.

xabs_eval(A, SrcTerms, Context, AVal, [])
	:-!,
	abs_eval(A, SrcTerms, Context, AVal).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Expression evaluation
	%%%%%%%%%%%%%%%%%%%%%%%%%%

abs_eval(cmnt(A), SrcTerms, Context, cmnt(AVal))
	:-!,
	abs_eval(A, SrcTerms, Context, AVal).

abs_eval(irule(A,B), SrcTerms, Context, irule(AVal,BVal))
	:-!,
	abs_eval(A, SrcTerms, Context, AVal),
	abs_eval(B, SrcTerms, Context, BVal).

abs_eval(irule(A), SrcTerms, Context, irule(AVal))
	:-!,
	abs_eval(A, SrcTerms, Context, AVal).

abs_eval(rule(A), SrcTerms, Context, rule(AVal))
	:-!,
	abs_eval(A, SrcTerms, Context, AVal).

abs_eval(dep(A), SrcTerms, Context, dep(AVal))
	:-!,
	abs_eval(A, SrcTerms, Context, AVal).

abs_eval((A = B), SrcTerms, Context, (AVal = BVal))
	:-!,
	abs_eval(A, SrcTerms, Context, AVal),
	abs_eval(B, SrcTerms, Context, BVal).

abs_eval(~(Xpr), SrcTerms, Context, Xpr)
	:-!.

abs_eval(&(Xpr), SrcTerms, Context, Val)
	:-!,
	(dmember(&(Xpr)=InitVal, SrcTerms) ->
		abs_eval(InitVal, SrcTerms, Context, Val)
		;
		dmember(Xpr=Val, Context)
	).

%abs_eval([], SrcTerms, Context, []) :-!.
abs_eval([Xpr | Xprs], SrcTerms, Context, [XprV | XXprs])
	:-!,
	abs_eval(Xpr, SrcTerms, Context, XprV),
	abs_eval(Xprs, SrcTerms, Context, XXprs).

abs_eval(case(Cond,Alternatives), SrcTerms, Context, Val)
	:-!,
	abs_eval(Cond, SrcTerms, Context, CondVal),
	dmember((CondVal:AltExpr), Alternatives),
	abs_eval(AltExpr, SrcTerms, Context, Val).

abs_eval(date, SrcTerms, Context, DateString)
	:-!,
	date(Date),
	sprintf(atom(DateString), '%t', [Date]).

abs_eval(time, SrcTerms, Context, TimeString)
	:-!,
	time(Time),
	sprintf(atom(TimeString), '%t', [Time]).

abs_eval(A appnd B, SrcTerms, Context, Val)
	:-!,
	abs_eval(A, SrcTerms, Context, AVal),
	abs_eval(B, SrcTerms, Context, BVal),
	append(AVal, BVal, Val).

abs_eval( (A + B), SrcTerms, Context, Val)
	:-!,
	abs_eval(A, SrcTerms, Context, AVal),
	abs_eval(B, SrcTerms, Context, BVal),
	sprintf(atom(Val), '%t%t', [AVal, BVal]).

abs_eval( (A ; B), SrcTerms, Context, Val)
	:-!,
	abs_eval(A, SrcTerms, Context, AVal),
	abs_eval(B, SrcTerms, Context, BVal),
	dmember( (search_path_sep = Sep), Context),
	sprintf(atom(Val), '%t%t%t', [AVal, Sep, BVal]).

abs_eval(local(PComp/PathList), SrcTerms, Context, Val)
	:-!,
	dmember(local_sep = Sep, Context),
	eval_path(PComp/PathList, SrcTerms, [path_sep = Sep | Context], Val).

abs_eval(PComp/PathList, SrcTerms, Context, Val)
	:-!,
	eval_path(PComp/PathList, SrcTerms, Context, Val).

abs_eval(File.Ext, SrcTerms, Context, Val)
	:-
	Ext \= [],
	!,
	abs_eval(File, SrcTerms, Context, FileVal),
	abs_eval(Ext, SrcTerms, Context, ExtVal),
	filePlusExt(FileVal,ExtVal,Val).

abs_eval(ext(Expr,EXT), SrcTerms, Context, XList)
	:-!,
	abs_eval(Expr, SrcTerms, Context, ExprList),
	abs_eval(EXT, SrcTerms, Context, EXTVal),
	extension(ExprList, EXTVal, XList).

/*
abs_eval(include_asis(Path), SrcTerms, Context, XList)
	:-
	exists_file(Path),
	!,
	open(Path, read, InS, []),
*/

abs_eval(Xpr, SrcTerms, Context, Val)
	:-
	atomic(Xpr),
	lookup(Xpr=XIV, SrcTerms, Context),
	!,
	abs_eval(XIV, SrcTerms, Context, Val).

abs_eval(Xpr, SrcTerms, Context, Xpr)
	:-
	atomic(Xpr),
	!.

abs_eval(Xpr, SrcTerms, Context, Val)
	:-
	Xpr =.. [Op | Args],
	Args \= [],
	abs_eval(Args, SrcTerms, Context, ArgsVal),
	append(ArgsVal, [IVal], XArgsVal),
	XXpr =.. [Op | XArgsVal],
	call(XXpr),
	!,
	abs_eval(IVal, SrcTerms, Context, Val).

ext(A,B,C) :- extension(A,B,C).

extension([], _, []):-!.
extension([Item | ExprList], EXT, [ItemEXT | List])
	:-!,
	catenate([Item, '.',EXT], ItemEXT),
	extension(ExprList, EXT, List).
extension(Item, EXT, Result)
	:-
	atom(Item),
	catenate([Item, '.',EXT], Result).
	

eval_path(PComp/PathList, SrcTerms, Context, Val)
	:-
	dmember(path_sep = PathSep, Context),
	eval_path(PComp/PathList, SrcTerms, Context, PathSep, Val).

eval_path(PComp/PathList, SrcTerms, Context, PathSep, Val)
	:-!,
	abs_eval(PComp, SrcTerms, Context, PCompVal),
	abs_eval(PathList, SrcTerms, Context, PathListVal),
	Val =.. [PathSep, PCompVal, PathListVal].

eval_path(PComp, SrcTerms, Context, PathSep, PCompVal)
	:-
	abs_eval(PComp, SrcTerms, Context, PCompVal).

/*-------------------------------------------------------------*
	context_name: obvious
	path_sep =  the directory path separator used by
			    the make (gnu make uses '/')
	local_sep = the directory path separator used by
				the local operating system
	object =    the file extension for object files used
                by the compiler/linker
	invoke =    the string (possibly empty) that make must
                prepend to executable files in order to actually
                run them ('' in unix; 'go32 ' in DJGPP V.1)
 *-------------------------------------------------------------*/
context(unix, 
		[
			context_name = unix,
			path_sep = '/',
			local_sep = '/',
			search_path_sep = ':',
			object = o,
			invoke = ''
		]).

context(mac, 
		[
			context_name = mac,
			path_sep = ':',
			local_sep = ':',
			search_path_sep = ':',
			object = o,
			invoke = ''
		]).

context(msw31, 
		[
			context_name = msw31,
			path_sep = '/',
			local_sep = '\\',
			search_path_sep = ';',
			object = obj,
			invoke = ''
		]).

context(djgpp, 
		[
			context_name = djgpp,
			path_sep = '/',
			local_sep = '\\',
			search_path_sep = ';',
			object = o,
			invoke = 'go32 '
		]).

context(os2, 
		[
			context_name = os2,
			path_sep = '/',
			local_sep = '\\',
			search_path_sep = ';',
			object = o,
			invoke = ''
		]).


endmod.
