/*-------------------------------------------------------------*
                    c2p.pro
               defStruct Type definitions generated from file:
                    c2p.typ
               by ALS defStruct Type Generator
               Macros written to file: c2p.mac
 *-------------------------------------------------------------*/

module utilities.
use c2pro.
endmod.

module c2pro.
use utilities.


%--- c2p defStruct ---

export accessC2P/3.
export setC2P/3.
accessC2P(inFile,_A,_B) :- arg(1,_A,_B).
setC2P(inFile,_A,_B) :- mangle(1,_A,_B).

accessC2P(inStream,_A,_B) :- arg(2,_A,_B).
setC2P(inStream,_A,_B) :- mangle(2,_A,_B).

accessC2P(curLine,_A,_B) :- arg(3,_A,_B).
setC2P(curLine,_A,_B) :- mangle(3,_A,_B).

accessC2P(curLineNum,_A,_B) :- arg(4,_A,_B).
setC2P(curLineNum,_A,_B) :- mangle(4,_A,_B).

accessC2P(ifdef_stack,_A,_B) :- arg(5,_A,_B).
setC2P(ifdef_stack,_A,_B) :- mangle(5,_A,_B).

accessC2P(fd_stack,_A,_B) :- arg(6,_A,_B).
setC2P(fd_stack,_A,_B) :- mangle(6,_A,_B).

accessC2P(info_table,_A,_B) :- arg(7,_A,_B).
setC2P(info_table,_A,_B) :- mangle(7,_A,_B).

accessC2P(fcn_filter,_A,_B) :- arg(8,_A,_B).
setC2P(fcn_filter,_A,_B) :- mangle(8,_A,_B).

accessC2P(outFile,_A,_B) :- arg(9,_A,_B).
setC2P(outFile,_A,_B) :- mangle(9,_A,_B).

accessC2P(outStream,_A,_B) :- arg(10,_A,_B).
setC2P(outStream,_A,_B) :- mangle(10,_A,_B).

export makeC2P/1.
makeC2P(_A) :- _A=..[c2p,nil,nil,nil,0,[[]],[],[],all,nil,nil].

export makeC2P/2.
makeC2P(_A,_B) :-
        struct_lookup_subst(
            [inFile,inStream,curLine,curLineNum,ifdef_stack,fd_stack,
                info_table,fcn_filter,outFile,outStream],
            [nil,nil,nil,0,[[]],[],[],all,nil,nil],_B,_C),
        _A=..[c2p|_C].

export xmakeC2P/2.
xmakeC2P(c2p(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J),[_A,_B,_C,_D,_E,_F,_G,_H,_I,_J]).

endmod.

module utilities.
typeProperties(c2p,
    [inFile,nil,inStream,nil,curLine,nil,curLineNum,0,ifdef_stack,[],
        fd_stack,[],info_table,[],fcn_filter,all,outFile,nil,outStream,nil]).
noteOptionValue(c2p,_A,_B,_C) :- setC2P(_A,_C,_B).
endmod.
/*==============================================================*
 | 		c2pro.pro
 |	Copyright(c) 1991-4 Applied Logic Systems, Inc.
 |
 |		C language translator - Top Level
 |
 | Author: Kevin A. Buettner
 |
 | Created: 4/3/90
 | Revision History:
 |	9/23/91		P. Raman
 |	4/94		K.Bowen
 |
 | Debugging levels
 | ----------------
 | Default level is 1;
 | Each level adds to what was shown at the previous level:
 |	0 - silent;
 |	1 - show open/close of input files; dots for stmt processing;
 |	2 - show token list of statemnt begin parsed;
 |	3 - show result of parsing;
 |	4 - show lines as read from source;
 | Levels are set by the command line switch:
 |	-d N
 | which must occur after the  -p divider.
 *===============================================================*/

%:- [c2p,cparse,ctoken,cexp,cout,cfiles,cmisc,cmacro].

module c2pro.
use avl.

	%-------------------------------------
	% Dynamic Declarations:
	%-------------------------------------

:- dynamic(cur_debug_level/1).
cur_debug_level(1).

:-dynamic(silent/0).
:-dynamic(skip_item/2).
:-dynamic(exclude_files/5).
:-dynamic(includePath/3).
:-dynamic(currentPath/3).
:-dynamic(imported/1).

	%-------------------------------------
	% OS-Specific File Loading (to disappear):
	%-------------------------------------

os_specific_file(unix,	cunix).
os_specific_file(dos,	cdos).
os_specific_file(macos,	cmac).
os_specific_file(vms,	cvax).

	%-------------------------------------
	% file suffixes.
	%-------------------------------------

inSuffix('spc').
outSuffix('src').

%%-------------------------------------
%% c2pro/0.
%% command line goal:
%% 	alspro c2pro -g c2pro -p file [file..] \
%%		[-Dflag[=num]]* [-Ipathname]*  \
%%		[-d debuglevel] [-b breakitem]
%%-------------------------------------

	%% old:
export ctrans/0.
ctrans :- c2pro.

export c2pro/0.

c2pro
	:-
		%% clean db & load os-specific stuff:
	cleanDB,
	(c2pro_os(_) ->
		true
		;
		builtins:als_system(SysVars),
		dmember(os=OS,SysVars),
		os_specific_file(OS,OS_File),
		reconsult(OS_File)
	),
		%% parse command line & setup options:
	get_cmdline_vals(SwitchVals),
	pull_out_files(SwitchVals, Files, ReducedSwitchVals),

	makeC2P(DummyState),
	setC2P(inFile, DummyState, command_line),
	setC2P(curLineNum, DummyState, 0),

	parse_options(ReducedSwitchVals, Defines, DummyState),
		%% process the files:
	recheck_defaults([sourcePath(_)]),
	calc_filters,
	c2pro(Files,Defines).

	%-------------------------------------
	%   pull_out_files/3
	%-------------------------------------

pull_out_files([], [], []).
pull_out_files([['-null_switch' , File] | SwitchVals], 
			[File | Files], ReducedSwitchVals)
	:-
	pull_out_files(SwitchVals, Files, ReducedSwitchVals).

pull_out_files([Switch | SwitchVals], Files, [Switch | ReducedSwitchVals])
	:-
	pull_out_files(SwitchVals, Files, ReducedSwitchVals).

	%-------------------------------------
	%   parse_options/3
	%-------------------------------------

parse_options([],[],State).
parse_options([FirstOpt | RestOpts], Defines, State)
	:-
	install_option(FirstOpt, Defines, RestDefines, State),
	parse_options(RestOpts, RestDefines, State).

/*--------------------------------------------------------------
 |   install_option/4
 |
 |	Options: 
 |	
 |	+Debugging:
 |	1.	Set debugging level (where Level is a number: 1 =< Level =< 9):
 |	 		option:  -d Level
 |	
 |	2.	Set break at decln processing on item:
 |			option:  -b Item  or -b [Item, ....]
 |	
 |	+Force definition of (C) constant:
 |		option:  -DName[=Num]
 |	
 |	+Setup include paths (use either):
 |		option:  -I Pathname
 |		option:  -IPathname
 |	
 |	+Establish a source path:
 |		option:  -srcpath Path
 |
 |	+Set path to a function filter spec file
 |		option:	 -filterFile Path
 |
 |	+Establish function filter 
 |		option:	 -filter FilterSpec
 |
 |		[ See below for description of function filters].
 |
 |	+Establish master name
 |		option:	 -master Name
 |
 |	+Set miscellaneous flag (Flag must be an atom, not one of above):
 |		option:  -Flag
 |	
 *-------------------------------------------------------------*/
	%% set debugging level:
	%% option:  -d Level[=Num]
install_option(['-d',InitLevel], Defines, Defines, State)
	:-
	(number(InitLevel) ->
		Level = InitLevel
		;
		name(InitLevel, ILCs),
		name(Level, ILCs)
	),
		
	abolish(cur_debug_level,1),
	assert(cur_debug_level(Level)).

	%% set break at decln processing on item:
	%% option:  -b Item  or -b [Item, ....]
install_option(['-b',BreakItem], Defines, Defines, State)
	:-
	set_break_items(BreakItem).

	%% option:  -DName[=Num]
install_option([Opt], [define(Id,Val)|RestDefines], RestDefines, State)
	:-
	name(Opt, [0'-,0'D|DefStr] ),
	!,
	getIdentifier(DefStr,IdStr,RestStr,State),
	name(Id,IdStr),
		%% Is -D in the form -D<Nam>=<Val>   ???
	getValueOpt(RestStr,Val).

	% option:  -I Pathname
install_option(['-I',PathAtm], Defines, Defines, State)
	:- !,
	name(PathAtm,PathStr),
	addIncludePath(PathStr).

	% option:  -IPathname
install_option([Opt], Defines, Defines, State)
	:-
	name(Opt,[0'-,0'I|PathStr]),
	!,
	addIncludePath(PathStr).

	% option: -srcpath Path
install_option(['-srcpath',Path], Defines, Defines, State)
	:-
	assert(sourcePath(Path)).

 	% option:	 -filterFile Path
install_option(['-filterFile',Path], Defines, Defines, State)
	:-
	set_filter_file(Path).

 	% option:	 -filter Filter
install_option(['-filter', Filter], Defines, Defines, State)
	:-
	set_filter_expr(Filter).

install_option(['-master', MasterName], Defines, Defines, State)
	:-
	assert(master_name(MasterName)).

install_option([Opt], Defines, Defines, State)
	:-
	name(Opt,[0'- | OptStr]),
	name(Flag,OptStr),
	output:assert(Flag).

	%% Skip anything else:
install_option(Opt, Defines, Defines, State)
	:-
	error('c2pro: Warning: unknown option: %t\n',[Opt],0).

		%% Is -D in the form -D<Nam>=<Val>   ???
getValueOpt([0'=|ValStr],[number(Num)])
	:-
	name(Num,ValStr),
	number(Num),
	!.
getValueOpt([0'=|ValStr],[string(Val)])
	:-
	atom_codes(Val, ValStr),
	!.
getValueOpt(_,[number(1)]).		% default value of -D flag is 1

recheck_defaults([]).
recheck_defaults([Item | Rest])
	:-
	recheck_default(Item),
	recheck_defaults(Rest).

:-dynamic(sourcePath/1).

recheck_default(sourcePath(_))
	:-
	(sourcePath(_) ->
		true
		;
		assert(sourcePath('.'))
	).

set_break_items(BreakItem)
	:-
	name(BreakItem, BICs),
	bufread(BICs, BItms0),
	(var(BItms0) ->
			%% BreakItem was a quoted var:
		BItms = BreakItem
		;
		BItms = BItms0
	),
	sbis(BItms).

:-dynamic(decln_break_on/1).

sbis([]).
sbis([BI | BItms])
	:-
	assert(decln_break_on(BI)),
	sbis(BItms).
sbis(BItms)
	:-
	assert(decln_break_on(BItms)).

/*-----------------------------------------
 |	Function filters:
 |
 |	Allows the specification of interfaces consisting of subsets of 
 |	of the full (source) library.  There are two broad types of 
 |	specifications: basic filters and compound filters.
 |
 |	Basic filters:  
 |	-------------
 |	These are terms of one of the following forms:
 |
 |		all  			 -	(default) include all functions from the library;
 |		all_except(List) -	include all functions from the library
 |							except the members of List
 |		List 			 - 	include just the elements of List
 |
 |	Here list consists of atoms naming functions, or expressions F/N,
 |	where F is an atom naming a function. [The N is ignored; allowing
 |	its presence is a convenience.]
 |
 |	Basic filters are specified by equations: 
 |
 |		FiltName = FiltTerm.
 |
 |	Here FiltName is an atom naming the filter and FiltTerm is one of
 |	the basic filter expressions described above. Typically these occur
 |	in specification files (see below).
 |
 |	Compound filters:
 |	----------------
 |	These are combinations of basic filters.  Abstractly,
 |	
 |		F1 + F2 + ... + Fn
 |	
 |	The result of the combination is evalutated to another basic filter.
 |	
 |	The rules for for evaluating these combinations are:
 |	
 |		all + <anything> 				= all;
 |		all_except(L1) + all_except(L2) = all_except(L1 intersect L2);
 |		List1 + List2 					= List1 union List2;
 |		List1 + all_except(List2) 		= all_except(List2 diff List1).
 |
 |	Here 'union' is 'set union' and diff is 'set difference'.
 |
 |	The rationale of this approach runs as follows.  In most settings, 
 |	the interfacing process involves several layers and files of code:
 |
 |		mylibxx.pro		<- a "smoothing/convenience" layer of code
 |		mylib.pro		<- the generated immediate dispatch layer code
 |		mylibinterf.a	<- the generated interface C code archived;
 |		mylib.a			<- the external C code archived as a library
 |
 |	One needs to co-ordinate the selection of function/predicate subsets
 |	at all levels.  The mechanism here controls the selection of functions
 |	included in mylib.pro and mylibinterf.a, and ultimately, those selected
 |	by the linker from mylib.a.  Since mylibxx.pro (which may really be
 |	a collection of files) is not generated, there is no immediate control
 |	of what is selected from it. However, a useful approach is to utilize
 |	the preprocessor mechanism to group the elements of mylibxx.pro into
 |	segments corresponding to the basic filters (which can be overlapping).
 |	by using expressions
 |
 |	#ifdef (syscfg:BFi)
 |	...
 |	#endif % BFi
 |
 |	The -A command line switch is used to pass a compound filter which
 |	an image is being made:
 |
 |	alspro -A	'BF1+BF2+...+BFn'
 |
 |	This causes each of the following facts to be asserted:
 |
 |		syscfg:BFi
 |
 |	This will select the appropriate units from mylibxx.pro, while the action
 |	of the mechanism here will make the approprate selections for
 |	mylib.pro and mylibinterf.a.
 *-------------------------------------------*/
set_filter_file(Path)
	:-
	exists_file(Path),
	!,
	assert(filter_file(Path)).

set_filter_file(Path)
	:-
	error('Error: Can''t find filter file: %t\n', [Path], 0).


set_filter_expr(FiltExpr)
	:-
	assert(raw_filter_expr(FiltExpr)).
	

calc_filters
	:-
	raw_filter_expr(FiltExprAtm),
	abolish(raw_filter_expr,1),
	!,
	atomread(FiltExprAtm, FiltExpr, [attach_fullstop(true),
									 syntax_errors(quiet)]),
	calc_filters(FiltExpr).

calc_filters.

	%% Incoming is 'all':
calc_filters(all)
	:-!,
	assert(fcn_filter(all)).

	%% Incoming is all_except(Excl):
calc_filters(all_except(Excl))
	:-
	assert(fcn_filter(all_except(Excl))).

	%% Incoming is explicit list of fcns to include:
calc_filters(FiltExpr)
	:-
	FiltExpr = [_|_],
	!,
	assert(fcn_filter(FiltExpr)).

	%% Incoming is an atom or A+B:
calc_filters(Expr)
	:-
	(atom(Expr); functor(Expr,+,2)),
	!,
	filter_file_path(Path),
	get_filter_info(Path,BasicFilters),
	calc_filters(Expr,BasicFilters,FinalFilter),
	assert(fcn_filter(FinalFilter)).

calc_filters(Expr)
	:-
	error('Error: Improper filter: %t\n', [Expr], 0),
	!,
	fail.

:-dynamic(filter_file/1).

filter_file_path(Path)
	:-
	filter_file(Path),
	!.

filter_file_path(Path)
	:-
	Path = './filters', 
	exists_file(Path),!.

filter_file_path(Path)
	:-
	Path = '../../syscfg',
	exists_file(Path),!.

filter_file_path(Path)
	:-
	error('Error: Can''t find filter file: %t\n', [Path], 0),
	!,
	fail.

get_filter_info(Path,Filters)
	:-
	open(Path,read,PS,[]),
	read_terms(PS,Filters),
	close(PS).

calc_filters(E+F,Filters,FinalFilter)
	:-!,
	calc_filters(E,Filters,E_Filter),
	calc_filters(F,Filters,F_Filter),
	filter_combine(E_Filter, F_Filter, FinalFilter).

calc_filters(Expr,Filters,FinalFilter)
	:-
	dmember(Expr=NextFilter, Filters),
	(NextFilter = (_ + _) ->
		calc_filters(NextFilter,Filters,FinalFilter)
		;
		FinalFilter = NextFilter
	).

/*-----------------------------------------------------------------
 |	all + <anything> 				= all;
 |	all_except(L1) + all_except(L2) = all_except(L1 intersect L2);
 |	List1 + List2 					= List1 union List2;
 |	List1 + all_except(List2)		= all_except(List2 diff List1)
 *----------------------------------------------------------------*/

filter_combine(all, _, all) :-!.
filter_combine(_, all, all) :-!.

filter_combine(all_except(L1), all_except(L2),all_except(L3))
	:-!,
	intersect(L1, L2, L3).

filter_combine(all_except(L1), L2, Result)
	:-!,
	filter_combine(L2, all_except(L1), Result).

filter_combine(L1, all_except(L2), all_except(L2, L3))
	:-!,
	list_diff(L2, L1, L3).

filter_combine(L1, L2, L3)
	:-
	union(L1, L2, L3).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%
	%% 		c2pro/2.
	%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c2pro([],_)
	:- !.

c2pro([FirstFile | Rest], Defines) 
	:-
	sourcePath(SrcPath),
	setup_the_input_file(FirstFile, SrcPath, InName, BaseName),
	outSuffix(OS),
	filePlusExt(BaseName,OS,OutName),

	init_state(InName, OutName, State),
	fcn_filter(FcnFilter),
	setC2P(fcn_filter, State, FcnFilter),
	do_c2pro(InName, State, Defines),
	!,
	c2pro(Rest, Defines).

c2pro([_ | Rest], Defines) 
	:-
	c2pro(Rest, Defines).

	%---------------------------------
	%  input_file_setup/3
	%---------------------------------

setup_the_input_file(FirstFile, SrcPath, InName, BaseName)
	:-
	input_file_setup(FirstFile, SrcPath, InName, BaseName),
	exists_file(InName),
	!.
setup_the_input_file(FirstFile, SrcPath, InName, BaseName)
	:-
	master_name(MasterName),
	inSuffix(IS), 
	filePlusExt(MasterName,IS,FileExt),
	pathPlusFile(SrcPath,FileExt,InName),
	exists_file(InName),
	!,
	BaseName = FirstFile.

setup_the_input_file(FirstFile, SrcPath, InName, BaseName)
	:-
	error('Can''t find source file: %t -- skipping\n',[FirstFile], 0).

input_file_setup(File, SrcPath, InName, BaseName)
	:-
	filePlusExt(BaseName,Ext,File),
	!,
	inSuffix(IS), 
	(IS = Ext ->
		InName = File
		;
		error('Bad source file: %t -- skipping\n',[File], 0)
	).

	%% Finding a path attached to a particular file
	%% overrides the source path specified with a switch:
input_file_setup(File, SrcPath, InName, BaseName)
	:-
	pathPlusFile(Path,BaseName,File),
	Path \= '',
	!,
	inSuffix(IS), 
	filePlusExt(File,IS,InName).

	%% File is a pure, unadorned name (no path, no ext); 
	%% use the SrcPath:
input_file_setup(File, SrcPath, InName, File)
	:-
	inSuffix(IS), 
	filePlusExt(File,IS,FileExt),
	pathPlusFile(SrcPath,FileExt,InName).

	%---------------------------------
	%  init_state/3
	%---------------------------------

init_state(InName, OutName, State)
	:-
	makeC2P(State),
	setC2P(inFile, State, InName),
	setC2P(outFile, State, OutName),
	open(OutName,write,OutStream,[]),
	setC2P(outStream, State, OutStream),
	gen_file_header(OutStream,InName,OutName),
	avl_create(Table),
	setC2P(info_table, State, Table).

	%---------------------------------
	% do_c2pro/3.
	%---------------------------------


do_c2pro(InName, State, Defines) 
	:-
	read_file(InName, State, Defines),
	closeFile(State),
	!.
do_c2pro(InName, State,_)
	:-
	closeFile(State),
	printf('\n>>>-->>> c2pro halted prematurely processing %t<<<---<<<\n',[InName]).

endmod.
/*======================================================================*
 |		cexp.pro
 |	Copyright (c) 1990-91 Applied Logic Systems, Inc.
 |
 |		Expression parser
 |
 | Author: Kevin A. Buettner
 | Creation: 4/6/90
 |
 | Revision History:
 |	Revised:	5/7/91, kev,	added expression_additive/3 for
 |					88k implementation.
 |	Updated:	9/5/91, raman,	bug fixes
 |
 | Major Procedures:
 |	expression/5 
 |		The first argument is the return value of the
 |		expression. Second arg is the file descriptor and 
 |		the third arg is the symbol table. The fourth and
 |		fifth  arguments are the list values typical of DCG rules.
 |
 |		The expression will be evaluated as much as
 |		possible.
 |		
 *======================================================================*/

module c2pro.

expression(Val,State) --> e12(Val,State).


/*
 * e12	-- conditional expression
 */

e12(Val,State) --> e11(V1,State), e12s(Val,V1,State).

e12s(Val,V1,State) -->
	[question], !, e12(V2,State), [colon], e12(V3,State),
	{eval_conditional(V1,V2,V3,Val)}.
e12s(V,V,State) --> [].

eval_conditional(0,_,V,V) :- !.
eval_conditional(N,V,_,V) :- number(N), !.
eval_conditional(V1,V2,V3,conditional(V1,V2,V3)).

/*
 * e11	-- logical or
 */

e11(Val,State) --> e10(V1,State), e11s(Val,V1,State).

e11s(Val,V1,State) --> 
	[lor], !, e10(V2,State), {eval_lor(V1,V2,V3)}, 
	e11s(Val,V3,State).
e11s(V,V,State) --> [].

eval_lor(0,0,0)		:- !.
eval_lor(N1,_,1)	:- number(N1), !.
eval_lor(_,N2,1)	:- number(N2), !.
eval_lor(N1,N2,lor(N1,N2)).

/*
 * e10	-- logical and
 */

e10(Val,State) --> e9(V1,State), e10s(Val,V1,State).

e10s(Val,V1,State) --> 
	[land], !, e9(V2,State), {eval_land(V1,V2,V3)}, 
	e10s(Val,V3,State).
e10s(V,V,State) --> [].

eval_land(0,_,0)	:- !.
eval_land(_,0,0)	:- !.
eval_land(N1,N2,1)	:- number(N1),number(N2), !.
eval_land(N1,N2,land(N1,N2)).


/*
 * e9	-- bitwise inclusive or
 */

e9(Val,State) --> e8(V1,State), e9s(Val,V1,State).

e9s(Val,V1,State) --> 
	[or], !, e8(V2,State), {eval_or(V1,V2,V3)}, 
	e9s(Val,V3,State).
e9s(V,V,State) --> [].

eval_or(N1,N2,V)
	:- 
	number(N1), number(N2), V is N1 \/ N2, !.
eval_or(N1,N2,or(N1,N2)).

/*
 * e8	-- bitwise exclusive or
 */

e8(Val,State) --> e7(V1,State), e8s(Val,V1,State).

e8s(Val,V1,State) -->
	[carat], !, e7(V2,State), {eval_xor(V1,V2,V3)}, 
	e8s(Val,V3,State).
e8s(V,V,State) --> [].

eval_xor(N1,N2,V)
	:-
	number(N1), number(N2), V is N1 xor N2, !.
eval_xor(N1,N2,xor(N1,N2)).

/*
 * e7	-- bitwise and
 */

e7(Val,State) --> e6(V1,State), e7s(Val,V1,State).

e7s(Val,V1,State) -->
	[and], !, e6(V2,State), {eval_and(V1,V2,V3)}, 
	e7s(Val,V3,State).
e7s(V,V,State) --> [].

eval_and(N1,N2,V)
	:-
	number(N1), number(N2), V is N1 /\ N2, !.
eval_and(N1,N2,and(N1,N2)).

/*
 * e6	-- equality operators
 */

e6(Val,State) --> e5(V1,State), e6s(Val,V1,State).

e6s(Val,V1,State) -->
		[EqOp], {isEqOp(EqOp)}, !,
		e5(V2,State), {e6s_eval(EqOp,V1,V2,V3)}, e6s(Val,V3,State).
e6s(V,V,State) --> [].

isEqOp(equalEqual).
isEqOp(notequal).

e6s_eval(equalEqual,N1,N2,V)
	:-
	number(N1),
	number(N2), !, 
	eval_Equal(N1,N2,V).
e6s_eval(equalEqual,N1,N2,equal(N1,N2)) :- !.
e6s_eval(notequal,N1,N2,V)
	:-
	number(N1),
	number(N2), !,
	eval_notEqual(N1,N2,V).
e6s_eval(notequal,N1,N2,not_equal(N1,N2)).

eval_Equal(N,N,1) :- !.
eval_Equal(_,_,0).

eval_notEqual(N,N,0) :- !.
eval_notEqual(_,_,1).

/*
 * e5	-- relational operators
 */

e5(Val,State) --> e4(V1,State), e5s(Val,V1,State).

e5s(Val,V1,State) -->
	[RelOp], {isRelOp(RelOp)}, !,
	e4(V2,State), {e5s_eval(RelOp,V1,V2,V3)}, e5s(Val,V3,State).
e5s(V,V,State) --> [].

isRelOp(langle).
isRelOp(rangle).
isRelOp(lessOrEqual).
isRelOp(greaterOrEqual).

e5s_eval(langle,N1,N2,V)
	:-
	number(N1),
	number(N2),
	!,
	eval_less(N1,N2,V).
e5s_eval(langle,N1,N2,N1<N2) :- !.
e5s_eval(rangle,N1,N2,V) :-
	number(N1),
	number(N2),
	!,
	eval_less(N2,N1,V).
e5s_eval(rangle,N1,N2,N1>N2) :- !.
e5s_eval(lessOrEqual,N1,N2,V)
	:-
	number(N1),
	number(N2),
	!,
	eval_lessOrEqual(N1,N2,V).
e5s_eval(lessOrEqual,N1,N2,N1=<N2) :- !.
e5s_eval(greaterOrEqual,N1,N2,V)
	:-
	number(N1),
	number(N2),
	!,
	eval_lessOrEqual(N2,N1,V).
e5s_eval(greaterOrEqual,N1,N2,N1>=N2).

eval_less(N1,N2,1) :- N1 < N2,!.
eval_less(N1,N2,0).

eval_lessOrEqual(N1,N2,1) :- N1 =< N2, !.
eval_less(N1,N2,0).

/*
 * e4	-- shift level operators
 */

e4(Val,State) --> e3(V1,State), e4s(Val,V1,State).

e4s(Val,V1,State) --> 
	[ShiftOp], {isShiftOp(ShiftOp)}, !,
	e3(V2,State), {e4s_eval(ShiftOp,V1,V2,V3)}, e4s(Val,V3,State).
e4s(V,V,State) --> [].

isShiftOp(leftshift).
isShiftOp(rightshift).

e4s_eval(leftshift,N1,N2,V)
	:-
	number(N1),
	number(N2),
	V is N1<<N2, !.
e4s_eval(leftshift,N1,N2,N1<<N2) :- !.
e4s_eval(rightshift,N1,N2,V)
	:-
	number(N1),
	number(N2),
	V is N1>>N2, !.
e4s_eval(rightshift,N1,N2,N1>>N2).

/*
 * e3	-- additive level operators
 */

e3(Val,State) --> e2(V1,State), e3s(Val,V1,State).

e3s(Val,V1,State) -->
	[AddOp], {isAddOp(AddOp)}, !,
	e2(V2,State), {e3s_eval(AddOp,V1,V2,V3)}, e3s(Val,V3,State).
e3s(V,V,State) --> [].

isAddOp(plus).
isAddOp(minus).

e3s_eval(plus,N1,N2,V)
	:-
	number(N1),
	number(N2),
	V is N1+N2, !.
e3s_eval(plus,N1,N2,N1+N2) :- !.
e3s_eval(minus,N1,N2,V) :-
	number(N1),
	number(N2),
	V is N1-N2, !.
e3s_eval(minus,N1,N2,N1-N2).

/*
 * e2	-- multiplicative level operators
 */

e2(Val,State) --> e1(V1,State), e2s(Val,V1,State).

e2s(Val,V1,State) -->
	[MulOp], {isMulOp(MulOp)}, !, 
	e1(V2,State), {e2s_eval(MulOp,V1,V2,V3)}, e2s(Val,V3,State).
e2s(V,V,State) --> [].

isMulOp(star).
isMulOp(slash).
isMulOp(percent).

e2s_eval(star,N1,N2,V)
	:-
	number(N1),
	number(N2),
	V is N1*N2, !.
e2s_eval(star,N1,N2,N1*N2) :- !.
e2s_eval(slash,N1,N2,V)
	:-
	number(N1),
	number(N2),
	V is N1/N2,!.
e2s_eval(slash,N1,N2,N1/N2) :- !.
e2s_eval(percent,N1,N2,V)
	:-
	number(N1),
	number(N2), 
	V is N1 mod N2, !.
e2s_eval(percent,N1,N2,mod(N1,N2)).

/*
 * e1	-- unary operators
 */

e1(Val,State) --> sizeof_exp(Val,State), !.
e1(Val,State) --> primary_exp(Val,State), !.
e1(Val,State) --> cast_exp(Val,State), !.
e1(Val,State) --> unop_exp(Val,State).

%

primary_exp(Num,State) --> [number(Num)], !.
primary_exp(Exp,State) --> [lparen], !, expression(Exp,State), [rparen].
primary_exp(Val,State) --> [ident(Id)], 		% enum constant
	{ accessC2P(info_table, State, Table),
	  avl_search(assign(Id),val(Val),Table) }, ! .
% primary_exp(ident(Id),State) --> [ident(Id)].

%

cast_exp(Val,State) --> [lparen], type_spec(State,_),  
	abstract_declt(State,_), [rparen], e1(Val,State).

%
% size returned is 32 (arbitrary)
%

sizeof_exp(32,State) --> 
	[fident(sizeof),lparen], !, type_spec(State,_), [rparen].

sizeof_exp(32,State) --> 
	[ident(sizeof), lparen], !, e1(_,State), [rparen].

sizeof_exp(32,State) --> 
	[ident(sizeof)], e1(_,State).

%

unop_exp(Val,State) --> [UnOp], {isUnary(UnOp)}, e1(E,State),
	{e1_eval(UnOp,E,Val)}.


isUnary(minus).
isUnary(plus).
isUnary(lnot).
isUnary(not).

e1_eval(minus,N,Val) :- number(N), Val is -N, !.
e1_eval(minus,E,-E).
e1_eval(lnot,E,Val) :- !, eval_lnot(E,Val).
e1_eval(not,N,Val) :- number(N), Val is \(N), !.
e1_eval(not,N,not(N)).

eval_lnot(0,1) :- !.
eval_lnot(N,0) :- number(N), !.
eval_lnot(E,lnot(E)).

endmod.
/*===================================================================
 |		cfiles.pro
 |	Copyright(c) 1991-94 Applied Logic Systems Inc.
 |
 |		File operations
 |
 | Author : P.Raman
 | Created : 9/23/91
 | Revision History :
 |
 | Module : fileops
 |
 | Exported predicates :
 |	parsePathStr/4	-- parse a path string into a list of files
 |	parsePathTokList/4	-- parse a token list into a list of files
 |	addIncludePath/1-- add a path for search of system include files
 |	findUserFile/2	-- search for a user file ( see comment for search order )
 |	findSysFile/4	-- search for a system file
 *==================================================================*/

module c2pro.

	% 
	% parsePathStr/4.
	% parsePathStr/5.
	%	parse a Path string and return a File list
	%	the first argument returns a flag that is set to 'abs'
	%	if the pathname is absolute, otherwise it is set to 'rel'.
	% 

parsePathStr(PathStr,Disk,CompList,Type) :-
	parsePathStr(PathStr,Disk,CompList,Type,Rest).

parsePathStr(PathStr,Disk,CompList,Type,Rest) :-
	diskOfPathStr(Disk,PathStr,PathStr1), 
	compsOfPathStr(CompList,PathStr1,Rest),
	typeOfPathStr(Disk,PathStr1,Type).

%
% parsePathTokList/4.
% parsePathTokList/5.
% 	parse a token list and return a File list (second arg).
%	First argument is bound to 'abs' if input token list
%	represents an absolute path name, otherwise it is bound 
%	to 'rel'.
%

parsePathTokList(TokList,Disk,CompList,Type) :-
	parsePathTokList(TokList,Disk,CompList,Type,Rest).

parsePathTokList(TokList,Disk,CompList,Type,Rest) :-
	diskOfPathTokList(Disk,TokList,TokList1),
	compsOfPathTokList(CompList,TokList1,Rest),
	typeOfPathTokList(Disk,TokList1,Type).

%
% addIncludePath/1
%	Argument is a string representing an include search path
%   The input serach path is added as the first search path,
%	and therefore if there are more than one search paths
%	to be included, they must be presented in reverse order.
%

addIncludePath(Str) :- 
	parsePathStr(Str,Disk,CompList,PathType),
	asserta(includePath(Disk,CompList,PathType)).
%	pathsyntax:asserta(includePath(Disk,CompList,PathType)).

%
% findUserFile/2
%	search for the named file in the current directory, then
%	in the system include directories
%

findUserFile(FileName,FileName) 
	:-	
	exists_file(FileName),			% abs pathname or found locally
	!.
findUserFile(PathName,ExpandedName) 		% use possiblePath
	:-		
	name(PathName,PathStr),
	parsePathStr(PathStr,Disk1,CompList1,PathType1),
	CompList1 \= [],
	PathType1 \= abs,
	possiblePath(Disk2,CompList2,PathType2),
	dappend(CompList2,CompList1,CompList),
	pathListToName(Disk2,CompList,PathType2,ExpandedName),
	exists_file(ExpandedName),
	!.
findUserFile(File,ExpandedName) 
	:-		
	sourcePath(Path),
	pathPlusFile(Path,File,ExpandedName),
	exists_file(ExpandedName).

%
%	search for a system file. If it is an absolute pathname
%	then we dont have to search for it, otherwise search in
%	in the system include paths.
%

findSysFile(Disk,CompList,abs,AbsName) 
	:-
	!,
	pathListToName(Disk,CompList,abs,AbsName),
	exists_file(AbsName).
findSysFile(Disk1,CompList1,rel,AbsName) :-
	CompList1 \= [],
	possiblePath(Disk2,CompList2,PathType),
	dappend(CompList2,CompList1,CompList),
	pathListToName(Disk2,CompList,PathType,AbsName),
	exists_file(AbsName),
	!.

possiblePath(Disk,Comps,PathType) :- includePath(Disk,Comps,PathType).
possiblePath(Disk,Comps,PathType) :- currentPath(Disk,Comps,PathType), !.

%
%	if a file that has just been opened has non-trvial
%	path components, then we assert it as the current path
%	and when that file is closed we retract it.
%

addto_searchPath(FName) :-
	name(FName,FStr),
	parsePathStr(FStr,Disk,Comps,PathType),
	deleteLast(Comps,PathComps),
	PathComps \= [],
	!,
	asserta(currentPath(Disk,PathComps,PathType)).
addto_searchPath( _).


dropfrom_searchPath(FName) :-
	name(FName,FStr),
	parsePathStr(FStr,Disk,Comps,PathType),
	deleteLast(Comps,PathComps),
	PathComps \= [],
	retract(currentPath(_,_,_)),		% remove the top assertion
	!.
dropfrom_searchPath( _).


endmod.
/*===============================================================*
 | 		cmacro.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 |		C macro processor
 |
 | Author: Kevin A. Buettner
 | Creation: 4/1/90
 | Revision History:
 | 	10/22/91 -- raman -- modified to parse subset of C
 *===============================================================*/

module c2pro.

	%-------------------------------------------------------
	% process_line(InLine,OutLine,InFD,OutFD,InTab,OutTab)
	%

process_line([],[],State) :- !.
process_line([pound, Tok | Rest ],[],State) :-
	!,
	macro_statement0(Tok,Rest,State).
process_line(Line, OutLine, State) :-
	macro_expand(multiple, Line, OutLine, State).

/*-----------------------------------------------------------*
 * macro_statement(Token,Rest,State)
 *
 *		Token	-- the token following the pound sign
 *		Rest	-- the rest of the line after Token
 *		InFD	-- the file stream descriptor (input)
 *		OutFD	-- the file stream descriptor returned
 *		InTab	-- the macro definition symbol table (input)
 *		OutTab	-- the macro definition symbol table to return
 *-----------------------------------------------------------*/

macro_statement0(ident(Id),Rest,State) :- !,
	macro_statement(Id,Rest,State).
macro_statement0(Tok,Rest,State) :-
	error('Unknown directive: %t\n',[Tok],State).


macro_statement(import,Rest,State) :- !,
	macro_expand(single,Rest,Rest0,State),
	import(Rest0,State).				% for Obj-C
macro_statement(include,Rest,State) :- !,
	macro_expand(single,Rest,Rest0,State),
	import(Rest0,State).	            % include is import
macro_statement(exclude,Rest,State) :- !,
	exclude(Rest,State).				% special
macro_statement(define,Rest,State) :- !,
	define(Rest,State).
macro_statement(ifdef,Rest,State) :- !,
	ifdef(Rest,State).
macro_statement(if,Rest,State) :- !,
	if(Rest,State).
macro_statement(ifndef,Rest,State) :- !,
	ifndef(Rest,State).
macro_statement(endif,_,State) :- !,
	endif_else_or_elif(endif,State).
macro_statement(elif,_,State) :- !,
	endif_else_or_elif(elif(_),State).
macro_statement(else,_,State) :- !,
	endif_else_or_elif(else,State).
macro_statement(undef,Rest,State) :- !,
	undef(Rest,State).
macro_statement(ident, Rest,State) :- !.		% SCCS tags
	macro_statement(pragma,_,State) :- !.		% consume pragmas
macro_statement(Tok,Rest,State) :- !,
	error('Unknown directive: %t\n',[Tok],State).

/*
 * import/2
 *	same as include except each imported file is imported
 *  exactly once
 */

import([string(FileName)],State) :-  	% #include "foo"
	!,
	user_import(FileName,State).
import([langle|PathSpec],State) :-	% #include <foo>
	!,
	sys_import(PathSpec,State).
import(PathSpec,State) :-
	parsePathTokList(PathSpec,Disk,CompList,PathType),   % #include foo
	findSysFile(Disk,CompList,PathType,FileName),
	!,
	dispatch_import(FileName,State).
import(Whatever,State) :-
	error('Bad #include or #import: %t\n',[Whatever],State).


user_import(FileName,State) :-
	findUserFile(FileName,NFileName),
	!,
	dispatch_import(NFileName,user,State).
user_import(FileName,State) :-
	error('Bad #include or #import: %t\n',[FileName],State).


sys_import(PathSpec,State) :-
	parsePathTokList(PathSpec,Disk,CompList,PathType),
	CompList \= [],
	findSysFile(Disk,CompList,PathType,FileName),
	!,
	dispatch_import(FileName,system,State).
sys_import(Whatever,State) :-
	error('Bad #include or #import: %t\n',[Whatever],State).


dispatch_import(FileName,IType,State) :-
	imported(FileName),
	!.
dispatch_import(FileName,IType,State) :-
	assert(imported(FileName)),
	openFile(FileName,State).

/*
 * exclude/2  
 *
 *	#exclude FileSpec/TypeSpec
 *
 *	where FielSpec can be "Pathname" or <Pathname>, and
 *	PathName can have wild-cards "?" to match single pathh
 *	component or a "*" to match any trailing components.
 *	/TypeSpec is optional, and can be either a single group
 *	from func,const,gvar,ctype or all to indicate all groups,
 *	or it can be a list [group,...] that specifies the groups.
 *
 *	Output is suppressed for the specified groups from files
 *	that match FileSpec.
 */

exclude([string(FileName)|Rest],State) :-
	name(FileName,FileStr),
	parsePathStr(FileStr,Disk,Comps,PathType),
	!,
	dispatch_exclude(user,Disk,Comps,PathType,Rest,State).
exclude([langle|PathSpec],State) :-			% #exclude <foo>
	parsePathTokList(PathSpec,Disk,Comps,PathType,[rangle|Rest]),	
	!,
	dispatch_exclude(system,Disk,Comps,PathType,Rest,State).
exclude(Whatever,State) :-
	error('Bad #exclude : %t\n',[Whatever],State).

dispatch_exclude(ExclType,Disk,Comps,PathType,Rest,State) :-
	after_exclude(Rest,What),
	output_exclude(ExclType,Disk,Comps,PathType,What).


after_exclude([slash,ident(What)], [What]) :- !.
after_exclude([slash,lbrac|Rest], What) :- !,
	exclude_list(Rest,What).
after_exclude( _, all).

exclude_list([rbrac|_],[]) :- !.
exclude_list([ident(What)|Rest],[What|TypeL]) :- 
	exclude_list(Rest,TypeL).


/*
 * undef/4		-- handle #undef
 */

undef([ident(Id)],State) :- 
	accessC2P(info_table, State, InTable),
	avl_search(Id,_,InTable),
	avl_insert(Id,unDefined,InTable,OutTable),
%pbi_write(avli=Id),pbi_nl,
	setC2P(info_table, State, OutTable),
	!.
undef([ident(_)],State) :- !.
undef(Whatever,State) :-
	error('Illegal #undef argument : %t\n',[Whatever],State).


/*
 * define/4		-- handle #defines
 */

define([ident(Id)|Defn],State) :- !,
	check_const(Id,Defn,State),
	accessC2P(info_table, State, InTable),
	addToTable(Id,d(Defn),State,InTable,OutTable),
	setC2P(info_table, State, OutTable).
define([fident(Id)|ArgsAndDefn],State) :-
	getArgs(Args,ArgsAndDefn,Defn),
	!,
	output_macro(Id,State),
	accessC2P(info_table, State, InTable),
	addToTable(Id,da(Args,Defn),State,InTable,OutTable),
	setC2P(info_table, State, OutTable).
define(Whatever,State) :-
	error('Bad #define : %t\n',[Whatever],State).

%
% check_const/4
%

check_const(Id,[string(_)|_],State) :-
	!,
	output_const(Id,str,State).
check_const(Id,Defn,State) :-
	macro_expand(single,Defn,OutDefn, State),
	expression(Val,State,OutDefn,_),
	const_type(Val,Type),
	!,
	output_const(Id,Type,State).
check_const(Id,Defn,State).

const_type(Val,int) :- integer(Val), !.
const_type(Val,double) :- float(Val).

/*
 * addToTable(Key,State,Defn,InTab,OutTab)
 *
 *		Tests for the presence of Key in the macro defn table.  If
 *		Key is already in the macro definition table, an error is
 *		reported using LineNum.  Otherwise, the macro name and the
 *		definition are added to the macro definition table (InTab)
 *		to produce OutTab.
 */

addToTable(Key,Defn,State,Table,Table) :-
%(Key = ident('XEvent') -> trace ; true),
	avl_search(Key,Data,Table),
	Data \= unDefined,
	!,
	error(' %t defined again\n',[Key],State).
addToTable(Key,Defn,State,InTable,OutTable) :-
%pbi_write(avli=Key),pbi_nl,
	avl_insert(Key,Defn,InTable,OutTable).


/*
 * getArgs/3	-- dcg rules for getting #define formal parameters
 */

getArgs([Arg|MoreArgs]) --> [lparen,ident(Arg)],!,moreArgs(MoreArgs).
getArgs([]) --> [lparen,rparen].

moreArgs([]) --> [rparen],!.
moreArgs([Arg|MoreArgs]) --> [comma,ident(Arg)],moreArgs(MoreArgs).


/*
 * ifdef(Rest,State)
 *
 *	-- handle ifdefs
 */		

%
% ifdef/5
%

ifdef([Token|_],State) :-
	is_identifier(Token,Id),
	!,
	do_ifdef(Id,State).
ifdef(_,State) :-
	error('Ill formed #ifdef, assume test fails\n',State),
	skipLines(State,What),
	check_elif(What,State).

%
% ifndef/5
%

ifndef([Token|_],State) :-
	is_identifier(Token,Id),
	!,
	do_ifndef(Id,State).
ifndef(_,State) :-
	error('Ill formed #ifndef, assume test fails\n',State),
	skipLines(State,What),
	check_elif(What,State).

%
% if/5
%

if(Exp,State) :-
	replace_defined(Exp, Exp1, State),
	macro_expand(single, Exp1, Exp2, State),
	replace_ident_with0(Exp2,Exp3),
	expression(Value,State,Exp3,_),
	!,
	do_if(Value,State).
if(_,State) :-
	error('Ill formed #if, assume test fails\n',State),
	do_if(0,State).

%
% endif_else_or_elif/5
%

endif_else_or_elif(Which,State) :-
	skipMore(Which,State),
	accessC2P(ifdef_stack, State, [[Lev|Rest] | Prev]),
	!,
	setC2P(ifdef_stack, State, [Rest | Prev]).
endif_else_or_elif(Which,State) :-
	error('Unmatched #endif\n',State).

%
% do_ifdef/5
%

do_ifdef(Id,State) :-
	accessC2P(info_table, State, InTable),
	avl_search(Id,Data,InTable),
	Data \= unDefined,
	!,
	accessC2P(ifdef_stack, State, [LevList | Prev]),
	setC2P(ifdef_stack, State, [[_ | LevList] | Prev]).
	
do_ifdef(Id,State) :-
	skipLines(State,What),
	check_elif(What,State).

%
% do_ifndef/5
%

do_ifndef(Id,State) :-
	accessC2P(info_table, State, InTable),
	avl_search(Id,Data,InTable),
	Data \= unDefined,
	!,
	skipLines(State,What),
	check_elif(What,State).
do_ifndef(Id,State) 
	:-
	accessC2P(ifdef_stack, State, [LevList | Prev]),
	setC2P(ifdef_stack, State, [[_|LevList] | Prev]).
	
/*
%
% check_else/5
%

check_else(else,State)
	:-
	accessC2P(ifdef_stack, State, [LevList | Prev]),
	setC2P(ifdef_stack, State, [[endif|LevList] | Prev]).
check_else(endif,State).
*/


%
% do_if/5
%

do_if(0,State) :-
	!,
	skipLines(State,What),
	check_elif(What,State).
do_if(_,State)
	:-
	accessC2P(ifdef_stack, State, [LevList | Prev]),
	setC2P(ifdef_stack, State, [[_|LevList] | Prev]).

%
% check_elif/5
%

check_elif(else,State)
	:-
	accessC2P(ifdef_stack, State, [LevList | Prev]),
	setC2P(ifdef_stack, State, [[endif|LevList] | Prev]).
check_elif(elif(Exp),State) :-
	replace_defined(Exp, Exp1, State),
	macro_expand(single, Exp1, Exp2, State),
	replace_ident_with0(Exp2,Exp3),
	expression(Value,State,Exp3,_),
	Value =\= 0,
	!,
	accessC2P(ifdef_stack, State, [LevList | Prev]),
	setC2P(ifdef_stack, State, [[_|LevList] | Prev]).
check_elif(elif(_),State) :-
	skipLines(State,What),
	check_elif(What,State).
check_elif(endif,State).

%
% skip_lines/3
%

skipLines(State,RetTok) :-
	getLine(Line,State),
	!,
	skipLines(Line,State,RetTok).
skipLines(State,endif) :-
	error('Missing #endif\n',State).

skipLines([pound,ident(endif) | _],State,endif) :- !.
skipLines([pound,ident(else) | _],State,else) :- !.
skipLines([pound,ident(elif) | Exp],State,elif(Exp)) :- !.
skipLines([pound,ident(Cond) | _],State,RetTok) :-
	conditional(Cond),
	!,
	skipLines(State,What),
	skipMore(What,State),
	skipLines(State,RetTok).
skipLines(_,State,RetTok) :-
	skipLines(State,RetTok).

conditional(if).
conditional(ifdef).
conditional(ifndef).

%
% skipMore/3
%	called when we've discovered a nested ifdef, ifndef or if

skipMore(endif,State).
skipMore(elif(_),State) :- 
	skipLines(State,What),
	skipMore(What,State).
skipMore(else,State) :-
	skipLines(State,What),
	skipMore(What,State).


/*
 * replace_defined/4
 * 	replace defined(Id) tokens by number(0) or number(1)
 *	depending on whether Id is defined or not.
 */

replace_defined( [],[],_).
replace_defined( [Tok,lparen,ident(Id),rparen | IL],[number(N)|OL],State) :-
	is_identifier(Tok,defined),
	!,
	defined_val(Id,State,N),
	replace_defined(IL,OL,State).
replace_defined( [ident(defined),ident(Id) | IL],[number(N)|OL],State) :-
	!,
	defined_val(Id,State,N),
	replace_defined(IL,OL,State).
replace_defined( [ Tok | IL ], [ Tok | OL ], State) :-
	replace_defined(IL,OL,State).


defined_val(Id,State,1) :- 
	accessC2P(info_table, State, Table),
	avl_search(Id,Data,Table),
	Data \= unDefined,
	!.
defined_val(_,_,0).

/*
 * replace_ident_with0/2
 * 	replace all ident(Id) tokens by number(0)
 */

replace_ident_with0([], []).
replace_ident_with0( [ Tok | ILine], [ number(0) | OLine]) :-
	is_identifier(Tok,_),
	!,
	replace_ident_with0(ILine,OLine).
replace_ident_with0( [ Tok | ILine], [ Tok | OLine]) :-
	replace_ident_with0(ILine,OLine).


/*
 * macro_expand(Mode,Line,OutLine,State)
 *
 *	Takes a Line as the input and produces OutLine as the
 *	output.  Mode is either single or multiple which determines
 *  whether actual arguments of a macro should be found entirely
 *	in the input line (#if and #include processing), or more lines can
 *  be scanned (normal processing). IFD is the input file decriptor
 *	and OFD is the final file descriptor. Tab is the macro expansion
 *	symbol table.
 */

macro_expand(Mode,ILine,OLine,State) :- var(ILine), !, OLine = [].
macro_expand(Mode,[],[],State) :- !.
macro_expand(Mode,[Head|Rest],EL,State) :-
	macro_expand(Mode,Head,Rest,EL,State).

macro_expand(Mode,Tok,Rest,EL,State) :-    /* simple macro replacement */
	is_identifier(Tok,Id),
	accessC2P(info_table, State, Table),
	avl_search(Id,Defn,Table),
	!,
	do_expand(Mode,Defn,Tok,Rest,EL,State).
macro_expand(Mode,Token1,Rest,[Token|ER],State) :-  /* (##) concatenation */
	nonvar(Rest),
	Rest = [poundpound|Rest2],
	macro_expand(Mode,Rest2,[Token2|ER],State),
	concat_tokens([Token1,Token2],Token),
	!.
macro_expand(Mode,Token,Rest,[Token|ER],State) :-
	macro_expand(Mode,Rest,ER,State).


concat_tokens(TokList,Token) :-
	concat_tokens(TokList,[],Str,What),
	name(Name,Str),
	Token =.. [What,Name].	
	
concat_tokens([Tok],InStr,OutStr,What) :-
	!,
	tok_to_list(Tok,Str,What),
	append(InStr,Str,OutStr).
concat_tokens([Tok|Rest],InStr,OutStr,What) :-
	tok_to_list(Tok,Str,_),
	append(InStr,Str,Str1),
	concat_tokens(Rest,Str1,OutStr,What).

%
% do_expand/8
%

do_expand(Mode,d(Repl),_,Rest,EL,State) :-            % macro without arguments
	!,
	dappend(Repl,Rest,Again),
	macro_expand(Mode,Again,EL,State).
do_expand(Mode,da(FArgs,Defn),Token,Rest,EL,State) :- % macro with args
	!,
	append(Rest,_,RestOT),		% set tail to unbound
	getMArgs(Mode,AArgs,RestOT,EndMark,State),
	do_expand1(Mode,FArgs,AArgs,Defn,Token,RestOT,EndMark,EL,State).
do_expand(Mode,_,Token,Rest,[Token|EL],State) :-      % undef'ed symbol
	macro_expand(Mode,Rest,EL,State).


%
% do_expand1/11
%

do_expand1(Mode,FArgs,AArgs,Defn,Token,RestOT,EndMark,EL,State) :-
	length(FArgs,NArgs),
	length(AArgs,NArgs),
	!,
	do_expand_args(AArgs,EArgs,State),
	do_subst(Defn,FArgs,EArgs,OutDefn),
	dappend(OutDefn,EndMark,Again),
	macro_expand(Mode,Again,EL0,State),
	concat_adj_strings(EL0,EL).
do_expand1(Mode,_, _, _, Token, Rest, Tail, [Token|ER],State) :-
%	error('Mismatched arity in macro : %t\n',[Token],IFD),
	macro_expand(Mode,Rest,ER,State).
   

%
% do_expand_args/4
%	prescan the actual arguments of a macro for macro substitutions

do_expand_args([],[],_).
do_expand_args([Arg|IArgs],[EArg|OArgs],State) :-
	macro_expand(single,Arg,EArg,STate),
	do_expand_args(IArgs,OArgs,State).
	

/*
 * getMArgs(Mode,Args,InList,RestList,State)
 *
 *	Called to get the left paren, the actual arguments, and the right paren
 *	for a macro defined with a #define statement.
 */

getMArgs(Mode,Args,ILine,RLine,State) :-
	getmInput(Mode,ILine,State),
	getMArgs1(Mode,Args,ILine,RLine,State).

getMArgs1(Mode,Args,[lparen| IL],RL,State) :-
	!,
	getmInput(Mode,IL,State),
	getMArgs2(Mode,Args,IL,RL,State).
getMArgs1(Mode,noargs,L,L,State).

getMArgs2(Mode,[],[rparen| L],L,State) :- !.
getMArgs2(Mode,[FA|RA],IL,RL,State) :-
	getMArg(Mode,FA,IL,L1,State),
	getmInput(Mode,L1,State),
	moreMArgs(Mode,RA,L1,RL,State).

%
% getMArg/6
%

getMArg(Mode,[],[rparen|T],[rparen|T],State) :- 
	!.
getMArg(Mode,[],[comma|T],[comma|T],State) :- 
	!.
getMArg(Mode,[lparen|More],[lparen|IL],OL,State) :-
	!,
	getmInput(Mode,IL,State),
	consumeThruRight(Mode,More,Tail,IL,L1,State),
	getmInput(Mode,L1,State),
	getMArg(Mode,Tail,L1,OL,State).
getMArg(Mode,[Any|More],[Any|IL],OL,State) :-
	getmInput(Mode,IL,State),
	getMArg(Mode,More,IL,OL,State).

%
% moreMArgs/6
%

moreMArgs(Mode,[],[rparen|L],L,State) :-
	!.
moreMArgs(Mode,[FA|RA],[comma|IL],OL,State) :-
	getmInput(Mode,IL,State),
	getMArg(Mode,FA,IL,L1,State),
	getmInput(Mode,L1,State),
	moreMArgs(Mode,RA,L1,OL,State).

/*
 * consumeThruRight/7 is used to consume all characters up to a right
 *	paren (handling the case of nested left, right paren pairs) in
 *	getMArgs and getIArgs above.
 *
 */

consumeThruRight(Mode,[rparen|Tail],Tail,[rparen|Line],Line,State) :- !.
consumeThruRight(Mode,[lparen|More],Tail,[lparen|IL],OL,State) :-
	!,
	getmInput(Mode,IL,State),
	consumeThruRight(Mode,More,ITail,IL,L1,State),
	getmInput(Mode,L1,State),
	consumeThruRight(Mode,ITail,Tail,L1,OL,State).
consumeThruRight(Mode,[Any|More],Tail,[Any|IL],OL,State) :-
	getmInput(Mode,IL,State),
	consumeThruRight(Mode,More,Tail,IL,OL,State).


%
% getmLines/3
%	get more lines until we have a nonempty list of tokens
%

getmLines(Line,State) :-
	var(Line),
	getLine(L,State),
	!,
	openTail(L,Line),
	getmLines(Line,State).
getmLines([],State) :- !.
getmLines([Tok|Rest],State).

openTail(L,Line)
	:-
	var(L), !, L=Line.

openTail([Item],[Item | Tail])
	:-!.

openTail([Item | Rest], [Item | Tail])
	:-
	openTail(Rest, Tail).

%
% getmInput/4
%

getmInput(single,Line,State) :- var(Line), !, Line = [].
getmInput(single,Line,State).
getmInput(multiple,Line,State) :-
	getmLines(Line,State).


/*
 * do_substs(InLines,FArgs,AArgs,OutLines)
 *
 *	Called to perform substitions in a list of lines where FArgs is
 *	a list of the formal arguments and AArgs is a list (of same length)
 *	of the actual arguments
 */

do_substs([],_,_,[]) :- !.
do_substs([Line|Lines],FArgs,AArgs,[OutLine|OutLines]) :-
	do_subst(Line,FArgs,AArgs,OutLine),
	do_substs(Lines,FArgs,AArgs,OutLines).

/*
 * do_subst(InLine,FArgs,AArgs,OutLine)
 *
 *	Called to perform substitions within a single line where FArgs is
 *	a list of formal Arguments and AArgs is a list (of same length) of
 *	the actual arguments.  The input line is InLine; the output line
 *	is OutLine.
 */

do_subst([],_,_,[]) :- !.
do_subst(Line,[],_,Line) :- !.
do_subst([pound,Tok|Rest],FArgs,AArgs,[string(String)|Result]) :-
	is_identifier(Tok,Id),
	assoc(Id,FArgs,AArgs,Subst),
	concat_tokens(Subst,NewTok),
	NewTok =.. [_,String],
	!,
	do_subst(Rest,FArgs,AArgs,Result).	
do_subst([Tok|Rest],FArgs,AArgs,Result) :-
	is_identifier(Tok,Id),
	assoc(Id,FArgs,AArgs,Subst),
	dappend(Subst,ResTail,Result),
	!,
	do_subst(Rest,FArgs,AArgs,ResTail).
do_subst([Tok|Rest],FArgs,AArgs,[Tok|ResTail]) :-
	do_subst(Rest,FArgs,AArgs,ResTail).

concat_adj_strings([],[]).
concat_adj_strings([string(Tok1),string(Tok2)|Rest],Result) :-
	!,
	atom_concat(Tok1,Tok2,Tok3),
	concat_adj_strings([string(Tok3)|Rest],Result).
concat_adj_strings([Tok|Rest],[Tok|Result]) :-
	concat_adj_strings(Rest,Result).

/*
 * assoc/4			-- associate a value with a key.
 */

assoc(Key,[Key|_],[RVal|_],RVal) :- 
	!.
assoc(Key,[_|T1],[_|T2],RVal) :- 
	assoc(Key,T1,T2,RVal).

endmod.
/*=================================================================*
 |		cout.pro
 |	Copyright(c) 1991-94 Applied Logic Systems Inc.
 |
 |		Output formatter for C translator
 |
 | Author : P.Raman
 |
 | Created : 9/23/91
 | Revision History :
 |
 | Major predicates :
 |	output_openFile/1	-- set up output control flags
 |	output_closeFile/1	-- remove output control flags
 |	output_exclude/5	-- set up exclude spec for files
 |	output_begin_silent/2- stop all output
 |	output_end_silent/2	-- resume output
 |	output_header/2		-- print header
 |	output_footer/2		-- print footer
 |	output_macro/3		-- outputs a macro identifier
 |	output_func_gvar/4	-- output a function/gvar spec
 |	output_type/5		-- output a c type/struct/union spec
 |	output_const/3		-- output a constant spec
 |	debug/2, debug/3	-- conditionally print debugging messages
 |	error/2, error/3	-- error reporting
 *=================================================================*/

module c2pro.

	%--------------------------------------- 
	% output_openFile/2.
	% 	print open file message and sets up output control predicates
	%

output_openFile(FName,OutStream)
	:-
	setup_skips(FName,OutStream).

setup_skips(FName,OutStream)
	:-		% already skipping
	skip_item(PrevName,all),
	FName \= PrevName,		% this makes sure that skip flags are
	!.				% are stacked when a file includes itself
setup_skips(FName,OutStream)
	:- 
	name(FName,FStr),
	parsePathStr(FStr,Disk,Comps,PathType),
	match_exclude(Disk,Comps,PathType,What),
	!,
	setup_skips_finish(FName,What,OutStream).
setup_skips(FName,OutStream)
	:-
	check_printf(OutStream,'\n/* Open file %t */\n\n',[FName]).

setup_skips_finish(FName,all,OutStream)
	:- !, 
	assert(skip_item(FName,all)).
setup_skips_finish(FName,What,OutStream)
	:-
	check_printf(OutStream,'\n/* Open file %t */\n\n',[FName]),
	setup_group_skips(What,FName).

setup_group_skips([],_).
setup_group_skips([Item|Rest],FName)
	:-
	assert(skip_item(FName,Item)),
	setup_group_skips(Rest,FName).

	%---------------------------------------------------------------
	%	For an #exclude spec on a user file (i.e #exclude "foo"),
	%	the pattern and incoming file must unify.
	%
	%	For an #exclude spec on a system file (i.e #exclude <foo>),
	%	we assume that the incoming file must be of abs path type,
	%	and the pattern must be of rel path type, and we use
	%	the includePath predicate to provide possible locations
	%	to complete the pattern path specification, and the
	%	completed pattern must unify with input file spec.
	%---------------------------------------------------------------

match_exclude(Disk,Comps,PathType,What)
	:-
	exclude_files(ExclType,EDisk,EComps,EPathType,What),
	match_exclude(ExclType,Disk,EDisk,Comps,EComps,PathType,EPathType).

match_exclude(_,Disk,Disk,Comps,Comps,PathT,PathT).
match_exclude(system,Disk,EDisk,Comps,EComps,abs,rel)
	:-
	includePath(Disk,InclComps,abs),
	append(InclComps,EComps,Comps).

	%---------------------------------------
	% Output_closeFile/2.
	%

output_closeFile(FName,OutStream)
	:-
	debug(1,'\nCLOSING FILE %t\n',[FName]),
	remove_skips(FName,OutStream).

remove_skips(FName,OutStream)
	:- 
	retract(skip_item(FName,all)),
	!.
remove_skips(FName,OutStream)
	:- 
	skip_item( _,all), !.
remove_skips(FName,OutStream)
	:- 
	check_printf(OutStream,'\n/* Close file %t */\n\n',[FName]),
	retract_skip(FName,func),
	retract_skip(FName,const),
	retract_skip(FName,ctype),
	retract_skip(FName,gvar).


retract_skip(FName,Type)
	:- 
	retract(skip_item(FName,Type)), !.
retract_skip( _, _).

check_printf(OutStream,Fmt,Args)
	:- 
	silent, !.
check_printf(OutStream,Fmt,Args)
	:- 
	printf(OutStream,Fmt,Args),
	flush_output(OutStream).

	%---------------------------------------
	% output_exclude/5.
	%	assert exclude specification
	%

output_exclude(ExclType,Disk,Comps,PathType,What)
	:- 
	assert(exclude_files(ExclType,Disk,Comps,PathType,What)).

	%---------------------------------------
	% output_begin_silent/2
	%

output_begin_silent(FD,Tab)
	:- assert(silent).


	%---------------------------------------
	% output_end_silent/2
	%

output_end_silent(FD,Tab)
	:-
	retract(silent), !.
output_end_silent(FD,Tab).

	%---------------------------------------
	% output_header/2.
	%

output_header(FileName,FD,Tab)
	:-
	printf('/* Generated from file %t */\n\n',[FileName]).

	%---------------------------------------
	% output_footer/2.
	%

output_footer(State).


	%---------------------------------------
	% output_macro/3.
	%

output_macro(Id,State)
	:-
	(silent; skip_item(_,all)), !.
output_macro(Id,State)
	:-
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'als_macro(\'%t\').\n',[Id]).

	%---------------------------------------
	% output_func_or_gvar/4.
	%

output_func_gvar(DSpec,Declt,State)
	:-
	accessC2P(info_table, State, Tab),
	deref(DSpec,Tab,RSpec,RDeclt,PSpec),
	scan_declt(Declt,ident(Name),ADeclt,PDeclt),
	append(PDeclt,PSpec,PType),
	output_func_gvar(PType,Name,DSpec,Declt,RSpec,RDeclt,ADeclt,State).

	%---------------------------------------
	%	clause 1 : int func(params);
	%	clause 2 : typedef int foo(params); foo func;
	%	clause 3 : ... gvar
	%

output_func_gvar([func|PRetType],Name,DSpec,Declt,
		RSpec,RDeclt,ADeclt,State)
	:-
	scan_declt(Declt,func( ident(Name),Params),ADeclt1,PDeclt),
	!,
	output_func(Name,DSpec,ADeclt1,Params,PRetType,State).
output_func_gvar([func|PRetType],Name,DSpec,Declt,
		RSpec,RDeclt,ADeclt,State)
	:-
	scan_declt(RDeclt,func( ident(_),Params),ADeclt1,PDeclt),
	!,
	output_func(Name,RSpec,ADeclt1,Params,PRetType,State).
output_func_gvar(PRetType,Name,DSpec,Declt, RSpec,RDeclt,ADeclt,State)
	:-
	output_gvar(Name,DSpec,Declt,ADeclt,PRetType,State).
	

output_func(Name,_,_,_,_,State)
	:-
	(silent; skip_item(_,all); skip_item(_,func)), !.

output_func(Name,DSpec,ADeclt,Params,PRetType,State)
	:- 
	dspec_to_str(DSpec,DSpecStr),
	adeclt_to_str(ADeclt,ADecltStr),
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'\nret(\'%s %s\')/%t=\'%t\'(',[DSpecStr,ADecltStr,PRetType,Name]),
	output_params(Params,State),
	accessC2P(inFile,State,FName),
%	fileName(FD,FName),
	pathPlusFile(_,File,FName),
	!,
	printf(OutStream,')/\'%t\'.\n',[File]).
output_func(Name,_,_,_,_,State)
	:-
	error('output_func for %t failed\n',[Name],State).

output_params( [], State)
	:- !, 
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'void/[void]',[]).
output_params( Params, State)
	:-
	output_neparams(Params,State).

output_neparams( [] ,_).
output_neparams( [ param(DSpec,Declt) | Rest ] ,State)
	:-
	dspec_to_str(DSpec,DSpecStr),
	scan_declt(Declt,ident(_),ADeclt,PDeclt),
	adeclt_to_str(ADeclt,ADecltStr),
	accessC2P(info_table,State,Tab),
	deref(DSpec,Tab,_,_,PSpec),
	append(PDeclt,PSpec,PType),
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'\'%s %s\'/%t',[DSpecStr,ADecltStr,PType]),
	print_comma_cond(Rest,State),
	output_neparams(Rest,State).


output_gvar(Name,DSpec,Declt,ADeclt,PType,State)
	:-
	( silent; skip_item( _, all); skip_item( _,gvar)), !.
output_gvar(Name,DSpec,Declt,ADeclt,PType,State)
	:-
	dspec_to_str(DSpec,DSpecStr),
	adeclt_to_str(ADeclt,ADecltStr),
%	fileName(FD,FName),
	accessC2P(inFile,State,FName),
	pathPlusFile(_,File,FName),
	!,
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'\nals_gvar(\'%t\',\'%s %s\',%t)/\'%t\'.\n\n',
		[Name,DSpecStr,ADecltStr,PType,File]).
output_gvar(Name,DSpec,Declt,ADeclt,PType,State)
	:-
	error('Error in output_gvar for global variable %t\n',[Name],State).


	%---------------------------------------
	% output_type/4.
	%	output prolog spec for a C type
	%

output_type(Name,DSpec,Declt,State)
	:-
	(silent; skip_item(_,all); skip_item(_,ctype)), !.
output_type(Name,DSpec,Declt,State)
	:-
	accessC2P(info_table,State,Tab),
	deref(DSpec,Tab,RSpec,RDeclt,PSpec),
	scan_declt(Declt,ident(Name),ADeclt,PDeclt),
	append(PDeclt,PSpec,PType),
	output_type(Name,RSpec,RDeclt,PType,State).

output_type(Name,RSpec,RDeclt,PType,State)
	:-
	( PType = [struct|_]; PType = [union|_]),
	get_type(RSpec,Type),
	get_struct(Type,SSpec,What),
	accessC2P(info_table,State,Tab),
	get_structdef(SSpec,What,Tab,Fields),
	!,
	output_struct(Name,Fields,State).
output_type(Name,_,_,PType,State)
	:-
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'\nals_type(\'%t\',%t).\n',[Name,PType]).


output_struct(Name,Fields,State)
	:-
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'\nals_struct(\'%t\',[',[Name]),
	accessC2P(info_table,State,Tab),
	output_components(Fields,'',State),
	printf(OutStream,']).\n',[]),
	!.
output_struct(Name,Fields,State)
	:-
	error('Error in output_struct for struct(%t,%t)\n',[Name,Fields],State).
	
output_components( [] ,_,_).
output_components( [ field(DSpec,Declts) | Rest ] ,Prefix,State)
	:-
	accessC2P(info_table,State,Tab),
	deref(DSpec,Tab,_,_,PSpec),
	output_fields(Declts,DSpec,PSpec,Prefix,State),
	print_comma_cond(Rest,State),
	output_components(Rest,Prefix,State).
	
output_fields( [] ,_,_,_,_).
output_fields( [ Field | Rest ] ,DSpec,PSpec,Prefix,State)
	:-
	output_field( Field, DSpec, PSpec, Prefix, State),
	print_comma_cond(Rest,State),
	output_fields( Rest, DSpec,PSpec,Prefix,State).

output_field( Field, DSpec, PSpec, Prefix, State)
	:-
	get_type(DSpec,Type),
	get_struct(Type,SSpec,What),
	get_fields(SSpec,SubFields),				% substructure found
	scan_declt(Field,ident(Name),_,[]),			% simple declarator
	!,
	catenate([Prefix,Name,'.'],NPrefix),
	output_components(SubFields,NPrefix,State).
output_field( Field,DSpec,PSpec,Prefix,State)
	:-
	scan_declt(Field,ident(Name),_,PDeclt),
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'\'%t%t\'=[',[Prefix,Name]),
	append(PDeclt,PSpec,PType),
	print_ptype(PType,DSpec,State).

	%---------------------------------------
	% print_ptype is used to print out the prolog type-list
	% for fields of a structure or union. It prints out the
	% type-list as is except for the last item. If the last item
	% is a struct or union, then it prints the declaration specifier.
	% This information is used in cutils.pro to access sub, sub-sub
	% and sub-sub-sub etc fields of a structure.

print_ptype(PType,DSpec,State)
	:-
	( PType = [struct|_]; PType=[union|_] ),
	!,
	dspec_to_str(DSpec,DSpecStr),
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'\'%s\']',[DSpecStr]).
print_ptype([Item],_,State)
	:- 
	!,
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'%t]',[Item]).
print_ptype([Item|Rest],DSpec,State)
	:-
	accessC2P(outStream,State,OutStream),
	printf(OutStream,'%t,',[Item]),
	print_ptype(Rest,DSpec,State).

	%---------------------------------------

print_comma_cond([],State).
print_comma_cond([_|_],State)
	:- 
	accessC2P(outStream,State,OutStream),
	printf(OutStream,',\n',[]).

	%---------------------------------------
	% output_const/4.
	%

output_const(Name,Type,State)
	:- 
	( silent; skip_item( _,all); skip_item( _,const)), 
	!.
output_const(Name,Type,State)
	:- 
	accessC2P(outStream, State, OutStream),
	printf(OutStream,'als_const(\'%t\',%t).\n',[Name,Type]).

	%---------------------------------------
	% debug/1.
	% debug/2.
	%

debug_default_level(1).

get_debug_level(Level)
	:-
	cur_debug_level(Level),!.
get_debug_level(Level)
	:-
	debug_default_level(1).

debug(Fmt)
	:-
	debug(Fmt,[]).

debug(Fmt,Args)
	:- 
	debug_default_level(Level),
	debug(Level,Fmt,Args).

debug(Level,Fmt,Args)
	:-
	get_debug_level(SysLevel),
	Level =< SysLevel,
	!,
	printf(user,Fmt,Args),
	flush_output(user).
debug(_,_,_).

dot_debug(Level,Fmt,Args)
	:-
	get_debug_level(SysLevel),
	dot_debug(SysLevel,Level,Fmt,Args).

dot_debug(SysLevel,Level,Fmt,Args)
	:-
	Level =< SysLevel,
	!,
	printf(user,Fmt,Args),
	flush_output(user).
dot_debug(SysLevel,Level,Fmt,Args)
	:-
	0 < SysLevel,
	!,
	put_atom(user,'.'),
	flush_output(user).
dot_debug(SysLevel,Level,Fmt,Args).

decln_debug(Level,DeclnSpec,DecltList,State)
	:-
	get_debug_level(SysLevel),
	decln_debug(SysLevel,Level,DeclnSpec,DecltList,State).

decln_debug(SysLevel,Level,DSpec,Declts,State)
	:-
	Level =< SysLevel,
	!,
	printf(user, 'DSPEC: %t   \nDECLN: %t\n',[DSpec,Declts]),
	flush_output(user),
	decln_break(Declts).
decln_debug(SysLevel,Level,DeclnSpec,DecltList,State).

decln_break(Declts)
	:-
	Declts = [ident(Item)],
	decln_break_on(Item),
	trace.
decln_break(_).

/*
 * error/2
 * error/3
 */

error(Fmt,State)
	:-
	error(Fmt,[],State).

error(Fmt,Args,0)
	:-!,
	printf(error_stream,Fmt,Args).

error(Fmt,Args,State)
	:-
	accessC2P(inFile, State, FileName),
	accessC2P(curLineNum, State, LineNum),

	name(Fmt, FmtCs),
	append("File %t, line %t: ",FmtCs,NewFmtCs), 
	name(NewFmt, NewFmtCs),
	append([FileName,LineNum],Args,NewArgs),

	printf(error_stream,NewFmt,NewArgs).

endmod.

/*=================================================================*
 | 		cmisc.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 | 		Miscellaneous predicates
 |
 | Major predicates :
 | 	isNumeric/2	-- returns the value of a decimal char
 |	isHex/2		-- returns the value of a hex char
 |	isOctal/2	-- returns the value of a Octal char
 |	isAlpha/1	-- tests for an alphabetic char
 |	buildName/3	-- make a symbol out of two symbols
 |	reverse/2	-- reverse a list
 |	deleteLast/2-- deletes the last item of a non-empty list
 *=================================================================*/

module c2pro.

%
% isNumeric is a map between decimal numeric characters 
% and the numbers that they represent.
%

isNumeric(NC, Num) :- 0'0 =< NC, NC =< 0'9, Num is NC - 0'0.


%
% isHex is a map between Hexadecimal characters and the numbers 
% that they represent.
%

isHex(NC, Num) :- isNumeric(NC,Num), !.
isHex(NC, Num) :- 0'a =< NC, NC =< 0'f, !, Num is NC-0'a+10.
isHex(NC, Num) :- 0'A =< NC, NC =< 0'F, Num is NC-0'A+10.

%

isOctal(NC, Num) :- 0'0 =< NC, NC =< 0'7, Num is NC - 0'0.


%
% isAlpha(Char) succeeds when Char is considered to be an 
% alphabetic character.
%

isAlpha(Char) :- 0'a =< Char, Char =< 0'z, !.
isAlpha(Char) :- 0'A =< Char, Char =< 0'Z, !.
isAlpha(0'$).
isAlpha(0'_).

isAlphaNumeric(Char) :- isAlpha(Char), !.
isAlphaNumeric(Char) :- isNumeric(Char,_).

/*
%
% buildName/3
% 	make a symbol out of two input symbols
%

buildName(N,S,O) :-
	name(N,NS),
	name(S,SS),
	dappend(NS,SS,OS),
	name(O,OS).
*/

%
% deleteLast/2
%

deleteLast([_],[]) :- !.
deleteLast([X|In],[X|Out]) :- deleteLast(In,Out).

%
% cleanDB/0
%	-- abolish all asserted predicates
%

cleanDB
	:-
	abolish(sourcePath,1),
	abolish(includePath,3),
	reset_includePath,
	abolish(currentPath,3),
	abolish(imported,1),
	abolish(skip_item,2),
	abolish(exclude_files,5).

reset_includePath :-
	defaultIncludePath(A,B,C),
	assertz(includePath(A,B,C)),
	fail.
reset_includePath.

endmod.
/*========================================================================
 | 			cparse.pro
 |	Copyright (c) 1990-4 Applied Logic Systems, Inc.
 |
 |		Parsing phase for C language translator
 |
 | Author: Kevin A. Buettner
 | Creation: 4/1/90
 | Revision History:
 | 		10/22/91 -- P. Raman -- modified (extensively) to parse subset of C
 |
 | Majro predicates :
 |
 | read_file/3		-- top level goal for C translator
 | push_file_stack/3	-- push a new file info frame onto a stack
 | type_spec/6		-- parses a type specifier (used by cexp)
 | abstract_declt/6	-- parses an abstarct declarator (used by cexp)
 | adeclt_to_str/2	-- converts an abstract declarator to string
 | scan_declt/4		-- construct an abstract declarator and type-list
 | dspec_to_str/2	-- convert a declaration specifier to string
 | deref/5			-- derefernce a declaration specifier
 | get_type/2		-- extract the type from a declaration specifier
 | get_struct/3		-- explode a structure or union declarator
 | get_tag/2		-- get the tag of a struct/union/enum specification
 | get_fields/2		-- get the fields of a strcut/union specification
 | get_structdef/4	-- get the structure definition of named structure
 | is_identifier/2	-- check whether a token is an identifier
 | getStatements/1	-- test for extracting C statements from input file
 |
 | Special input constructs :
 | ------------------------
 |
 | #exclude FileSpec/TypeSpec
 |		inhibits output for one or more groups for the specified
 |		file(s). FileSpec can be "PathName" or <PathName> with
 |		wild cards "?" (which matches a single directory component)
 |		and "*" (which matches any trailing components). TypeSpec
 |		is optional, and can be
 |			a single symbol from func, const, ctype, gvar
 |			the symbol "all" that stands for all the above symbols
 |			or a list [item,item,...] to be excluded.
 | als$begin_silent;
 | als$end_silent;
 |		they are used in pairs to suppress output between these two
 |		occurrences. The pair can straddle file boundaries and can
 |		also be nested.
 | als$ctype(Type)
 | als$const("Name")
 |		Type is one of : int,char,short,long,str,ptr,float,double
 *=======================================================================*/

module c2pro.

	%
	% read_file/3
	%	parses a file (stream) containing C statements and emits prolog
	%	specifications.  "Defines" is a list of symbol-value initializers 
	%	specified in the command line.

read_file(InitInFile, State, Defines) 
	:-
	openFile(InitInFile,State),
	accessC2P(info_table,State,Table),
	load_defines(Defines, State, Table, LoadedTable),
	setC2P(info_table,State,LoadedTable),
	read_lines([], State).

	%---------------------------------------------
	% load -Ddefine_constants into global table
	%

load_defines([],State,Tab,Tab).
load_defines([define(Id,Val)|Rest],State,ITab,OTab) :-
	addToTable(Id,d(Val),State,ITab,Tab1),
	load_defines(Rest,State,Tab1,OTab).

	%---------------------------------------------
	% read_lines/2
	%

read_lines(Line, State) :-
	get_statement(Line, Stmt, State),
	!,
	dot_debug(2,'\nPARSING STATEMENT : %t\n',[Stmt]),
	parse_statement(State,Stmt,Rest),
	read_lines(Rest,State).
read_lines(Line,State) :-
	output_footer(State).

%%--------------------------------------------------------------
%%	Get a statement from the input file.
%%	Lines are read from the input file and C style macro 
%%	processing is performed until a top-level semicolon (;) or
%%	@end (for Obj-C) is seen. 
%%--------------------------------------------------------------
	%-----------------------
	% get_statement/3
	%

get_statement(RemL,Stmt,State)
	:-
	get_statement1([stk_bot],RemL,Stmt,State).

	%-----------------------
	% get_statement1/4
	%

get_statement1([],Line,Line,State).		% empty stack (done)

get_statement1([S|SS],Rem,Stmt,State)
	:-
	get_statement2(Rem,Stmt,[S|SS],State).

	%-----------------------
	% get_statement2/4
	%

get_statement2([],Stmt,Stk,State)
	:-	% get more input
	get_next_line(Line,State),
	process_line(Line,Line1,State),
	get_statement2(Line1,Stmt,Stk,State).

get_statement2([Tok|Toks],Stmt,Stk,State)
	:-
	check_for_end(Stk,Stk1,[Tok|Toks],_),
	append([Tok|Toks],RStmt,Stmt),
	get_statement1(Stk1,[],RStmt,State).

	%-----------------------
	% get_next_line/2
	%

get_next_line(Line,State)
	:-
	getLine(Line,State),
	debug(4,'Line : %t\n',[Line]),
	!.
get_next_line(Line,State) 				% eof
	:-			
	pop_context(State),
	get_next_line(Line,State).

	%
	% check_for_end/4
	% 	check for top level semicolon (or @end for Obj-C)

check_for_end(Stk,Stk,[],[]) :- !.		% empty input
check_for_end([],[]) --> !.				% empty stack
check_for_end([stk_bot],[]) -->		% pop stack
	[semicolon], !.
check_for_end([at_end,stk_bot],[]) -->	% (Obj-C)
	[at,ident(end)], !.
check_for_end([rcurly,inline,stk_bot],[]) -->
	[rcurly], !.
check_for_end([Tok|Stk],RStk) -->		% pop simple token
	[Tok], !,
	check_for_end(Stk,RStk).
check_for_end(Stk,RStk) -->				% push tok (Obj-C)
	[at,ident(Tok)],
	{ at_symbol(Tok) },
	!,
	check_for_end([at_end|Stk],RStk).
check_for_end(Stk,RStk) --> [Tok], 		% push tok
	{ matched_pair(Tok,MTok) },	
	!,
	check_for_end([MTok|Stk],RStk).
check_for_end(Stk,RStk) --> [Tok],		% skip token
	check_for_end(Stk,RStk).

at_symbol(interface).
at_symbol(implementation).
at_symbol(protocol).

matched_pair(lparen,rparen).
matched_pair(lbrac,rbrac).
matched_pair(lcurly,rcurly).
matched_pair(ident(inline),inline).


%%-------------------------------------------------------
%%
%%	parse statements that can be seen in a .spc file
%%
%%-------------------------------------------------------
	
% parse_statement/5

parse_statement(State) -->				% als$... stmts
	als_statement(State),
	!.
parse_statement(State) -->		% Obj-C @stmts
	[at],
	!,
	objc_statement(State).
parse_statement(State) -->
	decln_specs(State,DeclnSpec),
	declt_list(State,DecltList),
	consume_semicolon(State),
	!,
	{	decln_debug(3,DeclnSpec,DecltList,State),
		process_decln(DeclnSpec,DecltList,State) }.
parse_statement(State,Toks,[]) :-
	error('parse failed :\nDiscarded tokens %t\n',[Toks],State).

consume_semicolon(State) --> [semicolon], !.
consume_semicolon(State,Toks,[]) :-
	error('parse error\nParsed successfully %t\nI don\'t understand %t\nRemaining tokens %t\n\n',
		[DSpec,Declt,Toks],State).

%%-------------------------------------------------------
%%
%%	statements that are not part of standard C
%%
%%-------------------------------------------------------

% als_statement/4
%	recognizes the following statements in the source file
%	als$begin_silent;
%	als$end_silent;
%	als$ctype(Type);
%	als$const('Name');

als_statement(State) -->
	[ident('als$begin_silent'),semicolon], !,
	{ output_begin_silent(State) }.
als_statement(State) --> 
	[ident('als$end_silent'),semicolon],
	{ output_end_silent(State) }.
als_statement(State) --> 
	[fident('als$ctype'),lparen,ident(Name),rparen,semicolon], 
	!,
	{ 
	  accessC2P(info_table,State,Table),
	  find_type(Name,Table,DSpec,Declt),
	  output_type(Name,DSpec,Declt,State)
	}.
als_statement(State) -->
	[fident('als$const'),lparen,string(CName),rparen,semicolon],
	!,
	{ 
	  accessC2P(info_table,State,Table),
	  avl_search(CName,Data,Table),
	  check_const(CName,Data,State)
	}.

%
% objc_statement/5
%	recognizes following Obj-C constructs
%	@{interface/protocol} Name(Category) .... @end
%	@{interface/protocol} Name [:SuperClass]
%		[{ [@public fields] [@private fields]}] @end
%	@class Declts;

objc_statement(State) -->
	[ident(Type),Tok,lparen,ident(Category),rparen],
	{ interfaceOrProtocol(Type),
	  is_identifier(Tok,Id) },
	!,
	check_for_end([at_end,stk_bot],_).
objc_statement(State) -->
	[ident(Type),Tok],
	{ interfaceOrProtocol(Type),
	  is_identifier(Tok,Class) },
	!,
	superclass_opt,
	instance_variables_opt(State,Public),
	{ 
	  accessC2P(info_table,State,Table),
	  insert_type(ident(Class),
		[type=struct([fields=Public])],ident('0none'),Table,OutTab),
	  setC2P(info_table,State,OutTab)
	},
	check_for_end([at_end,stk_bot],_).
objc_statement(State) -->
	[ident(class)],
	declt_list(State,DecltList),
	consume_semicolon(State,[at,ident(class)],DecltList),
	{ process_class_decln(DecltList,State) }.


process_class_decln([],State).
process_class_decln([Declt|Rest],State) :-
	scan_declt(Declt,ident(Name),_,_),
	accessC2P(info_table,State,Table),
	insert_type(ident(Name),[type=int],Declt,Table,OutTable),
	setC2P(info_table,State,OutTable),
	process_class_decln(Rest,State).

interfaceOrProtocol(interface).
interfaceOrProtocol(protocol).

instance_variables_opt(State,Public) --> [lcurly], !,
	public_field_list_opt(State,Public),
	other_field_lists_opt(State,InTab),
	[rcurly].
instance_variables_opt(State,[]) --> [].

superclass_opt --> [ colon, ident(Id)], !.
superclass_opt --> [].

public_field_list_opt(State,Public) --> 
	[at,ident(public)],
	!,
	field_list(State,Public).
public_field_list_opt(State,[]) --> [].

other_field_lists_opt(State) -->
	[at,ident(_)],
	!,
	field_list(State,_,_),
	other_field_lists_opt(State).
other_field_lists_opt(State) -->
	field_list(State,_,_),
	!.
other_field_lists_opt(State) --> [].
	


%%-------------------------------------------------------------
%%
%%	rules defining a declaration specifier (lhs of declaration)
%%
%%-------------------------------------------------------------

% decln_specs/6

decln_specs(State,DeclnSpec) -->
	storage_class_spec(StorageSpec),
	!,
	{ append(StorageSpec,OtherSpec,DeclnSpec) },
	decln_specs(State,OtherSpec).
decln_specs(State,DeclnSpec) -->
	type_spec(State,TypeSpec),
	!,
	{ append(TypeSpec,OtherSpec,DeclnSpec) },
	decln_specs_more(State,OtherSpec).
decln_specs(State,[]) --> [].

%
% decln_spec_more/6  (non deterministic)
%	there is no ! in the first clause because
%	we want to allow for the possiblity that the tokens
%	we parsed as being additional declaration specifiers
%	may in fact be part of the declarator.
%

decln_specs_more(State,Spec) -->
	type_spec(State,TypeSpec),			
	{ append(TypeSpec,OtherSpec,Spec) },
	decln_specs_more(State,OtherSpec).
decln_specs_more(State,[]) --> [].				


%
% storage_class_spec/3
%

storage_class_spec([storageClass=Class]) --> 
	[ident(Class)], {storage_spec(Class)}.

storage_spec(typedef).
storage_spec(static).
storage_spec(extern).
storage_spec(register).
storage_spec(auto).
storage_spec(pascal).			% Mac specific

%
% type_spec/2
%

type_spec(State,[]) --> 				% const
	[ident(const)], 
	!.
type_spec(State,[type=int]) --> 	% enum [tag] { ID[=Val] }
	[ident(enum)],
	!, 
	enum_type_spec(State).
type_spec(State,Spec) --> 				% [long] float/double
	long_opt, 
	float_type_spec(Spec), 
	!.
type_spec(State,Spec) -->				% [signed/unsigned] int
	int_type_spec(Spec),
	!.
type_spec(State,[type=struct(Spec)]) -->  % struct
	[ident(struct)], 
	!,
	struct_or_union_type_spec(State,Spec).
type_spec(State,[type=Spec]) --> 				% typedef name
	typedef_name(State,Spec), 
	!.
type_spec(State,[type=union(Spec)]) --> 	% union
	[ident(union)],
	!,
	struct_or_union_type_spec(State,Spec).
type_spec(State,[type=void]) --> 				% void
	[ident(void)],
	!.
type_spec(State,[]) --> 						% volatile
	[ident(volatile)].

%
%	optional long qualifier for float/double
%

long_opt --> [ident(long)], !.
long_opt --> [].

%
% enum_type_spec/5
% 	enumeration type specifier
%

enum_type_spec(State) --> 
	enum_tag_opt,
	[lcurly],
	!,
	enum_def(State).
enum_type_spec(State) --> [ident(Tag)], !.
enum_type_spec(State) --> [Tok],
	{ error('Unexpected token %t in enum statement\n',[Tok],State) }.

enum_tag_opt --> [ident(Tag)], !.
enum_tag_opt --> [].

enum_def(State) --> 
	enum_const(State),
	!, 
	enum_def_rest(State).
enum_def(State) --> [Tok], 
	{ error('Expecting constant expression, found %t\n',[Tok],State) }.
	
enum_const(State) --> 
	[ident(Item)],
	{ output_const(Item,int,State) },
	enum_exp_opt(Item,State).

enum_exp_opt(Item,State) --> [equal], !, 
	expression(Val,State),
	{ 
	  accessC2P(info_table, State, InTable),
	  avl_insert(assign(Item),val(Val),InTable,OutTable),
	  setC2P(info_table, State, OutTable)
	}.
enum_exp_opt( _, State) --> [].


enum_def_rest(State) --> [rcurly], !.
enum_def_rest(State) --> [comma], !,
	enum_def(State).
enum_def_rest(State) --> [Tok],
	{ error('Expecting , or } in enum declaration, instead found %t\n',
		[Tok],State) }.

%
% float_type_spec/3
%


float_type_spec([type=float]) --> [ident(float)], !.
float_type_spec([type=double]) --> [ident(double)], !.
float_type_spec([type=extended]) --> [ident(extended)].

%
% int_type_spec/3
%

int_type_spec(Spec) --> [ident(signed)], !, signed_type(Spec).
int_type_spec(Spec) --> [ident(unsigned)], !, unsigned_type(Spec).
int_type_spec([type=long]) --> [ident(long)], !, int_opt.
int_type_spec([type=short]) --> [ident(short)], !, int_opt.
int_type_spec([type=int]) --> [ident(int)], !.
int_type_spec([type=char]) --> [ident(char)], !.
int_type_spec([type=comp]) --> [ident(comp)].	% 64 bit int for Mac

signed_type([type=long]) --> [ident(long)], !, int_opt.
signed_type([type=short]) --> [ident(short)], !, int_opt.
signed_type([type=int]) --> [ident(int)], !.
signed_type([type=char]) --> [ident(char)], !.
signed_type([type=int]) --> [].

unsigned_type([type=unsigned_long]) --> [ident(long)], !, int_opt.
unsigned_type([type=unsigned_short]) --> [ident(short)], !, int_opt.
unsigned_type([type=unsigned_int]) --> [ident(int)], !.
unsigned_type([type=unsigned_char]) --> [ident(char)], !.
unsigned_type([type=unsigned_int]) --> [].

int_opt --> [ident(int)], !.
int_opt --> [].

%
% struct_or_union_type_spec/6
%

struct_or_union_type_spec(State,[fields=List|TagList]) -->	% Obj-C
	struct_tag_opt(TagList),
	[lcurly,at,Tok,lparen,ident(Class),rparen,rcurly],
	{ is_identifier(Tok,defs) },
	!,
	{ accessC2P(info_table, State, Tab),
	  find_type(ident(Class),Tab,[type=struct([fields=List])],_) }.

struct_or_union_type_spec(State,[fields=List|TagList]) --> 
	struct_tag_opt(TagList),
	[lcurly],
	!,
	field_list(State,List),
	[rcurly].
struct_or_union_type_spec(State,[tag=Tag]) --> 
	[ident(Tag)], !.
struct_or_union_type_spec(State,[]) -->
	[Tok],
	{ error('invalid struct or union reference, found %t\n',[Tok],State) }.

struct_tag_opt([tag=Tag]) --> [ident(Tag)], !.
struct_tag_opt([]) --> [].

field_list(State,[Field|Rest]) --> 
	comp_decln(State,Field),
	!,
	field_list(State,Rest).
field_list(State,[]) --> [].

comp_decln(State,field(DSpec,DecltList)) --> 
	decln_specs(State,DSpec), 
	comp_declt_list(State,DecltList).

comp_declt_list(State,[Declt|Rest]) --> 
	comp_declt(State,Declt),
	comp_declt_list_rest(State,Rest).

comp_declt_list_rest(State,[]) --> [semicolon], !.
comp_declt_list_rest(State,Rest) --> [comma], 
	comp_declt_list(State,Rest).

comp_declt(State,CDeclt) --> 
	declt(State,Declt),
	!,
	comp_declt_rest(State,Declt,CDeclt).
comp_declt(State,bit(ident('0none'),Width)) -->
	[colon],
	expression(Width,State).

comp_declt_rest(State,Declt,bit(Declt,Width)) -->
 	[colon],
	!,
	expression(Width,State).
comp_declt_rest(State,Declt,Declt) --> [].

%
% typedef_name/5
%

typedef_name(State,ident(Tok)) --> 
	[Token], 
	{ is_identifier(Token,Tok),
	  accessC2P(info_table, State, Table),
	  find_type(ident(Tok),Table,_,_) 
	}.

%%------------------------------------------------------------
%%
%%	rules defining a declarator
%%
%%------------------------------------------------------------

% declt/6

declt(State,XDeclt) -->
	declt1(State,Declt), 
	declt_rest(State,Declt,XDeclt).

declt1(State,qual(Qual,QualID,Declt)) -->			% near/far
	[ident(Qual)],											% pascal/huge
	{ qualifier(Qual,QualID) },
	!,
	declt(State,Declt).
declt1(State,paren(Declt)) --> [lparen], !, 		% ( declt )
	declt(State,Declt),
	[rparen].
declt1(State,Declt) --> [star], !,				% * declt
	ptr_declt(State,Declt).
declt1(State,Declt) -->								% simple declt
	simple_declt(Declt).


declt_rest(State,Declt,func(Declt,Params)) -->	% func declt
	[lparen],
	!,
	param_type_list(State,Params),
	after_func.
declt_rest(State,Declt,RDeclt) --> [lbrac], !,			% array declt
	array_bounds([lbrac],Toks),
	after_array(array(Declt,[lbrac|Toks]),RDeclt).			
declt_rest(State,Declt,Declt) --> [].					% other declt

%
% qualifier/2
%

qualifier('_Far', far).
qualifier('_far', far).
qualifier('_Dfar', far).
qualifier('_DFar', far).
qualifier('_dfar', far).
qualifier('_dFar', far).
qualifier('_Near', near).
qualifier(far, far).
qualifier(near, near).
qualifier(huge,huge).
qualifier(pascal,pascal).

%
% things that can follow a function declarator
%	= num
%	= name
%	= { num, ... }

after_func --> [equal, number(_)], !.
after_func --> [equal, ident(_)], !.
after_func --> [equal, lcurly], !, numlist, [rcurly].
after_func --> [].

numlist --> [number(_)], !, numlist.
numlist --> [comma], !, numlist.
numlist --> [].

%
% things that can follow an array declarator
%	another array bounds declaration as in a mult-dimensional array
%

after_array(Declt,RDeclt) -->  [lbrac], !,
	array_bounds([lbrac],Toks),
	after_array(array(Declt,[lbrac|Toks]),RDeclt).
after_array(Declt,Declt) --> [].


% collect tokens between [ and ], taking care of nesting

array_bounds([],[]) --> !.					% done
array_bounds([lbrac|Rest],[rbrac|Toks]) -->	% pop lbrac
	[rbrac],
	!,
	array_bounds(Rest,Toks).
array_bounds(Stack,[lbrac|Toks]) -->		% push lbrac
	[lbrac],
	!,
	array_bounds([lbrac|Stack],Toks).
array_bounds(Stack,[Tok|Toks]) -->			% collect toks
	[Tok],
	array_bounds(Stack,Toks).

%
% declt_list/6
%

declt_list(State,[Declt|Rest]) -->
	declt(State,Declt),
	!,
	declt_list_rest(State,Rest).
declt_list(State,[]) --> [].

declt_list_rest(State,Rest) --> [comma], !, 
	declt_list(State,Rest).
declt_list_rest(State,[]) --> [].


% simple_declt/3
% 	allow single or double quoted strings to be used
% 	as identifiers as an escape from macro expansion

simple_declt(ident(Id)) --> [ident(Id)].
simple_declt(ident(Id)) --> [fident(Id)].
simple_declt(ident(Id)) --> [string(Id)].
simple_declt(ident(Id)) --> [char(Id)].


%
% param_type_list/6
%

param_type_list(State,[]) --> [rparen], !.
param_type_list(State,[Param|Rest]) -->
	param_decln(State,Param),
	param_type_rest(State,Rest).

param_type_rest(State,[]) --> [rparen], !.
param_type_rest(State,[]) --> [comma,dotdotdot,rparen], !.
param_type_rest(State,Rest) --> [comma], !,
	param_type_list(State,Rest).
param_type_rest(State,[]) --> [Tok],
	{error('Unexpected param separator, found %t\n',[Tok],FD) }.

param_decln(State,param(Decln,Declt)) -->
	decln_specs(State,Decln),
%	Decln \= [],			% type nonempty 
	declt_or_abstract(State,Declt).

declt_or_abstract(State,Declt) --> 
	declt(State,Declt),
	!.
declt_or_abstract(State,Declt) --> 
	abstract_declt(State,Declt).

%
% abstract_declt/6
%	( declt )
%	* declt
%	

abstract_declt(State,Declt) --> 
	neabstract_declt(State,Declt),
	!.
abstract_declt(State,ident('0none')) --> [].

neabstract_declt(State,Declt) --> 
	neabstract_declt1(State,Declt1),
	neabstract_rest(State,Declt1,Declt).
		
neabstract_declt1(State,paren(Declt)) -->		% ( ab_declt )
	[lparen],
	neabstract_declt(State,Declt),
	[rparen],
	!.
neabstract_declt1(State,ptr([],Declt)) -->	% * ab_declt
	[star],
	!,
	abstract_declt(State,Declt).
neabstract_declt1(State,ident('0none')) --> [].
		% neabstract_rest makes sure
		% that func or array follows this
	
	
neabstract_rest(State,InDeclt,OutDeclt) -->	% ab_func ()
	[lparen],
	!,
	param_type_list(State,Params),
	neabstract_rest(State, func(InDeclt,Params),OutDeclt).
neabstract_rest(State,InDeclt,OutDeclt) -->	% ab_array []
	[lbrac],
	!,
	array_bounds([lbrac],Toks),
	neabstract_rest(State,array(InDeclt,[lbrac|Toks]),OutDeclt).

neabstract_rest(State, paren(Declt) , paren(Declt) ) --> !.
neabstract_rest(State,func(Declt,Params), func(Declt,Params)) -->  !.
neabstract_rest(State,array(Declt,Toks),array(Declt,Toks)) --> !.
neabstract_rest(State,ptr(Type,Declt),ptr(Type,Declt)) --> [].

%
% ptr_declt/6
%	pointer declarator
%

ptr_declt(State,ptr(Type,Declt)) -->
	type_spec_list_opt(State,Type),
	declt(State,Declt),
	!.

% type_spec_list_opt/5 (non-deterministic)
% 	no cut in first clause because what looks like a
%	a type could have been an identifier
%	e.g  typedef char batty;
%		 int * batty coda; (ANSI proposed standard)

type_spec_list_opt(State,TypeL) -->
	type_spec(State,Type), 	
	{ append(Type,Rest,TypeL) },
	type_spec_list_opt(State,Rest).
type_spec_list_opt(State,[]) --> [].


%%-----------------------------------------------------------
%%
%%	process a complete declaration 
%%		type_spec declt, declt...
%%
%%-----------------------------------------------------------


% process_decln/3.

process_decln(DSpec,Declts,State) :-
	is_typedef(DSpec),
	!,
	accessC2P(info_table, State, InTable),
	check_tagged_structdef(DSpec,InTable,OutTable),
	setC2P(info_table, State, OutTable),
	process_typedef_declts(Declts,DSpec,State).

process_decln(DSpec,Declts,State) :-
	accessC2P(info_table, State, InTable),
	check_tagged_structdef(DSpec,InTable,OutTable),
	setC2P(info_table, State, OutTable),
	accessC2P(fcn_filter, State, FcnFilter),
	process_declts(Declts,DSpec,FcnFilter,State).

check_tagged_structdef(DSpec,InTab,OutTab) :-
	is_tagged_structdef(DSpec),
	insert_tagref(DSpec,InTab,OutTab),
	!.
check_tagged_structdef(_,Tab,Tab).

%

process_typedef_declts([],_,State).
process_typedef_declts([Declt|Rest],DSpec,State) :-
	scan_declt(Declt,ident(Name),_,_),
	accessC2P(info_table, State, InTable),
	insert_type(ident(Name),DSpec,Declt,InTable,OutTable),
	setC2P(info_table, State, OutTable),
	output_type(Name,DSpec,Declt,State),
	process_typedef_declts(Rest,DSpec,State).

%---------------

process_declts( [] ,DSpec,_,State).

		%% debugging:
process_declts( [ Declt | Rest ] ,DSpec,FcnFilter,State) 
	:-
%	printf('process_decl: %t \n',[Declt]),
	fail.

process_declts( [ Declt | Rest ] ,DSpec,FcnFilter,State) 
	:-
	Declt = func(paren(ptr([],func(ident(_),[]))),[]),
	!,
	process_declts(Rest,DSpec,FcnFilter,State).

process_declts( [ Declt | Rest ] ,DSpec,FcnFilter,State) 
	:-
	Declt = func(ident(FID),[]),
	not(member(FID, ['XCreateRegion','XrmInitialize',
				 'XrmUniqueQuark','XUniqueContext' ]) ), 
	!,
	process_declts(Rest,DSpec,FcnFilter,State).

process_declts( [ func(ident(FID),Params) | Rest ] ,DSpec,FcnFilter,State) 
	:-
%printf('process fcn decl: %t -- filter = %t\n',[FID,FcnFilter]),
	!,
			%% This handles the recursive call to process_declts:
	process_fcn_decl(FcnFilter,func(ident(FID),Params),Rest,DSpec,State).


process_declts( [ Declt | Rest ] ,DSpec,FcnFilter,State) 
	:-
	(Declt = ptr([],func(ident(_),[]));
	    Declt = ptr([],ptr([],func(ident(_),[])))
	),
	!,
	process_declts(Rest,DSpec,FcnFilter,State).

process_declts( [ Declt | Rest ] ,DSpec,FcnFilter,State) 
	:-
	output_func_gvar(DSpec,Declt,State),
	process_declts(Rest,DSpec,FcnFilter,State).

process_fcn_decl(all,FDeclt,Rest,DSpec,State)
	:-!,
	output_func_gvar(DSpec, FDeclt, State),
	process_declts(Rest,DSpec,FcnFilter,State).

process_fcn_decl(all_except(Excl),FDeclt,Rest,DSpec,State)
	:-!,
	FDeclt = func(ident(FID),Params), 
	(dmember(FID, Excl) ->
		true
		;
		(dmember(FID/_, Excl) ->
			true
			;
			output_func_gvar(DSpec, FDeclt, State)
		)
	),
	process_declts(Rest,DSpec,FcnFilter,State).

process_fcn_decl(List,FDeclt,Rest,DSpec,State)
	:-
	FDeclt = func(ident(FID),Params), 
	(dmember(FID, List) ->
		output_func_gvar(DSpec, FDeclt, State)
		;
		(dmember(FID/_, List) ->
			output_func_gvar(DSpec, FDeclt, State)
			;
			true
		)
	),
	process_declts(Rest,DSpec,FcnFilter,State).

process_fcn_decl(FcnFilter,_,Rest,DSpec,State)
	:-
	process_declts(Rest,DSpec,FcnFilter,State).

%
%	adeclt_to_str/2
%	declt_to_str/2
%		returns a string representation of a declarator
%

adeclt_to_str(Var,"") :- var(Var), !.
adeclt_to_str(ident('0none'),"") :- !.
adeclt_to_str(ident(Name),Str) :-
	name(Name,Str).
adeclt_to_str(qual(Qual,ID,Declt),Str) :-
	name(Qual,QualStr),
	append(QualStr,[0' |Str1],Str),
	adeclt_to_str(Declt,Str1).
adeclt_to_str(paren(Declt),Str) :-
	adeclt_to_str(Declt,Str1),
	append([0'( |Str1],")",Str).
adeclt_to_str(func(Declt,Params),Str) :-
	adeclt_to_str(Declt,Str1),
	append(Str1,"()",Str).
adeclt_to_str(array(Declt,Toks),[0'* |Str]) :-
	adeclt_to_str(Declt,Str).
adeclt_to_str(ptr(Type,Declt),[0'* |Str]) :-
	type_specs_to_str(Type,Str1),
	append(Str1,Str2,Str),
	adeclt_to_str(Declt,Str2).


declt_to_str(ident('0none'),"") :- !.
declt_to_str(ident(Name),Str) :-
	name(Name,Str).
declt_to_str(qual(Qual,ID,Declt),Str) :-
	name(Qual,QualStr),
	append(QualStr,[0' |Str1],Str),
	declt_to_str(Declt,Str1).
declt_to_str(paren(Declt),Str) :-
	declt_to_str(Declt,Str1),
	append([0'( |Str1],")",Str).
declt_to_str(func(Declt,Params),Str) :-
	declt_to_str(Declt,Str1),
	append(Str1,"()",Str).
declt_to_str(array(Declt,Toks),Str) :-
	declt_to_str(Declt,Str1),
	toks_to_str(Toks,Str2),
	append(Str1,Str2,Str).
declt_to_str(ptr(Type,Declt),[0'* |Str]) :-
	type_specs_to_str(Type,Str1),
	append(Str1,Str2,Str),
	declt_to_str(Declt,Str2).
declt_to_str( bit(Declt,Width),Str) :-
	declt_to_str(Declt, Str1),
	name(Width,WidthStr),
	append( Str1,[0': |WidthStr],Str).


type_specs_to_str([],[]).
type_specs_to_str([TypeSpec|Rest],Str) :-
	get_type(TypeSpec,Type),
	type_to_str(Type,TypeStr),
	append(TypeStr,[0' |RestStr],Str),
	type_specs_to_str(Rest,RestStr).

toks_to_str([],[]).
toks_to_str([Tok|Toks],Str) :-
	tok_to_str(Tok,Str1),
	append(Str1,RestStr,Str),
	toks_to_str(Toks,RestStr).

%
%	scan_declt/4.
%		traverses a declarator, first arg, until it matches with
%		pattern, second arg, and builds a term identical with
%		first arg up to the match, except that it leaves a variable
%		at the match. It also builds a in-to-out list of declarators
%		encountered eg. for **foo[] we construct [array,ptr,ptr]
%		

scan_declt( Declt,Declt,X,[]) :- !.
scan_declt( qual(Qual,ID,Declt),Pat,qual(Qual,ID,X),[ID|PDeclt]) :-
	scan_declt(Declt,Pat,X,PDeclt).
scan_declt( paren(Declt),Pat,paren(X),PDeclt) :-
	scan_declt(Declt,Pat,X,PDeclt).
scan_declt( func(Declt,Params) ,Pat,func(X,Params),PDeclt) :-
	scan_declt(Declt,Pat,X,PDeclt1),
	append(PDeclt1,[ func ],PDeclt).
scan_declt( array(Declt,Toks) ,Pat,array(X,Toks),PDeclt) :-
	scan_declt(Declt,Pat,X,PDeclt1),
	Toks = [lbrac|Rest],
	deleteLast(Rest,NewToks),
	toks_to_str(NewToks,Str),
	name(Size,Str),
	fixup_array_size(Size,FSize),
	append(PDeclt1, [ array(FSize) ] ,PDeclt).
scan_declt( ptr(Type,Declt), Pat, ptr(Type,X), PDeclt) :-
	scan_declt(Declt,Pat,X,PDeclt1),
	append(PDeclt1,[ptr],PDeclt).
scan_declt( bit(Declt,Width), Pat, bit(X,Width), PDeclt) :-
	scan_declt(Declt, Pat,X, PDeclt1 ),
	append( PDeclt1, [bit(Width)], PDeclt).


fixup_array_size('',0) :- !.
fixup_array_size(X,X).

%
% dspec_to_str/2.
%	get the type string for a type

dspec_to_str(DSpec,TypeStr) :-
	get_type(DSpec, Type),
	type_to_str(Type, TypeStr).

	
type_to_str(int,"int").
type_to_str(char,"char").
type_to_str(short,"short").
type_to_str(long,"long").
type_to_str(float,"float").
type_to_str(double,"double").
type_to_str(extended,"extended").
type_to_str(unsigned_int,"unsigned int").
type_to_str(unsigned_char,"unsigned char").
type_to_str(unsigned_short,"unsigned short").
type_to_str(unsigned_long,"unsigned long").
type_to_str(void,"void").
type_to_str(ident(Name),Str) :- name(Name,Str).
type_to_str(struct(Spec), Str) :-	% tagged structure
	get_tag(Spec,Tag),
	!,
	name(Tag,TagName),
	append("struct ",TagName,Str).
type_to_str(struct(Spec), Str) :-  % untagged structure
	get_fields(Spec,Fields),
	fields_to_str(Fields,Str1),
	append("struct {",Str1, Str2),
	append(Str2,"}", Str).
type_to_str(union(Spec), Str) :-	% tagged union
	get_tag(Spec,Tag),
	!,
	name(Tag,TagName),
	append("union ",TagName,Str).
type_to_str(union(Spec), Str) :-	% untagged union
	get_fields(Spec,Fields),
	fields_to_str(Fields,Str1),
	append("union {",Str1, Str2),
	append(Str2,"}", Str).


fields_to_str([],[]).
fields_to_str([field(DSpec,Declts)|Rest], Str) :-
	dspec_to_str(DSpec,Str1),
	declts_to_str(Declts,Str2),
	append(Str1,[0' |Str2],Str3),
	append(Str3,";",Str4),
	append(Str4,StrRest,Str),
	fields_to_str(Rest, StrRest).

declts_to_str([],[]).
declts_to_str([Declt|Rest],Str) :-
	declt_to_str(Declt,Str1),
	append(Str1,Tail,Str),
	add_comma_cond(Rest,Tail,StrRest),
	declts_to_str(Rest, StrRest).

add_comma_cond([],Tail,Tail).
add_comma_cond([_|_],[0', |Tail],Tail).

%
% deref/5.
%	Dereference a declaration specifier (arg 1) using a
%	symbol table (arg 2) and return the final declaration
%	specifier (arg 3) and the list of operators (symbols)
%	encountered (arg 4).

deref(DSpec,Tab,DSpec,ident('0none'),[PType]) :-
	get_type(DSpec, Type),
	terminal_deref_type(Type, PType),
	!.
deref(DSpec,Tab,RSpec,RDeclt,PSpec) :-
	get_type(DSpec,Type),
	find_type(Type, Tab, DSpec1, Declt1),
	scan_declt(Declt1,ident(_),_,PDeclt),
	( PDeclt = [func|_] ->
	  RSpec = DSpec1,
	  RDeclt = Declt1
	;
	  RSpec = RSpec1,
	  RDeclt = RDeclt1
	),
	append(PDeclt,PType,PSpec),
	deref(DSpec1,Tab,RSpec1,RDeclt1,PType).


terminal_deref_type(Type, Type) :-	terminal_type(Type), !.
terminal_deref_type(Type,What) :- get_struct(Type,_,What).

terminal_type(int).
terminal_type(long).
terminal_type(char).
terminal_type(short).
terminal_type(unsigned_int).
terminal_type(unsigned_char).
terminal_type(unsigned_short).
terminal_type(unsigned_long).
terminal_type(float).
terminal_type(double).
terminal_type(extended).
terminal_type(comp).		% 64 bit int Mac
terminal_type(void).


%
% get_type/2.
%

get_type(DSpec,Type) :- dmember(type=Type,DSpec), !.
get_type(DSpec,void).

%
% get_struct/3.
%

get_struct(struct(Spec),Spec,struct).
get_struct(union(Spec),Spec,union).

%
% get_tag/2.
%

get_tag(Spec,Tag) :- dmember(tag=Tag,Spec).

%
% get_fields/2.
%

get_fields(Spec,Fields) :- dmember(fields=Fields,Spec).

%
% get_structdef/4.
%

get_structdef(SSpec,What,Tab,Fields) :- 
	get_fields(SSpec,Fields), 
	!.
get_structdef(SSpec,What,Tab,Fields) :-
	get_tag(SSpec,Tag),
	find_tagref(What,Tag,Tab,DSpec),
	get_type(DSpec,Type),
	get_struct(Type,SDefSpec,What),
	get_fields(SDefSpec,Fields).

%
% is_typedef/1.
%

is_typedef(DeclnSpec) :- dmember(storageClass=typedef,DeclnSpec).

%
% is_tagged_structdef/1.
%

is_tagged_structdef(DSpec) :- 
	get_type(DSpec,Type),
	get_struct(Type,SSpec,_),
	get_tag(SSpec,_),
	get_fields(SSpec,_).

%
% is_structdef/1.
%

is_structdef(DSpec) :-
	get_type(DSpec,Type),
	get_struct(Type,SSpec,_),
	get_fields(SSpec,_).


%
% is_identifer(Token,Id)	succeeds if Token is an identifier
% and returns the name in Id.
%

is_identifier(ident(Id),Id) :- !.
is_identifier(fident(Id),Id).



%%---------------------------------------------------------
%%
%%  procedures to add types into the global table
%%	and search for a given type in the global table
%%
%%---------------------------------------------------------

%
% insert_tagref/3
%

insert_tagref(DSpec,InTab,OutTab) :-
	get_type(DSpec,Type),
	get_struct(Type,SSpec,What),
	get_tag(SSpec,Tag),
	TagRef =.. [What,[tag=Tag]],
	insert_type(TagRef,DSpec,ident('0none'),InTab,OutTab).

%
% find_tagref/4.
%

find_tagref(What,Tag,Tab,DSpec) :-
	TagRef =.. [What,[tag=Tag]],
	find_type(TagRef,Tab,DSpec,_).

%
% insert_type/5.
%

insert_type(Name,DSpec,Declt,InTab,OutTab) :-
%pbi_write(avli=type(Name)),pbi_nl,
	avl_insert(type(Name),def(DSpec,Declt),InTab,OutTab).


%
% find_type/3.
%

find_type(Type,Tab,DSpec,Declt) :-
	avl_search(type(Type),def(DSpec,Declt),Tab), !.


%%---------------------------------------------------------
%%
%% debugging hooks
%%
%%---------------------------------------------------------


getStatements(FileName) :-
	newTable(Tab),
	create_file_stack(Tab,Tab1),
	openFile(FileName,FD),
	getStmtLoop(FD,Tab1).
   
getStmtLoop(FD,Tab) :-
	get_statement([],Stmt,FD,NFD,Tab,NTab),
	!,
	write(Stmt), nl,
	getStmtLoop(NFD,NTab).
getStmtLoop(_,_).


endmod.		%% parser
/*===========================================================================*
 |			ctoken
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 |			C language tokenizer
 |
 | Author: Kevin A. Buettner
 | Creation: 3/31/90
 | 
 | Revision History :
 | 	8/29/91  -- removed line comment char (P. Raman)
 |	4/94 -- revised to work with new stream I/O and package state and
 |		tables, etc., into a struct (Ken Bowen)
 |
 | Major Procedures:
 |
 |	getlines/1	-- This procedure is used for testing.  Its single
 |			   argument is a file name.  It will read the file,
 |			   tokenize each line and write the tokenized lines
 |			   out to the current output device.
 |
 |	openFile/2	-- This procedure takes a file name as its first
 |			   argument and builds a structure which describes
 |			   certain attributes about the tokenization of the
 |			   file.  This state variable is the second parameter.
 |
 |	getLine/3	-- This procedure returns a tokenized line as its
 |			   first argument.  The second argument is an input
 |			   argument and is the input state.  The third argument
 |			   is the state after getting the line in question.
 |
 |	fileName/2	-- When given a state variable as the first argument,
 |			   unifies the second argument with the file name.
 |
 |	lineNum/2	-- When given a state variable as the first argument,
 |			   unifies the second argument with the present line
 |			   number.  This value may be useful for printing
 |			   out error messages.
 *===========================================================================*/


module c2pro.

/*-----------------------------------------------------------------------*
 |	Primitives which manipulate State and Table
 *-----------------------------------------------------------------------*/

	%% Assumes a context already exists in state( inFile is \= nil, etc.)
push_new_context(NewFileName, NewStream, State)
	:-
	accessC2P(inFile, State, OldFileName),
	accessC2P(inStream, State, OldStream),
	accessC2P(curLine, State, CurLine),
	accessC2P(curLineNum, State, CurLineNum),
	accessC2P(ifdef_stack, State, OldifdefStack),
	accessC2P(fd_stack, State, OldFdStack),

	OldContext = c(OldFileName,OldStream,CurLine,CurLineNum,OldifdefStack),
	setC2P(fd_stack, State, [OldContext | OldFdStack]),

	setC2P(inFile, State, NewFileName),
	setC2P(inStream, State, NewStream),
	setC2P(curLine, State, nil),
	setC2P(curLineNum, State, 1),
	setC2P(ifdef_stack, State, [[] | OldifdefStack]).

pop_context(State)
	:-
	accessC2P(inFile, State, CurFileName),
	accessC2P(inStream, State, CurInStream),
	(is_stream(CurInStream,TheStream) ->
		close(CurInStream)
		;
		true
	),

	accessC2P(outStream,State,OutStream),
	output_closeFile(CurFileName, OutStream),

	accessC2P(fd_stack, State, [OldContext | OldFdStack]),
	setC2P(fd_stack, State, OldFdStack),

	OldContext = c(OldFileName,OldStream,OldLine,OldLineNum,OldifdefStack),

	setC2P(inFile, State, OldFileName),
	debug(1,'RETURNING TO:%t\n',[OldFileName]),
	setC2P(inStream, State, OldStream),
	setC2P(curLine, State, OldLine),
	setC2P(curLineNum, State, OldLineNum),
	setC2P(ifdef_stack, State, OldifdefStack).

		%% 4/18/94 (kab): Moved ifdef(level) and fd(stack) usage
		%% out of the AVL tree and into (mangled) stacks in State


check_level([],_) :- !.
check_level(_,FD) :- error('Missing #endif',FD).

/*-----------------------------------------------------------------------*
 * openFile builds the starting state
 *-----------------------------------------------------------------------*/


openFile(NewFileName, State)
	:-
	open(NewFileName, read, NewStream, []),
	push_new_context(NewFileName, NewStream, State),
	accessC2P(outStream,State,OutStream),
%	output_openFile(NewFileName,OutStream),
	setup_skips(NewFileName,OutStream),
	addto_searchPath(NewFileName),
	debug(1,'>>OPEN: %t <<\n',[NewFileName]),
	getInput(_, State).

closeFile(State)
	:-
	accessC2P(outStream,State,OutStream),
	accessC2P(inFile,State,InFile),
	output_closeFile(InFile,OutStream),
	close(OutStream).
	






/*-----------------------------------------------------------------------*
 * getLine(TokLine,State) repeatedly calls getToken until an
 * entire line of input is obtained.  The line of tokenized input is
 * turned into a list an returned via TokLine.
 *
 * getLine will fail after closing the File upon end of file.
 *-----------------------------------------------------------------------*/


getLine(TokLine,State) :-
	getToken(Tok,State),
	getLine1(Tok,TokLine,State).

getLine1(eof,_,State) :-		% fail on eof if it is first token
	!,
	fail.
getLine1(Tok,TokLine,State) :-
	getLine(Tok,TokLine,State).

getLine2(TokLine,State) :-
	getToken(Tok,State),
	getLine(Tok,TokLine,State).

getLine(eof,[],State) :- !.
getLine(eoln,[],State) :- !.
getLine(backslash,Line,State) :- !,
	getToken(Tok,State),
	getLine_backslash(Tok,Line,State).
getLine(Tok,[Tok|More],State) :-
	getLine2(More,State).

%% If backslash comes before end-of-line, then merge lines together
getLine_backslash(eoln,Line,State) :- !,
	getLine2(Line,State).
getLine_backslash(Other,[backslash,Other|More],State) :-
	getLine2(More,State).

/*-----------------------------------------------------------------*
 * getToken(Token,State)
 *
 * Gets a token from the file stream in the InState variable and unifies
 * it with Token.  OutState is set to the new output state.
 *-----------------------------------------------------------------*/

getToken(Token,State)
	:-
	accessC2P(curLineNum, State, InitLineNum),
	skipSpace(State),
	accessC2P(curLineNum, State, CurLineNum),
	(InitLineNum \= CurLineNum ->
		Token = eoln 	%% we have end_of_line when we advance to
				%% a different line while skipping space
		;
		accessC2P(curLine, State, CurLine),
		getToken(CurLine,ResLine,Token,CurLineNum,OutLineNum,State),
		setC2P(curLine, State, ResLine)
	).

/*
getToken(eoln,Stream,Stream,InLine,Line,State) :-
	InLine \= Line,		%% we have end_of_line when we advance to
	!.					%% a different line while skipping space
*/
getToken([],[],eof,LineNum,LineNum,State) 
	:- !.				%% EOF processing
getToken(InStream, OutStream, Token, _, LineNum, State)
	:-
	getInput(InStream,State),
	getToken1(InStream,Token,OutStream,LineNum,State).


getToken1(InStream, number(Num), OutStream, Line, State) :-
	getNumber(InStream,OutStream,Num,State),
	!.
getToken1( [ 0'' | InStream ], char(ConstName), OutStream, Line, State) :-
	!,
	getInput(InStream,State),
	getCharConst(InStream, Const, OutStream,Line, _, State),
	name(ConstName,Const).
getToken1( [ 0'" | InStream], string(StringName), OutStream, Line, State) :-
	!,
	getInput(InStream,State),
	getString(InStream, String, OutStream,Line, State),
	name(StringName,String).
getToken1( [ Char1,Char2,Char3 |Stream], Token, Stream, _, State) :-
	isSpecial3(Char1,Char2,Char3,Token),
	!,
	getInput(Stream,State).
getToken1( [ Char1,Char2 | Stream], Token, Stream, _, State) :-
	isSpecial2(Char1,Char2,Token),
	!.
getToken1( [ Char | Stream],  Token, Stream, _, State) :-
	isSpecial(Char, Token),
	!.
getToken1( [ IdChar | InStream ], Token, OutStream, _, State) :-
	isAlpha(IdChar),
	!,
	getInput(InStream,State),
	getIdentifier(InStream,IdString,OutStream,State),
	name(Identifier,[IdChar |IdString]),
	classifyIdentifier(Identifier,OutStream,Token).
getToken1( [ Other | Stream], other(Other), Stream, Line,State).


/*
 * getNumber(InStream,OutStream,Num) 
 *
 */

getNumber( [ Char,NChar | InStream], OutStream, Num,State) :-
	hexMarker(Char,NChar),
	!,
	getInput(InStream,State),
	getHex(InStream, 0, NStream, Num,State),
	getInput(NStream,State),
	intMarkerOpt(NStream, OutStream).
getNumber( [ Char | InStream], OutStream, Num,State) :-
	isNumeric(Char,Digit),
	!,
	getInput(InStream,State),
	getDecimal(InStream, Digit, NStream, INum,State),
	getMoreNum(NStream, INum, InStream,OutStream,Num,State).
getNumber([ 0'. , Char | InStream ], OutStream, Num,State) :-
	isNumeric(Char,Digit),
	getInput(InStream,State),
	Fract is Digit / 10,
	getFractAndExponent(InStream,Fract,100,OutStream,Mant,Exp,State),
	Num is Mant * 10^Exp.


% intMarkerOpt  -- eats sequences [lL][uU] or [uU][lL]

intMarkerOpt --> [0'l ], !, unsignedMarkerOpt.
intMarkerOpt --> [0'L ], !, unsignedMarkerOpt.
intMarkerOpt --> [0'u ], !, longMarkerOpt.
intMarkerOpt --> [0'U ], !, longMarkerOpt.
intMarkerOpt --> [].

unsignedMarkerOpt --> [0'u ], !.
unsignedMarkerOpt --> [0'U ], !.
unsignedMarkerOpt --> [].

longMarkerOpt --> [0'l ], !.
longMarkerOpt --> [0'L ], !.
longMarkerOpt --> [].


% floatMarkerOpt -- eats [fFlL]

floatMarkerOpt --> [Char], { floatMarkChar(Char)}, !.
floatMarkerOpt --> [].

floatMarkChar(0'f).
floatMarkChar(0'F).
floatMarkChar(0'l).
floatMarkChar(0'L).

% hexMarker

hexMarker(0'0,0'x).
hexMarker(0'0,0'X).

% getHex 

getHex( [ Char | InStream ], INum, OutStream,Num,State) :-
	isHex(Char,INum1),
	!,
	getInput(InStream,State),
	INum2 is INum*16+INum1,
	getHex(InStream, INum2, OutStream, Num,State).
getHex( Stream, Num, Stream, Num,State).

%

getOctal( [ Char | InStream ], INum, OutStream,Num,State) :-
	isOctal(Char,Num1),
	!,
	getInput(InStream,State),
	Num2 is INum*8+Num1,
	getOctal(InStream, Num2, OutStream, Num,State).
getOctal(Stream, Num, Stream, Num,State).

%

getDecimal( [ Char | InStream ], INum, OutStream,Num,State) :-
	isNumeric(Char,INum1),
	!,
	getInput(InStream,State),
	INum2 is INum*10+INum1,
	getDecimal(InStream, INum2, OutStream, Num,State).
getDecimal(Stream, Num, Stream, Num,State).

%


%  getMoreNum/5
%  clause 1 : digit_seq . digit_seq exponent_opt float_marker_opt
%  clause 2 : digit_seq exponent float_marker_opt
%  clause 3 : 0 octal number
%  clause 4 : decimal number

getMoreNum( [ 0'.| InStream ], INum, _,OutStream,Num,State) :-
	!,
	getInput(InStream,State),
	getFractAndExponent(InStream,INum,10,OutStream,Mant,Exp,State),
	Num is Mant * 10^Exp.
getMoreNum( [ Char | InStream ], INum, _,OutStream,Num,State) :-
	exponentChar(Char),
	!,
	getInput(InStream,State),
	getExponent(InStream, NStream, Exp,State),
	floatMarkerOpt(NStream, OutStream),
	Num is INum * 10^Exp.
getMoreNum( InStream, _, [ 0'0 | OldStream ] ,OutStream,Num,State) :-
	getOctal(OldStream, 0, InStream, Num,State),  % parse the string as octal
	!,
	intMarkerOpt(InStream, OutStream).
getMoreNum( InStream, Num, _, OutStream, Num,State) :-
	intMarkerOpt(InStream, OutStream).

%

getFractAndExponent(InStream,INum,Mag,OutStream,Fract,Exponent,State) :-
	getFraction(InStream,INum,Mag,Stream1,Fract,State),
	getExponentOpt(Stream1,Stream2,Exponent),
	floatMarkerOpt(Stream2,OutStream).

getFraction( [ Char | InStream ],INum,Mag,OutStream,Num,State) :-
	isNumeric(Char,Digit),
	!,
	getInput(InStream,State),
	Fract is Digit / Mag + INum,
	NMag is Mag * 10,
	getFraction(InStream,Fract,NMag,OutStream,Num,State).
getFraction( Stream, Num, _, Stream, Num,State).

getExponentOpt( [ Char | InStream ], OutStream, Exponent,State) :-
	exponentChar(Char),
	!,
	getInput(InStream,State),
	getExponent(InStream, OutStream, Exponent,State).
getExponentOpt(Stream, Stream, 0,State).

getExponent(InStream,OutStream,Exponent,State) :-
	getSignOpt(Sign,InStream,NStream,State),
	getDecimal(NStream, 0, OutStream, Num,State),
	Exponent is Num * Sign.


exponentChar(0'e).
exponentChar(0'E).

getSignOpt(1) --> [0'+], !.
getSignOpt(-1) --> [0'-], !.
getSignOpt(1) --> [].


/*
 * getCharConst
 */

getCharConst( [ 0'' | Stream ], [], Stream,_,_,State) :- !.
getCharConst( [ 0'\n | Stream ], [], [0'\n|Stream],Line,FName,State) :- !,
	writeWarning('Unterminated single quote string',State).
getCharConst( [ Char | InStream ], [Char|Rest], OutStream,Line,FName,State) :-
	getInput(InStream,State),
	getCharConst(InStream, Rest, OutStream, Line,FName,State).
getCharConst([],[],[],Line,FName,State) :-
	writeWarning('Unterminated single quote string',State).

/*
 * getString( InStream,String,OutStream,Line, FName) is used to read the 
 * characters up til the next double quote.
 */

getString( [ 0'" | Stream], [], Stream,_,State) :- !.
getString( [ 0'\n | Stream], [], [0'\n|Stream],Line,State) :- !,
	writeWarning('Unterminated double quote string',State).
getString( [ Char | InStream ], [Char|Rest], OutStream,Line,State) :-
	getInput(InStream,State),
	getString(InStream, Rest, OutStream,Line,State).
getString([],[],[],Line,State) :-
	writeWarning('Unterminated double quote string',State).

/*
 * getIdentifier(InStream,IdString,OutStream, State) is used to get an
 *	identifier and put it in IdString.
 */

getIdentifier([Char | InStream],[Char|IdString],OutStream,State) :-
	isAlpha(Char),
	!,
	getInput(InStream,State),
	getIdentifier(InStream,IdString,OutStream,State).
getIdentifier([Char | InStream],[Char|IdString],OutStream,State) :-
	isNumeric(Char,_),
	!,
	getInput(InStream,State),
	getIdentifier(InStream,IdString,OutStream,State).
getIdentifier(Stream,[],Stream,State).

/*
 * classifyIdentifier(Identifier, InStream, Token) is used to build a token
 * out of the Identifier based upon the upcoming character in InStream.
 */

classifyIdentifier(Id,[0'( | _],fident(Id)) :- !.
classifyIdentifier(Id, _, ident(Id)).

/*
 * isSpecial(Char,Tok) is map between characters which are considered
 * to be tokens by themselves and the symbolic name of the token.
 */

isSpecial(0'(, lparen).
isSpecial(0'), rparen).
isSpecial(0'[, lbrac).
isSpecial(0'], rbrac).
isSpecial(0'<, langle).
isSpecial(0'>, rangle).
isSpecial(0'+, plus).
isSpecial(0'-, minus).
isSpecial(0'*, star).
isSpecial(0'|, or).
isSpecial(0'&, and).
isSpecial(0'#, pound).
isSpecial(0'@, at).
isSpecial(0'=, equal).
isSpecial(0':, colon).
isSpecial(0',, comma).
isSpecial(0'., dot).
isSpecial(0'~, not).
isSpecial(0'!, lnot).
isSpecial(0'\\, backslash).
isSpecial(0'/, slash).
isSpecial(0'?, question).
isSpecial(0'%, percent).
isSpecial(0'^, carat).
isSpecial(0'{, lcurly).
isSpecial(0'}, rcurly).
isSpecial(0';, semicolon).

/*
 * isSpecial2(Char1,Char2,Tok) is a map between 2 character sequences and
 * the tokens which they represent.
 */

isSpecial2(0'<,0'<,leftshift).
isSpecial2(0'>,0'>,rightshift).
isSpecial2(0'!,0'=,notequal).
isSpecial2(0'<,0'=,lessOrEqual).
isSpecial2(0'>,0'=,greaterOrEqual).
isSpecial2(0'=,0'=,equalEqual).
isSpecial2(0'&,0'&,land).			% logical and
isSpecial2(0'|,0'|,lor).			% logical or
isSpecial2(0'+,0'+,plusplus).
isSpecial2(0'-,0'-,minusminus).
isSpecial2(0'.,0'.,dotdot).		% parent directory in unix
isSpecial2(0':,0':,coloncolon).	% parent directory in mac
isSpecial2(0'#,0'#,poundpound).	% concat operator of C preprocessor

% isSpecial3/4

isSpecial3(0'.,0'.,0'.,dotdotdot). % varargs



%
% convert a token in a list of characters and also 
% return the principal functor of the token.
%

tok_to_list(ident(Name),Str,ident) :- !, name(Name,Str).
tok_to_list(fident(Name),Str,fident) :- !, name(Name,Str).
tok_to_list(number(Num),Str,number) :- !, name(Num,Str).
tok_to_list(char(Const),Str,str) :- !, name(Const,Str).
tok_to_list(string(Name),Str,string) :- !,  name(Name,Str).
tok_to_list(other(Name),Str,other) :- !, name(Name,Str).
tok_to_list(Tok,[Char],other) :- 
	isSpecial(Char,Tok),
	!.
tok_to_list(Tok,[Char1,Char2],other) :-
	isSpecial2(Char1,Char2,Tok),
	!.
tok_to_list(Tok,[Char1,Char2,Char3],other) :-
	isSpecial3(Char1,Char2,Char3,Tok),
	!.

%
% convert a token into a C string
%

tok_to_str(number(Num),Str) :- !,
	name(Num,Str).
tok_to_str(ident(Name),Str) :- !,
	name(Name,Str).
tok_to_str(fident(Name),Str) :- !,
	name(Name,Str).
tok_to_str(char(Name),[0''|Str]) :- !,
	name(Name,Str1),
	append(Str1,"'",Str).	
tok_to_str(string(Name),[0'"|Str]) :- !,
	name(Name,Str1),
	append(Str1,"""",Str).
tok_to_str(other(Name),[0'',Name,0'']) :- 
	!.
tok_to_str(Tok,[Char]) :- 
	isSpecial(Char,Tok), !.
tok_to_str(Tok,[Char1,Char2]) :- 
	isSpecial2(Char1,Char2,Tok), !.
tok_to_str(Tok,[Char1,Char2,Char3]) :- 
	isSpecial3(Char1,Char2,Char3,Tok), !.

/*
 * State manipulation predicates
 *
 *	fileStream(State,FileStream)
 *	lineNum(State,LineNum)
 *	fileName(State,FileName)
 */

checkState(State) :- functor(State,tokState,3).
fileStream(State,FileStream) :- checkState(State), arg(1,State,FileStream).
lineNum(State,LineNum) :- checkState(State), arg(2,State,LineNum).
fileName(State,Name) :- checkState(State), arg(3,State,Name).

/*
 * lineCommentChar(Char)
 *
 * Enumerates the characters which denote line comments
 *
 */

% lineCommentChar(0';).		%% Semicolon is the line comment character


/*-----------------------------------------------------------------
%%%%% * skipSpace(InStream,OutStream,InLine,OutLine,FName,State)
 * skipSpace(State)
 *
 * It is assumed that the InputStream has at least K characters in it.
 * InLine is the line number of the line we are on before processing any
 * characters in InputStream.  OutLine is the line number that we get to
 * after the processing.
 * 
 *----------------------------------------------------------------*/

skipSpace(State)
	:-
	accessC2P(curLine, State, LineList),
	accessC2P(curLineNum, State, CurNum),
	skipSpace(LineList, ResultLine, CurNum, NewNum, State),
	setC2P(curLine, State, ResultLine),
	setC2P(curLineNum, State, NewNum).

skipSpace(LineList,ResultLine,CurNum,NewNum, State)
	:-
	var(LineList),
	getInput(LineList,State),
	!,
	accessC2P(curLineNum, State, InterNum),
	skipSpace(LineList,ResultLine,InterNum,NewNum, State).
skipSpace([],[],Num,Num, State).			%% EOF
skipSpace(LineList,ResultLine,CurNum,NewNum, State)
	:-
	LineList = [First | Rest],
	skipSpace(First,Rest,ResultLine,CurNum,NewNum, State).
	

/*
skipSpace([],[],Line,Line,_,State) :- !.	%% EOF
skipSpace([First | Rest],OutStream, InLine,OutLine,FName,State) :-
	getInput(Rest,State),
	skipSpace(First,Rest,OutStream, InLine,OutLine,FName,State).
*/


skipSpace(0'\n, Rest, ResultLine, InNum, OutNum,State)
	:- !, 						%% NewLine Character
	MidNum is InNum + 1,		
	skipSpace(Rest,ResultLine,MidNum,OutNum,State).

skipSpace(SpaceChar, Rest, ResultLine, InNum, OutNum,State)
	:- 						%% Space Characters
	SpaceChar =< 32,
	!,					
	skipSpace(Rest,ResultLine,InNum,OutNum,State).

skipSpace(0'/, [ 0'/ | Rest], ResultLine, InNum, OutNum,State)
	:- !,						%% Line Comment
	skipToEOL(Rest, Rest2, State),
	MidNum is InNum+1,
	skipSpace(Rest2,ResultLine,MidNum,OutNum,State).

skipSpace(0'/, [0'* | Rest], ResultLine, InNum,OutNum,State)
	:- !,						%% C-Style Comments
	skipToCommentEnd(Rest,Rest2,InNum,MidNum,State),
	skipSpace(Rest2,ResultLine,MidNum,OutNum,State).

skipSpace(Any, Tail, [Any|Tail], Num, Num, State). 	%% Non-Space char


skipToEOL([],[],State) :- !.			%% EOF
skipToEOL([First | Rest], ResultLine,State)
	:-
	getInput(Rest,State),
	skipToEOL(First,Rest,ResultLine,State).

skipToEOL(0'\n,ResultLine,ResultLine,State)
	:- !.
skipToEOL(Any,Rest,ResultLine,State)
	:-
	skipToEOL(Rest,ResultLine,State).

skipToCommentEnd([],[],Line,Line,State)
	:- !,											%% EOF
	writeWarning('Unclosed C-Style Comment',State).

skipToCommentEnd([First|Rest], ResultLine, InLine, OutLine,State)
	:-
	getInput(Rest,State),
	skipToCommentEnd(First,Rest,ResultLine,InLine,OutLine,State).

skipToCommentEnd(0'\n, Rest, ResultLine, InLine,OutLine,State)
	:- !,											%% NewLine Character
	NewLine is InLine+1,
	skipToCommentEnd(Rest,ResultLine,NewLine,OutLine,State).

skipToCommentEnd(0'/, [0'* | Rest], ResultLine, InLine,OutLine,State)
	:- !,											%% Nested C-Style Comment
	skipToCommentEnd(Rest,Rest2,InLine,InLine2,State),
	skipToCommentEnd(Rest2,ResultLine,InLine2,OutLine,State).

skipToCommentEnd(0'*, [0'/ | Rest], Rest, Line, Line, State)
	:- !.											%% End of C-Style Comment

skipToCommentEnd(Any, Rest, ResultLine, InLine, OutLine,State)
	:-
	skipToCommentEnd(Rest,ResultLine,InLine,OutLine,State).

/*---------------------------------------------------------------------*
 * getInput(List,State)
 *
 * We would like to view the file as one long list of characters.  
 * We can, of course, read in the file all at once into a long list,
 * but I find this aesthetically unpleasing as the file may be very 
 * large.  We only need to look ahead K characters anyway, so we call
 * getInput(List) to advance the size of the list if possible.
 *---------------------------------------------------------------------*/

/*
getInput(List,State) :-
	getInput(3,List,State).		%% K=3, List will always have 3 chars in it

getInput(0,List,State) :- !.		%% Have enough characters in the list
getInput(N,List,State) :- 		
	var(List),				%% Don't have enough characters yet
	!,
	get0(C),				%% So get some more
	eofCheck(C,N,List,State).
getInput(N,[],State) :- !.		%% At end of file, don't need any more chars
getInput(N,[_|Tail],State) :-
	NP is N-1,
	getInput(NP,Tail,State).

eofCheck(-1,_,[],State) :- !.
eofCheck(C,N,[C|Tail],State) :-
	NP is N-1,
	getInput(NP,Tail,State).
*/
/*
getInput(List,State) 
	:-
	accessC2P(curLine, State, RealList),
	nonvar(RealList), 
	(RealList = nil ->
		accessC2P(inStream, State, InStream),
		getInput(InStream, List,State)
		;
		true
	),
	!.
getInput(List,State) 
	:-
	accessC2P(inStream, State, InStream),
	getInput(InStream, List,State).
*/
getInput(List,State) 
	:-
	var(List),
	!,
	accessC2P(inStream, State, InStream),
	getInput(InStream, List,State).

getInput(nil,State) 
	:-
	accessC2P(inStream, State, InStream),
	getInput(InStream, _,State).

getInput(_,State).

getInput(InStream, List,State)
	:-
	get_line(InStream, LineUIA),
	debug(4,'get_LINE:%t\n',[LineUIA]),
	!,
	cont_getInput(LineUIA, InStream, List, State).

cont_getInput('', InStream, List, State)
	:-!,
	accessC2P(curLineNum, State, CurLineNum),
	NextLineNum is CurLineNum + 1,
	setC2P(curLineNum, State, NextLineNum),
	getInput(InStream, List,State).

cont_getInput(LineUIA, InStream, List, State)
	:-
	name(LineUIA, List0),
	append(List0, [0'\n | _], List),
	accessC2P(curLineNum, State, CurLineNum),
	NextLineNum is CurLineNum + 1,
	setC2P(curLineNum, State, NextLineNum),
	setC2P(curLine, State, List).

writeWarning(Warning,State)
	:-
	accessC2P(inFile, State, FileName),
	accessC2P(curLineNum, State, LineNum),
	printf('File: %t, line %t: %t\n', [FileName,LineNum,Warning]).

/*-----------------------------------------------------------------------*
 * getLines is used for testing.   It tokenizes a file and writes out the
 * tokenized lines.
 *-----------------------------------------------------------------------*/

getLines(FileName) :-
	openFile(FileName,State),
	getLines1(State).

getLines1(State) :-
	getLine(TokLine,State,NextState),
	!,
	write(TokLine),nl,
	getLines1(NextState).
getLines1(_).			%% succeed when getLine eventually fails
						%% upon eof


endmod.				%% to.kenizer
/*=====================================================================*
 |			cdos.pro 
 |	Copyright (c) 1991-1994 Applied Logic Systems, Inc.
 |
 |		Path syntax for C include files for dos systems
 |
 | For each functionality, there are two versions of a predicate, one
 | that operates on string input, and another that operates on a token
 | list input.
 |
 | Terminology (Path Syntax): Path = Disk : component component ....
 *=====================================================================*/

module c2pro.

c2pro_os(dos).

	%
	% default system include paths
	%

defaultIncludePath('c',[highc,inc],abs).

	%
	% get the disk component of a path specification
	%

diskOfPathStr(Disk) --> diskName(Disk), [0': ], { Disk \= ''}, !.
diskOfPathStr('') --> [].

diskName(Disk) --> diskStr(DiskStr), {name(Disk,DiskStr)}.

diskStr([Char|Rest]) --> [Char], { isAlphaNumeric(Char) }, !,
						 diskStr(Rest).
diskStr([]) --> [].

	%
	% extract 'Disk:' from a path token list
	%

diskOfPathTokList(Disk) --> [ident(Disk),colon], !.
diskOfPathTokList('') --> [].

	%
	% the type of a path is either abs or rel
	%

typeOfPathStr(Disk,[0'\|_],abs) :- !.
typeOfPathStr(Disk,CompStr,rel).

	%

typeOfPathTokList(Disk,[backslash|_],abs) :- !.
typeOfPathTokList(Disk,CompTokList,rel).

	%
	% get a list of directory and file components of a path
	% specification. In the predicate that takes a string argument,
	% we allow the path specification to have ? as a component or
	% * as the last component

compsOfPathStr(Comps) --> [0'\], !, compsOfPathStr(Comps).
compsOfPathStr( _ ) --> [0'* ], !.
compsOfPathStr([_|Comps]) --> [0'? ], !, moreComps(Comps).
compsOfPathStr([Comp|Comps]) --> compOfPathStr(Comp), !, 
	moreComps(Comps).
compsOfPathStr([]) --> [].

moreComps(Comps) --> [0'\], !, compsOfPathStr(Comps).
moreComps([]) --> [].

compOfPathStr(File) --> compStr(FileStr), {name(File,FileStr)}.

compStr([Char|Rest]) --> [Char], {isAlphaNumeric(Char)}, !,
	compStr(Rest).
compStr([0'. |Rest]) --> [0'. ], !, compStr(Rest).
compStr([0'- |Rest]) --> [0'- ], !, compStr(Rest).
compStr(Str) --> [ 0''], !, endQuote(Str).
compStr([]) --> [].

endQuote([]) --> [ 0''], !.
endQuote([Char|Rest]) --> [Char], endQuote(Rest).

	%

compsOfPathTokList(Comps) --> [backslash], !, compsOfPathTokList(Comps).
compsOfPathTokList( _ ) --> [star], !.
compsOfPathTokList([_|Comps]) --> [question], !,
	moreCompsTokList(Comps).
compsOfPathTokList([Comp|Comps]) --> compOfPathTokList(Comp), !,
	moreCompsTokList(Comps).
compsOfPathTokList([]) --> [].

moreCompsTokList(Comps) --> [backslash], !, compsOfPathTokList(Comps).
moreCompsTokList([]) --> [].

compOfPathTokList(Comp) --> compTokList(CompTokList),
	{ CompTokList \= [], catenate(CompTokList,Comp) }.

compTokList([Id|Rest]) --> [ident(Id)], !, compTokList(Rest).
compTokList(['..'|Rest]) --> [dotdot], !, compTokList(Rest).
compTokList(['.'|Rest]) --> [dot], !, compTokList(Rest).
compTokList(['-'|Rest]) --> [minus], !, compTokList(Rest).
compTokList(['--'|Rest]) --> [minusminus], !, compTokList(Rest).
compTokList([]) --> [].

	%
	% convert a ground file list to a pathname
	%

pathListToName(Disk,Comps,Type,Name) :-
	diskSym(Disk,DiskList),
	absMark(Type,TypeList),
	addPathSep(Comps,NList),
	append(DiskList,TypeList,List1),
	append(List1,NList,List2),
	catenate(List2,Name).

diskSym('',[]) :- !.
diskSym(X,[X,':']).
 
absMark(abs,['\']).
absMark(rel,[]).

addPathSep([],[]).
addPathSep([File|IList],[File|OList]) :-
	possPathSep(IList,OList,NList),
	addPathSep(IList,NList).

possPathSep([],L,L).
possPathSep([_|_],['\'|L],L).

endmod.
