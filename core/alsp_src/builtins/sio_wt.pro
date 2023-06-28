/*=============================================================*
 | 		sio_wt.pro
 |	Copyright (c) 1990-1996 Applied Logic Systems, Inc.
 |
 |		write_term and company
 |
 | Authors: 	Ken Bowen
 |		Kevin Buettner
 | Creation:	5/8/90
 |
 | Modules:	sio, builtins
 |
 | Exported Procedures:
 |		write_term(Stream,Term,WriteOptions)
 *=============================================================*/


module sio.

:-	make_gv("_depth_computation_constant").

/*---------------------------------------------------------------------*
 | write_term(Term,Options)
 | write_term(Stream,Term,Options)
 |
 |	These predicates write out Term on Stream (current output stream
 |	in the two argument version) with the options Options.  The
 |	options may be any of the following:
 |
 |	quoted(Bool)		-- Bool is true or false; 
 |		If true, forces symbols to be written out in such a manner 
 |		that read may be used to read them back in.  If false,  symbols 
 |		will be written out without any special quoting; i.e, embedded 
 |		control characters will be written out to the output device as is.
 |
 |	ignore_ops(Bool)	-- Bool is true or false; 
 |		If true, operators will be output in function notation (operators 
 |		are ignored.)  If false, operators will be printed out appropriately.
 |
 |	portrayed(Bool)		-- not yet implemented;
 |
 |	numbervars(Bool)	-- Bool is true or false;
 |		If true, terms of the form $VAR(N) where N is an integer will print 
 |		out as a letter.
 |
 |	lettervars(Bool)	-- Bool is true or false;
 |		If Bool is true, variables will be printed out as letters.  If false, 
 |		variables will be printed as _N where N is computed via the address 
 |		that the variable lives at.  This latter mode will be more suited to 
 |		debugging purposes where correspondences between variables in various 
 |		calls is required.
 |
 |	maxdepth(N,Atom1,Atom2)	-- 
 |		N is the maximum depth to which to print. Atom1 is the atom to 
 |		output when this depth has been reached.  Atom2 is the atom to 
 |		output when this depth has been reached at the tail of a list.
 |
 |	maxdepth(N)		-- 
 |		Same as maxdepth(N,*,...)
 |
 |	line_length(N)		-- 
 |		N is the length in characters of the output line.  The pretty printer 
 |		will attempt to put attempt to break lines before they exceed the 
 |		given line length.
 |
 |	indent(N)		-- 
 |		N is the initial indentation to use.
 |
|	quoted_strings(Bool)	-- Bool is true or false;
 |		If true, lists of suitably small integers will print out as a 
 |		double quoted string.  If false, these lists will print out
 |		as lists of small numbers.
 |
 |	depth_computation(Val)	-- Val is flat or nonflat;
 |		This indicates the manner of depth computation. If Val is flat, 
 |		all arguments of a term or list will be treated as being at the 
 |		same depth.  If Val is nonflat, then each subsequent argument in 
 |		a term (or each sebsequent element of a list) will be considered to 
 |		be at a depth one greater than the preceding structure argument 
 |		(or list element).
 |
 |	line_end(Bool)  -- When true, nl(_) is normal; when false, 
 |		line-breaks (new lines) are preceeded by a \
 *---------------------------------------------------------------------*/

export write_term/2.
export write_term/3.

write_term(Term,Options) 
	:-
	get_current_output_stream(Stream),
	write_term(Stream,Term,Options).

write_term(Stream_or_alias,Term,Options) 
	:-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	winfo_write_term(Stream,WInfo),
	write_options(Options,WInfo),
	nonvar(Stream_or_alias),	%% Preserve vars for error reporting
	nonvar(Options),
	!,
	write_term0(Stream,Term,WInfo).

export write/1.
export write/2.

write(Term) 
	:-
	get_current_output_stream(Stream),
	write(Stream,Term).

write(Stream_or_alias,Term) 
	:-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	nonvar(Stream_or_alias),	%% Preserve var for error reporting
	winfo_write(Stream,WInfo),
	!,
	write_term0(Stream,Term,WInfo).

export writeq/1.
export writeq/2.

writeq(Term) 
	:-
	get_current_output_stream(Stream),
	writeq(Stream,Term).

writeq(Stream_or_alias,Term) 
	:-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	nonvar(Stream_or_alias),
	winfo_writeq(Stream,WInfo),
	!,
	write_term0(Stream,Term,WInfo).

export write_canonical/1.
export write_canonical/2.

write_canonical(Term) 
	:-
	get_current_output_stream(Stream),
	write_canonical(Stream,Term).

write_canonical(Stream_or_alias,Term) 
	:-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	nonvar(Stream_or_alias),	%% Preserve var for error reporting
	winfo_write_canonical(Stream,WInfo),
	!,
	write_term0(Stream,Term,WInfo).

export print/1.
export print/2.

print(Term) 
	:-
	get_current_output_stream(Stream),
	print(Stream,Term).

print(Stream_or_alias,Term) 
	:-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	nonvar(Stream_or_alias),	%% Preserve var for error reporting
	winfo_print(Stream,WInfo),
	!,
	write_term0(Stream,Term,WInfo).

write_term0(Stream,Term,WInfo) 
	:-
	winfo_depth_computation(WInfo,DC),
	(DC = flat ->  
		set_depth_computation_constant(0)
		;
		set_depth_computation_constant(1) 
	),
	winfo_maxdepth(WInfo,MaxDepth),
	winfo_line_len(WInfo,Width),
	wd(Term,MaxDepth,OutList,[],WInfo),
	winfo_indent(WInfo,Indent),
	winfo_line_end(WInfo,LineEnd),
	output_string(Stream,OutList,Width,Indent,LineEnd),
	fail.

write_term0(Stream,Term,WInfo).

write_options(Var,WInfo) 
	:-
	var(Var),
	!,
	instantiation_error(2).
write_options([],WInfo) :- !.

write_options([Option|Options],WInfo) 
	:-!,
	write_option(Option,WInfo),
	write_options(Options,WInfo).

write_options(Culprit, WInfo) 
	:-
	type_error(list,Culprit,2).

is_bool(true).
is_bool(false).

write_option( Var, 		           WInfo) :- var(Var), !,	 instantiation_error(3).
write_option( quoted(Val),         WInfo) :- is_bool(Val),!, set_winfo_quoted(WInfo,Val).
write_option( ignore_ops(Val),     WInfo) :- is_bool(Val),!, set_winfo_ignore_ops(WInfo,Val).
write_option( portrayed(Val),      WInfo) :- is_bool(Val),!, set_winfo_portrayed(WInfo,Val).
write_option( numbervars(Val),     WInfo) :- is_bool(Val),!, set_winfo_numbervars(WInfo,Val).
write_option( lettervars(Val),     WInfo) :- is_bool(Val),!, set_winfo_lettervars(WInfo,Val).
write_option( quoted_strings(Val), WInfo) :- is_bool(Val),!, set_winfo_quoted_strings(WInfo,Val).
write_option( line_end(Val),       WInfo) :- is_bool(Val),!, set_winfo_line_end(WInfo,Val).

write_option( depth_computation(Val), WInfo) 
	:-
	nonvar(Val),
	(Val = flat ; Val = nonflat),
	!,
	set_winfo_depth_computation(WInfo,Val).

write_option(maxdepth(N,Atom1,Atom2),WInfo) 
	:-
	integer(N),
	N > 0,
	atom(Atom1),
	atom(Atom2),
	!,
	set_winfo_maxdepth(WInfo,N),
	set_winfo_maxdepth_atom(WInfo,Atom1),
	set_winfo_maxdepth_listtail(WInfo,Atom2).

write_option(maxdepth(N),WInfo) 
	:-
	integer(N),
	N > 0,
	!,
	set_winfo_maxdepth(WInfo,N).

write_option(line_length(N),WInfo) 
	:-
	integer(N),
	N > 0,
	!,
	set_winfo_line_len(WInfo,N).

write_option(indent(N),WInfo) 
	:-
	integer(N),
	N > 0,
	!,
	set_winfo_indent(WInfo,N).

write_option(Culprit, WInfo) 
	:-
	domain_error(write_option,Culprit,3).

/*------------------------------------------------------------------------------------*
 |		winfo structure
 |
 |	The winfo structure (for write info) is a structure with winfo
 |	as the principal functor.  Arguments of the structure control the 
 |	manner in which a term is written out.
 |
 |			SlotName			PrimaryDefaultValue
 |			========        	============
 | 	winfo(	Quoted,					true
 |			Ignore_ops,				true
 |			Portrayed,				true
 |			Numbervars,				true
 |			Maxdepth_atom,
 |			Listtail,
 |			Indent,
 |			Lettervars,				true
 |			Varcounter,
 |			Quoted_strings,			true
 |			Wt_opts,
 |			LineEnd,
 |			ShowDelay,				<Flag>
#if (all_procedures(syscfg,intconstr,0,_))
 |			ShowInterval			<Flag>
#endif
 |		 )
 |
 |	Wt_opts is a term of the form:
 |		
 |		wt_opts(Line_len, MaxDepth, Depth_computation)
 |			
 |	The access routines below are grouped in triplets (or pairs), as
 |	follows:
 |	- winfo_<xxx>(Winfo, Val)		- access to <xxx>: unifies Val with winfo value
 |	- winfo_<xxx>(Winfo)			- set primary default value for <xxx> (binds slot var)
 |	- set_winfo_<xxx>(Winfo,Val)	- re-sets value for <xxx> (mangles slot var)
 *------------------------------------------------------------------------------------*/

winfo_quoted(WInfo,Value)				:-	arg(1,WInfo,Value).
winfo_quoted(WInfo)						:-	arg(1,WInfo,true).
set_winfo_quoted(WInfo,Value)			:-	mangle(1,WInfo,Value).

winfo_ignore_ops(WInfo,Value)			:- 	arg(2,WInfo,Value).
winfo_ignore_ops(WInfo)					:-	arg(2,WInfo,true).
set_winfo_ignore_ops(WInfo,Value) 		:-	mangle(2,WInfo,Value).

winfo_portrayed(WInfo,Value)			:-	arg(3,WInfo,Value).
winfo_portrayed(WInfo)					:-	arg(3,WInfo,true).
set_winfo_portrayed(WInfo,Value) 		:-	mangle(3,WInfo,Value).

winfo_numbervars(WInfo,Value)			:-	arg(4,WInfo,Value).
winfo_numbervars(WInfo)					:-	arg(4,WInfo,true).
set_winfo_numbervars(WInfo,Value) 		:-	mangle(4,WInfo,Value).

winfo_maxdepth_atom(WInfo,Value) 		:- 	arg(5,WInfo,Value).
set_winfo_maxdepth_atom(WInfo,Value) 	:-	mangle(5,WInfo,Value).

winfo_maxdepth_listtail(WInfo,Value) 	:- 	arg(6,WInfo,Value).
set_winfo_maxdepth_listtail(WInfo,Value):-	mangle(6,WInfo,Value).

winfo_indent(WInfo,Value)				:-	arg(7,WInfo,Value).
set_winfo_indent(WInfo,Value) 			:-	mangle(7,WInfo,Value).

winfo_lettervars(WInfo,Value)			:-	arg(8,WInfo,Value).
winfo_lettervars(WInfo)					:-	arg(8,WInfo,true).
set_winfo_lettervars(WInfo,Value) 		:-	mangle(8,WInfo,Value).

winfo_varcounter(WInfo,Value) 			:-	arg(9,WInfo,Value).
set_winfo_varcounter(WInfo,Value) 		:-	mangle(9,WInfo,Value).

winfo_quoted_strings(WInfo,Value)		:-	arg(10,WInfo,Value).
winfo_quoted_strings(WInfo) 			:-	arg(10,WInfo,true).
set_winfo_quoted_strings(WInfo,Value)	:-	mangle(10,WInfo,Value).

winfo_wt_opts(WInfo,WTOPTS) 			:-	arg(11,WInfo,WTOPTS).

winfo_line_len(WInfo,Value) 			:-	arg(11,WInfo,WO), arg(1,WO,Value).
set_winfo_line_len(WInfo,Value) 		:-	arg(11,WInfo,WO), mangle(1,WO,Value).

winfo_maxdepth(WInfo,Value) 			:-	arg(11,WInfo,WO), arg(2,WO,Value).
set_winfo_maxdepth(WInfo,Value) 		:-	arg(11,WInfo,WO), mangle(2,WO,Value).

winfo_depth_computation(WInfo,Value) 	:-	arg(11,WInfo,WO), arg(3,WO,Value).
set_winfo_depth_computation(WInfo,Value) :- arg(11,WInfo,WO), mangle(3,WO,Value).

winfo_line_end(WInfo,Value)				:-	arg(12,WInfo,Value).
set_winfo_line_end(WInfo,Value)			:-	mangle(12,WInfo,Value).

winfo_show_delay(WInfo,Value)				:-	arg(13,WInfo,Value).
set_winfo_show_delay(WInfo,Value)			:-	mangle(13,WInfo,Value).

#if (all_procedures(syscfg,intconstr,0,_))
winfo_show_interval(WInfo,Value)				:-	arg(14,WInfo,Value).
set_winfo_show_interval(WInfo,Value)			:-	mangle(14,WInfo,Value).
#endif

	/*------------------------------------------------------------------------*
	 | Default winfos 
	 | - several alternate winfo structures for different default contexts.
	 |
 	 | 	winfo(	Quoted, Ignore_ops, Portrayed, Numbervars,
 	 |			Maxdepth_atom, Listtail, Indent,
 	 |			Lettervars, Varcounter, Quoted_strings, 
 	 |			wt_opts(Line_len, MaxDepth, Depth_computation), LineEnd,
	 |			ShowDelay,
#if (all_procedures(syscfg,intconstr,0,_))
	 |			ShowInterval
#endif
 	 |		 )
	 | Distinguished/presented by:
 	 |
	 |	winfo_write_term(Stream, Winfo)
	 |	winfo_write(Stream, Winfo)
	 |	winfo_writeq(Stream, Winfo)
	 |	winfo_write_canonical(Stream, Winfo)
	 |	winfo_write_print(Stream, Winfo)
	 *-------------------------------------------------------------------------*/
#if (all_procedures(syscfg,intconstr,0,_))
		%% Quoted, Lettervars, Quoted_strings
winfo_write_term( Stream,
			winfo(false, false,false,false,'*','...',0,true,0,true,WO,true,SD,SI)) 
	:-
    WO = wt_opts(_,_,_),
    stream_wt_opts(Stream,WO),
	current_prolog_flag(show_delay, SD), 
	current_prolog_flag(show_interval, SI). 

		%% No quoting, Numbervars, No Lettervars, No Quoted_strings,
		%% Very long lines 
winfo_write( Stream,
			winfo(false,false,false,true,'*','...',0,false,0,false,WO,true,SD,SI)) 
	:-
    WO = wt_opts(2047,Depth,DepthComputation),	%% override default length
    stream_wt_maxdepth(Stream,Depth),
    stream_wt_depth_computation(Stream,DepthComputation),
	current_prolog_flag(show_delay, SD),
	current_prolog_flag(show_interval, SI). 

winfo_writeq( Stream,
			winfo(true, false,false,true, '*','...',0,true,0,true,WO,true,SD,SI)) 
	:-
    WO = wt_opts(_,_,_),
    stream_wt_opts(Stream,WO),
	current_prolog_flag(show_delay, SD),
	current_prolog_flag(show_interval, SI). 

winfo_write_canonical( Stream,
			winfo(true, true, false,false,'*','...',0,true,0,false,WO,true,SD,SI)) 
	:-
    WO = wt_opts(_,_,_),
    stream_wt_opts(Stream,WO),
	current_prolog_flag(show_delay, SD),
	current_prolog_flag(show_interval, SI). 

winfo_print( Stream,
			winfo(false,false,true, true, '*','...',0,false,0,false,WO,true,SD,SI)) 
	:-
    WO = wt_opts(2047,Depth,DepthComputation),	%% override default length
    stream_wt_maxdepth(Stream,Depth),
    stream_wt_depth_computation(Stream,DepthComputation),
	current_prolog_flag(show_delay, SD),
	current_prolog_flag(show_interval, SI). 

#else

		%% Quoted, Lettervars, Quoted_strings
winfo_write_term( Stream,
			winfo(false, false,false,false,'*','...',0,true,0,true,WO,true,SD)) 
	:-
    WO = wt_opts(_,_,_),
    stream_wt_opts(Stream,WO),
	current_prolog_flag(show_delay, SD). 

		%% No quoting, Numbervars, No Lettervars, No Quoted_strings,
		%% Very long lines 
winfo_write( Stream,
			winfo(false,false,false,true,'*','...',0,false,0,false,WO,true,SD)) 
	:-
    WO = wt_opts(2047,Depth,DepthComputation),	%% override default length
    stream_wt_maxdepth(Stream,Depth),
    stream_wt_depth_computation(Stream,DepthComputation),
	current_prolog_flag(show_delay, SD). 

winfo_writeq( Stream,
			winfo(true, false,false,true, '*','...',0,true,0,true,WO,true,SD)) 
	:-
    WO = wt_opts(_,_,_),
    stream_wt_opts(Stream,WO),
	current_prolog_flag(show_delay, SD). 

winfo_write_canonical( Stream,
			winfo(true, true, false,false,'*','...',0,true,0,false,WO,true,SD)) 
	:-
    WO = wt_opts(_,_,_),
    stream_wt_opts(Stream,WO),
	current_prolog_flag(show_delay, SD). 

winfo_print( Stream,
			winfo(false,false,true, true, '*','...',0,false,0,false,WO,true,SD)) 
	:-
    WO = wt_opts(2047,Depth,DepthComputation),	%% override default length
    stream_wt_maxdepth(Stream,Depth),
    stream_wt_depth_computation(Stream,DepthComputation),
	current_prolog_flag(show_delay, SD). 
#endif


    /*---------------------------------------------------------------------*
     | wd/5
     | wd(Term,Depth,List,Hole,WInfo)
     |
     | Write out the term to the depth given by the second argument.
     *---------------------------------------------------------------------*/

wd(Exp,Depth,L1,Hole,WInfo) 
	:-
    wd(Exp,1200,Depth,nospace,Space,L1,L2,0,WInfo),
    wd_space(Space,L2,Hole).


    /*----------------------------------------------------------------------------*
     | wd/9
     | wd(Term,Precedence,Depth,SpaceIn,SpaceOut,List,Hole,BP,WInfo)
     |
     | Term is the term to output.
     |
     | Precedence is the current precedence level of the term.  This is
     | related to the operator for which the term is a subexpression of.  It
     | will either be equal to the operators precedence or one less depending
     | on the associativity.
     |
     | Depth is the maximum depth to write to.  When we get to zero, we either
     | output a * or a ... depending upon where we are at.
     |
     | SpaceIn will either be one of two constants: space or nospace.  space
     | indicates that previous output will require an operator (or negative
     | number) to have a space in front of it.
     |
     | SpaceOut will be unified with one of nospace or space.
     |
     | List is the list of characters which we are producing to output
     |
     | Hole is the hole at the end of the list so that procedure which
     | call this one may have easy access to the end of the list.
     |
     | BP is the base priority of the breaks which we will be putting
     | into the output stream.  This base priority is used to compute the
     | actual priority of a break.
     |
     | WInfo is a structure containing write information (things like the
     | write depth, whether it is flat or non flat characters to write out
     | in the event that the write depth is exceeded etc.
     *----------------------------------------------------------------------------*/

#if (all_procedures(syscfg,intconstr,0,_))
wd(Var, Lev, Depth, SpaceIn, nospace, L1, Hole,BP,WInfo) 
	:-
	'$is_delay_var'(Var),
	winfo_show_interval(WInfo,true),
	reified_interval(Var, SPrt),
	!,
	wd(SPrt, Lev, Depth, SpaceIn, nospace, L1, Hole,BP,WInfo).
#endif

wd(Var, Lev, Depth, SpaceIn, nospace, L1, Hole,BP,WInfo) 
	:-
	'$is_delay_var'(Var),
	winfo_show_delay(WInfo,true),
	'$delay_term_for'(Var, VarDelayTerm),
	!,
	VarDelayTerm = '$delay'(_,_,_,Goal),
	functor(Goal,F,A),
	sio_var_to_atom(Var,VarAtom),
	wd('$delay'(VarAtom,F/A), Lev, Depth, SpaceIn, nospace, L1, Hole,BP,WInfo).

wd(Var, Lev, Depth, _, nospace,[VarAtom|Hole],Hole,BP,WInfo) 
	:-
	'$is_delay_var'(Var),
	!,
	sio_var_to_atom(Var,VarAtom0),
	'$atom_concat'('_$',VarAtom0,VarAtom).

    %% == Write out variables (lettervar option, first occurrence of variable)
wd(Var, Lev, Depth, _, nospace,[VarAtom|Hole],Hole,BP,WInfo) 
	:-
	var(Var),
	winfo_lettervars(WInfo),
	!,
	winfo_varcounter(WInfo,VarNum),
	sio_lettervar(VarNum,VarAtom0),
	'$atom_concat'('_',VarAtom0,VarAtom),
	Var = '%lettervar%'(VarAtom),
	NextVarNum is VarNum+1,
	set_winfo_varcounter(WInfo,NextVarNum).

    %% == Write out variables (non-lettervar). Note that we still use the
    %%    lettervar mechanism in order to handle the gc problem.
wd(Var, Lev, Depth, _, nospace,[VarAtom|Hole],Hole,BP,WInfo) 
	:-
	var(Var),
	!,
	sio_var_to_atom(Var,VarAtom),
	Var = '%lettervar%'(VarAtom).

    %% == Write out occurences of lettervar variables
wd('%lettervar%'(Atom), Lev, Depth, _, nospace,[Atom|Hole],Hole,BP,WInfo) 
	:-
	atom(Atom),
	!.

    %% == Handle numbervars terms
wd('$VAR'(Num), Lev, Depth, _, nospace,[VarAtom|Hole],Hole,BP,WInfo) 
	:-
	winfo_numbervars(WInfo),
	sio_lettervar(Num,VarAtom),
	!.

    %% == Handle negative numbers
wd(Neg, Lev, Depth, SpaceIn, nospace,List,Hole,BP,WInfo) 
	:-
	number(Neg),
	Neg < 0,
	!,
	wd_space(SpaceIn,List,[NegAtom|Hole]),
	sio_sprintf_number(Neg,NegAtom,_).

    %% == Handle non-negative numbers
wd(Num, Lev, Depth, _, nospace,[NumAtom|Hole],Hole,BP,WInfo) 
	:-
	number(Num),
	!,
	sio_sprintf_number(Num,NumAtom,_).

    %% == Handle atoms
wd(Atom, Lev, Depth, SpaceIn, SpaceOut,List,Hole,BP,WInfo) 
	:-
	atom(Atom),
	!,
	winfo_quoted(WInfo,Quoted),
	wd_atom(Quoted,Atom,SpaceIn,SpaceOut,List,Hole).

    %% == Write out a * if too deep
wd(Term, Lev, 0, SpaceIn, space,List,Hole,BP,WInfo) 
	:-!,
    winfo_maxdepth_atom(WInfo,TooDeepAtom),
    wd_space(SpaceIn,List,[TooDeepAtom | Hole]).

    %% == Write out double quoted strings (lists of a special form)
wd([H | T], Lev, Depth, _, nospace, [0'"| L1], Hole, BP,WInfo) 
	:-
    winfo_quoted_strings(WInfo),
    is_string(H,T,L1,[0'" | Hole]),
    !.

    %% == Write out lists
wd([H | T], Lev, Depth, _, nospace, [0'[,Break | L1], Hole, BP,WInfo) 
	:-
    winfo_ignore_ops(WInfo,false),
    !,
    bpup(BP,BPup),			%% up the base priority
    left_break(BPup,Break,ILRestore),
    wdarg(H, Depth, L1, L2, BPup,WInfo),	%% Depth decremented in wdarg
    tail_depth(Depth, TailDepth),
    wdlt(T, TailDepth, L2, [0'],ILRestore | Hole], BPup ,WInfo).

    %% == Write out curly bracketed terms
wd({Term}, Lev, Depth, _, nospace, [0'{,Break | L1], Hole, BP, WInfo) 
	:-!,
    bpup(BP,BPup),
    left_break(BPup,Break,ILRestore),
    NewDepth is Depth-1,
    wd(Term,1200,NewDepth,nospace,_,L1,[0'},ILRestore | Hole],BP,WInfo).

    %% == Write out terms involving operators
wd(OpTerm, Lev, Depth, SpaceIn, SpaceOut, List, Hole, BP,WInfo) 
	:- 
    winfo_ignore_ops(WInfo,false),
    functor(OpTerm,F,A), 
    is_op(F,A,OLev,Assoc),
    !,
    NewDepth is Depth-1,
    wdop(Assoc,Lev,OLev,F,OpTerm,NewDepth,SpaceIn,SpaceOut,List,Hole,BP,WInfo).

    %% == Write out structures
wd(Struct, Lev, Depth, SpaceIn, nospace, L1, Hole,BP,WInfo) 
	:-
    functor(Struct,F,A),
    bpup(BP,BPup),			%% up the base priority
    left_break(BPup,Break,ILRestore),
    winfo_quoted(WInfo,Quoted),
    wd_atom(Quoted,F,SpaceIn,_,L1,[0'(,Break | L2]),
    arg(1,Struct,A1),
    wdarg(A1,Depth,L2,L3,BPup,WInfo),
    tail_depth(Depth,TailDepth),
    wdargs(2,A,Struct,TailDepth,L3,[0'),ILRestore | Hole],BPup,WInfo).

    /*
     * wdlt/2
     * 
     * Write out a list tail
     */
    
    %% == Handle variables
wdlt(Var, Depth, [0'|,Break | L1], Hole, BP,WInfo) :-
    var(Var),
    !,
    comma_break(BP,Break,ILRestore),
    wd(Var,Depth,L1,[ILRestore | Hole],WInfo).

    %% == Handle end of list
wdlt([], Depth, L, L, BP,WInfo) :-
    !.

    %% == Handle other terms when the depth is zero
wdlt(Other, 0, [0',, Dots | Hole], Hole,BP,WInfo) :-
    winfo_maxdepth_listtail(WInfo,Dots),
    !.

    %% == When we have a list, write out a comma followed by the head of the
    %%    list followed by the tail...all with the appropriate depth.
wdlt([H | T], Depth, [0',,Break | L1], Hole,BP,WInfo) :-
    !,
    comma_break(BP,Break,ILRestore),
    wdarg(H,Depth,L1,L2,BP,WInfo),
    tail_depth(Depth,TailDepth),
    wdlt(T,TailDepth,L2,[ILRestore | Hole],BP,WInfo).

    %% == When we have a tail which is not a list, write out a vertical bar
    %%    and call wd on the tail
wdlt(Term, Depth, [0'|,Break | L1], Hole,BP,WInfo) :-
    comma_break(BP,Break,ILRestore),
    wdarg(Term,Depth,L1,[ILRestore | Hole],BP,WInfo).
    
    /*
     * wdarg/5
     *
     * write out an argument, that is something which will potentially be
     * delimited by commas.  The 999 is one less than the comma precedence.
     */
    
wdarg(Arg, Depth, List, Hole,BP,WInfo) :-
    NewDepth is Depth-1,
    wd(Arg,999,NewDepth,nospace,_,List,Hole,BP,WInfo).


    /*
     * wdargs/7
     *
     * Write out remaining arguments to a structure.
     */

%% == Succeed once all of the arguments have been handled
wdargs(N,Max,Struct,Depth,Hole,Hole,BP,WInfo) :-
    N > Max,
    !.

%% == Indicate if left to right depth is too deep
wdargs(N,Max,Struct,0, [Dots|Hole],Hole,BP,WInfo) :-
    !,
    winfo_maxdepth_listtail(WInfo,Dots).

%% == Otherwise, output a comma, the next argument in structure and then
%%    recurse.
wdargs(N,Max,Struct,Depth, [0',,Break | L1], Hole, BP,WInfo) :-
    comma_break(BP,Break,ILRestore),
    arg(N,Struct,Arg),
    wdarg(Arg,Depth, L1,L2,BP,WInfo),
    NN is N+1,
    tail_depth(Depth,TailDepth),
    wdargs(NN,Max,Struct,TailDepth, L2,[ILRestore | Hole], BP,WInfo).
    


    /*
     * tail_depth/2
     * 
     * Returns the depth to use for the "tail" of a list or structure
     * depending upon the write mode.
     */
    
tail_depth(Depth,NewDepth) :-
	get_depth_computation_constant(C),
	NewDepth is Depth - C.


    /*
     * is_string/4, is_string/3
     *
     * Determines if a list is printable as a double quoted string and
     * returns the character sequence if so.
     */

is_string(H,T,[0'\\,H | NewT],Hole) :-
	nonvar(H),
	H = 0'\",
	!,
	nonvar(T),
	is_string(T,NewT,Hole).
is_string(H,T,[0'\\,H | NewT],Hole) :-
	nonvar(H),
	H = 0'\\,
	!,
	nonvar(T),
	is_string(T,NewT,Hole).
is_string(H,T,[H | NewT],Hole) :-
	integer(H),
	nonvar(T),
	32 =< H, H =< 126,
	is_string(T,NewT,Hole).

is_string([],Hole,Hole) :- !.
is_string([H | T], L, Hole) :-
	is_string(H,T,L,Hole).



    /*
     * wdop
     *
     * Write out a term involving an operator.
     */

    %% == Parentheses needed
wdop(Assoc,Lev,OLev,OFunc,Exp,Depth,SpaceIn,nospace, L1,Hole, BP,WInfo) :-
    Lev < OLev,
    !,
    wd_space(SpaceIn, L1,[0'(, break(BPup,_,1,ILR) | L2]),
    bpup(BP,BPup),
    wdop(Assoc,1200,OLev,OFunc,Exp,Depth,nospace,_, L2, 
	 [0'),ilrestore(ILR) | Hole],BPup,WInfo).

    %% == Prefix operators
wdop(fy,_,OLev,Func,Exp,Depth,SpaceIn,SpaceOut, L1,Hole, BP,WInfo) :-
    !,
    wd_prefix(Func,SpaceIn,SpaceInt, L1,L2,WInfo),
    arg(1,Exp,A1),
    wd(A1,OLev,Depth,SpaceInt,SpaceOut, L2,Hole, BP,WInfo).
wdop(fx,_,OLev,Func,Exp,Depth,SpaceIn,SpaceOut, L1,Hole, BP,WInfo) :-
    !,
    wd_prefix(Func,SpaceIn,SpaceInt, L1,L2,WInfo),
    arg(1,Exp,A1),
    Lev1 is OLev-1,
    wd(A1,Lev1,Depth,SpaceInt,SpaceOut, L2,Hole, BP,WInfo).

    %% == Postfix operators
wdop(yf,_,OLev,Func,Exp,Depth,SpaceIn,SpaceOut, L1,Hole, BP,WInfo) :-
    !,
    arg(1,Exp,A1),
    wd(A1,OLev,Depth,SpaceIn,SpaceInt, L1,L2, BP,WInfo),
    wd_postfix(Func,SpaceInt,SpaceOut, L2,Hole,WInfo).
wdop(xf,_,OLev,Func,Exp,Depth,SpaceIn,SpaceOut, L1,Hole, BP,WInfo) :-
    !,
    arg(1,Exp,A1),
    Lev1 is OLev-1,
    wd(A1,Lev1,Depth,SpaceIn,SpaceInt, L1,L2, BP,WInfo),
    wd_postfix(Func,SpaceInt,SpaceOut, L2,Hole,WInfo).

    %% == Infix operators
wdop(xfx,_,OLev,Func,Exp,Depth,Space1,Space4, L1,Hole, BP,WInfo) :-
    !,
    Lev1 is OLev-1,
    arg(1,Exp,A1),
    arg(2,Exp,A2),
    wd(A1,Lev1,Depth, Space1,Space2, L1,L2, BP,WInfo),
    wd_infix(Func,Lev1,A2, Space2,Space3, L2,L3, BP,ILR,WInfo),
    wd(A2,Lev1,Depth, Space3,Space4, L3,[ilrestore(ILR) | Hole],BP,WInfo).
wdop(xfy,_,OLev,Func,Exp,Depth,Space1,Space4, L1,Hole,BP,WInfo) :-
    !,
    Lev1 is OLev-1,
    arg(1,Exp,A1),
    arg(2,Exp,A2),
    wd(A1,Lev1,Depth, Space1,Space2, L1,L2,BP,WInfo),
    wd_infix(Func,OLev,A2, Space2,Space3, L2,L3, BP,ILR,WInfo),
    wd(A2,OLev,Depth, Space3,Space4, L3,[ilrestore(ILR) | Hole], BP,WInfo).
wdop(yfx,_,OLev,Func,Exp,Depth,Space1,Space4, L1,Hole, BP,WInfo) :-
    !,
    arg(1,Exp,A1),
    arg(2,Exp,A2),
    Lev2 is OLev-1,
    wd(A1,OLev,Depth, Space1,Space2, L1,L2,BP,WInfo),
    wd_infix(Func,Lev2,A2, Space2,Space3, L2,L3, BP,ILR,WInfo),
    wd(A2,Lev2,Depth, Space3,Space4, L3,[ilrestore(ILR) | Hole],BP,WInfo).


    /*
     * wd_prefix/6
     *
     * wd_prefix emits a prefix operator putting in spaces as necessary.
     */

    %% == Operator has non-alpha characters (like '-')
wd_prefix(Op,InSpace,space, List,Hole,WInfo) :- 
    sio_isgraphicatom(Op),
    !,
    wd_space(InSpace, List,[Op | Hole]).

    %% == Operator is only alpha characters.
wd_prefix(Op,InSpace,nospace, List,Hole,WInfo) :-
    wd_space(InSpace, List, [Op,0'  | Hole]).

    /*
     * wd_postfix/6
     *
     * wd_postfix emits a postfix operator putting in spaces where necessary.
     */

    %% == Operator has non-alpha characters (like '-')
wd_postfix(Op,InSpace,space, List,Hole,WInfo) :-
    sio_isgraphicatom(Op),
    !,
    wd_space(InSpace, List,[Op|Hole]).

    %% == Operator is only alpha characters
wd_postfix(Op,_,space, [0' ,Op | Hole],Hole,WInfo).

    /*
     * wd_infix/10
     *
     * wd_infix emits an infix operator putting in spaces as necessary.
     */

/* -- old code --
    %% == Operator is one of ->/2 or ;/2
wd_infix(Op,Lev,A2,InSpace,nospace, [spop(Pri,_,Op,ILR) | Hole],Hole,
        	BasePri, ILR,WInfo) :-
    is_arrow_or_semi(Op,OPri),
    Pri is BasePri + OPri,
    !.
-- old code --*/

    %% == Operator is  ->/2
wd_infix('->',Lev,A2,InSpace,nospace, [0' ,'->',space(Pri,_,1,ILR) | Hole],Hole,
        	BasePri, ILR,WInfo) :-
    Pri is BasePri + 3,
    !.

    %% == Operator is ;/2
wd_infix(';',Lev,A2,InSpace,nospace, [spop(Pri,_,';',ILR) | Hole],Hole,
        	BasePri, ILR,WInfo) :-
    Pri is BasePri + 2,
    !.

    %% == Operator is one of :-/2 or -->/2
wd_infix(Op,Lev,A2,_,nospace, [0' ,Op,space(Pri,_,2,ILR) | Hole],Hole,
						BasePri, ILR,WInfo) :-
    Lev >= 1199,
    !,
    Pri is BasePri+1.
    
    %% == Operator is comma
wd_infix(',',Lev,A2,InSpace,nospace, [0',, space(Pri,_,0,ILR) | Hole], Hole,
    BasePri, ILR,WInfo) :-
    !,
    Pri is BasePri+4.

    %% == Operator is '=' in certain places
wd_infix('=',Lev,A2,InSpace,nospace, [0' ,'=',space(Pri,_,1,ILR) | Hole],
    Hole, BasePri, ILR,WInfo) :-
    BasePri =< 5,
    !,
    Pri is BasePri+5.

    %% == Operator has non-alpha characters (like '-')
wd_infix(Op,Lev,A2,InSpace,OutSpace, L1,Hole, BasePri,ILR,WInfo) :- 
%wd_infix(Op,Lev,A2,InSpace,OutSpace, [0' | L1], Hole, BasePri,ILR,WInfo) :- 
    sio_isgraphicatom(Op),
    !,
    spaceahead(Lev,A2,InSpace,IntSpace),
    op_pri(Op,2,BasePri,Pri),
    infix_ild(Op,ILD),
    winfo_quoted(WInfo,Quoted),
    wd_atom(Quoted,Op,IntSpace,OutSpace,L1,[break(Pri,_,ILD,ILR) | Hole]).
%    wd_atom(Quoted,Op,IntSpace,OutSpace,L1,[space(Pri,_,ILD,ILR) | Hole]).

    %% == Operator is only alpha characters.
wd_infix(Op,Lev,A2,InSpace,OutSpace, [0'  | L1], Hole,BasePri,ILR,WInfo) :-
    op_pri(Op,2,BasePri,Pri),
    winfo_quoted(WInfo,Quoted),
    wd_atom(Quoted,Op,InSpace,OutSpace,L1,[space(Pri,_,_,ILR) | Hole]).


    %% == see if second argument will require space in order to put
    %%    space before the infix operator for a balanced look.
spaceahead(_,_,space,space) :- !.	%% already need space
spaceahead(Lev,A2,_,space) :-
    nonvar(A2),
    functor(A2,F,A),
    F \= '.',
    is_op(F,A,OLev,Assoc),
    spaceneeded(Assoc,Lev,OLev),
    !.
spaceahead(_,_,_,nospace).

    %% prefix operator will be coming up in second argument
spaceneeded(fx,_,_).
spaceneeded(fy,_,_).
    %% == parens will be needed on second argument
spaceneeded(_,Lev,OLev) :- OLev > Lev.


/*-- old code --
is_arrow_or_semi('->', 3).
is_arrow_or_semi(';',  2).
-- old code --*/

infix_ild(',',0) :- !.
infix_ild(_,_).		%% leave variable to fill in later on



    /*
     * is_op/4
     * is_op(F,A,Lev,Assoc)
     *
     * Determine if a functor F with arity A is an operator and if so unify
     * Lev with the operator's precedence level and Assoc with its
     * associativity.
     */
    
is_op(F,A,Lev,Assoc) :-
    current_op(Lev,Assoc,F),
    assocs(A,Assoc),
    !.
    

    /*
     * assocs/2
     *
     * Enumerates legal arities and associativities.
     */

assocs(1,fx).
assocs(1,fy).
assocs(1,yf).
assocs(1,xf).
assocs(2,xfx).
assocs(2,xfy).
assocs(2,yfx).

    /*
     * wd_space/3
     *
     * writes out a space maybe.
     */

wd_space(space,[0'  | Hole],Hole) :- !.
wd_space(nospace,Hole,Hole).


    /*
     * wd_atom(Quoted,Atom,SpaceIn,SpaceOut,List,Hole)
     *
     * wd_atom is responsible for putting an atom into one of our open ended
     * lists
     *
     *		Quoted	-- either true or false.  true mean we want to atom
     *			   to be quoted if necessary. false means to write
     *			   out the atom as is.
     *		Atom	-- the atom to work on
     *		SpaceIn	-- space indicated that the last token previously
     *			   output was a graphic symbol. nospace indicates
     *			   that it was not.  Hence, if the atom we are
     *			   outputting is also a graphic symbol, a space will
     *			   be required.
     *		SpaceOut-- set to space if the atom is a graphic symbol (and
     *			   quoted set to true); set to nospace otherwise.
     *		List	-- the list into which to place the possibly
     *			   transformed atom
     *		Hole	-- the hole at the end of the list
     */

wd_atom(true,Atom,SpaceIn,SpaceOut,List,Hole) :-
	!,
	sio_qatom(Atom,QAtom,_),
	wd_atom_space(SpaceIn,QAtom,SpaceOut,List,[QAtom|Hole]).
wd_atom(false,Atom,_,nospace,[Atom|Hole],Hole).

wd_atom_space(SpaceIn,Atom,space,List,Hole) :-
	sio_isgraphicatom(Atom),
	!,
	wd_space(SpaceIn,List,Hole).
wd_atom_space(_,Atom,nospace,L,L).


    /*
     * bpup increments the base priority.
     */
    
bpup(BP,BPup) :- BPup is BP+5.

    /*
     * comma_break(BP,Break,ILR) generates a suitable break for comma level
     * operators.
     */
    
comma_break(BP,break(Pri,_,_,ILR),ilrestore(ILR)) :-
    Pri is BP + 4.

    /*
     * left_break(BP,Break,ILR) generates a suitable break for a left paren or
     * square bracket.
     */

left_break(BP,break(Pri,_,_,ILR),ilrestore(ILR)) :-
    Pri is BP + 5.


    /*
     * op_pri/4 gets the priority to use for determining line breaks.
     */

op_pri(Op,2,BasePri,Pri) :-
    op_pri(Op,IPri),
    !,
    Pri is BasePri + IPri.
op_pri(_,_,BasePri,Pri) :- Pri is BasePri+5.

op_pri(':-',1).
op_pri(';',2).
op_pri('->',3).
op_pri(',',4).
op_pri(_,5).


    /*-------------------------------------------------------------------------------*
     |
     | output_string/5
	 | output_string(Stream,List,Width,InitialIndent,LineEnd) 
     |
     | This procedure and its subsidiary routines form the second component
     | of the pretty printing package.  The first component, wd was
     | responsible for the list which output_string receives.  wd has put
     | in suggested positions for line breaks (probably far too many) in the
     | output string.   output_string will output this string and put in the
     | actual line breaks in addition to controlling the indentation.
     |
     |	Stream	-- 	the stream to write to
     |	List	-- 	list with the characters or atoms to output and
     |			   	suggested break positions
     |	Width	-- 	the width of the line
     |	InitialIndent -- 
	 |				the initial indentation in characters (spaces)
     |				After the first break, the pretty printer will indent
     |				by this number of spaces.  This is so that facilities
     |				like showanswers or the debugger may get output
     |				which is neatly lined up.
 	 |	LineEnd -- 	false/true: whether to quote eol with \ , or not
     *-------------------------------------------------------------------------------*/

output_string(Stream,List,Width,InitialIndent,LineEnd) 
	:-
    NetWidth is Width-InitialIndent,
    sio_position_in_line(Stream,_,InitialPosition0),
    InitialPosition is InitialPosition0 - InitialIndent,
	% FIXME: What if InitialPosition is less than 0
    scan(List,InitialPosition,[],NetWidth,0,[],LineEnd),
%    debug_write(List),nl,
    os(List,Stream,0,InitialIndent).

debug_write([]) :- nl,!.
debug_write([H|T]) :- integer(H), !, put(H), debug_write(T).
debug_write([H|T]) :- put(0' ),write(H),put(0' ),debug_write(T).


/*-----------------------------------------------------------------------------------*
 | scan/7, scan/8
 |
 | scan(List,Count,Breaks,LineLen,BreakLev,IL,EL)
 | scan(Head,Tail,Count,Breaks,LineLen,BreakLev,IL,EL)
 |
 | These procedures scan the list to be output and fill in the break
 | information as necessary.  The parameters are as follows:
 |
 |  List	--	List to scan and fill in break information
 |  Count	--	Number of characters in "line" so far.  The line in question
 |		   		is the line considered from the last break with indentation.
 |  Breaks	--	information on where previous breaks are/can be.  This is
 |		   		a list prioritized so that the first elements are the places
 |		   		to cause line breaks at first.
 |  LineLen	--	The maximum length of a line.  This parameter is
 |		   		constant throughout the scan procedure and will not vary.
 |  BreakLev --	An integer which tells us whether or not to automatically
 |		   		generate a break when a break is seen.  If the priority of
 |		   		the break found is less than BreakLev, then a break is
 |		   		generated.  Subsequent breaks will see a different break
 |		   		level.  This mechanism will cause a significant break to
 |		   		be generated after a relatively insignificant one.  Thus
 |		   		the structure of the program will be somewhat more evident.
 |  ILL		--	Indentation Level; This is a list of even length, every
 |		   		two elements of the list form one logical component (being
 |		   		more space efficient than alternate methods).  The first
 |		   		element is the position.  The second component is the
 |		   		indentation level.  This component will often be a
 |		   		variable.  As such, it will be ignored when determining
 |		   		the actual indentation level for a break.  This structure
 |		   		is necessary since it is possible to pass over many ilrestore
 |		   		directives before determining that a break is necessary.
 |		   		In order to easily add to the list, it is arranged that
 |		   		more recent positions are added to the front of the list.
 |	EL		--	Boolean (true/false): whether end of lines are normal (true)
 |				or extended (quoted with \) (false)
 |		
 *-----------------------------------------------------------------------------------*/

scan([],Count,Break,LineLen,BreakLev,ILL,EL) :- !.
scan([H|T],Count,Breaks,LineLen,BreakLev,ILL,EL) :-
	scan(H,T,Count,Breaks,LineLen,BreakLev,ILL,EL).


%% == Handle the ilrestore directives
scan(ilrestore(ILRestore),T,Count,Breaks,LineLen,BreakLev,ILL,EL) :-
	!,
	scan(T,Count,Breaks,LineLen,BreakLev,[Count,ILRestore | ILL],EL).

%% == Generate a break if we've gone too far.
scan(H,T,Count,[b(Pos,Break) | MoreBreaks],LineLen,BreakLev,ILL,EL) :-
	Count >= LineLen,
	!,
	indentation_level(ILL,Pos,IL),
	break_ILDelta(Break,ILD),	
	break_Priority(Break,Priority),
	break_ILRestore(Break,IL),		%% set the level to restore to
	break_Status(Break,Status),
	ilNew(ILD,Priority,IL,BreakLev,NewIL),	%% Compute new indentation level
	breakCount(Break,EL,BreakCount),
	AdjPos is Pos+BreakCount,		%% want stuff below to subtract
						%% off the number of characters
						%% occupied by the break
	make_status(Pos,NewIL,Break,EL,Status),
	fixBreaks(MoreBreaks,AdjPos,NewBreaks,NewIL),	%% fix the break info
	fixILL(ILL,AdjPos,NewIL,NewILL),
	NewCount is NewIL*4+Count-AdjPos,
	scan([H|T],NewCount,NewBreaks,LineLen,Priority,NewILL,EL).

%% == skip over actual characters and increment count
scan(Char,T,Count,Breaks,LineLen,BreakLev,ILL,EL) :-
	integer(Char),
	!,
	NewCount is Count+1,
	scan(T,NewCount,Breaks,LineLen,BreakLev,ILL,EL).

%% == skip over atom and increment count
scan(Atom,T,Count,Breaks,LineLen,BreakLev,ILL,EL) :-
	atom(Atom),
	!,
	atom_length(Atom,AtomLength),
	NewCount is Count+AtomLength,
	scan(T,NewCount,Breaks,LineLen,BreakLev,ILL,EL).

%% == Must have a break of some sort.  Handle mandatory break generation.
scan(Break,T,Count,Breaks,LineLen,BreakLev,ILL,EL) :-
	break_Priority(Break,Priority),
	Priority < BreakLev,
	!,
	indentation_level(ILL,Count,IL),
	break_ILDelta(Break,ILD),
	break_ILRestore(Break,IL),
	break_Status(Break,break),
	ilNew(ILD,Priority,IL,BreakLev,NewIL),
	breakCount(Break,EL,BreakCount),
	AdjCount is Count+BreakCount,
	fixBreaks(Breaks,AdjCount,NewBreaks,NewIL),
	fixILL(ILL,AdjCount,NewIL,NewILL),
	NewCount is NewIL*4-BreakCount,
	scan(T,NewCount,NewBreaks,LineLen,Priority,NewILL,EL).

%% == Insert break into break list
scan(Break,T,Count,Breaks,LineLen,BreakLev,ILL,EL) :-
	break_Priority(Break,Priority),
	break_ILRestore(Break,ILRestore),
	insertBreak(Breaks,Priority,b(Count,Break),NewBreaks),
	breakCount(Break,EL,BreakCount),
	NewCount is Count+BreakCount,
	scan(T,NewCount,NewBreaks,LineLen,BreakLev,[Count,ILRestore | ILL],EL).


    /*
     * make_status(Pos,NewIL,EL,Status)
     *
     * Used to fill in the break status.  The usual thing to do is set the
     * status to break, but when there is still room on the line to do
     * the proper indentation, we will choose to do that instead.  This means
     * that parens and short functors or predicate names will not be sitting
     * all by themselves on a line.
     *
     */

make_status(Pos,NewIL,BreakStruct,_,space(NSpaces)) :-
    Pos =< NewIL*4,
    functor(BreakStruct,F,_),
    F \= spop,
    !,
    NSpaces is NewIL*4-Pos.
make_status(_,_,_,false,qbreak) :-!.
make_status(_,_,_,_,break).


    /*
     * indentation_level(ILL,Position,IL)
     *
     * Determines the indentation level, IL from a list of positions and
     * associated indentation levels, ILL for a given position (Position).
     */
    
%% == Handle case of the empty list -- indentation level 0
indentation_level([],_,0) :- !.
%% == Found a good indentation level when the list position is less than
%%    the input position and the indentation level is non-variable.
indentation_level([LPos,LIL | More],Pos,IL) :-
    nonvar(LIL),
    LPos =< Pos,
    !,
    LIL = IL.
%% == Traverse the list some more looking for a smaller position
indentation_level([_,_ | More],Pos,IL) :-
    indentation_level(More,Pos,IL).



    /*
     * insertBreak(Breaks,Priority,Break,NewBreaks)
     *
     * Inserts the prioritized break into the break list at the proper
     * point.
     */

insertBreak([],_,Break,[Break]) :- !.
insertBreak([H|T], Priority, Break, NewBreaks) :-
    arg(2,H,B),
    break_Priority(B,BP),
    insertBreak(BP,Priority,H,T,Break,NewBreaks).
    
%% == Replace break of same priority
insertBreak(Pri,Pri,H,T,Break,[Break|T]) :- !.
%% == Priority of new break less than first in list; add to front
insertBreak(BP,Pri,H,T,Break,[Break,H|T]) :-
    Pri < BP,
    !.

%% == Otherwise, need to keep scanning the breaks.
insertBreak(_,Pri,H,T,Break,[H|NewT]) :-
    insertBreak(T,Pri,Break,NewT).



    /*
     * breakCount(Break,EL,BreakCount)
     *
     * Returns the number of characters occupied by a non-taken break.
     */
    
breakCount(break(_,_,_,_),false,1) :- !.
breakCount(break(_,_,_,_),_,0) :- !.
breakCount(spop(_,_,Atom,_),_,N) :- atom_length(Atom,NC), !, N is NC+2.
breakCount(space(_,_,_,_),_,1).


    /*
     * ilNew(ILD,Priority,IL,BreakLev,NewIL)
     *
     */

ilNew(ILD,_,IL,_,NewIL) :-
    nonvar(ILD),
    !,
    NewIL is IL+ILD.
ilNew(0,Pri,IL,BreakLev,IL) :-
    Pri =< BreakLev,
    !.
ilNew(1,_,IL,_,NewIL) :-
    NewIL is IL+1.

    /*
     * fixBreaks(Pos,B,NewB,IL)
     *
     * Removes the superfluous breaks from the break list and updates the
     * position information.
     */
    
fixBreaks([],Pos,[],_) :- !.
fixBreaks([b(BPos,Break) | More], Pos, NewBreaks, IL) :-
    fixBreaks(BPos,Pos,Break,More,NewBreaks,IL).

%% Remove break from list.
fixBreaks(BPos,Pos,Break,More,NewBreaks,IL) :-
    BPos =< Pos,
    !,
    fixBreaks(More,Pos,NewBreaks,IL).
%% Update position of good break
fixBreaks(BPos,Pos,Break,More,[b(BPNew,Break) | MoreNew],IL) :-
    BPNew is BPos - Pos + IL*4,
    fixBreaks(More,Pos,MoreNew,IL).

    /*
     * fixILL(ILL,Pos,IL,NewILL),
     *
     * Used to update the indentation level list after a break has been
     * performed.
     */

fixILL([LPos,LIL | More],Pos,IL,[NewLPos,LIL | NewILL]) :-
    LPos > Pos,
    !,
    NewLPos is LPos - Pos + IL*4,
    fixILL(More,Pos,IL,NewILL).
fixILL(_,Pos,IL,[0,IL]) :- !.




    /*
     * break_Priority(Break,Priority)
     * break_Status(Break,Status)
     * break_ILDelta(Break,ILDelta)
     * break_ILRestore(Break,ILRestore)
     *
     * These access the components of a break regardless of the functor.
     */

break_Priority(Break,Priority) :- arg(1,Break,Priority).
break_Status(Break,Status) :- arg(2,Break,Status).
break_ILDelta(Break,0) :- functor(Break,spop,_), !.
break_ILDelta(Break,ILD) :- arg(3,Break,ILD).
break_ILRestore(Break,ILR) :- arg(4,Break,ILR).


    /*
     * os(String,Stream,IL,II)
     *
     * Output the string with indentation level IL.
     */

os([],Stream,_,_) :-
	!.
os([H | T],Stream,IL,II) :-
	os2(H,T,Stream,IL,II).

	%% == Handle special operators 
	%% (Operators to be written out AFTER a line break)
os2(spop(Pri,nobreak,Atom,IL),T,Stream,IL,II) 
	:-!,
	put_code(Stream,0' ),
	put_atom(Stream,Atom),
	put_code(Stream,0' ),
	os(T,Stream,IL,II).

os2(spop(Pri,break,Atom,_IL),T,Stream,IL,II) 
	:-
	ILP is IL-1,
	nl(Stream),
	tab(Stream,II),
	indent(Stream,ILP),
	put_atom(Stream,Atom),
	atom_length(Atom,AtomLength),
	TabWidth is 4-AtomLength,
	tab(Stream,TabWidth),
	!,
	os(T,Stream,IL,II).

	%% == Handle a break

os2(break(Pri,nobreak,_,IL),T,Stream,IL,II) 
	:-!,
	os(T,Stream,IL,II).

os2(break(_,BreakType,ILD,_IL),T,Stream,IL,II) 
	:-
	dmember(BreakType, [break,qbreak]),
	ILNew is IL+ILD,
	(BreakType = qbreak -> put_code(Stream, 0'\\) ; true),
	nl(Stream),
	tab(Stream,II),
	indent(Stream,ILNew),
	eat_space(T,TT),
	!,
	os(TT,Stream,ILNew,II).

os2(break(_,space(Spaces),ILD,_IL),T,Stream,IL,II) 
	:-
	ILNew is IL+ILD,
	tab(Stream,Spaces),
	eat_space(T,TT),
	!,
	os(TT,Stream,ILNew,II).

	%% == Handle a space break

os2(space(Pri,nobreak,_,IL),T,Stream,IL,II) 
	:-!,
	put_code(Stream,0' ),
	os(T,Stream,IL,II).

os2(space(_,break,ILD,_IL),T,Stream,IL,II) 
	:-
	ILNew is IL+ILD,
	nl(Stream),
	tab(Stream,II),
	indent(Stream,ILNew),
	!,
	os(T,Stream,ILNew,II).

os2(space(_,space(Spaces),ILD,_IL),T,Stream,IL,II) 
	:-
	ILNew is IL+ILD,
	tab(Stream,Spaces),
	!,
	os(T,Stream,ILNew,II).

	%% == do an ilrestore

os2(ilrestore(IL),T,Stream,_,II) 
	:-!,
	os(T,Stream,IL,II).

	%% == put out an atom
os2(H,T,Stream,IL,II) 
	:-
	atom(H),
	!,
	put_atom(Stream,H),
	os(T,Stream,IL,II).

	%% == put out the character
os2(H,T,Stream,IL,II) :- 
	put_code(Stream,H),
	os(T,Stream,IL,II).


    /*
     * indent/1 outputs the right number of spaces for the given
     * indentation level.
     */

indent(Stream,Count) :-
	Count =< 0,
	!.
indent(Stream,Count) :-
	put_atom(Stream,'    '),
	NewCount is Count-1,
	indent(Stream,NewCount).

export tab/2.

tab(Stream,Count) :-
	Count =< 0,
	!.
tab(Stream,Count) :-
	put_code(Stream,0' ),
	NewCount is Count-1,
	tab(Stream,NewCount).

    /*
     * eat_space/2 will consume a space immediately following a break
     * if one exists.
     */

eat_space([' ' | T], T) :- !.
eat_space([0'   | T],T) :- !.
eat_space(L,L).

endmod.		%% sio


module builtins.
use sio.

    /*-------------------------------------------------------------*
     * write_out/[1,2] are used by listing to write out the clauses.
     *-------------------------------------------------------------*/

write_out(Term) 
	:-
	sio:get_current_output_stream(Stream),
	write_out(Stream, Term).

write_out(Stream, Term) 
	:-
	write_term(Stream,Term,[]),
	put_atom(Stream,'.'),
	nl(Stream).

/*-----------------------------------------------------------------------*
 | write_substs/4
 | write_substs(Stream,Names,Substs, NonAnonNames)
 | write_substs(+,+,+, -)
 |
 |	write_substs/4 is called by showanswers to write out a list of 
 |	substitutions on output Stream; it returns the list NonAnonNames
 |	of names of non-anonymous variables for which values were output.
 |	{Used by showanswers to avoid prompting for (return/;) when no
 |	binds are output -- all vars in query are anoymous.}
 |
 |	Names and Substs are lists of the same length, with the items
 |	corresponding pairwise: An item N on Names is the atom which is the
 |	name of a variable which was typed in in the original top-level query;
 |	The corresponding item on Substs is really the original variable
 |	assigned to N, now (possibly) bound to some item;
 |
 |	WARNING: Assumes that Names \= [].  This is guaranteed by the only
 |	call to write_substs blt_shl.pro, which is in showanswers/4.
 |
 |	write_substs/[2,4] will write out the substitutions, each
 |	on its own line, without emitting a newline for the last line of the 
 |	last substitution.  This permits the calling procedure (showanswers)
 |	to look for a response on the same line.  Variables are handled as 
 |	intelligently as possible.
 |
 |	A number of opertaions are performed:
 |	1)	Any anonymous vars (named by '_') and their values are deleted;
 |	2)	Actual input variable names are substituted for any original
 |		input variables which remain unbound;
 |	3)	Letter names are generated and substituted for any unbound variables 
 |		through out all of the output terms;
 |	4)	The resulting equations (Name = Value) are actually written out.
 |
 |  12 Sept 1995 -- KAB: Modifications to avoid binding delay variables during 
 |				printing, and to provide appropriate representations of interval 
 |				terms, and of other non-instantiated frozen variables.
 |  14 April 1996 -- KAB: Mofications to eliminate use of failure and use
 |				of mangle.
 *-----------------------------------------------------------------------*/

:- dynamic('$is_delay_var'/1).

write_substs(Stream,Names,Substs,NewNames) 
	:-
	delete_anon_vars(Names,Substs,NewNames,NewSubsts),
	cont_write_substs(NewNames,NewSubsts,Stream).

delete_anon_vars([],[],[],[]).

delete_anon_vars(['_'|N1],[_|S1],N2,S2) 
	:- !,
	delete_anon_vars(N1,S1,N2,S2).

delete_anon_vars([N|N1],[S|S1],[N|N2],[S|S2]) 
	:-
	delete_anon_vars(N1,S1,N2,S2).

cont_write_substs(Names,Substs,StreamOrAlias) 
	:-
	stream_or_alias_ok(StreamOrAlias,Stream),
	stream_wt_maxdepth(Stream, MaxDepth),
	subst_orig_toplevel_names(Names,Substs),
	stream_wt_depth_computation(Stream, DC),
	(DC = flat ->  DCC = 0 ; DCC = 1 ),
	subst_gen_letter_names(Substs,MaxDepth,DCC,Substs0),
	(Names = [] ->
		nl(Stream),
		(current_prolog_flag(anonymous_solutions, true) ->
			write_term(Stream,'<Anonymous_Solution>',[])
			;
			true
		)
		;
		show_substs_ext(Names,Substs0,Stream)
	).

subst_orig_toplevel_names([],[]).

subst_orig_toplevel_names([N|Ns],[S|Ss]) 
	:- 
	var(S),
	'$is_delay_var'(S),
	!,
	subst_orig_toplevel_names(Ns,Ss).

subst_orig_toplevel_names([N|Ns],['%lettervar%'(N)|Ss]) 
	:-
	subst_orig_toplevel_names(Ns,Ss).

subst_orig_toplevel_names([_|Ns],[_|Ss]) 
	:-
	subst_orig_toplevel_names(Ns,Ss).

epsilon_show(1e-6).

export set_eps_show/1.
set_eps_show(V)
	:-
	abolish(epsilon_show,1),
	assert(epsilon_show(V)).

/*-------------------------------------------------------------
 |	subst_gen_letter_names/4
 |	subst_gen_letter_names(InTerm,Depth,DCC,OutTerm) 
 |	subst_gen_letter_names(+,+,+,-) 
 |
 |	subst_gen_letter_names/6
 |	subst_gen_letter_names(InTerm,InCounter,Depth,DCC,OutTerm,OutCounter) 
 |	subst_gen_letter_names(+,+,+,+,-,-) 
 |
 |	Substitutes generated letter names for any variables
 |	occurring in Term; recurses through the structure of Term
 *-------------------------------------------------------------*/

subst_gen_letter_names(InTerm,Depth,DCC,OutTerm) 
	:-
	subst_gen_letter_names(InTerm,0,Depth,DCC,OutTerm,_). 

subst_gen_letter_names(Var,Ctr,Depth,DCC,Var,Ctr) 
	:-
	var(Var),
	'$is_delay_var'(Var),
	!.

subst_gen_letter_names(Var,VarNum,Depth,DCC,'%lettervar%'(VarAtom),NextVarNum)
	:-
	var(Var),
	!,
	sio_lettervar(VarNum,VarAtom0),
	'$atom_concat'('_',VarAtom0,VarAtom),
	Var = '%lettervar%'(VarAtom),
	NextVarNum is VarNum+1.

subst_gen_letter_names(Atomic,Ctr,Depth,DCC,Atomic,Ctr) 
	:-
	atomic(Atomic),
	!.

%subst_gen_letter_names([InHead | InTail], Ctr, 0, ['*'], Ctr) 
subst_gen_letter_names([InHead | InTail], Ctr, 0, DCC,['...'], Ctr) 
	:-!.

subst_gen_letter_names([InHead | InTail], InCtr, Depth,DCC,[OutHead | OutTail], OutCtr) 
	:-!,
	NewDepth is Depth - DCC,
	subst_gen_letter_names(InHead, InCtr, NewDepth, DCC,OutHead, InterCtr),
	subst_gen_letter_names(InTail, InterCtr, NewDepth, DCC,OutTail, OutCtr).

subst_gen_letter_names(InStruct,Ctr,0,DCC,'*',Ctr) 
	:-!.

subst_gen_letter_names(InStruct,InCtr,Depth,DCC,OutStruct,OutCtr) 
	:-
	NewDepth is Depth - DCC,
	InStruct =.. [Functor | InArgs],
	subst_gen_letter_names(InArgs, InCtr,NewDepth, DCC,OutArgs, OutCtr),
	OutStruct =.. [Functor | OutArgs].

export show_substs_ext/3.
show_substs_ext(Ns, Ss, Stream)
	:-
		%% VPairs is for printing delay terms:
	rpairs_list(Ns, Ss, VPairs),
	show_substs_ext(Ns, Ss, VPairs, Stream).

show_substs_ext([],[],_,Stream) 
	:- !.

show_substs_ext([N|Ns],[S|Ss],VPairs,Stream) 
	:-
	wr_subs3(N,S,VPairs,Stream),
	!,
	show_substs_ext(Ns,Ss,VPairs,Stream).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% show_[delay,interval]_binding/4
	%%   -- defined in blt_frez.pro
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wr_subs3(N,S,VPairs,Stream)
	:-
	'$is_delay_var'(S),
	!,
		%% inconstrs/0 is defined (or not) in main.c
	(intconstr ->
		show_interval_binding(N,S, VPairs, Stream)
		;
		show_delay_binding(N,S, VPairs, Stream)
	).
	
wr_subs3(N,S,_,Stream)
	:-
	printf(Stream, '\n%t=%t ', ['%lettervar%'(N), S], [quoted(true)]).

rpairs_list([],[],[]).

rpairs_list([N | Ns],[S | Ss], [(S,N) | Pairs])
	:-
	var(S),
	!,
	rpairs_list(Ns,Ss,Pairs).

rpairs_list([N | Ns],[S | Ss], Pairs)
	:-
	rpairs_list(Ns,Ss,Pairs).

exact_lookup([(Entry, Item) | DVarAtmsList], Term, Item)
	:-
	Entry == Term,
	!.

exact_lookup([ _ | DVarAtmsList], Term, Item)
	:-
	exact_lookup(DVarAtmsList, Term, VName).

endmod.		%% builtins

