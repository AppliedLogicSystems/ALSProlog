/*==========================================================================
 |			macro.pro
 |		Copyright (c) 1990-92 Applied Logic Systems, Inc.
 |
 |		Macro Expansion Tool for ALS Prolog
 |
 |		Based on Sei-Ich Kondoh & Takaski Chikayama, "Macro processing 
 |		in Prolog", in Logic Programming, Proc. 5th Intl Symposium &
 |		Conference, Kowalski & Bowen, eds, pp 466-480.
 |
 |	Author: Mitanu Paul;  revisions by KA Bowen
 *=========================================================================*/

/*
:- op(925,fx,``).
:- op(930,fx,`).
:- op(1200,xfx,=>).
:- op(1190,xfx,when).
:- op(1180,xfx,where).
:- op(1170,xfx,with).
:- op(1160,xfx,if).
*/

module macroxp.

export expand_macros/2.

expand_macros((:- Body),NewClause)
	:-!,
	expand_macros([],Body,NewClause).
expand_macros((Head :- Body),NewClause)
	:-!,
	expand_macros(Head,Body,NewClause).
expand_macros(Clause,NewClause)
	:-
	expand_macros(Clause,true,NewClause).

expand_macros(Head,Body,NewClause)
	:-
	expand_head(Head,Body,NewHead,NewBody),
	expand_body(NewBody,ExpandedBody),
	flatten_body(ExpandedBody,FlattenedBody),
	simplify_body(FlattenedBody,SimplifiedBody),
	make_clause(NewHead,SimplifiedBody,NewClause).

expand_head(Head,Body,Head,Body)
	:-
	var(Head), !.
expand_head([],Body,[],Body)
	:- !.
expand_head(``Head,Body,Head,Body)
	:- !.
expand_head(`Head,Body,Head,Body)
	:-
	(atomic(Head); var(Head)), !.
expand_head(`[HHead | HRest],Body,[ExpandedHHead | ExpandedHRest],NewBody)
	:- !,
	expand_head(HHead,Body,ExpandedHHead,Body1),
	expand_head(`HRest,Body1,ExpandedHRest,NewBody).
expand_head(`[],Body,[],Body)
	:- !.
expand_head(`Function,Body,NewFunction,NewBody)
	:- !,
	Function=..[Functor | Args],
	expand_head(`Args,Body,NewArgs,NewBody),
	NewFunction=..[Functor | NewArgs].
expand_head(Head,Body,NewHead,NewBody)
	:-
	match_pattern(Head,MatchedHead,WhenClause,WhereClause),
	!,
	expand_head(MatchedHead,(WhereClause,Body,WhenClause),NewHead,NewBody).
expand_head(Head,Body,NewHead,NewBody)
	:-
	expand_head(`Head,Body,NewHead,NewBody).
/*

These are not needed. The above predicate does the job.

expand_head(Head,Body,Head,Body)
	:-
	atomic(Head),
	!.
expand_head([HHead | HRest],Body,[ExpandedHHead | ExpandedHRest],NewBody)
	:-
	!,
	expand_head(HHead,Body,ExpandedHHead,Body1),
	expand_head(HRest,Body1,ExpandedHRest,NewBody).
expand_head([],Body,[],Body)
	:-!.
expand_head(Function,Body,NewFunction,NewBody)
	:- 
	!,
	Function=..[Functor | Args],
	expand_head(Args,Body,NewArgs,NewBody),
	NewFunction=..[Functor | NewArgs].
*/

expand_body(Body,Body)
	:-
	var(Body), !.
expand_body(``Body,Body)
	:- !.
expand_body(`Body,Body)
	:-
	(atomic(Body); var(Body)), !.
expand_body(`((Body1,Body2)),(ExpandedBody1,ExpandedBody2))
	:- !,
	expand_body(Body1,ExpandedBody1),
	expand_body(Body2,ExpandedBody2).
expand_body(`((Body1;Body2)),(ExpandedBody1;ExpandedBody2))
	:- !,
	expand_body(Body1,ExpandedBody1),
	expand_body(Body2,ExpandedBody2).
expand_body(`((Body1->Body2)),(ExpandedBody1->ExpandedBody2))
	:- !,
	expand_body(Body1,ExpandedBody1),
	expand_body(Body2,ExpandedBody2).
expand_body(Predicate,ExpandedBody)
	:-
	match_pattern(Predicate,NewPredicate,WhenClause,WhereClause),
	!,
	expand_body((WhenClause,NewPredicate,WhereClause),ExpandedBody).
expand_body((Body1,Body2),(ExpandedBody1,ExpandedBody2))
	:- !,
	expand_body(Body1,ExpandedBody1),
	expand_body(Body2,ExpandedBody2).
expand_body((Body1;Body2),(ExpandedBody1;ExpandedBody2))
	:- !,
	expand_body(Body1,ExpandedBody1),
	expand_body(Body2,ExpandedBody2).
expand_body((Body1->Body2),(ExpandedBody1->ExpandedBody2))
	:- !,
	expand_body(Body1,ExpandedBody1),
	expand_body(Body2,ExpandedBody2).
expand_body(Predicate,Predicate)
	:-
	atomic(Predicate), !.
expand_body(Predicate,(ExpandedPrefix,NewPredicate,ExpandedSuffix))
	:- !,
	Predicate=..[Functor | Args],
	expand_body(`Args,NewArgs,true,true,Prefix,Suffix),
	NewPredicate=..[Functor | NewArgs],
	expand_body(Prefix,ExpandedPrefix),
	expand_body(Suffix,ExpandedSuffix).

expand_body(Item,Item,Prefix,Suffix,Prefix,Suffix)
	:-
	var(Item), !.
expand_body(``Item,Item,Prefix,Suffix,Prefix,Suffix)
	:- !.
expand_body(`Item,Item,Prefix,Suffix,Prefix,Suffix)
	:-
	(atomic(Item); var(Item)), !.
expand_body(`[Item | RestItems],[NewItem | NewRestItems],Prefix,Suffix,
					ExpandedPrefix,ExpandedSuffix)
	:- !,
	expand_body(Item,NewItem,Prefix,Suffix,Prefix1,Suffix1),
	expand_body(`RestItems,NewRestItems,Prefix1,Suffix1,
						ExpandedPrefix,ExpandedSuffix).
expand_body(`[],[],Prefix,Suffix,Prefix,Suffix)
	:- !.
expand_body(`Item,NewItem,Prefix,Suffix,ExpandedPrefix,ExpandedSuffix)
	:- !,
	Item=..[Functor | Args],
	expand_body(`Args,NewArgs,Prefix,Suffix,
						ExpandedPrefix,ExpandedSuffix),
	NewItem=..[Functor | NewArgs].
expand_body(Item,NewItem,Prefix,Suffix,ExpandedPrefix,ExpandedSuffix)
	:-
	match_pattern(Item,MatchedItem,WhenClause,WhereClause),
	!,
	expand_body(MatchedItem,NewItem,(Prefix,WhenClause),
			(Suffix,WhereClause),ExpandedPrefix,ExpandedSuffix).
expand_body(Item,NewItem,Prefix,Suffix,ExpandedPrefix,ExpandedSuffix)
	:-
	expand_body(`Item,NewItem,Prefix,Suffix,
						ExpandedPrefix,ExpandedSuffix).

flatten_body(X,X)
	:-
	var(X), !.
flatten_body(((A,B),C),F)
	:- !,
	flatten_body((A,B,C),F).
flatten_body((A,B,C),(FA,FBC))
	:- !,
	flatten_body(A,FA),
	flatten_body((B,C),FBC).
flatten_body((A;B),(FA;FB))
	:- !,
	flatten_body(A,FA),
	flatten_body(B,FB).
flatten_body((A->B),(FA->FB))
	:- !,
	flatten_body(A,FA),
	flatten_body(B,FB).
flatten_body(A,A).


simplify_body(X,X)
	:-
	var(X), !.
simplify_body((true,true),true)
	:- !.
simplify_body((true,A),S)
	:- !,
	simplify_body(A,S).
simplify_body((A,true),S)
	:- !,
	simplify_body(A,S).
simplify_body((A,B),S)
	:- !,
	simplify_body(A,SA),
	simplify_body(B,SB),
	reduce((SA,SB),S).
simplify_body((A;B),(SA;SB))
	:- !,
	simplify_body(A,SA),
	simplify_body(B,SB).
simplify_body((A->B),(SA->SB))
	:- !,
	simplify_body(A,SA),
	simplify_body(B,SB).
simplify_body(X,X).


reduce((true,B),B)
	:- !.
reduce((A,true),A)
	:- !.
reduce(X,X).

make_clause(Head,true,Head)
	:- !.
make_clause([],Body,(:-Body))
	:-!.
make_clause(Head,Body,(Head :-Body)).

match_pattern(Pattern,Replacement,WhenClause,WhereClause)
	:-
	macro_defn(Pattern,Replacement,WhenClause,WhereClause,WithClause,
							IfClause),
	IfClause,
	assert_clauses(WithClause,user),
	!.

assert_clauses([],_)
	:- !.
assert_clauses([Module:Clause | Rest],_)
	:- !,
	Module:assert(Clause),
	assert_clauses(Rest,Module).
assert_clauses([Clause | Rest],Module)
	:-
	Module:assert(Clause),
	assert_clauses(Rest,Module).

export define_macro/1.
		%% Clauses to handle all the possibilities for including
		%% or omitting when,where,with,if:
define_macro((Pattern => Replacement when WhenClause where WhereClause
						with WithClause if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,
					WhereClause,WithClause,IfClause)),
	!.
define_macro((Pattern => Replacement where WhereClause
						with WithClause if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,
					WhereClause,WithClause,IfClause)),
	!.
define_macro((Pattern => Replacement when WhenClause
						with WithClause if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,true,
							WithClause,IfClause)),
	!.
define_macro((Pattern => Replacement when WhenClause where WhereClause
								if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,
							WhereClause,IfClause)),
	!.
define_macro((Pattern => Replacement when WhenClause where WhereClause
							with WithClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,
					WhereClause,WithClause,true)),
	!.
define_macro((Pattern => Replacement with WithClause if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,true,
							WithClause,IfClause)),
	!.
define_macro((Pattern => Replacement where WhereClause if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,
						WhereClause,[],IfClause)),
	!.
define_macro((Pattern => Replacement where WhereClause with WithClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,
					WhereClause,WithClause,true)),
	!.
define_macro((Pattern => Replacement when WhenClause if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,true,[],
								IfClause)),
	!.
define_macro((Pattern => Replacement when WhenClause with WithClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,true,
							WithClause,true)),
	!.
define_macro((Pattern => Replacement when WhenClause where WhereClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,
							WhereClause,[],true)),
	!.
define_macro((Pattern => Replacement when WhenClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,WhenClause,true,[],
									true)),
	!.
define_macro((Pattern => Replacement where WhereClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,WhereClause,[],
									true)),
	!.
define_macro((Pattern => Replacement with WithClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,true,WithClause,
									true)),
	!.
define_macro((Pattern => Replacement if IfClause))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,true,[],IfClause)),
	!.
define_macro((Pattern => Replacement))
	:-
	macroxp:assertz(macro_defn(Pattern,Replacement,true,true,[],true)),
	!.

		/*----------------------------------------
		 |	File Processing
		 *---------------------------------------*/


export mx_cl/0.
mx_cl :-
	set_prolog_flag(undefined_predicate, fail),
	builtins:command_line(CL),
	dappend(_,['-s',Source|_],CL),
	(filePlusExt(SourceName,SourceExt,Source) ->
		SourceFile = Source
		;
		filePlusExt(Source,pro,SourceFile)
	),
	(dappend(_,['-t',Target|_],CL) ->
		TargetFile = Target
		;
		(filePlusExt(SourceName,SourceExt,Source) ->
			(SourceExt \= pro ->
				filePlusExt(SourceName,pro,TargetFile)
				;
				filePlusExt(SourceName,ppo,TargetFile)
			)
			;
			filePlusExt(Source,pro,TargetFile)
		)
	),
	(dappend(_,['-m',Macro|_],CL) ->
		MacroFile = Macro
		;
		(filePlusExt(SourceName,SourceExt,Source) ->
			filePlusExt(SourceName,mac,MacroFile)
			;
			filePlusExt(Source,mac,MacroFile)
		)
	),
	macro_expand_files(SourceFile, TargetFile, MacroFile).
			
export mx/0.
mx :-
	macro_expand.

export macro_expand/0.
macro_expand
	:-
	set_prolog_flag(undefined_predicate, fail),
	write('source='),read(Source),
	write('target='),read(Target),
	macro_expand_files(Source, Target).

export macro_expand_files/2.
macro_expand_files(Source, Target)
	:-
		% see(Source),
	open(Source,read,SourceStream,[]),
		% tell(Target),
	open(Target,write,TargetStream,[]),
	macro_expand(SourceStream, TargetStream),
	!,
	close(SourceStream), close(TargetStream).

export macro_expand_files/3.
macro_expand_files(SourceFile, TargetFile, MacroFile)
	:-
	macroxp:abolish(macro_defn,6),
	consult_to(macroxp, -MacroFile),
	macro_expand_files(SourceFile, TargetFile),
	macroxp:abolish(macro_defn,6).

export macro_expand/2.
macro_expand(SourceStream, TargetStream)
	:-
	read_term(SourceStream, Item, [vars_and_names(Vars,VNames)]),
	dispatch_macro_expand(Item, Vars,VNames, SourceStream, TargetStream).

dispatch_macro_expand(end_of_file, _,_,_,_) :-!.
dispatch_macro_expand(Item, Vars,VNames,SourceStream, TargetStream)
	:-
	expand_macros(Item, XItem),
	Vars = VNames,
	(macro_show_source ->
		printf(TargetStream, "   % %t.\n", [Item])
		;
		true
	),
	printf(TargetStream, "%t.\n", [XItem]),
	macro_expand(SourceStream, TargetStream).

macro_show_source.

endmod.

