/*=====================================================================
 |			blt_db.pro
 |		Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |
 |	Builtin predicates for manipulating the internal clause database
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *====================================================================*/

module builtins.

/*------------------------------------------------------------------------*
 | abolish/2.
 | abolish(Pred, Arity)
 |  
 |	Defined in terms of abolish/3 which, is a C-defined builtin:
 |	abolish/3
 |	abolish(Mod, Pred, Arity)
 |
 | abolish/1
 | abolish(Pred/Arity)
 | 
 | Required by the standard.
 *------------------------------------------------------------------------*/

:-	compiletime,
	module_closure(abolish,2,abolish).

:-	compiletime,
	module_closure(abolish,1,abolish1_aux).

abolish1_aux(Mod, P/A)
	:-!,
	abolish(Mod, P, A).

abolish1_aux(Mod, Expr)
	:-
	atom(Expr),
	abolish(Mod, P, 0).

/*------------------------------------------------------------------------*
 |	remove_clauses/2
 |	remove_clauses(Pred, Arity)
 |
 |	Similar to abolish/2, but does not delete the name table entry
 |	for Pred/Arity, and hence does not kill any dynamic(Pred/Arity)
 |	declaration.
 |		To be more efficiently defined at the C-level later.
 *------------------------------------------------------------------------*/
:-	compiletime,
	module_closure(remove_clauses,2,remove_clauses).

remove_clauses(Mod, Pred, Arity)
	:-
	abolish(Mod, Pred, Arity),
	Mod:dynamic(Pred/Arity).

/*------------------------------------------------------------------------*
 | clause/2, clause/3
 |
 |	The proper way to perform clause retrieval relative to a module foo is:
 |
 |           foo:clause(f(X),B)
 |
 |	However, as a concession to the ISO standard, the following will be
 |	allowed to be equivalent to the above:
 |
 |           clause(foo:f(X),B)
 |
 | See the listing/0 and listing/1  code for definitions of next_clause/2
 | and rest_clauses/3.
 *------------------------------------------------------------------------*/

:-	compiletime,
	module_closure(clause,2,'$clause'),
	module_closure(clause,3,clause).
   
'$clause'(_, (Module:Head), Body) 
	:-!,
	clause(Module,Head,Body,_).

'$clause'(Module,Head,Body) 
	:-
	clause(Module,Head,Body,_).

	%% DBRef is ground:
clause(Module,Head,Body,DBRef) 
	:-
	nonvar(DBRef),
	!,
	$source(DBRef,Clause),		%% decompile the DBRef
	clauseParts(Clause,Head,Body).

	%% Don't have a DBRef yet:
clause(Module,Head,Body,DBRef) 
	:-
	nonvar(Head),
	(atom(Head) ; compound(Head)),
	!,
	functor(Head,F,A),
	get_firstarg(A,Head,FirstArg),
		%% Get the first clause of the procedure:
	$procinfo(_,Module,F,A,First,_),!,
		%% Cycle through DBRefs until find one that works:
	get_clauses(First,DBRef,FirstArg),
	$source(DBRef,Clause),
	clauseParts(Clause,Head,Body).

clause(Module,Head,Body,DBRef) 
	:-
	var(Head),
	instantiation_error(clause(Head,Body)).

clause(Module,Head,Body,DBRef) 
	:-
	nonvar(Head), 
	not(atom(Head)), 
	not(compound(Head)),
	type_error(callable, Head, clause(Head,Body)).

clause(Module,Head,Body,DBRef) 
	:-
	permission_error(access, private_procedure, Head, clause(Head,Body)).

clauseParts((Head :- Body),Head,Body) 
	:- !.
clauseParts(Head,Head,true).

/*------------------------------------------------------------------------*
 |	clauses/2
 |
 |       Recursively get all database references of clauses starting
 |       from the given first clause.
 *------------------------------------------------------------------------*/

clauses(0,_) 
	:-!,
	fail.

clauses(FirstDBRef,DBRef) 
	:-
	'$clauseinfo'(FirstDBRef,NextDBRef,_,_),
	clauses(FirstDBRef,NextDBRef,DBRef).

clauses(0,_,_) 
	:-!,
	fail.

clauses(DBRef,Next,DBRef).

clauses(Previous,Current,DBRef) 
	:-
	'$clauseinfo'(Current,Next,_,_),
	clauses(Current,Next,DBRef).

/*------------------------------------------------------------------------*
 | Filter clauses whose first arguments don't match the first argument
 | of the template while their data base references are collected.
 | If the first argument of the template is a variable, cycle all clauses.
 | Otherwise, get clauses whose first arguments match with the first argument
 | of the template.
 *------------------------------------------------------------------------*/

get_clauses(First,DBRef,FirstArg) 
	:-
	nonvar(FirstArg), 
	!,
	filter_clauses(First,DBRef,FirstArg).

get_clauses(First,DBRef,FirstArg) 
	:-
	clauses(First,DBRef).

get_firstarg(0,_,_) 
	:- !.

get_firstarg(_,Head,FirstArg) 
	:- 
	arg(1,Head,FirstArg).

/*------------------------------------------------------------------------*
 | filter_clauses/2
 | 
 |	The function of this procedure is same as the function of clauses/2
 |	except that this procedure filters out clauses whose first arguments
 |  don't match with the given first argument.
 *------------------------------------------------------------------------*/ 

filter_clauses(0,_,_) 
	:- !,
	fail.

filter_clauses(FirstDBRef,DBRef,FirstArg) 
	:-
	'$clauseinfo'(FirstDBRef,NextDBRef,_,_),
	filter_clauses(FirstDBRef,NextDBRef,DBRef,FirstArg).

filter_clauses(0,_,_,_) 
	:- 
	!,fail.

filter_clauses(DBRef,Next,DBRef,FirstArg) 
	:- 
	'$firstargkey'(DBRef,FirstArg).

filter_clauses(Previous,Current,DBRef,FirstArg) 
	:-
	'$clauseinfo'(Current,Next,_,_),
	filter_clauses(Current,Next,DBRef,FirstArg).

/*------------------------------------------------------------------------*
 | retract/1, retract/2
 *------------------------------------------------------------------------*/

:-	compiletime,
	module_closure(retract,2,retract),
	module_closure(retract,1,$retract).

$retract(Module,X) 
	:-
	retract(Module,X,_).

	%% DBRef is non-variable:
retract(Module,Clause,DBRef) 
	:-
	nonvar(DBRef),
	!,
		%% Decompile and delete if necessary:
	$source(DBRef,Clause),
	erase(DBRef).

	%% DBRef is variable. Must do some searching:
retract(Module,Clause,DBRef) 
	:-
		%% Get the head, take it apart, & find first clause:
	clauseParts(Clause,Head,_),
	functor(Head,F,A),
	$procinfo(_,Module,F,A,First,_),
		%% Start searching for retraction:
	get_firstarg(A,Head,FirstArg),
	get_clauses(First,DBRef,FirstArg),
	$source(DBRef,Clause),
	erase(DBRef).

export abolish_module/1.
abolish_module(Mod)
	:-
	all_procedures(Mod, P, A, _),
	Mod:abolish(P,A),
	fail.
abolish_module(Mod).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%	Predicates for lists of clauses 		%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*!---------------------------------------------------------------
 |	assert_each/2
 |	assert_each(ClauseList,Module)
 |	assert_each(+,+)
 |	
 |	- asserts each clause on ClauseList in module Module
 |	
 |	If ClauseList is a list of clauses, asserts each of these clauses
 |	in module Module.
 *!--------------------------------------------------------------*/

export assert_each/2.

assert_each([],_).
assert_each([Clause | RestClauseList],Module)
	:-
	builtins:assertz(Module,Clause,_,0),
	assert_each(RestClauseList,Module).



		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%	Predicates for manipulating modules 	%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*----------------------------------------------------------------------*
 | Create a new module (no effect if module already exists)
 *----------------------------------------------------------------------*/

export create_new_module/1.
create_new_module(Mod)
	:-
	'$icode'(-10,Mod,0,0,0).

/*----------------------------------------------------------------------*
 | Export predicates from a module
 |	-- low-level; needs module opened in icode buffer;
 *----------------------------------------------------------------------*/

export doexport/1.
doexport([]) :-!.

doexport([P1 | Ps]) 
	:-!,
	doexport(P1),
	doexport(Ps).

doexport((P1,P2)) 
	:-!,
	doexport(P1),
	doexport(P2).

doexport(P/A) 
	:- !,
	atom(P),
	integer(A),
	A >= 0,
	!,
	functor(PP,P,0),		/* intern the predicate name */
	$icode(-11, PP, A, 0, 0).	/* export the predicate name */

	%%%% Needs to be made into proper error exceptions
			%% e.g., instantiation_error(clause(Head,Body)),
			%%  	type_error(callable, Head, clause(Head,Body)).

doexport(Pat) 
	:-
	write(error_stream,'Invalid P/A in export list.  Ignoring it.'),
	nl(error_stream).

export export_from/2.
export_from(Mod, PredAs)
	:-
	'$icode'(-10,Mod,0,0,0),
	doexport(PredAs),
%	'$icode'(-11,0,0,0,0).
	'$icode'(-9,0,0,0,0).

/*----------------------------------------------------------------------*
 | Set use declarations in a module
 *----------------------------------------------------------------------*/

export douse/1.
douse((M1,M2)) 
	:-
	douse(M1),
	douse(M2).

douse([]) :-!.
douse([N | Ns]) :-
	douse(N),
	douse(Ns).

douse(M) :-
	atom(M),
	!,
	functor(MM,M,0),		/* intern the module name */
	$icode(-8, MM, 0, 0, 0).	/* add to module use list */

	%% Need error exceptions....

export use_by_mod/2.
use_by_mod(Mods, ByMod)
	:-
	'$icode'(-10,ByMod,0,0,0),
	douse(Mods),
	'$icode'(-9,0,0,0,0).



/*---------------------------------------------------------------------
 | 				Hash table (expandable) predicates
 |
 |	The core hash tables are physically simply terms of the form
 |
 |		hashArray(.........)
 |
 |	We are exploiting the fact that the implementation of terms is
 |	such that a term is an array of (pointers to) its arguments. So
 |	what makes a hash table a hash table below is the access routines 
 |	implemented using basic hashing techniques.  We also exploit the
 |	destructive update feature mangle/3.
 |	Each argument (entry) in a hash table here is a (pointer) to a 
 |	list [E1, E2, ....] where each Ei is a cons term of the form
 |
 |			[Key | Value]
 |	
 |	So a bucket looks like:	
 |	
 |		[ [Key1 | Val1], [Key2 | Val2], ....]
 |
 |	where each Keyi hashes into the index (argument number) of this 
 |	bucket in the term
 |	
 |		hashArray(.........)
 |
 |	The complete hash tables are terms of the form
 |
 |		hastTable(Depth,Size,RehashCount,hashArray(....))
 |
 |	where:
 |		Depth		= the hashing depth of keys going in;
 |		Size		= arity of the hashArray(...) term;
 |		RehashCount	= counts (down) the number of hash entries
 |						which have been made; when then counter
 |						reaches 0, the table is expanded and
 |						rehashed.
 |
 |	The basic (non-multi) versions of these predicates overwrite
 |	existing key values; i.e., if Key-Value0 is already present
 |	in the table, then hash inserting Key-Value1 will cause the
 |	physical entry for Value0 to be physcially altered to become
 |	Value1 (uses mangle/3).
 |
 |	The "-multi" versions of these predicates do NOT overwrite
 |	existing values, but instead treat the Key-___ cons items as
 |	tagged pushdown lists, so that if [Key | Value0] was present,
 |	then after hash_multi_inserting Key-Value1, the Key part of the
 |	bucket looks like: [Key | [Value1 | Value0] ]; i.e., it is
 |
 |		[Key, Value1 | Value0]
 |
 |	Key hashing is performed by the predicate
 |
 |		hashN(Key,Size,Depth,Index),
 |
 *--------------------------------------------------------------------*/

/*---------------------------------------------------------------*
 |	hash_create/1 
 |	hash_create(Table)
 |	hash_create(-)
 |
 |	- creates a small hash table, hashing on keys to depth 3.
 *---------------------------------------------------------------*/
export hash_create/1.

hash_create(Table) 
	:-
	hash_create(3,Table).

hash_create(Depth, hashTable(Depth,Size,RehashCount,Array)) 
	:-
	hashTableSizes(Size,RehashCount),
	!,
	functor(Array,hashArray,Size),
	initHashTable(Size,Array).

initHashTable(0,_) 
	:- !.
initHashTable(N,T) 
	:-
	arg(N,T,[]),
	NN is N-1,
	initHashTable(NN,T).


/*---------------------------------------------------------------*
 |	hash_member/3
 |	hash_member(Key,Bucket,Pair)
 |	hash_member(+,+,-)
 |
 |	- seeks first Pair in Bucket with left element = Key
 |
 |	Resatisfiable
 *---------------------------------------------------------------*/
hash_member(Key,[Pair|_],Pair) 
	:- 
	arg(1,Pair,Key).
hash_member(Key,[_|BucketTail],Pair) 
	:- 
	hash_member(Key,BucketTail,Pair).

/*---------------------------------------------------------------*
 |	hash_insert/3
 |	hash_insert(Key,Value,Table)
 |	hash_insert(+,+,+)
 |
 |	-	inserts Key-Value pair in hash Table
 |
 |	Unitary: overwrites any existing key-value entry 
 *---------------------------------------------------------------*/
export hash_insert/3.

hash_insert(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Array,Bucket),
	hash_insert(Bucket,Array,Index,Key,Value,Table).

/*---------------------------------------------------------------*
 |	hash_insert/6
 |	hash_insert(Bucket,Array,Index,Key,Value,Table)
 |	hash_insert(+,+,+,+,+,+)
 | 
 |	- inserts key-value pair in hash table bucket
 |
 |	Unitary: overwrites any existing key-value entry 
 |	FOR Key; NOTE however, that other Key0,Val0 pairs can
 |	have hashed into the same bucket; they are NOT changed.
 *---------------------------------------------------------------*/
hash_insert(Bucket,Array,Index,Key,Value,Table) 
	:-
	hash_member(Key,Bucket,E),
	!,
	mangle(2,E,Value).
hash_insert(Bucket,Array,Index,Key,Value,Table) 
	:-
	mangle(Index,Array,[[Key|Value]|Bucket]),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount-1,
	hash_rehash(NewRehashCount,Table).

/*---------------------------------------------------------------*
 |	hash_insert_multi/3
 |	hash_insert_multi(Key,Value,Table)
 |	hash_insert_multi(+,+,+)
 |
 |	-	inserts Key-Value pair in hash Table
 |
 |	Multi-Valued: accumulates multiple key-value entries
 *---------------------------------------------------------------*/

export hash_insert_multi/3.

hash_insert_multi(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Array,Bucket),
	hash_insert_multi(Bucket,Array,Index,Key,Value,Table).

/*---------------------------------------------------------------*
 |	hash_insert/6
 |	hash_insert(Bucket,Array,Index,Key,Value,Table)
 |	hash_insert(+,+,+,+,+,+)
 | 
 |	- inserts key-value pair in hash table bucket
 |
 |	Multi-Valued: accumulates multiple key-value entries
 *---------------------------------------------------------------*/
hash_insert_multi(Bucket,Array,Index,Key,Value,Table) 
	:-
	hash_member(Key,Bucket,E),
	!,
	arg(2,E,CurValue),
	mangle(2,E,[Value | CurValue]).

	%% Note that [Key,Value] = [Key | [Value] ]
hash_insert_multi(Bucket,Array,Index,Key,Value,Table) 
	:-
	mangle(Index,Array,[ [Key,Value] | Bucket]),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount-1,
	hash_rehash(NewRehashCount,Table).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
hash_rehash(Count,Table) 
	:-
	Count > 0,
	!,
	mangle(3,Table,Count).

hash_rehash(_,Table) 
	:-
	arg(2,Table,Size),
	hashTableSizes(NewSize,RHC),
	NewSize>Size,
	!,
	functor(NewArray,hashArray,NewSize),
	initHashTable(NewSize,NewArray),
	mangle(2,Table,NewSize),
	mangle(3,Table,RHC),
	arg(4,Table,Array),
	mangle(4,Table,NewArray),
	hash_rehash2(Array,Size,Table).

hash_rehash2(Array,Size,Table) 
	:-
	hash_elements(Array,Size,Key,Data),
	hash_insert(Key,Data,Table),
	fail.
hash_rehash2(_,_,_).


/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_elements/3.

hash_elements(Key,Data,Table) 
	:-
	arg(2,Table,Size),
	arg(4,Table,Array),
	hash_elements(Array,Size,Key,Data).

hash_elements(Array,N,Key,Data) 
	:-
	arg(N,Array,Bucket),
	hash_member(_,Bucket,[Key|Data]).

hash_elements(Array,N,Key,Data) 
	:-
	N > 0,
	NN is N-1,
	hash_elements(Array,NN,Key,Data).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_lookup/3.
hash_lookup(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Array,Bucket),
	hash_member(Key,Bucket,[_|Value2]),
	!,
	Value = Value2.

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_delete/3.
hash_delete(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	hash_delete(Array,Index,Key,Value,Table),
	!.

/*---------------------------------------------------------------*
 | hash_delete(Struct,Index,Key,Value,Table) 
 | Deletes Key,Value pair from bucket with index Index;
 | Note that this bucket may contain more than one distinct
 | key-value pair, but that for any given Key, there is only
 | one pair in the bucket with key = Key.
 *---------------------------------------------------------------*/
	%% Case: Key matches key of FIRST pair in the bucket:
hash_delete(Struct,Index,Key,Value,Table) 
	:-
	arg(Index,Struct,EList),		%% get element list
	arg(1,EList,Elem),				%% get first element on list
	arg(1,Elem,Key),				%% match key
	arg(2,Elem,Value),				%% match value
	!,
	arg(2,EList,REList),			%% get rest of element list
	mangle(Index,Struct,REList),	%% delete element off of list
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount+1,
	mangle(3,Table,NewRehashCount).		%% update count

	%% Case: Key does NOT match key of FIRST pair in the bucket;
	%% Since Key hashed into this bucket, there must be a
	%% non-empty tail to the bucket:
hash_delete(Struct,Index,Key,Value,Table) 
	:-
	arg(Index,Struct,EList),		%% get element list
	arg(2, EList, EListTail),
	hash_delete_l(EList,EListTail,Key,Value),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount+1,
	mangle(3,Table,NewRehashCount).		%% update count

	%% EListTail must be non-empty:
hash_delete_l(EList,EListTail,Key,Value)
	:-
	arg(1,EListTail,Elem),			%% get first element on list tail
	arg(1,Elem,Key),			%% match key
	arg(2,Elem,Value),			%% match value
	!,
	arg(2,EListTail,REList),		%% get rest of element list tail
	mangle(2,EList,REList).			%% delete element off of list tail

	%% Haven't found Key yet, so 
	%% EListTail must be non-empty:
hash_delete_l(EList,EListTail,Key,Value)
	:-
	EListTail \= [],
	!,
	arg(2, EListTail, EListTail2),
	hash_delete_l(EListTail,EListTail2,Key,Value).
hash_delete_l(EList,EListTail,Key,Value).

/*---------------------------------------------------------------*
 | hash_delete(Struct,Index,Key,Value,Table) 
 | Deletes Key,Value pair from bucket with index Index;
 | Note that this bucket may contain more than one distinct
 | key-value pair, but that for any given Key, there is only
 | one pair in the bucket with key = Key.
 *---------------------------------------------------------------*/
export hash_delete_multi/3.
hash_delete_multi(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Struct,Bucket),		%% get bucket list
	hash_delete_multi(Bucket,Key,Value,Table),
	!.

	%% Case: Key matches key of first cons in Bucket:
hash_delete_multi(Bucket,Key,Value,Table) 
	:-
	arg(1,Bucket,EList),		%% get first element on bucket
	arg(1,EList,Key),		%% match key
	rip_out(EList, Value),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount+1,
	mangle(3,Table,NewRehashCount).		%% update count

	%% Case: Key does NOT match key of first cons in Bucket:
	%% So tail of Bucket must be non-empty:
hash_delete_multi(Bucket,Key,Value,Table) 
	:-
	arg(2, Bucket, BucketTail),
	hash_delete_multi_l(Bucket,BucketTail,Key,Value),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount+1,
	mangle(3,Table,NewRehashCount).		%% update count

	%% Case: Key matches key of first cons in BucketTail:
hash_delete_multi_l(Bucket,BucketTail,Key,Value)
	:-
	arg(1,BucketTail,Elem),		%% match key
	arg(1,Elem,Key),		%% match key
	!,
	rip_out(Elem, Value).

	%% Case: Key does NOT match key of first cons in BucketTail:
	%% Therefore, haven't found Key in Bucket yet,
	%% So tail of BucketTail must be non-empty:
hash_delete_multi_l(Bucket,BucketTail,Key,Value)
	:-
	EListTail \= [],
	!,
	arg(2, BucketTail, BucketTail2),
	hash_delete_multi_l(BucketTail,BucketTail2,Key,Value).
hash_delete_multi_l(Bucket,BucketTail,Key,Value).

rip_out(EList, Value)
	:-
	arg(2,EList,REList),
	arg(1,REList,RELHead),				
	disp_rip_out(RELHead, Value, EList, REList).
	
disp_rip_out(Value, Value, EList, REList)
	:-
	arg(2, REList, RETail),
	mangle(2, EList, RETail).

disp_rip_out(RELHead, Value, EList, REList)
	:-
	rip_out(REList, Value).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_delete_pattern/3.

hash_delete_pattern(Key,Value,Table) 
	:-
	arg(2,Table,Size),
	arg(4,Table,Array),
	hash_delete_pattern(Array,Size,Key,Value,Table).

hash_delete_pattern(Array,N,Key,Value,Table) 
	:-
	arg(N,Array,Bucket),				%% should be more efficient
	hash_member(_,Bucket,[Key|Value]),	%% way to do this...
	hash_delete(Array,N,Key,Value,Table).

hash_delete_pattern(Array,N,Key,Value,Table) 
	:-
	N > 0,
	NN is N-1,
	hash_delete_pattern(Array,NN,Key,Value,Table).

hashTableSizes(31,47).
hashTableSizes(61,92).
hashTableSizes(127,191).
hashTableSizes(251,377).
hashTableSizes(509,764).
hashTableSizes(1021,1532).
hashTableSizes(2039,3059).
hashTableSizes(4089,6134).
hashTableSizes(8191,12287).
hashTableSizes(16381,24572).
hashTableSizes(32749,49124).
hashTableSizes(65521,98282).
hashTableSizes(131071,20000000).

/*---------------------------------------------------------------------*
 | make_hash_table/1
 |
 | This procedure is similar to make_gv/1.  It creates hash table access
 | predicates which use the hashing predicates above.
 |
 | make_hash_table will build two access predicates setName and getName where
 | Name is an atom or list denoting the name of the hash table. The access
 | predicates take two arguments, the first of which is the key and the
 | second is the value.
 *---------------------------------------------------------------------*/

:-
	compiletime,
	module_closure(make_hash_table,1).

make_hash_table(Mod,Name) :-
	atom(Name),
	!,
	name(Name,NameList),
	make_hash_table(Mod,NameList).



	%% see if already present:
make_hash_table(Mod,Name) 
	:-
	name(ResetFunc,	[0'r, 0'e, 0's, 0'e, 0't | Name]),
	functor(ResetHead, ResetFunc, 0),
	Mod:clause(ResetHead,_), 		
	name(SetFunc,	[0's, 0'e, 0't | Name]),
	functor(SetHead,   SetFunc,   2),
	Mod:clause(SetHead,_),
	!.

	%% not already present:
make_hash_table(Mod,Name) 
	:-
	make_hash_framework(Mod,Name,VN),
	hash_create(InitialHashTable),
	gv_set(VN,InitialHashTable),
	name(AtomicName, Name),
	hash_create(InitialHashTable0),
	global_gv_info:assert_at_load_time(gvi(AtomicName,VN,Mod,InitialHashTable0)).

export make_hash_framework/3.
make_hash_framework(Mod,Name,VN)
	:-
	name(ResetFunc,	[0'r, 0'e, 0's, 0'e, 0't | Name]),
	name(SetFunc,	[0's, 0'e, 0't | Name]),
	name(SetMFunc,	[0's, 0'e, 0't, 0'm | Name]),
	name(GetFunc,	[0'g, 0'e, 0't | Name]),
	name(PGetFunc,	[0'p, 0'g, 0'e, 0't | Name]),
	name(DelFunc,	[0'd, 0'e, 0'l | Name]),
	name(PDelFunc,	[0'p, 0'd, 0'e, 0'l | Name]),

	functor(ResetHead, ResetFunc, 0),
	functor(SetHead,   SetFunc,   2),
	functor(SetMHead,  SetMFunc,  2),
	functor(GetHead,   GetFunc,   2),
	functor(PGetHead,  PGetFunc,  2),
	functor(DelHead,   DelFunc,   2),
	functor(PDelHead,  PDelFunc,  2),

	arg(1,SetHead, Key),	arg(2,SetHead, Val),
	arg(1,SetMHead, Key),	arg(2,SetMHead, Val),
	arg(1,GetHead, Key),	arg(2,GetHead, Val),
	arg(1,PGetHead,Key),	arg(2,PGetHead,Val),
	arg(1,DelHead, Key),	arg(2,DelHead, Val),
	arg(1,PDelHead,Key),	arg(2,PDelHead,Val),

	gv_alloc(VN),

	Mod:assert_at_load_time(
			(ResetHead :- hash_create(TB), gv_set(VN,TB))),
	Mod:assert_at_load_time(
			(SetHead  :- gv_get(VN,TB), hash_insert(Key,Val,TB))),
	Mod:assert_at_load_time(
			(SetMHead :- gv_get(VN,TB), hash_insert_multi(Key,Val,TB))),
	Mod:assert_at_load_time(
			(GetHead  :- gv_get(VN,TB), hash_lookup(Key,Val,TB))),
	Mod:assert_at_load_time(
	        (PGetHead :- gv_get(VN,TB), hash_elements(Key,Val,TB))),
	Mod:assert_at_load_time(
			(DelHead  :- gv_get(VN,TB), hash_delete(Key,Val,TB))),
	Mod:assert_at_load_time(
			(PDelHead :- gv_get(VN,TB), hash_delete_pattern(Key,Val,TB))).

endmod.		%% blt_db.pro: Internal Database Builtins File
