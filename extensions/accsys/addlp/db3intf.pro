/*========================================================================
 |					db30.pro
 | 	Copyright (c) 1986-90, Applied Logic Systems, Inc.
 |
 |	 	Routines for accessing a dBase III/III+ database (non-indexed)
 |			AccSys Version
 |
 |	Author: Keith Hughes(original prolog version)
 |			Ken Bowen (current AccSys version)
 |	Date:		6/6/86, 11/91
 *=======================================================================*/

module user.

export get_head/2.
get_head([X|_],X).

endmod.

module accsys_db3.

#if (syscfg:d3i)
:-dynamic(number_of_dbf_buffers/2).

export pad_start/4.
pad_start(Buf,CurStart,Len,PadStart)
	:-
	CurPos is Len - 1,
	pad_back(Buf,CurPos,PadStart0),
	PadStart is PadStart0 + 1.

pad_back(Buf,-1,-1) :-!.
pad_back(Buf,CurPos,CurPos)
	:-
    '$uia_peekb'(Buf,CurPos,C),
	C \= 32,
	!.
pad_back(Buf,CurPos,PadStart)
	:-
	NewPos is CurPos - 1,
	pad_back(Buf,NewPos,PadStart).

/*--------------------------------------------------------------------
 |		access: Access the file to retrieve information for Head.
 |	
 |	 access was written as it appears below because many clauses may 
 |	be using the relation at the same time. Many C compilers only 
 |	allow =< 10 disk files to be open at once, so this allows many 
 |	more uses of a given relation. [This observation is true for
 |	the original Prolog version which utilized Keith's buffering
 |	and the virtual file channel extension;  not clear that this 
 |	is not now a problem -- we have to see what happens with the
 |	AccSys stuff.
 *-------------------------------------------------------------------*/

/*!-----------------------------------------------------------------------------
 |	close_all_tables/0.
 |	close_all_tables
 |	close_all_tables
 |
 |	- closes all open tables in a given module
 |
 |	Defined as a module closure; hence closes all modules which are open in
 |	the module in which it is called.
 *!----------------------------------------------------------------------------*/
:-module_closure(close_all_tables,0).
export close_all_tables/0.

close_all_tables(Module)
	:-
	(save_all_indexfiles(Module) -> true ; true),
	d_close_all_tables.

/*!-----------------------------------------------------------------------------
 |	close_table/1
 |	close_table(FilePtr)
 |	close_table(+)
 |
 |	- closes the specified open table
 |
 |	If FilePtr is a table file pointer returned from a call to open_table/3,
 |	then close_table(FilePtr) closes that table.
 *!----------------------------------------------------------------------------*/
export close_table/1.
close_table(FilePtr)
	:-
	dDclose(FilePtr,_).

/*!-----------------------------------------------------------------------------
 |	open_table/3.
 |	open_table(Name,FilePtr,NumberRecords)
 |	open_table(+,-,-)
 |
 |	- opens a database table
 |
 |	Inputs:
 |	Name - the name (path to) a database table
 |
 |	Outputs:
 |	FilePtr		a pointer to the table, for use in subsequent calls;
 |	NumberRecords	the number of recordsin the table when opened
 *!----------------------------------------------------------------------------*/
export open_table/3.
open_table(Name,FilePtr,NumberRecords)
	:-
	num_dbf_buffers(Name, Buffs),
	dDopen(Name, d_SINGLE, Buffs, FilePtr),
	FilePtr > 0,
	dDreccnt(FilePtr, NumberRecords, _).

/*!-----------------------------------------------------------------------------
 |	access_db3/3.
 |	access_db3(RelName,Head,Module) 
 |	access_db3(+,+,+) 
 |	access_db3/4.
 |	access_db3(AccPath,RelName,Head,Module) 
 |	access_db3(+,+,+,+) 
 |
 |	- access a table and match against a goal
 |
 |	Inputs:
 |	RelName		a relation (table/file) name
 |	Head		a Prolog goal
 |	Module		a module
 |	AccPath		a full path a table file
 |
 |	In access_db3/3, the table/relation name is taken to also be the file name,
 |	while in access_db3/4, the table/relation name is distinct from the full
 |	path to the file.
 |
 |	Head is normally a paritially instantiated Prolog goal whose functor is
 |	identical with RelName and whose arity is the number of columns in the table.
 |	
 |	This predicate performs non-indexed lookup on the table; rows from the
 |	table are retrieved and unified against Head until either a successful
 |	unification occurs, or the table is exhausted.  This process is 
 |	re-satisfiable; ie, if a success row is found and the table is not
 |	exhausted, a choice point is left.
 *!----------------------------------------------------------------------------*/
export access_db3/3.
export access_db3/4.
export access_db3/6.
export access_db3/7.

access_db3(RelName,Head,Module) 
	:-
	open_table(RelName,FilePtr,NumberRecords),
	!,
	access_db3(1,NumberRecords,Name,FilePtr,Head,Module,close).

access_db3(AccPath,RelName,Head,Module) 
	:-
	open_table(AccPath,FilePtr,NumberRecords),
	!,
	access_db3(1,NumberRecords,RelName,FilePtr,Head,Module,close).

access_db3(CurNum,NumberRecords,Name,FilePtr,Head,Module) 
	:-
	access_db3(CurNum,NumberRecords,Name,FilePtr,Head,Module,close).


	%% catch when at end of file, and quit: 
	%% don't close the file because the ptr to it is now cached;
	%% gets closed with everything later.
access_db3(CurNum,NumberRecords,Name,FilePtr,_,_,close) 
	:-
	CurNum > NumberRecords,
	!,
	fail.

access_db3(CurNum,NumberRecords,Name,FilePtr,_,_,_) 
	:-
	CurNum > NumberRecords,
	!,
	fail.

	%% normal read: get information from the record
access_db3(CurNum,_,Name,FilePtr,Head,Module,_) 
	:-
	dDgetrec(FilePtr,CurNum,RecBuf,_),
	Module:getInfo(opaque(Name),RecBuf,Head).

	%% previous clause failed (i.e., we backtracked into it);
	%% try to read next record:
access_db3(CurNum,NumberRecords,Name,FilePtr,Head,Module,CloseFlag) 
	:-
	NextNum is CurNum + 1,
			%% try again:
	access_db3(NextNum,NumberRecords,Name,FilePtr,Head,Module,CloseFlag).

/*!-----------------------------------------------------------------------------
 |	access_db3_all/5
 |	access_db3_all(AccPath,RelName,Head,Module,List) 
 |	access_db3_all(+,+,+,+,-) 
 |
 |	-	finds all solutions to a query
 |
 |	Similar to access_db?/?, but returns the list of all success rows in the
 |	final argument List.
 *!----------------------------------------------------------------------------*/
export access_db3_all/5.
access_db3_all(AccPath,RelName,Head,Module,List) 
	:-
	num_dbf_buffers(AccPath, Buffs),
	dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	FilePtr > 0,
	dDreccnt(FilePtr, NumberRecords, _),
	!,
	bagOf(Head,
		  access_db3(1,NumberRecords,RelName,FilePtr,Head,Module),
		  List).

export access_db3_xfp/5.
access_db3_xfp(AccPath,RelName,Head,Module,FilePtr) 
	:-
	num_dbf_buffers(AccPath, Buffs),
	dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	FilePtr > 0,
	dDreccnt(FilePtr, NumberRecords, _),
	!,
	access_db3(1,NumberRecords,RelName,FilePtr,Head,Module).

num_dbf_buffers(Name, Buffs)
	:-
	number_of_dbf_buffers(Name, Buffs),!.

	%% default:
num_dbf_buffers(Name, 1).

num_ndx_buffers(Name, Buffs)
	:-
	number_of_ndx_buffers(Name, Buffs),!.

	%% default:
num_ndx_buffers(Name, 4).

/*=======================================================================
 | 				db31.pro
 |		Copyright (c) 1986 Applied Logic Systems, Inc.
 |
 |		Utilities for writing to dBaseIII database files (non-indexed).
 |			AccSys Version
 |
 |	Author: Keith Hughes(original prolog version)
 |			Ken Bowen (AccSys version)
 |	Date:		6/6/86, 11/91
 *======================================================================*/

export atom_or_null/1.
atom_or_null(Item)
    :-
    var(Item), !, Item = ''.
atom_or_null(Item)
    :-
    atom(Item).

export atom_or_null/2.
atom_or_null(Item,Null)
    :-
    var(Item), !, Item = Null.
atom_or_null(Item,_)
    :-
    atom(Item).

export number_or_null/1.
number_or_null(Item)
    :-
    var(Item), !, Item = 0.
number_or_null(Item)
    :-
    number(Item).

export number_or_null/2.
number_or_null(Item,Null)
    :-
    var(Item), !, Item = Null.
number_or_null(Item,_)
    :-
    number(Item).

export date_or_null/1.
date_or_null(Item)
	:-
	date_pattern(1900,1,1,DefDate),
	date_or_null(Item, DefDate).

export date_or_null/2.
date_or_null(Item, Null)
	:-
	var(Item),!,Item=Null.
date_or_null(Item, _)
	:-
	valid_date(Item).

export list_or_null/2.
list_or_null(Item, Null)
	:-
	var(Item),!,Item=Null.
list_or_null([], _).
list_or_null([_|_], _).

/*!-----------------------------------------------------------------------------
 |	output_db3/4.
 |	output_db3(AccPath,RelName,Head,RecordNumber)
 |	output_db3(AccPath,RelName,Head,RecordNumber)
 |	output_db3/5
 |	output_db3(AccPath,RelName,Head,RecordNumber,Module)
 |	output_db3(+,+,+,-,+)
 |
 |	- outputs (inserts) a record to a database table
 |
 |	Inputs:
 |		AccPath		the full path to the table file
 |		RelName		the relation name (=functor of Head)
 |		Head		the goal whose args make up the row to insert
 |	Outputs:
 |		RecordNumber		the number assigned to the record
 *!----------------------------------------------------------------------------*/
export output_db3/4.
output_db3(AccPath,RelName,Head,RecordNumber)
	:-
	output_db3(AccPath,RelName,Head,RecordNumber,user).

export output_db3/5.
output_db3(AccPath,RelName,Head,RecordNumber,Module)
	:-
    num_dbf_buffers(RelName, Buffs),
    dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	Module:putInfo(opaque(RelName),RecBuf,Head),
	'$uia_pokeb'(RecBuf,0,0' ),
	dDapprec(FilePtr, RecBuf, _),
	a_d_recno(RecordNumber).

%--------------------------------------------------------------------------
% delete: Non-Deterministic Delete a record from the database
%--------------------------------------------------------------------------

/*!-----------------------------------------------------------------------------
 |	delete_db3/4.
 |	delete_db3(AccPath,RelName,Head,Module) 
 |	delete_db3(+,+,+,+) 
 |
 |	- deletes a record from a database table
 |
 |	Uses Head as a query to locate a matching row in the table, and then
 |	deletes that row; is re-satisfiable, in that subsequent calls will
 |	attempt to find (and delete) matching rows following the last matched
 |	and deleted row. The query solution is non-indexed, so that the
 |	effectively performs an access_db3 call to locate the record.
 *!----------------------------------------------------------------------------*/
export delete_db3/4.
delete_db3(AccPath,RelName,Head,Module) 
	:-
    num_dbf_buffers(AccPath, Buffs),
    dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	dDreccnt(FilePtr, NumberRecords, _),
	do_delete_db3(1,NumberRecords,RelName,FilePtr,Head,Module).

do_delete_db3(CurNum,NumberRecords,RelName,FilePtr,Head,Module) 
	:- 
	CurNum > NumberRecords,
	!, 
	fail.

do_delete_db3(CurNum,NumberRecords,RelName,FilePtr,Head,Module)
	:-
	dDgetrec(FilePtr,CurNum,RecBuf,_),
	Module:getInfo(opaque(RelName),RecBuf,Head),
	dDdelrec(FilePtr, CurNum, RetVal).

do_delete_db3(CurNum,NumberRecords,RelName,FilePtr,Head,Module)
	:-
	NextNum is CurNum + 1,
	do_delete_db3(NextNum,NumberRecords,RelName,FilePtr,Head,Module).

export delete_db3_by_rec_num/3.
delete_db3_by_rec_num(AccPath,RecNo,RetVal) 
	:-
    num_dbf_buffers(AccPath, Buffs),
    dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	dDreccnt(FilePtr, NumberRecords, _),
	RecNo =< NumberRecords,
	dDdelrec(FilePtr, RecNo, RetVal).



%--------------------------------------------------------------------------
% det_delete: Deterministic Delete a record from the database
%--------------------------------------------------------------------------

/*!-----------------------------------------------------------------------------
 |	det_delete_db3/4.
 |	det_delete_db3(AccPath,RelName,Head,Module) 
 |	det_delete_db3(+,+,+,+) 
 |
 |	- deterministically deletes a record from a database table
 |
 |	Uses Head as a query to locate a matching row in the table, and then
 |	deletes that row. Is not re-satisfiable and non-indexed.
 *!----------------------------------------------------------------------------*/
export det_delete_db3/4.
det_delete_db3(AccPath,RelName,Head,Module) 
	:-
    num_dbf_buffers(AccPath, Buffs),
    dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	dDreccnt(FilePtr, NumberRecords, _),
	do_det_delete_db3(1,NumberRecords,RelName,FilePtr,Head,Module).

do_det_delete_db3(CurNum,NumberRecords,RelName,FilePtr,Head,Module) 
	:- 
	CurNum > NumberRecords,
	!, 
	fail.

do_det_delete_db3(CurNum,NumberRecords,RelName,FilePtr,Head,Module)
	:-
	dDgetrec(FilePtr,CurNum,RecBuf,_),
	Module:getInfo(opaque(RelName),RecBuf,Head),
	dDdelrec(FilePtr, CurNum, RetVal),
	!.

do_det_delete_db3(CurNum,NumberRecords,RelName,FilePtr,Head,Module)
	:-
	NextNum is CurNum + 1,
	do_det_delete_db3(NextNum,NumberRecords,RelName,FilePtr,Head,Module).

%--------------------------------------------------------------------------
% update: Update a record in the database
%--------------------------------------------------------------------------

/*!-----------------------------------------------------------------------------
 |	update_db3/5
 |	update_db3(AccPath,RelName,MatchHead,OutHead,Module) 
 |	update_db3(+,+,+,+,+) 
 |
 |	- updates a database row in place
 |
 |	MatchHead is used to locate record which is then replaced by the row
 |	determined by OutHead.  Effectively, this is a access, followed by
 |	a delete, followed by an output (in place). Not re-satisfiable, and
 |	non-indexed.
 *!----------------------------------------------------------------------------*/
export update_db3/5.
update_db3(AccPath,RelName,MatchHead,OutHead,Module) 
	:-
    num_dbf_buffers(AccPath, Buffs),
    dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	dDreccnt(FilePtr, NumberRecords, _),
	do_update_db3(1,NumberRecords,RelName,opaque(RelName),FilePtr,MatchHead, OutHead,Module).

export update_db3/6.
update_db3(AccPath,RelName,Part,MatchHead,OutHead,Module) 
	:-
    num_dbf_buffers(AccPath, Buffs),
    dDopen(AccPath, d_SINGLE, Buffs, FilePtr),
	dDreccnt(FilePtr, NumberRecords, _),
	do_update_db3(1,NumberRecords,RelName,Part,FilePtr,MatchHead, OutHead,Module).

do_update_db3(CurNum,NumberRecords,RelName,Part,FilePtr,MatchHead, OutHead,Module) 
	:- 
	CurNum > NumberRecords,
	!, 
	fail.

do_update_db3(CurNum,NumberRecords,RelName,Part,FilePtr,MatchHead, OutHead,Module)
	:-
	dDgetrec(FilePtr,CurNum,InRecBuf,_),
	Module:getInfo(opaque(RelName),InRecBuf,MatchHead),
	Module:putInfo(Part,OutRecBuf,OutHead),
	dDupdrec(FilePtr, CurNum, OutRecBuf, RetVal),
	!.

do_update_db3(CurNum,NumberRecords,RelName,Part,FilePtr,MatchHead, OutHead,Module)
	:-
	NextNum is CurNum + 1,
	do_update_db3(NextNum,NumberRecords,RelName,Part,FilePtr,MatchHead, OutHead,Module).

#endif	%% d3i

	/**************************************************************
	 *
	 *		INDEXING
	 *
	 **************************************************************/

#if (syscfg:d3x)

/*!-----------------------------------------------------------------------------
 |	iaccess_db3/4
 |	iaccess_db3(RelName,DBFPath,Head,Module)
 |	iaccess_db3(+,+,+,+)
 |
 |	- performs an indexed access to a table row
 |
 |	Inputs:
 |		RelName		the relation name
 |		DBFPath		the full path to the table file
 |		Head		a goal as in access_db3/?
 |		Module		a module
 |
 |	If indicies have been constructed for the table, this call utilizes any
 |	instantiated arguments of Head to attempt to access a row of the table.
 |	Not resatisfiable.  Note the unfortunate inversion of the first two
 |	arguments between this predicate and access_db3/4.  {Any plan to
 |	correct this must fix the outputs of the concept transformer.}
 *!----------------------------------------------------------------------------*/
export iaccess_db3/4.
iaccess_db3(RelName,DBFPath,Head,Module)
	:-
		%% Note pre-calculation of Arity:
	Module:indexing(RelName,Arity,PatternList),
	match_pattern(PatternList,Head,Arity,AccessPattern,Match),
	!,
	idx_access_db3(AccessPattern,Match,RelName,DBFPath,Head,Arity,Module).

		%% No indexing pattern applies, so try direct access:
iaccess_db3(RelName,DBFPath,Head,Module)
	:-
	access_db3(DBFPath,RelName,Head,Module).
	
/*!-----------------------------------------------------------------------------
 |	ioutput_db3/3.
 |	ioutput_db3(DBFPath,RelName,Head)
 |	ioutput_db3(+,+,+)
 | 
 |	- outputs a row to an indexed table
 *!----------------------------------------------------------------------------*/
export ioutput_db3/3.
ioutput_db3(DBFPath,RelName,Head)
	:-
	ioutput_db3(DBFPath,RelName,Head,user,_).

export ioutput_db3/4.
ioutput_db3(DBFPath,RelName,Head,Module)
	:-
	ioutput_db3(DBFPath,RelName,Head,Module,RecordNumber).

export ioutput_db3/5.
ioutput_db3(DBFPath,RelName,Head,Module,RecordNumber)
	:-
		%% Note pre-calculation of Arity:
	Module:indexing(RelName,Arity,PatternList),
	output_db3(DBFPath,RelName,Head,RecordNumber,Module),

	match_all_patterns(PatternList,Head,Arity,MatchList),
	idx_all_output_db3(MatchList,RelName,RecordNumber,DBFPath,Head,Arity,Module).

/*!-----------------------------------------------------------------------------
 |	idelete_db3/3.
 |	idelete_db3(DBFPath,RelName,Head)
 |	idelete_db3(+,+,+)
 |
 |	- deletes a row from and indexed table
 *!----------------------------------------------------------------------------*/
export idelete_db3/3.
idelete_db3(DBFPath,RelName,Head)
	:-
	idelete_db3(DBFPath,RelName,Head,user,_).

export idelete_db3/4.
idelete_db3(DBFPath,RelName,Head,Module)
	:-
	idelete_db3(DBFPath,RelName,Head,Module,RecordNumber).

export idelete_db3/5.
idelete_db3(DBFPath,RelName,Head,Module,RecordNumber)
	:-
		%% Note pre-calculation of Arity:
	Module:indexing(RelName,Arity,PatternList),
	match_pattern(PatternList,Head,Arity,AccessPattern,Match),
	idx_access_db3(AccessPattern,Match,RelName,DBFPath,Head,Arity,Module,TheRecNo),
	delete_db3_by_rec_num(DBFPath,TheRecNo,RetVal),
	match_all_patterns(PatternList,Head,Arity,MatchList),
	idx_all_delete_db3(MatchList,RelName,TheRecNo,DBFPath,Head,Arity,Module).

/*----------------------------------------------------------
 |	Access patterns look like this:
 |	If Head is f(...) of arity, N, then an access pattern
 |	is of the form
 |
 |		<FF>(...)
 |
 |	where <FF> is a functor of arity N+1.  The expression
 |	in the n+1st place provides the access method information.
 *---------------------------------------------------------*/
/*----------------------------------------------------------
 | match_pattern/5
 | match_pattern(PatternList,Head,Arity,AccessPattern,Match).
 | match_pattern(+,+,+,-, -)
 |
 |	- seeks a matching index pattern
 |
 |	PatternList - a List of AccessPatterns
 |  Head - a (partially instatiated) term (predicate call)
 |  Arity - the arity of Head
 |	AccessPattern - the pattern on PatternList which was matched 
 |	Match - the match value [returned by match_idx_pattern]
 |
 |	Recurses down PatternList, seeking the first matching pattern
 *---------------------------------------------------------*/
match_pattern([AccessPattern | PatternList],Head,Arity,AccessPattern, Match)
	:-
	match_idx_pattern(1,Arity,AccessPattern,Head,Match).
match_pattern([_ | PatternList],Head,Arity,AccessPattern,Match)
	:-
	match_pattern(PatternList,Head,Arity,AccessPattern,Match).

/*----------------------------------------------------------
 | match_all_patterns/4
 | match_all_patterns(PatternList,Head,Arity,SuccessList)
 | match_all_patterns(+,+,+,-)
 |
 |	- seeks all matching index patterns
 |
 |	PatternList - a List of AccessPatterns
 |  Head - a (partially instatiated) term (predicate call)
 |  Arity - the arity of Head
 |	SuccessList - a list of pairs AccessPattern+Match corresponding
 |					to successful matches, where
 |					AccessPattern = pattern on PatternList which was matched 
 |					Match = match value [returned by match_idx_pattern]
 |
 |	Recurses down PatternList, seeking all matching patterns
 *---------------------------------------------------------*/
match_all_patterns([],_,_,[]).
match_all_patterns([AccessPattern | PatternList],Head,Arity,
					[AccessPattern+Match | SuccessList])
	:-
	match_idx_pattern(1,Arity,AccessPattern,Head,Match),
	match_all_patterns(PatternList,Head,Arity,SuccessList).
match_all_patterns([_ | PatternList],Head,Arity,SuccessList)
	:-
	match_all_patterns(PatternList,Head,Arity,SuccessList).

/*----------------------------------------------------------
 | match_idx_pattern/5
 | match_idx_pattern(Pos,Arity,AccessPattern,Head,Match)
 | match_idx_pattern(+,+,+,+,-)
 |
 | - computes a list matching comps for a pattern & head
 |
 |	Pos 	- the position in Head of the arg currently being matched
 |	Arity	- the arity of Head
 |	AccessPattern 	- the pattern expression
 |	Head	- the head being matched
 |	Match 	- the match value returned
 |
 |	Attempts to sweep across the Head/AccessPattern pair, from
 |	arg position Pos thru the last arg (Arity), computing 
 |	whether the corresponding head arg / pattern arg match;
 |	returns Match as the list of match results from the
 |	corresponding arg matches; essentially recursively applies
 |	idx_match_made to each pair.
 *---------------------------------------------------------*/
export match_idx_pattern/5.
match_idx_pattern(Pos,Arity,AccessPattern,Head,[])
	:-
	Pos > Arity,
	!.
match_idx_pattern(Pos,Arity,AccessPattern,Head, Match)
	:-
	arg(Pos,AccessPattern,IdxSpec),
	arg(Pos,Head,IncomingVal),
	idx_match_made(IdxSpec,IncomingVal,Match,MatchTail),
	!,
	NextPos is Pos+1,
	match_idx_pattern(NextPos,Arity,AccessPattern,Head,MatchTail).

/*----------------------------------------------------------
 |	idx_match_made/4
 |	idx_match_made(IdxSpec,IncomingVal,Match,MatchTail)
 |	idx_match_made(+,+,-,-)
 |
 |	- tests a pattern / incoming args pair for matching
 |
 |	IdxSpec		- the entry from the index spec (+/-)
 |	IncomingVal	- the entry from the incoming Head
 |	Match		- a list of form: [... | MatchTail]
 |	MatchTail	- a variable
 |
 |	(Match, MatchTail) form an extensible list; 
 |	If IncomingVal is non_var, 
 |		Then Match = [ IncomingVar | MatchTail]
 |		Else Match = MatchTail
 *---------------------------------------------------------*/
idx_match_made(+,IncomingVal,[IncomingVal | MatchTail], MatchTail)
	:-!,
	nonvar(IncomingVal).
idx_match_made(-,_,Match,Match).


/*----------------------------------------------------------
 *---------------------------------------------------------*/
idx_access_db3(AccessPattern,Match,RelName,DBFPath,Head,Arity,Module)
	:-
	idx_access_db3(AccessPattern,Match,RelName,DBFPath,Head,Arity,Module,TheRecNo).

idx_access_db3(AccessPattern,Match,RelName,DBFPath,Head,Arity,Module,TheRecNo)
	:-
	M is Arity+1,
	arg(M, AccessPattern, AM), 
	AM = idxa_db3(_, [Match,Key]^IFCN, [Key,RecNo]^IAP, RecNo),
	Module:call(IFCN),
	Module:call(IAP),
	open_table(DBFPath,FilePtr,_),
	(atom(RecNo) ->
		TheRecNo = RecNo
		;
		xmem(TheRecNo, RecNo)
	),
	dDgetrec(FilePtr,TheRecNo,RecBuf,_),
	Module:getInfo(opaque(RelName),RecBuf,Head).

xmem(Item, [Item | _ ]).
xmem(Item, [_ | Tail ])
	:-!,
	xmem(Item, Tail).
xmem(Item, Item).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
				%	IFCN:  <ifnc>(Match,Key),
				%	IAP:   iap(Key,RecNo,....),

export iap/5.
iap(pdx,Key,RecNo,IndexName,Module)
	:-
	Module:index_structs(IndexName,pdx,VNum),
	!,
	gv_get(VNum, IStruct),
	retr_ixr(Key,RecNo,IStruct).
	
iap(pmx,Key,RecNo,IndexName,Module)
	:-
	Module:index_structs(IndexName,pmx,VNum),
	!,
	gv_get(VNum, IStruct),
	retr_imxr(Key,RecNo,IStruct).
	
iap(hdx,Key,RecNo,IndexName,Module)
	:-
	Module:index_structs(IndexName,hdx,VNum),
	!,
	gv_get(VNum, IStruct),
	hash_lookup(Key,KeyEntries,IStruct),
	(member(RecNo, KeyEntries); KeyEntries = [_ | RecNo]).
	
iap(IndexType,Key,RecNo,IndexName,Module)
	:-
	Module:index_path(IndexName,FileName),
	restore_index(Module,IndexName,FileName),
	iap(IndexType,Key,RecNo,IndexName,Module).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
idx_all_output_db3([],RelName,RecordNumber,DBFPath,Head,Arity,Module).
idx_all_output_db3([Pattern+Match | MatchList],RelName,RecordNumber,DBFPath,Head,Arity,Module)
	:-
	idx_output_db3(Pattern,Match,RelName,RecordNumber,DBFPath,Head,Arity,Module),
	idx_all_output_db3(MatchList,RelName,RecordNumber,DBFPath,Head,Arity,Module).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
idx_output_db3(Pattern,Match,RelName,RecNo,DBFPath,Head,Arity,Module)
	:-
	M is Arity+1,
	arg(M, Pattern, AM), 
	AM = idxa_db3(IndexName, [Match,Key]^IFCN, _^iap(IndexType,_,_,_,_), _),
		%% Instatiate Key:
	Module:call(IFCN),
	(Module:index_structs(IndexName,IndexType,VNum) ->
		true
		;
		Module:index_path(IndexName,FileName),
		restore_index(Module,IndexName,FileName),
		Module:index_structs(IndexName,IndexType,VNum)
	),
	!,
	gv_get(VNum, IStruct),
	insert_idx_rec(IndexType,Key,RecNo,IStruct).

insert_idx_rec(pdx,Key,RecNo,IStruct)
	:-
	insert_ixr(Key,RecNo,IStruct).

insert_idx_rec(pmx,Key,RecNo,IStruct)
	:-
	insert_imxr(Key,RecNo,IStruct).

insert_idx_rec(hdx,Key,RecNo,IStruct)
	:-
	hash_insert_multi(Key,RecNo,IStruct).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
idx_all_delete_db3([],RelName,RecordNumber,DBFPath,Head,Arity,Module).
idx_all_delete_db3([Pattern+Match | MatchList],RelName,RecordNumber,DBFPath,Head,Arity,Module)
	:-
	idx_delete_db3(Pattern,Match,RelName,RecordNumber,DBFPath,Head,Arity,Module),
	idx_all_delete_db3(MatchList,RelName,RecordNumber,DBFPath,Head,Arity,Module).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
idx_delete_db3(Pattern,Match,RelName,RecNo,DBFPath,Head,Arity,Module)
	:-
	M is Arity+1,
	arg(M, Pattern, AM), 
	AM = idxa_db3(IndexName, [Match,Key]^IFCN, _^iap(IndexType,_,_,_,_), _),
		%% Instatiate Key:
	Module:call(IFCN),
	(Module:index_structs(IndexName,IndexType,VNum) ->
		true
		;
		Module:index_path(IndexName,FileName),
		restore_index(Module,IndexName,FileName),
		Module:index_structs(IndexName,IndexType,VNum)
	),
	!,
	gv_get(VNum, IStruct),
	delete_idx_rec(IndexType,Key,RecNo,IStruct).

delete_idx_rec(pdx,Key,RecNo,IStruct)
	:-
	delete_ixr(Key,RecNo,IStruct).

delete_idx_rec(hdx,Key,RecNo,IStruct)
	:-
	hash_delete_multi(Key,RecNo,IStruct).


	/* -------------------------------------
 	 |	PIDX Indexes
	 * ------------------------------------- */
/*!-----------------------------------------------------------------------------
 |	setup_pdx/1.
 |	setup_pdx(Name)
 |	setup_pdx(+)
 |	setup_pdx/2.
 |	setup_pdx(Module, Name)
 |	setup_pdx(+, +)
 |
 |	-	sets up a pdx (determinate prolog hash table) index named Name
 |
 |	setup_pdx/1 is a module closure expanding to setup_pdx/2.
 *!----------------------------------------------------------------------------*/
:-module_closure(setup_pdx,1).
export setup_pdx/1.
export setup_pdx/2.
setup_pdx(Module,Name)
	:-
	make_pdx_idx(Name,Module).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
export make_pdx_idx/2.
export make_pdx_idx/4.

make_pdx_idx(Name,Module)
	:-
	make_pdx_idx(Name,500,500,Module).

make_pdx_idx(Name,Size,Incr,Module)
	:-
	functor(FF,ixt,Size),
	set_all_args(1,Size,FF,0),
	IStruct = ix(0,FF,Size,Incr),
	fin_make_idx(pdx,Name,IStruct,Module,GVN).

fin_make_idx(hdx,Name,IStruct,Module,GVN)
	:-!,
	name(Name,NameCs),
	make_hash_framework(Module,NameCs,GVN),
	gv_set(GVN, IStruct).

	%% pdx, pmx:
fin_make_idx(_,Name,IStruct,Module,GVN)
	:-
	Module:make_gv(Name),
	catenate(set,Name,SetName),
	SetIt =.. [SetName, IStruct],
	Module:call(SetIt),
	Module:clause(SetIt,gv_set(GVN,_)).

/*
fin_make_idx(pdx,Name,IStruct,Module,GVN)
	:-
	Module:make_gv(Name),
	catenate(set,Name,SetName),
	SetIt =.. [SetName, IStruct],
	Module:call(SetIt),
	Module:clause(SetIt,gv_set(GVN,_)).

fin_make_idx(pmx,Name,IStruct,Module,GVN)
	:-
	fin_make_idx(pdx,Name,IStruct,Module,GVN).
*/


/*!-----------------------------------------------------------------------------
 |	make_pmx_idx/2.
 |	make_pmx_idx(Name,Module)
 |	make_pmx_idx(Name,Module)
 |
 |	-	sets up a pmx (non-determinate prolog hash table) index named Name
 |
 *!----------------------------------------------------------------------------*/
export make_pmx_idx/2.
export make_pmx_idx/4.

make_pmx_idx(Name,Module)
	:-
	make_pmx_idx(Name,500,500,Module).

make_pmx_idx(Name,Size,Incr,Module)
	:-
	functor(FF,ixt,Size),
	set_all_args(1,Size,FF,[]),
	IStruct = ix(0,FF,Size,Incr),
	fin_make_idx(pdx,Name,IStruct,Module,GVN).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
export set_all_args/4.
set_all_args(Cur,Size,FF,ArgVal)
	:-
	Cur > Size, !. 
set_all_args(Cur,Size,FF,ArgVal)
	:-
	arg(Cur,FF,ArgVal),
	Next is Cur +1,
	set_all_args(Next,Size,FF,ArgVal).

/*!-----------------------------------------------------------------------------
 |	save_indexfile/1
 |	save_indexfile(FileName)
 |	save_indexfile(+)
 |	save_indexfile/2
 |	save_indexfile(Module,FileName)
 |	save_indexfile(+,+)
 |
 |	- writes in in-memory pdx or pmx out to disk
 |
 |
 |	save_indexfile/1 is a module closure which expands to save_indexfile/2
 *!----------------------------------------------------------------------------*/
:-module_closure(save_indexfile,1).
export save_indexfile/1.
export save_indexfile/2.

save_indexfile(Module,FileName)
	:-
	Module:bagof(Index,index_path(Index,FileName),Indicies),
	complete_save_indexfile(FileName,Indicies,Module).

complete_save_indexfile(FileName,Indicies,Module)
	:-
	open(FileName,write,OutS,[]),
	write_clause(OutS, FileName),
	write_out_indicies(Indicies,Module,OutS),
	close(OutS).

write_out_indicies([],Module,OutS).
write_out_indicies([IndexName | Indicies],Module,OutS)
	:-
	(atom(IndexName) ->
		Module:index_structs(IndexName,Type,GVNum),
		gv_get(GVNum, IStruct),
		Index = idx(IndexName,Type,IStruct)
		;
		Index = IndexName
	),
	write_out_index(Index,IndexName,OutS),
	write_out_indicies(Indicies,Module,OutS).

/*!-----------------------------------------------------------------------------
 |	save_all_indexfiles/1
 |	save_all_indexfiles(Module)
 |	save_all_indexfiles(+)
 |
 |	- writes all indexfiles from Module out to disk
 *!----------------------------------------------------------------------------*/
export save_all_indexfiles/1.
save_all_indexfiles(Module)
	:-
	Module:setOf(IndexFile, I^index_path(I,IndexFile), IFiles),
	save_each_indexfile(IFiles,Module).

save_each_indexfile([],_).
save_each_indexfile([FileName | IFiles],Module)
	:-
	save_indexfile(Module,FileName),
	save_each_indexfile(IFiles,Module).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
export add_index/4.
add_index(IndexName,Type,Module,FileName)
	:-
	Module:index_structs(IndexName,Type,GVNum),
	gv_get(GVNum, IStruct),
	finish_add_index(FileName,IStruct,IndexName,Type,Module).

finish_add_index(FileName,IStruct,IndexName,Type,Module)
	:-
	exists_file(FileName),
	!,
	read_indexfile(FileName,ExistingTerms),
	update_insert_index(ExistingTerms,IndexName,IStruct,Type,NewTermList),
	complete_save_indexfile(FileName,NewTermList,Module).

finish_add_index(FileName,IStruct,IndexName,Type,Module)
	:-
	complete_save_indexfile(FileName,[idx(IndexName,Type,IStruct)],Module).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
update_insert_index([],IndexName,IStruct,Type,[idx(IndexName,Type,IStruct)]).

update_insert_index([idx(IndexName,Type,_) | OldTerms],IndexName,IStruct,
					Type,[idx(IndexName,Type,IStruct) | NewTerms])
	:-!.
update_insert_index([Tm | OldTerms],IndexName,IStruct, Type,[Tm | NewTerms])
	:-
	update_insert_index(OldTerms,IndexName,IStruct,Type,NewTerms).

write_out_index(Index,IndexName,OutS)
	:-
	write_clause(OutS,Index).

/*!-----------------------------------------------------------------------------
 |	restore_index/2.
 |	restore_index(IndexName,FileName)
 |	restore_index(+,+)
 |	restore_index/3.
 |	restore_index(Module,IndexName,FileName)
 |	restore_index(+,+,+)
 |
 |	- reloads an in-memory pdx or pmx index from disk
 |
 |	restore_index/2 is a module closure which expands to |	restore_index/3.
 *!----------------------------------------------------------------------------*/
:-module_closure(restore_index,2).
export restore_index/2.
export restore_index/3.

restore_index(Module,IndexName,FileName)
	:-
	read_indexfile(FileName,Terms),
	fin_restore(Terms, Module).

read_indexfile(FileName,Terms)
	:-
	open(FileName,read,InS,[]),
	read_term(InS,FileFileName,[]),
	match_path_name(FileName,FileFileName),
/*
	(FileName = FileFileName, !;
		pathPlusFile(_,FileFileName,FileName) ),
*/
	read_terms(InS,Terms),
	close(InS).

fin_restore([], Module).
fin_restore([Index | Terms], Module)
	:-
	Index = idx(IndexName,Type,IStruct),
	fin_make_idx(Type,IndexName,IStruct,Module,GVNum),
	install_index_str(IndexName,Type,GVNum,Module),
	fin_restore(Terms, Module).

match_path_name(FileName,FileName) :-!.
match_path_name(FileName,FileFileName)
	:-
	pathPlusFile(_,FileName0,FileFileName),
	pathPlusFile(_,FileName0,FileName).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
install_index_str(IndexName,Type,GVNum,Module)
	:-
	Module:index_structs(IndexName,Type,GVNum),
	!.
install_index_str(IndexName,Type,GVNum,Module)
	:-
	Module:assert(index_structs(IndexName,Type,GVNum)).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
insert_ixr(IX,RecNo,IStruct)
	:-
	IStruct = ix(CurMax,FF,Size, Incr),
	IX =< Size,
	!,
	mangle(IX,FF,RecNo),
	max(IX,CurMax,NewMax),
	mangle(1, IStruct, NewMax).

insert_ixr(IX,RecNo,IStruct)
	:-
	IStruct = ix(CurMax,FF,Size, Incr),
	NewSize is Size + Incr,
	functor(NewFF,ixt,NewSize),
	copy_slots(1,Size,FF,NewFF),
	Size1 is Size+1,
	set_all_args(Size1,NewSize,NewFF,0),
	mangle(2,IStruct,NewFF),
	mangle(3,IStruct,NewSize),

write(new_istruct_installed_size(Size,NewSize)),nl,

	mangle(IX,NewFF,RecNo),
	max(IX,CurMax,NewMax),
	mangle(1, IStruct, NewMax).

delete_ixr(IX,RecNo,IStruct)
	:-
	IStruct = ix(CurMax,FF,Size, Incr),
	IX =< Size,
	!,
	arg(IX,FF,RecNo),
	mangle(IX,FF,0).
	
/*----------------------------------------------------------
 *---------------------------------------------------------*/
insert_imxr(IX,RecNo,IStruct)
	:-
	IStruct = ix(CurMax,FF,Size, Incr),
	IX =< Size,
	!,
	arg(IX,FF,RecList), 
	(dmember(RecNo, RecList) -> 
		true 
		; 
		mangle(IX,FF,[RecNo | RecList])
	),
	max(IX,CurMax,NewMax),
	mangle(1, IStruct, NewMax).

insert_imxr(IX,RecNo,IStruct)
	:-
	IStruct = ix(CurMax,FF,Size, Incr),
	NewSize is Size + Incr,
	functor(NewFF,ixt,NewSize),
	copy_slots(1,Size,FF,NewFF),
	Size1 is Size+1,
	set_all_args(Size1,NewSize,NewFF,[]),
	mangle(2,IStruct,NewFF),
	mangle(3,IStruct,NewSize),

write(new_imstruct_installed_size(Size,NewSize)),nl,

	arg(IX,FF,RecList), 
	(dmember(RecNo, RecList) -> 
		true 
		; 
		mangle(IX,FF,[RecNo | RecList])
	),
	max(IX,CurMax,NewMax),
	mangle(1, IStruct, NewMax).

delete_imxr(IX,RecNo,IStruct)
	:-
	IStruct = ix(CurMax,FF,Size, Incr),
	IX =< Size,
	!,
	arg(IX,FF,RecList),
	drem(RecList,RecNo,IX,FF).

drem([],RecNo,IX,FF) :-!.
drem([RecNo],RecNo,IX,FF)
	:-!,
	mangle(IX,FF,[]).
drem([RecNo | RecListTail],RecNo,IX,FF)
	:-!,
	mangle(IX,FF,RecListTail).
drem(RecList,RecNo,IX,FF)
	:-
	RecList = [_|RecListTail],
	ddel(RecList,RecListTail,RecNo).

ddel(RecList, RecListTail, RecNo)
	:-
	RecListTail = [HRec | RLT2],
	!,
	(HRec = RecNo ->
		mangle(2, RecList, RLT2)
		;
		ddel(RecListTail, RLT2, RecNo)
	).
ddel(RecList, RecListTail, RecNo).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
copy_slots(Cur,Size,Src,Dst)
	:-
	Cur > Size, !.
copy_slots(Cur,Size,Src,Dst)
	:-
	arg(Cur,Src,AA),
	arg(Cur,Dst,AA),
	Next is Cur + 1,
	copy_slots(Next,Size,Src,Dst).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
retr_ixr(IX,RecNo,ix(CurMax,FF,_,_))
	:-
	IX =< CurMax,
	arg(IX, FF, RecNo).

retr_imxr(IX,RecNo,ix(CurMax,FF,_,_))
	:-
	IX =< CurMax,
	arg(IX, FF, RecList),
	member(RecNo, RecList).

/*-----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
export bld_idx/6.
bld_idx(pdx,RelName,IndexName,Module,Pattern,KeyFcn)
	:-
	Module:table_access_path(RelName,AccPath),
	open_table(AccPath,FilePtr,NumberRecords),
	!,
	catenate(get,IndexName, GetName),
	GetIt =.. [GetName, IStruct],
	(Module:call(GetIt) ->
		true
		;
		make_pdx_idx(IndexName,Module),
		Module:call(GetIt)
	),
	functor(Pattern,_,Arity),
	do_pdx_ix(1,NumberRecords,RelName,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn),
	Module:clause(GetIt,gv_get(VNum,_)),
	install_index_str(IndexName,pdx, VNum,Module).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
	%% Exhausted all records:
do_pdx_ix(CurNum,NumberRecords,Name,FilePtr,_,IStruct,Pattern,Arity,KeyFcn) 
	:-
	CurNum > NumberRecords,
	!.

	%% Record exists and is not marked as deleted:
do_pdx_ix(CurNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn) 
	:-
	dDgetrec(FilePtr,CurNum,RecBuf,_),
	Module:getInfo(opaque(Name),RecBuf,Head),
	!,


	match_idx_pattern(1,Arity,Pattern,Head,Match),
	KeyCall =.. [KeyFcn, Match, Key],
	(Module:call(KeyCall) ->
		insert_ixr(Key,CurNum,IStruct) 
		; true
	),
	NextNum is CurNum + 1,
	do_pdx_ix(NextNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn).

	%% Record existed, but was marked as deleted:
do_pdx_ix(CurNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn) 
	:-
	NextNum is CurNum + 1,
	do_pdx_ix(NextNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn).


/*----------------------------------------------------------
 *---------------------------------------------------------*/
bld_idx(pmx,RelName,IndexName,Module,Pattern,KeyFcn)
	:-
	Module:table_access_path(RelName,AccPath),
	open_table(AccPath,FilePtr,NumberRecords),
	!,
	catenate(get,IndexName, GetName),
	GetIt =.. [GetName, IStruct],
	(Module:call(GetIt) ->
		true
		;
		make_pmx_idx(IndexName,Module),
		Module:call(GetIt)
	),
	functor(Pattern,_,Arity),
	do_pmx_ix(1,NumberRecords,RelName,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn),
	Module:clause(GetIt,gv_get(VNum,_)),
	install_index_str(IndexName,pmx, VNum,Module).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
	%% Exhausted all records:
do_pmx_ix(CurNum,NumberRecords,Name,FilePtr,_,IStruct,Pattern,Arity,KeyFcn) 
	:-
	CurNum > NumberRecords,
	!.

	%% Record exists and is not marked as deleted:
do_pmx_ix(CurNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn) 
	:-
	dDgetrec(FilePtr,CurNum,RecBuf,_),
	Module:getInfo(opaque(Name),RecBuf,Head),
	!,


	match_idx_pattern(1,Arity,Pattern,Head,Match),
	KeyCall =.. [KeyFcn, Match, Key],
	(Module:call(KeyCall) ->
		insert_imxr(Key,CurNum,IStruct) 
		; true
	),
	NextNum is CurNum + 1,
	do_pmx_ix(NextNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn).

	%% Record existed, but was marked as deleted:
do_pmx_ix(CurNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn) 
	:-
	NextNum is CurNum + 1,
	do_pmx_ix(NextNum,NumberRecords,Name,FilePtr,Module,IStruct,Pattern,Arity,KeyFcn).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
bld_idx(hdx,RelName,IndexName,Module,Pattern,KeyFcn)
	:-
	Module:table_access_path(RelName,AccPath),
	open_table(AccPath,FilePtr,NumberRecords),
	!,
	catenate(get,IndexName, GetName),

	GetKey =.. [GetName, Key, Val],
	(Module:clause(GetKey,(gv_get(GVNum,_),hash_lookup(_,_,_))) ->
		gv_get(GVNum,HashTable)
		;
		Module:make_hash_table(IndexName),
		Module:clause(GetKey,(gv_get(GVNum,_),hash_lookup(_,_,_))),
		gv_get(GVNum,HashTable)
	),
	functor(Pattern,_,Arity),
	do_hdx_ix(1,NumberRecords,RelName,FilePtr,Module,HashTable,Pattern,Arity,KeyFcn),
	install_index_str(IndexName, hdx,GVNum,Module).

/*----------------------------------------------------------
 *---------------------------------------------------------*/
do_hdx_ix(CurNum,NumberRecords,Name,FilePtr,_,HashTable,Pattern,Arity,KeyFcn) 
	:-
	CurNum > NumberRecords,
	!.

do_hdx_ix(CurNum,NumberRecords,Name,FilePtr,Module,HashTable,Pattern,Arity,KeyFcn) 
	:-
	dDgetrec(FilePtr,CurNum,RecBuf,_),
	Module:getInfo(opaque(Name),RecBuf,Head),


	match_idx_pattern(1,Arity,Pattern,Head,Match),
	KeyCall =.. [KeyFcn, Match, Key],
	(Module:call(KeyCall) ->
		hash_insert_multi(Key,CurNum,HashTable)
		; true
	),
	NextNum is CurNum + 1,
	do_hdx_ix(NextNum,NumberRecords,Name,FilePtr,Module,HashTable,Pattern,Arity,KeyFcn).

#endif	%% d3x

endmod.

