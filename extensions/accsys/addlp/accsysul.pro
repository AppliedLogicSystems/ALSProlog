/*=============================================================================
 |				accsysul.pro 	
 |	Copyright (c) 1991-1995 Applied Logic Systems, Inc.
 |
 |		-- Low Level Layer above AccSys dBase Library Predicates
 |
 |	Author: Ilyas Cicekli
 |	Date  : 10/1/1991
 |	Additions/Revisions: Ken Bowen, May, 1993; Winter 94-95
 *============================================================================*/
 
/*---------------------------------------------------------------------------*
 |
 |
 |	Raw Feature Tags:
 |	----------------
 |	d3i		- dBaseIII/IV simple i/0 (no indexing, no memo, no mgmt)
 |
 |	d3x 	- dBaseIV indexing machinery (no memo; no mgmt)		-- not used in this file
 |
 |	d3z		- dBaseIII/IV management machinery (no d3t)
 |
 |	Feature Tags:
 |	------------
 |	d3i  (= d3i)
 |		- dBaseIII simple interface (i/o; no indexing, no memo, no mgmt)
 |
 |	d3iz  (= d3i & d3z)
 |		- dBaseIII simple interface (i/o; no mgmt)
 |
 |	d3ixz  (= d3i & d3x & d3z)
 |		- dBase indexed(IV) interface (i/o with indexing & mgmt; no memo)
 *---------------------------------------------------------------------------*/
/*
	%% Raw feature tags:
:-dynamic(d3i/0).
:-dynamic(d3x/0).		-- not used in this file
:-dynamic(d3z/0).

*/

/*---------------------------------------------------------------------------*
	Categorization of accsysul.pro predicates by raw feature tag:
	============================================================
	d3i
	---
	dDbuffs/3
	dDopen/4
	dDclose/2
	dDreccnt/3
	dDreclen/2
	dDgetrec/4
	dDgetrec/5
	dDrecsta/3
	dDapprec/3
	dDinsrec/4
	dDupdrec/4
	dDdelrec/3
	dDrclrec/3
	dDrmrec/3
	dDflush/2
	
	dU3itodf/5
	dU3itodk/5
	dUatocf/3
	dUatonf/5
	dUdfto3i/4
	dUdkto3i/4
	dUdftodk/3
	dUdtonf/5
	dUdtonk/3
	dUexpnm/3
	dUleap/2
	dUnftod/3
	dUnftonk/5
	dUnktoa/6
	dUnktod/3
	dUtoday/3

	d3x  -- not linked any more
	---
	dXactidx/3
	dXbuffs/2
	dXclose/2
	dXcopy/3
	dXcurkey/4
	dXdeaidx/2
	dXflush/2
	dXforwrd/2
	dXgetrno/4
	dXidxbuf/3
	dXkeylen/2
	dXnxtkey/4
	dXopen/4
	dXprvkey/4
	dXputkey/4
	dXrewind/2
	dXrmkey/4
	dXupdkey/5

	d3z
	---
	dDcreat/3
	dDcopy/3
	dDfields/2
	dDfldnm/8
 *---------------------------------------------------------------------------*/
module accsys_db3.

:-dynamic(accsys_file_link_cache/4).

:-module_closure(flush_dD_buffers,1).
export flush_dD_buffers/1.
export flush_dD_buffers/2.

flush_dD_buffers(Mod,Name)
	:-
	Mod:table_access_path(Name,Path),
	accsys_file_link_cache(Path, _, _, Ptr),
	a_dDflush(Ptr,_).

/********************************************************************
 *																	*
 * 	dD Functions - DBF File Handling Functions 			*
 *																	*
 *******************************************************************/

#if (syscfg:d3i)

/**********************************************************
 *		SIMPLE DATABASE I/O PREDICATES (d3i)
 **********************************************************/

/*!-d3i-----------------------------------------------------------------
 | dDbuffs/3
 | dDbuffs(DBFName,Buffs,RetVal)
 | dDbuffs(+,+,-)
 |
 |	-	determines the required number of buffers to open a DBF file
 |
 | Inputs:
 |	DBFName 	name of DBF database file
 |	Buffs 		number of buffers requested
 |
 | Outputs:
 |	RetVal 		memory required to allocate file buffers or error code
 |
 |				(memory size is positive; error codes are negative)
 *--------------------------------------------------------------------*/
export dDbuffs/3.
dDbuffs(DBFName,Buffs,RetVal) :-
	a_dDbuffs(DBFName,Buffs,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDopen/4
 | dDopen(DBFName,ModeStr,Buffs,RetVal)
 | dDopen(+,+,+,-)
 |
 |	-	opens a DBF file
 |
 | Inputs:
 | 	DBFName 	name of DBF database file
 | 	ModeStr 	file open mode (a string)
 | 	Buffs 		number of buffers to be used
 |
 | Outputs:
 |	RetVal 		file reference pointer or NULL (0).
 *--------------------------------------------------------------------*/
export dDopen/4.
dDopen(DBFName,_,_,RetVal) 
	:-
	accsys_file_link_cache(DBFName, ModeStr, Buffs, RetVal),
	!.
dDopen(DBFName,ModeStr,Buffs,RetVal) 
	:-
	'$c_constinfo'(ModeStr,Mode),
	a_dDopen(DBFName,Mode,Buffs,RetVal),
	RetVal > 0,
	!,
	assert(accsys_file_link_cache(DBFName, ModeStr, Buffs, RetVal)).

dDopen(DBFName,ModeStr,Buffs,RetVal) 
	:-
	accsys_file_link_cache(ODBFName, OModeStr, OBuffs, ORetVal),
	ODBFName \= DBFName,
	retract(accsys_file_link_cache(ODBFName, OModeStr, OBuffs, ORetVal)),
	'$c_constinfo'(ModeStr,Mode),
	a_dDopen(DBFName,Mode,Buffs,RetVal),
	RetVal > 0,
	!,
	assert(accsys_file_link_cache(DBFName, ModeStr, Buffs, RetVal)).

dDopen(DBFName,ModeStr,Buffs,RetVal) 
	:-
	dDglobal_vs(dretcode,ErrVal),
	a_retcode(ErrCode,		ErrVal,  ErrorText),
	dDglobal_vs(d_report,RepErrVal),
	printf('Accsys DBF open error %t (%t): %t [rep: %t]\n',
			[DBFName,ErrVal,ErrorText,RepErrVal]).

export d_close_all_tables/0.
d_close_all_tables
	:-
	accsys_file_link_cache(A, B, C, TblPtr),
	dDclose(TblPtr,_),
	retract(accsys_file_link_cache(A, B, C, TblPtr)),
	!,
	d_close_all_tables.
d_close_all_tables.

/*!-d3i-----------------------------------------------------------------
 | dDclose/2
 | dDclose(DBFPTR,RetVal)
 | dDclose(+,-)
 |
 |	- closes a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file refrence 
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDclose/2.
dDclose(DBFPTR,RetVal) :-
	a_dDflush(DBFPTR,_),
	retract_all(accsys_file_link_cache(DBFName, _, _, DBFPTR)),
	a_dDclose(DBFPTR,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDreccnt/3
 | dDreccnt(DBFPTR,NumofRecs,RetVal)
 | dDreccnt(+,-,-)
 |
 |	-	returns number of records in a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file refrence 
 |
 | Outputs:
 | 	NumofRecs 	Number of records
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDreccnt/3.
dDreccnt(0,NumofRecs,RetVal) :-!, fail.
dDreccnt(DBFPTR,NumofRecs,RetVal) :-
	'$uia_alloc'(4,NumofRecsPtr),
	a_dDreccnt(DBFPTR,NumofRecsPtr,RetVal),
	'$uia_peekl'(NumofRecsPtr,0,NumofRecs).

/*!-d3i-----------------------------------------------------------------
 | dDreclen/2
 | dDreclen(DBFPTR,RetVal)
 | dDreclen(+,-)
 |
 |	- Returns the record length for a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 |
 | Outputs:
 |	RetVal 		Record length or error code
 |				(Record length is positive; error codes are negative)
 *--------------------------------------------------------------------*/
export dDreclen/2.
dDreclen(DBFPTR,RetVal) :-
	a_dDreclen(DBFPTR,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDgetrec/4
 | dDgetrec(DBFPTR,RecNo,Rec,RetVal)
 | dDgetrec(+,+,-,-)
 |
 |	-	reads a record from a DBF file by record number
 |
 | Inputs:
 |	DBFPTR 		DBF file reference
 | 	RecNo 		Record number
 |
 | Outputs:
 | 	Rec 		Record buffer
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
/*!-d3i-----------------------------------------------------------------
 | dDgetrec/5
 | dDgetrec(DBFPTR,RecNo,RecLen,Rec,RetVal)
 | dDgetrec(+,+,+,-,-)
 |
 |	-	reads a record from a DBF file by record number
 |
 | Inputs:
 |	DBFPTR 		DBF file reference
 | 	RecNo 		Record number
 |	RecLen 		Record length
 |
 | Outputs:
 | 	Rec 		Record buffer
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDgetrec/4.
dDgetrec(DBFPTR,RecNo,Rec,RetVal) :-
	a_dDreclen(DBFPTR,RecLen),
	'$c_constinfo'('dNOOPEN',DNOOPEN),
	RecLen \= DNOOPEN,
	'$uia_alloc'(RecLen,Rec),
	a_dDgetrec(DBFPTR,RecNo,Rec,RetVal).

export dDgetrec/5.
dDgetrec(DBFPTR,RecNo,RecLen,Rec,RetVal) :-
	'$uia_alloc'(RecLen,Rec),
	a_dDgetrec(DBFPTR,RecNo,Rec,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDrecsta/3
 | dDrecsta(DBFPTR,RecNo,RetVal)
 | dDrecsta(+,+,-)
 |
 |	-	returns the status of a DBF file record
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	RecNo 		Record number
 |
 | Outputs:
 |	RetVal 		Status of record (active or deleted) or error code
 *--------------------------------------------------------------------*/
export dDrecsta/3.
dDrecsta(DBFPTR,RecNo,RetVal) :-
	a_dDrecsta(DBFPTR,RecNo,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDapprec/3
 | dDapprec(DBFPTR,Rec,RetVal)
 | dDapprec(+,+,-)
 |
 |	-	appends a record to a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	Rec 		Record buffer
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDapprec/3.
dDapprec(DBFPTR,Rec,RetVal) :-
	a_dDapprec(DBFPTR,Rec,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDinsrec/4
 | dDinsrec(DBFPTR,RecNo,Rec,RetVal)
 | dDinsrec(+,+,+,-)
 |
 |	-	inserts a new record in a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	RecNo 		Record number
 | 	Rec 		Record buffer
 |
 | Outputs:
 |	RetVal 		Success or error code
 |
 *--------------------------------------------------------------------*/
export dDinsrec/4.
dDinsrec(DBFPTR,RecNo,Rec,RetVal) :-
	a_dDinsrec(DBFPTR,RecNo,Rec,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDupdrec/4
 | dDupdrec(DBFPTR,RecNo,Rec,RetVal)
 | dDupdrec(+,+,+,-)
 |
 |	- updates an existing record in a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	RecNo 		Record number
 | 	Rec 		Record buffer
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDupdrec/4.
dDupdrec(DBFPTR,RecNo,Rec,RetVal) :-
	a_dDupdrec(DBFPTR,RecNo,Rec,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDdelrec/3
 | dDdelrec(DBFPTR,RecNo,RetVal)
 | dDdelrec(+,+,-)
 |
 |	-	marks a record in a DBF file as 'deleted'
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	RecNo 		Record number
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDdelrec/3.
dDdelrec(DBFPTR,RecNo,RetVal) :-
	a_dDdelrec(DBFPTR,RecNo,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDrclrec/3
 | dDrclrec(DBFPTR,RecNo,RetVal)
 | dDrclrec(+,+,-)
 |
 |	-	removes 'deleted' mark from a record in a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	RecNo 		Record number
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDrclrec/3.
dDrclrec(DBFPTR,RecNo,RetVal) :-
	a_dDrclrec(DBFPTR,RecNo,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDrmrec/3
 | dDrmrec(DBFPTR,RecNo,RetVal)
 | dDrmrec(+,+,-)
 |
 |	-	physically removes the record with with given record number
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	RecNo 		Record number
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDrmrec/3.
dDrmrec(DBFPTR,RecNo,RetVal) :-
	a_dDrmrec(DBFPTR,RecNo,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDflush/2
 | dDflush(DBFPTR,RetVal)
 | dDflush(+,-)
 |
 |	-	forces the contents of file buffers for a DBF file to be written to the file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDflush/2.
dDflush(DBFPTR,RetVal) :-
	a_dDflush(DBFPTR,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dDglobal_vs/2
 | dDglobal_vs(GlobalVarName,Val)
 | dDglobal_vs(+,-)
 |
 |	-	returns values of internal Accsys global variables
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 |
 | Outputs:
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDglobal_vs/2.
dDglobal_vs(GlobalVarName,Val)
	:-
	gv_encoding(GlobalVarName, GVCodeNum),
	acc_gv(GVCodeNum, Val).

gv_encoding(dversion, 0).
gv_encoding(d_report, 1).
gv_encoding(dretcode, 2).
gv_encoding(d_blksiz, 3).
gv_encoding(d_request, 4).
gv_encoding(d_recno, 5).

a_retcode('SUCCESS',	 0,  'successful function return code').
a_retcode(dERROR,		-1,  'general error code').
a_retcode(dBADNAME,		-2,  'bad file or field name').
a_retcode(dBADFILE,		-3,  'accessed file is not organized in the correct file format').
a_retcode(dOUTRANGE,	-4,  'out of range error').
a_retcode(dNOTFOUND,	-5,  'requested object not found').
a_retcode(dBOF,			-6,  'beginning of index file condition (pointer reached the top of index file)').
a_retcode(dEOF,			-7,  'end of index file condition (pointer reached the bottom of index file)').
a_retcode(dKEYVIOL,		-8,  'key violation').
a_retcode(dMISMATCH,	-9,  'mismatch between index and data file').
a_retcode(dNOOPEN,		-10, 'file not open').
a_retcode(dILLEGAL,		-11, 'illegal request').
a_retcode(dIOERR,		-12, 'I/O error').
a_retcode(dMEMERR,		-13, 'memory allocation failure').
a_retcode(dENCRYPT,		-14, 'file encrypted').
a_retcode(dRDONLY,		-15, 'file in read-only mode').

/*!-d3i-----------------------------------------------------------------
 | acc_record_to_fields/3
 | acc_record_to_fields(DBFPTR,Rec,Fields)
 | acc_record_to_fields(+,+,-)
 |
 |	-	converts a DBF file record (UIA) to a list of field values
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	Rec 		Record buffer
 |
 | Outputs:
 | 	Fields 		List of field values
 |
 *--------------------------------------------------------------------*/
acc_record_to_fields(DBFPTR,Rec,Fields) :-
	'$c_constinfo'('SUCCESS',SUCCESS),
	'$c_constinfo'('dNOOPEN',DNOOPEN),
	a_dDfields(DBFPTR,NumofFields),
	NumofFields \= DNOOPEN,
	acc_record_to_fields(1,NumofFields,SUCCESS,DBFPTR,Rec,Fields).

acc_record_to_fields(Pos,NumofFields,SUCCESS,DBFPTR,Rec,[FieldVal | Fields]) :-
	Pos =< NumofFields, !,
	dDfldno(DBFPTR,Pos,Off,Name,Type,Width,Dec,SUCCESS), 
	Offset is Off - 1,
	'$uia_peek'(Rec,Offset,Width,FieldVal),
	NewPos is Pos + 1,
	acc_record_to_fields(NewPos,NumofFields,SUCCESS,DBFPTR,Rec,Fields).
acc_record_to_fields(Pos,NumofFields,SUCCESS,DBFPTR,Rec,[]) :-
	Pos > NumofFields, !.

/*!-d3i-----------------------------------------------------------------
 | acc_fields_to_record/3
 | acc_fields_to_record(DBFPTR,Fields,Rec)
 | acc_fields_to_record(+,+,-)
 |
 |	-	converts a list of field values to a UIA for insertion as a record in a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 | 	Fields 		List of field values
 |
 | Outputs:
 | 	Rec 		Record buffer
 *--------------------------------------------------------------------*/
acc_fields_to_record(DBFPTR,Fields,Rec) :-
	'$c_constinfo'('SUCCESS',SUCCESS),
	'$c_constinfo'('dNOOPEN',DNOOPEN),
	a_dDfields(DBFPTR,NumofFields),
	NumofFields \= DNOOPEN,
	a_dDreclen(DBFPTR,RecLen),
	RecLen \= DNOOPEN,
	'$uia_alloc'(RecLen,Rec),
	acc_fields_to_record(1,NumofFields,SUCCESS,DBFPTR,Fields,Rec).

acc_fields_to_record(Pos,NumofFields,SUCCESS,DBFPTR,[FieldVal | Fields],Rec) :-
	Pos =< NumofFields, !,
	dDfldno(DBFPTR,Pos,Off,Name,Type,Width,Dec,SUCCESS), 
	Offset is Off - 1,
	space_padded_uia(Width,FieldVal,PaddedFieldVal),
	%%$uia_pokes(Rec,Offset,PaddedFieldVal),
	'$uia_poke'(Rec,Offset,Width,PaddedFieldVal),
	NewPos is Pos + 1,
	acc_fields_to_record(NewPos,NumofFields,SUCCESS,DBFPTR,Fields,Rec).
acc_fields_to_record(Pos,NumofFields,SUCCESS,DBFPTR,[],Rec) :-
	Pos > NumofFields, !.

#endif		%% d3i

#if (syscfg:d3z)

/**********************************************************
 *		DATABASE MANAGEMENT PREDICATES (d3z)
 **********************************************************/

/*!-d3z----------------------------------------------------------------------------
 |	dDcreat/3
 |	dDcreat(DBFName,Fields,RetVal)
 |	dDcreat(+,+,-)
 |	
 |	-	creates a new empty  dBase file (table) according to a specification
 |
 |	 Inputs:
 |		DBFName: 	name of DBF database file
 |	 	Fields:  	list of terms in the form of "field(Name,Type)".
 |	
 |	'Name' must be an atom.
 |	'Type' must be one of the following:
 |		'C<nnn>'	- character, width = <nnn>; e.g., C23
 |		'N<nn.mm>'	- numeric, decminal; e.g. N12.5	
 |		'F<nn.mm>'	- float; e.g. F14.8
 |						For both numeric and float:
 |						<nn> is total field width, including 1 for the
 |						decimal point; <mm> is width of decimal part
 |		'D'			- date		- width  = 8 bytes
 |		'M'			- memo		- width  = 1 bytes
 |		'L'			- logical	- width  = 10 bytes
 |	
 |	Outputs:
 |		RetVal:		Success or errro code
 |
 |	Limitations:
 |	1.	Maximum of 255 fields in any table.
 |	2a.	Character field max is 254.
 |	2b.	Numeric and float fields max is 20, including decimal point.
 |	2c.	Other types widths according to defaults above.
 |	3.	Max width field name = 10 chars.
 |	4.	Max record length = 4000 bytes.
 |	5.	Legal chars in field names = uppercase chars, 0-9, and underscore(_)
 |	6.	Field names must begin with a letter.
 *!----------------------------------------------------------------------------*/
export dDcreat/3.
dDcreat(DBFName,FieldsList,RetVal) 
	:-
	create_fields(FieldsList,FieldsPtrList,0,NumofItems),
	c_createn_abs(ptr,NumofItems,FieldsPtrList,Fields),
	NumofFields is (NumofItems >> 1), 
	!,
	(a_dDcreat(DBFName,NumofFields,Fields,RetVal) -> true ; true),
	c_free_abs(Fields),
	free_fields(FieldsPtrList).

create_fields([],[],N,N) :- !.
create_fields([field(Name,Type)|FList],[NamePtr,TypePtr|Fields],In,Out) :-
	c_create_abs(str,Name,NamePtr),
	c_create_abs(str,Type,TypePtr),
	NewIn is In + 2,
	create_fields(FList,Fields,NewIn,Out). 

free_fields([]) :- !.
free_fields([Ptr|Rest]) :-
	c_free_abs(Ptr),
	free_fields(Rest).

/*!-d3z--------------------------------------------------------------
 | dDcopy/3
 | dDcopy(SrcDBFName,DestDBFName,RetVal)
 | dDcopy(+,+,-)
 |
 |	-	creates a new empty dBase file with the same header as the input file
 |
 | Inputs:
 | 	SrcDBFName 	name of original DBF database file
 | 	DestDBFName	name of new DBF database file
 |
 | Outputs:
 |	RetVal 		Success or error code
 *------------------------------------------------------------------*/
export dDcopy/3.
dDcopy(SrcDBFName,DestDBFName,RetVal) :-
	a_dDcopy(SrcDBFName,DestDBFName,RetVal).

/*!-d3z-----------------------------------------------------------------
 | dDfields/2
 | dDfields(DBFPTR,RetVal)
 | dDfields(+,-)
 |
 |	-	returns the number of fields per record in a DBF file
 |
 | Inputs:
 | 	DBFPTR 		DBF	file reference 
 |
 | Outputs:
 |	RetVal 		Number of fields or error code
 |				(Number of fields is positive; error codes are negative)
 *--------------------------------------------------------------------*/
export dDfields/2.
dDfields(DBFPTR,RetVal) :-
	a_dDfields(DBFPTR,RetVal).

/*!-d3z-----------------------------------------------------------------
 | dDfldnm/8
 | dDfldnm(DBFPTR,Name,Off,Pos,Type,Width,Dec,RetVal) 
 | dDfldnm(+,+,-,-,-,-,-,-) 
 |
 |	-	returns info about a field, by field name
 |
 | Inputs:
 |	DBFPTR 		DBF file reference
 |	Name 		Field name
 |
 | Outputs:
 | 	Off 		Field offset
 |	Pos 		Field position
 | 	Type 		Field type
 | 	Width 		Field width
 |	Dec 		Decimal places
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDfldnm/8.
dDfldnm(DBFPTR,Name,Off,Pos,Type,Width,Dec,RetVal) :-
	'$uia_alloc'(4,OffPtr),
	'$uia_alloc'(4,PosPtr),
	'$uia_alloc'(1,TypePtr),
	'$uia_alloc'(4,WidthPtr),
	'$uia_alloc'(4,DecPtr),
	a_dDfldnm(DBFPTR,Name,OffPtr,PosPtr,TypePtr,WidthPtr,DecPtr,RetVal),
	'$uia_peekl'(OffPtr,0,Off),
	'$uia_peekl'(PosPtr,0,Pos),
	'$uia_peeks'(TypePtr,0,1,Type),
	'$uia_peekl'(WidthPtr,0,Width),
	'$uia_peekl'(DecPtr,0,Dec).

#endif		%% d3z

#if (syscfg:(d3i ; d3z) )

/*!-d3z-d3i-------------------------------------------------------------
 | dDfldno/8
 | dDfldno(DBFPTR,Pos,Off,Name,Type,Width,Dec,RetVal)
 | dDfldno(+,+,-,-,-,-,-,-)
 |
 |	-	returns info about a field, by field number
 |
 | Inputs:
 |	DBFPTR 		DBF file reference
 |	Pos 		Field position
 |
 | Outputs:
 | 	Off 		Field offset
 |	Name 		Field name
 | 	Type 		Field type
 | 	Width 		Field width
 |	Dec 		Decimal places
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDfldno/8.
dDfldno(DBFPTR,Pos,Off,Name,Type,Width,Dec,RetVal) :-
	'$uia_alloc'(4,OffPtr),
	'$uia_alloc'(11,NamePtr),
	'$uia_alloc'(1,TypePtr),
	'$uia_alloc'(4,WidthPtr),
	'$uia_alloc'(4,DecPtr),
	a_dDfldno(DBFPTR,Pos,OffPtr,NamePtr,TypePtr,WidthPtr,DecPtr,RetVal),
	'$uia_peekl'(OffPtr,0,Off),
	'$uia_peeks'(NamePtr,0,Name),
	'$uia_peeks'(TypePtr,0,1,Type),
	'$uia_peekl'(WidthPtr,0,Width),
	'$uia_peekl'(DecPtr,0,Dec).

#endif 		%% (syscfg:(dsi ; d3z) )

#if (syscfg:d3z)

/*!-d3z-----------------------------------------------------------------
 | dDdate/5
 | dDdate(DBFPTR,Month,Day,Year,RetVal)
 | dDdate(+,-,-,-,-)
 |
 |	-	returns date of last update of a DBF file
 |
 | Inputs:
 |	DBFPTR 		DBF file reference
 |
 | Outputs:
 |	Month 		Month
 | 	Day 		Day
 | 	Year 		Year
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dDdate/5.
dDdate(DBFPTR,Month,Day,Year,RetVal) :-
	'$uia_alloc'(4,MonthPtr),
	'$uia_alloc'(4,DayPtr),
	'$uia_alloc'(4,YearPtr),
	a_dDdate(DBFPTR,MonthPtr,DayPtr,YearPtr,RetVal),
	'$uia_peekl'(MonthPtr,0,Month),
	'$uia_peekl'(DayPtr,0,Day),
	'$uia_peekl'(YearPtr,0,Year).

#endif		%% d3z

/********************************************************************
 *																	*
 * 				dU Functions - Utility Functions 					*
 *																	*
 *******************************************************************/

#if (syscfg:d3i)

/*!-d3i-----------------------------------------------------------------
 | dU3itodf/5
 | dU3itodf(Month,Day,Year,DateField,RetVal)
 | dU3itodf(+,+,+,-,-)
 |
 |	- Converts 3 integers (month, day, year) to a date field
 |
 | Inputs:
 |	Month 		Month
 | 	Day 		Day
 | 	Year 		Year
 |
 | Outputs:
 |	DateField	eight-byte dBASE date field
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/

export dU3itodf/5.
dU3itodf(Month,Day,Year,DateField,RetVal) :-
	'$uia_alloc'(8,DateField),
	a_dU3itodf(Month,Day,Year,DateField,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dUatocf/3
 | dUatocf(Str,Width,CField)
 | dUatocf(+,+,-)
 |
 |	- Converts a string to a character field
 |
 | Inputs:
 |	Str 		ASCII character string
 | 	Width 		Width of character field
 |
 | Outputs:
 |	CField 		character field with padded blanks
 *--------------------------------------------------------------------*/
export dUatocf/3.
dUatocf(Str,Width,CField) :-
	'$uia_alloc'(Width,CField),
	a_dUatocf(Str,Width,CField).

/*!-d3i-----------------------------------------------------------------
 | dUatonf/5
 | dUatonf(Str,Width,Decimal,NField,RetVal)
 | dUatonf(+,+,+,-,-)
 |
 |	- Converts a string to a numeric/float field
 | 
 | Inputs:
 |   Str         ASCII character string 
 |   Width       Width of character field 
 |	Decimal 	Decimal places
 |
 | Outputs:
 |   NField      numeric field  
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dUatonf/5.
dUatonf(Str,Width,Decimal,NField,RetVal) :-
    '$uia_alloc'(Width,NField),
    a_dUatonf(Str,Width,Decimal,NField,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dUdfto3i/4
 | dUdfto3i(DateField,Month,Day,Year)
 | dUdfto3i(+,-,-,-)
 |
 |	- Converts a date field to 3 integers (month, day, year)
 |
 | Inputs:
 |	DateField	eight-byte dBASE date field
 |
 | Outputs:
 |	Month 		Month
 | 	Day 		Day
 | 	Year 		Year
 *--------------------------------------------------------------------*/
export dUdfto3i/4.
dUdfto3i(DateField,Month,Day,Year) :-
	'$uia_alloc'(4,MonthPtr),
	'$uia_alloc'(4,DayPtr),
	'$uia_alloc'(4,YearPtr),
	a_dUdfto3i(DateField,MonthPtr,DayPtr,YearPtr),
	'$uia_peekl'(MonthPtr,0,Month),
	'$uia_peekl'(DayPtr,0,Day),
	'$uia_peekl'(YearPtr,0,Year).

/*!-d3i-----------------------------------------------------------------
 | dUdtonf/5
 | dUdtonf(DoubleNum,Width,Decimal,NField,RetVal)
 | dUdtonf(+,+,+,-,-)
 |
 |	- Converts a double to a numeric/float field
 | 
 | Inputs:
 |   DoubleNum 	Double floating point number
 |   Width       Width of character field 
 |	Decimal 	Decimal places
 |
 | Outputs:
 |   NField      numeric field  
 |	RetVal 		Success or error code
 *--------------------------------------------------------------------*/
export dUdtonf/5.
dUdtonf(DoubleNum,Width,Decimal,NField,RetVal) :-
    '$uia_alloc'(Width,NField),
    a_dUdtonf(DoubleNum,Width,Decimal,NField,RetVal).


/**********************
/*!-d3i-----------------------------------------------------------------
 | dUexpnm/3
 | dUexpnm(FName,FExtension,ExpFName)
 | dUexpnm(+,+,-)
 |
 | Inputs:
 | 	FName 		Original file name  
 |   FExtension 	File extension
 |
 | Outputs:
 | 	ExpFName 	Expanded file name
 *--------------------------------------------------------------------*/
export dUexpnm/3.
dUexpnm(FName,FExtension,ExpFName) :-
	'$strlen'(FName,FNameSize),
	'$strlen'(FExtension,FExtensionSize),
	ExpFNameSize is FNameSize + FExtensionSize + 1,
    '$uia_alloc'(ExpFNameSize,ExpFName),
	a_dUexpnm(FName,FExtension,ExpFName).
**********************/

/*!-d3i-----------------------------------------------------------------
 | dUleap/2
 | dUleap(Year,RetVal)
 | dUleap(+,-)
 |
 |	- determines whether a year is a leap year
 |
 | Inputs:
 | 	Year 		Year
 |
 | Outputs:
 | 	RetVal 		non-zero if leap year; zero if not leap year
 *--------------------------------------------------------------------*/
export dUleap/2.
dUleap(Year,RetVal) :-
	a_dUleap(Year,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dUnftod/3
 | dUnftod(NField,Width,RetVal)
 | dUnftod(+,+,-)
 | 
 |	- Converts a numeric/float field to a double
 |
 | Inputs:
 |   NField      Numeric field  
 |   Width       Width of character field 
 |
 | Outputs:
 |   RetVal  	Double floating point number
 *--------------------------------------------------------------------*/
export dUnftod/3.
dUnftod(NField,Width,RetVal) :-
	a_dUnftod(NField,Width,RetVal).

/*!-d3i-----------------------------------------------------------------
 | dUtoday/3
 | dUtoday(Month,Day,Year)
 | dUtoday(-,-,-)
 |
 |	- Gets today's date
 |
 | Inputs:
 |
 | Outputs:
 |	Month 		Month
 | 	Day 		Day
 | 	Year 		Year
 *--------------------------------------------------------------------*/
export dUtoday/3.
dUtoday(Month,Day,Year) :-
	'$uia_alloc'(4,MonthPtr),
	'$uia_alloc'(4,DayPtr),
	'$uia_alloc'(4,YearPtr),
	a_dUtoday(MonthPtr,DayPtr,YearPtr),
	'$uia_peekl'(MonthPtr,0,Month),
	'$uia_peekl'(DayPtr,0,Day),
	'$uia_peekl'(YearPtr,0,Year).

#endif		%% d3i - utilities

#if (syscfg: d3t)

/********************************************************************
 *																	*
 * 				dT Functions - Memo Functions 					    *
 *																	*
 *******************************************************************/

/*!-d3t-----------------------------------------------------------------
 |	dTcreat/2
 |	dTcreat(Dbtname, RetVal)
 |	dTcreat(+, -)
 *--------------------------------------------------------------------*/
export dTcreat/2.
dTcreat(Dbtname, RetVal)
	:-
	a_dTcreat(Dbtname, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTaddmm/6
 |	dTaddmm(Dbtptr, Dbtlength, Memobuff, Memofield, Setnew, RetVal)
 |	dTaddmm(+, +, +, -, -, -)
 *--------------------------------------------------------------------*/
export dTaddmm/6.
dTaddmm(Dbtptr, Dbtlength, Memobuff, Memofield, Setnew, RetVal)
	:-
	a_dTaddmm(Dbtptr, Dbtlength, Memobuff, Memofield, Setnew, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTchkmm/2
 |	dTchkmm(Memofield, RetVal)
 |	dTchkmm(+, -)
 *--------------------------------------------------------------------*/
export dTchkmm/2.
dTchkmm(Memofield, RetVal)
	:-
	a_dTchkmm(Memofield, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTclose/2
 |	dTclose(Dbtptr, RetVal)
 |	dTclose(+, +)
 *--------------------------------------------------------------------*/
export dTclose/2.
dTclose(Dbtptr, RetVal)
	:-
	a_dTclose(Dbtptr, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTgetmm/6
 |	dTgetmm(Dbtptr, Memofield, Dbtoffset, Dbtlength, Memobuff, RetVal)
 |	dTgetmm(+, +, +, +, -, -)
 *--------------------------------------------------------------------*/
export dTgetmm/6.
dTgetmm(Dbtptr, Memofield, Dbtoffset, Dbtlength, Memobuff, RetVal)
	:-
	'$uia_alloc'(Dbtlength,Memobuff),
	a_dTgetmm(Dbtptr, Memofield, Dbtoffset, Dbtlength, Memobuff, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTmemosz/4
 |	dTmemosz(Dbtptr, Memofield, Size, RetVal)
 |	dTmemosz(+, +, -, -)
 *--------------------------------------------------------------------*/
export dTmemosz/4.
dTmemosz(Dbtptr, Memofield, Size, RetVal)
	:-
	a_dTmemosz(Dbtptr, Memofield, Size, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTmemuse/2
 |	dTmemuse(Dbtname, RetVal)
 |	dTmemuse(+, -)
 *--------------------------------------------------------------------*/
export dTmemuse/2.
dTmemuse(Dbtname, RetVal)
	:-
	a_dTmemuse(Dbtname, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTnewmm/5
 |	dTnewmm(Dbtptr, Dbtlength, Memobuff, Memofield, RetVal)
 |	dTnewmm(+, +, +, -, -)
 *--------------------------------------------------------------------*/
export dTnewmm/5.
dTnewmm(Dbtptr, Dbtlength, Memobuff, Memofield, RetVal)
	:-
	a_dTnewmm(Dbtptr, Dbtlength, Memobuff, Memofield, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTopen/3
 |	dTopen(Dbtname, Mode, RetVal)
 |	dTopen(+, +, -)
 *--------------------------------------------------------------------*/
export dTopen/3.
dTopen(Dbtname, Mode, RetVal)
	:-
	a_dTopen(Dbtname, Mode, RetVal).

/*!-d3t-----------------------------------------------------------------
 |	dTupdmm/6
 |	dTupdmm(Dbtptr, Dbtlength, Memobuff, Memofield, Setnew, RetVal)
 |	dTupdmm(+, +, +, -, -, -)
 *--------------------------------------------------------------------*/
export dTupdmm/6.
dTupdmm(Dbtptr, Dbtlength, Memobuff, Memofield, Setnew, RetVal)
	:-
	a_dTupdmm(Dbtptr, Dbtlength, Memobuff, Memofield, Setnew, RetVal).

#endif		%% d3t - memo

endmod.  %% accsys_db3.
