/*-----------------------------------------------------------------------------*
 | 				cutils.pro
 | Copyright (c) 1991-96, Applied Logic Systems Inc.
 |		Distribution rights per Copying ALS
 |
 | 		C data manipulation predicates
 |
 | Creation Date : 3 Oct, 1991
 | Author : P. Raman
 |
 | Modification History :
 |		c_rconst/2 should no longer be used to access C globals
 |		Added string handling predicates which include handling Pascal strings
 |
 | Exported predicates :
 |
 | 	c_const(+Name,-Val) 
 |		get the value of a C defined constant.
 |
 | 	c_rconst(+Name,-Val) 
 |		get the value of a C defined runtime constant
 |
 | 	c_alloc(+Type,-UIA)
 |		allocate a UIA that can hold data of type Type.
 |
 | 	c_alloc_abs(+Type,-Ptr)
 |		malloc area that can hold data of type Type.
 |
 | 	c_allocn(+Type,+Num,-UIA) 
 |		allocate UIA to hold Num items of type Type.
 |
 | 	c_allocn_abs(+Type,+Num,-Ptr)
 |		malloc area to hold Num items of type Type.
 |
 |	c_free(+UIA)
 |		does nothing (freeing taken care of by gc)
 |
 |	c_free_abs(+Ptr)
 |		mfree the area pointed by Ptr.
 |
 |	c_set(+Obj,+Type1,+Val)
 |	c_set(+Obj,+Type2,[+FieldName,+Val,...])
 |		destructively modify a UIA or C data area with Val
 |		of type Type. In the first form Type1 can be a integral
 |		type or 'str'. In the second form Type2 must be a 
 |		structure type, and FieldName is the name of a field
 |		to be modified with value Val. Subfields are identified 
 |		by their (C-style) dot-separated pathname, except when
 |		the sub-structure is a type-reference, in which case
 |		FieldName is the name of the field, and Val is (recursively)
 |		a list of FieldName - Value pairs.
 |
 |	c_set_str(+Obj,+Off,+Len,+SymOrUIA)
 |		destructively modify a segment of data in the UIA or
 |		C data Obj at an offset Off by no more than Len 
 |		bytes taken from SymOrUIA.
 |
 |	c_set_raw(+Obj,+Off,+Len,+UIA)
 |		writes Len characters from UIA into Obj starting
 |		at offset Off.
 |
 |	c_setn(+Obj,+Type,+I,+Val)
 |	c_setn(+Obj,+SType,+I,[+FieldName,+Val,...])
 |		perform c_set/3 on the I-th component of an array, where
 |		I=0 refers to the first element and so on. In the
 |		first form, Type must be a base type. In the second form
 |		SType must be a structure type ( substructures are handled
 |		in the same manner as in c_set/3).
 |
 |	c_examine(+Obj,+Type,-Val)
 |	c_examine(+Obj,+SType,[+FieldName,-Val,...])
 |		examine a UIA or C ptr Obj, assuming its type 
 |		to be Type. In the first form, Type is either
 |		an intergral type or 'str'. In the second form,
 |		SType must be a structure type.
 |
 |	c_examine_str(+Obj,+Off,+Len,-Val)
 |		examine a segment of Obj starting at offset Off
 |		until first null character or Len characters
 |		whichever comes first and return the value as
 |		as a UIA in Val.
 |	c_examine_raw(+Obj,+Off,+Len,-Val)
 |		reads Len characters starting at offset Off
 |		into Obj and returns the result as a UIA in Val.
 |		Note that the resulting UIA can have null and
 |		other funny characters.
 |
 |	c_examinen(+Obj,+Type,+I,-Val)
 |	c_examinen(+Obj,+SType,+I,[+FieldName,-Val,...])
 |		examine the I the component of array of type Type/SType.
 |
 |	c_create(+Type,+Val,-UIA)
 |	c_create(+SType,[+FieldName,+Val,...],-UIA)
 |		equivalent to c_alloc/2 plus c_set/3. Type can be
 |		an integral type or 'str'. SType must be a structure 
 |		type.
 |
 |	c_create_abs(+Type,+Val,-Ptr)
 |	c_create_abs(+SType,[+FieldName,+Val,...],-Ptr)
 |		equivalent to c_alloc_abs/2 plus c_set/3. Type can be
 |		an integral type or 'str'. SType must be a structure 
 |		type.
 |
 |	c_createn(+Type,+Num,[+Val,...],-UIA)
 |	c_createn(+SType,+Num,[[+FieldName,+Val,...], ...], -UIA)
 |		allocate and initialize an array of data valuesin a UIA.
 |		equivalent to c_allocn/3 plus several c_setn/4.
 |
 |	c_createn_abs(+Type,+Num,[+Val,...],-Ptr)
 |	c_createn_abs(+SType,+Num,[[+FieldName,+Val,...], ...], -Ptr)
 |		same as c_createn/4, except that the data is created
 |		in a C malloced area.
 |
 |	c_create_arglist([+Name,+Val,...],-Ptr)
 |	c_create_arglist([+Name,+Val,...],-Ptr,-PtrList)
 |		create an initialized ARGLIST structure for X Toolkit
 |		and return a pointer to the arg structure in Ptr and
 |		a list of pointers that can be freed using c_free_arglist
 |
 |	c_free_arglist(+PtrList)
 |		does c_free_abs on each item of PtrList.
 |
 |	c_call(CFuncPtr,Arglist,RetVal)
 |		calls the C function whose address is CFuncPtr with
 |		arguments from Arglist that are assumed to be longs
 |		and binds RetVal with the return value of function.
 |
 |	Note :
 |		The following type codes are recognized by $c_set/$c_examine
 |
 |      1  -- int
 |      2  -- unsigned int
 |      3  -- long
 |      4  -- unsigned long
 |      5  -- pointer
 |      6  -- char
 |      7  -- unsigned char
 |      8  -- short
 |      9  -- unsigned short
 |      10 -- string
 |      11 -- string of given length (length is 4th arg)
 |      12 -- float
 |      13 -- double
 |      14 -- far pointer  (DOS only)
 |	15 -- raw data of given length
 *-----------------------------------------------------------------------------*/

module builtins.

export c_const/2.
export c_rconst/2.

export c_alloc/2.
export c_alloc_abs/2.
export c_allocn/3.
export c_allocn_abs/3.

export c_free/1.
export c_free_abs/1.

export c_set/3.
export c_set_str/4.
export c_set_raw/4.
export c_setn/4.

export c_examine/3.
export c_examine_str/4.
export c_examine_raw/4.
export c_examinen/4.

export c_create/3.
export c_create_abs/3.
export c_createn/4.
export c_createn_abs/4.

export c_create_arglist/2.
export c_create_arglist/3.
export c_free_arglist/1.

export c_call/3.

export cstr2uia/2.
export pstr2uia/2.
export cstr2str/2.
export pstr2str/2.
export cstr2pstr/1.
export pstr2cstr/1.

export c_bind_callback/3.
export c_unbind_callback/3.

/*============================================*/


% allocator/3

allocator(uia, Size, UIA) :- '$uia_alloc'(Size, UIA).
allocator(abs, Size, Ptr) :- '$c_malloc'(Size, Ptr).


/************************************************
 *
 * Get the value of a C defined constant
 *
 ************************************************/

c_const(Name, Value) :- '$c_constinfo'(Name, Value).


/************************************************
 *
 * Get the value of a C defined global variable
 *
 ************************************************/

c_rconst(Name, Value) :- '$c_rconstinfo'(Name, Value).




/************************************************
 *
 * Allocate/free an UIA or C data area
 *
 ************************************************/


c_alloc(Type, UIA) :- 
	'$c_typeinfo'(Type, Size, _),
	'$uia_alloc'(Size,UIA).



c_alloc_abs(Type, Ptr) :- 
	'$c_typeinfo'(Type, Size, _), 
	'$c_malloc'(Size, Ptr).

%

c_allocn(Type, N, UIA) :- 
	'$c_typeinfo'(Type, Size, _),
	TotalSize is N * Size,
	'$uia_alloc'(TotalSize, UIA).



c_allocn_abs(Type, N, Ptr) :- 
	'$c_typeinfo'(Type, Size, _),
	TotalSize is N * Size,
	'$c_malloc'(TotalSize, Ptr).


%

c_free( UIA ).


 
c_free_abs(Ptr) :- '$c_free'(Ptr).


/*************************************************
 *
 *  Set the contents of a UIA or C data area
 *
 *************************************************/


c_set(Object, str, Value) :-
	!,
	'$c_set'(Object,[f(0, 10, Value)]).
c_set(Object, Type, Value) :-
	'$c_typeinfo'(Type, _, TypeCode),
	TypeCode \= 0,
	!,
	'$c_set'(Object,[f(0, TypeCode, Value)]).
c_set(Object, Type, Values) :-
	'$c_structinfo'(Type, _, Fields),
	translate(Values, 0, Fields, List),
	'$c_set'(Object, List).

%

c_set_str(Object, Offset, Length, Value) :-
	'$c_set'(Object,[f(Offset,11,Value,Length)]).

%

c_set_raw(Object, Offset, Length, Value) :-
	'$c_set'(Object,[f(Offset,15,Value,Length)]).

%

c_setn(Object, Type, Index, Value) :-
	'$c_typeinfo'(Type, Size, TypeCode),
	TypeCode \= 0,
	!,
	Offset is Index * Size,
	'$c_set'(Object,[f(Offset, TypeCode, Value)]).
c_setn(Object, Type, Index, Values) :-
	'$c_structinfo'(Type, Size, Fields),
	Offset is Index * Size,
	translate(Values, Offset, Fields, List ),
	'$c_set'(Object, List).

/*
 * Examine family of predicates 
 */


c_examine(Object, str, Value) :-
	!,
	'$c_examine'(Object,[f(0,10,Value)]).
c_examine(Object, Type, Value) :-
	'$c_typeinfo'(Type, _, TypeCode),
	TypeCode \= 0,
	!,
	'$c_examine'(Object,[f(0,TypeCode,Value)]).
c_examine(Object, Type, Values) :-
	'$c_structinfo'(Type, _, Fields),
	translate(Values, 0, Fields, List),
	'$c_examine'(Object, List).	

%

c_examine_str(Object, Offset, Length, Value ) :-
	'$c_examine'(Object, [f(Offset,11,Value,Length)]).

%

c_examine_raw(Object, Offset, Length, Value ) :-
	'$c_examine'(Object, [f(Offset,15,Value,Length)]).

%

c_examinen(Object, Type, Index, Value) :-
	'$c_typeinfo'(Type, Size, TypeCode),
	TypeCode \= 0,
	!,
	Offset is Index * Size,
	'$c_examine'(Object,[f(Offset, TypeCode, Value)]).
c_examinen(Object, Type, Index, Values) :-
	'$c_structinfo'(Type, Size, Fields),
	Offset is Index * Size,
	translate(Values, Offset, Fields, List ),
	'$c_examine'(Object, List).

/*************************************************************
 *
 * 	Convenience "create" functions equivalent to c_alloc
 * 	and one or more c_set
 *
 *************************************************************/


c_create(Type, Value, UIA) :- 
	w_create(uia, Type, Value, UIA).



c_create_abs(Type, Value, Ptr) :- 
	w_create(abs, Type, Value, Ptr).


w_create(Tag, str, Value, Object) :-
	!,
	name(Value,Str),
	length(Str,Size),
	NSize is Size + 1,
	allocator(Tag,NSize,Object),
	'$c_set'(Object,[f(0,10,Value)]).	
w_create(Tag,Type, Value, Object ) :- 
	'$c_typeinfo'(Type, Size, TypeCode),
	TypeCode \= 0,
	!,
	allocator(Tag, Size, Object),
	'$c_set'(Object, [f(0,TypeCode,Value)]).
w_create(Tag, Type, Values, Object) :-
	'$c_structinfo'(Type, Size, Fields),
	allocator(Tag, Size, Object),
	translate(Values, 0, Fields, List),
	'$c_set'(Object, List).

%

c_createn(Type, Count, Values, UIA) :- 
	w_createn(uia, Type, Count, Values, UIA).



c_createn_abs(Type, Count, Values, Ptr) :-
	w_createn(abs, Type, Count, Values, Ptr).


w_createn(Tag, Type, Count, Values, Object) :-
	'$c_typeinfo'(Type, Size, TypeCode),
	TotSize is Size * Count,
	allocator(Tag, TotSize, Object),
	makelist(TypeCode, Size, Values, Count, 0, Type, List),
	'$c_set'(Object, List).


makelist(TypeCode, Size, InList, Count, Offset, InType, OutList) :-
	TypeCode \= 0,
	!,
	makebaselist(InList, Count, Offset, TypeCode, Size, OutList).
makelist(_, _, InList, Count, Offset, InType, OutList) :-
	'$c_structinfo'(InType, Size, Fields),
	makestructlist(InList, Count, Offset, Fields, Size, OutList).



% translate/4

translate([], _, _, []).
translate([Name, Value| Rest], Shift, Fields, List ) :-
	pick(Name, Fields, Offset, TypeCode),
	!,
	Disp is Offset + Shift,
	dispatch_type(TypeCode, Disp, Value, List, RestList),
	translate(Rest, Shift, Fields, RestList).
translate([Name, []| Rest], Shift, Fields, List ) :-
	!,
	translate(Rest, Shift, Fields, List).
translate([Name, [SubName,SubValue|Rest1]| Rest], Shift, Fields, List ) :-
	catenate([Name,'.',SubName],FieldName),
	translate([FieldName,SubValue,Name,Rest1|Rest], Shift, Fields, List).
	

% pick/4

pick(Item, [f(Item,Offset,TypeCode)|_], Offset, TypeCode) :- !.
pick(Item, [f(Item,Offset,TypeCode,Count)|_], 
	Offset, array(TypeCode,Count)) :- !.
pick(Item, [_|Rest], Offset, TypeCode) :-
	pick(Item, Rest, Offset, TypeCode).


dispatch_type(TypeCode, Disp, Value, [f(Disp,TypeCode,Value)|X], X) :-
	integer(TypeCode),
	!.
dispatch_type(array(TypeCode,Count), Disp, Value, List, RestList) :- !,
	translate_array(TypeCode, Disp, Value, Count, List, RestList).	
dispatch_type(Type, Disp, Value, List, RestList) :-
	'$c_structinfo'(Type, _, Fields),
	translate(Value, Disp, Fields, SubList),
	append(SubList,RestList,List).


% TypeCode is integer => no more derefencing required

translate_array(TypeCode, Disp, Values, Count, List, RestList) :-
	integer(TypeCode),
	!,
	makebaselist(Values, Count, Disp, TypeCode, Size, BaseList),
	append(BaseList, RestList, List).
translate_array(Type, Disp, Values, Count, List, RestList) :-
	'$c_structinfo'(Type, Size, Fields),
	makestructlist(Values, Count, Disp, Fields, Size, StructList),
	append(StructList, RestList, List).


%

makebaselist([], _, _,  _, _, []) :- !.
makebaselist(_, 0, _, _, _, []) :- !.
makebaselist([X|Xs], Count, Offset, TypeCode, Size, List) :-
	List = [f(Offset,TypeCode,X)| Rest],
	NOffset is Offset + Size,
	NCount is Count - 1,
	makebaselist(Xs, NCount, NOffset, TypeCode, Size, Rest).


makestructlist([], _, _,  _, _, []) :- !.
makestructlist(_, 0, _, _, _, []) :- !.
makestructlist([X|Xs], Count, Offset, Fields, Size, List) :-
	translate(X,Offset,Fields,Flist),
	append(Flist,Rest,List),
	NOffset is Offset + Size,
	NCount is Count - 1,
	makestructlist(Xs, NCount, NOffset, Fields, Size, Rest).

%

c_create_arglist(Values,Ptr) :- c_create_arglist(Values,Ptr,_).

c_create_arglist([],0,[]) :-!.
c_create_arglist(Values,Ptr,[Ptr|PtrList]) :-
	'$c_typeinfo'('Arg',ArgSize,_),
	length(Values,Num),
	NumArgs is Num // 2,
	TotSize is ArgSize * NumArgs,
	'$c_malloc'(TotSize,Ptr),
	setarg_loop(Values,Ptr,ArgSize,PtrList).

setarg_loop([],_,_,[]).
setarg_loop([Name,Value|Rest],Ptr,ArgSize,PtrList) :-
	c_create_abs(str,Name,AbsName),
	( atom(Value) -> 
		c_create_abs(str,Value,Value0),
		PtrList = [AbsName,Value0|RestList]
	; 
		Value0 = Value,
		PtrList = [AbsName|RestList]
	),
	x_XtSetArg(Ptr,AbsName,Value0),
	NPtr is Ptr + ArgSize,
	setarg_loop(Rest, NPtr, ArgSize, RestList).

%

c_free_arglist([]).
c_free_arglist([Ptr|Rest]) :- c_free_abs(Ptr), c_free_arglist(Rest).


%
% c_call(CFuncPtr,ArgList,RetVal)
%


c_call(F,A,R) :- '$c_call'(F,A,R).


%
%	String Handling (For Pascal AND CStrings)
%


% Convert a C String into a UIA
cstr2uia(CString,UIA) :-
	'$c_makeuia'(0,CString,UIA).

% Convert a Pascal String into a UIA
pstr2uia(PString,UIA) :-
	'$c_makeuia'(1,PString,UIA).

% Convert a C String into a Prolog String
cstr2str(CString,String) :-
	'$c_makeuia'(0,CString,UIA),
	name(UIA,String).

% Convert a Pascal String into a Prolog String
pstr2str(PString,String) :-
	'$c_makeuia'(1,PString,UIA),
	name(UIA,String).

% Convert (in place) from C String to Pascal String.
cstr2pstr(CString) :-
	'$c_convertcstrpstr'(0,CString).

% Convert (in place) from Pascal String to C String.
pstr2cstr(PString) :-
	'$c_convertcstrpstr'(1,PString).
	
%
%	Callback Facility
%

c_bind_callback(Func, Object, Term) :- '$c_bind_callback'(Func, Object, Term).
c_unbind_callback(Func, Object, Term) :- '$c_unbind_callback'(Func, Object, Term).

export read_eval_results/2.

read_eval_results(Atom, Results) :-
	atom(Atom),
	open(atom(Atom), read, Stream, []),
	read_term(Stream, Term, [variables(Results)]),
	close(Stream),
	call(Term).

endmod.
