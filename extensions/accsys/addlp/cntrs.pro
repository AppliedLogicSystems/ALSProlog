/*===============================================================
 |		cntrs.pro
 |	Copyright (c) 1993-4 Applied Logic Systems, Inc.
 |
 |		Autogen counter table interface and related code
 *==============================================================*/

module dBaseIII.

:-dynamic(specific_table_access_path/2).

export make_ctr_file/0.
make_ctr_file
    :-
	std_dbd2cpt_data('ctrTbl',dBaseIII,GenData),
    make_table([field(ctrNam,'C30'),field(ctrVal,'N10.0')],
               [], ctrTbl,GenData,dummy_db,Return),
	(Return = 'SUCCESS' ->
		true
		;
		message('Cannot create counter table file (ctrTbl.dbf)!!!!'),
		!,
		fail
	).

endmod.

module user.
use accsys_db3.
use dBaseIII.

export check_ctr_file/0.
check_ctr_file
    :-
    exists_file('ctrTbl.dbf'), !.

check_ctr_file
    :-
	make_ctr_file.

export init_internal_ctr/1.
init_internal_ctr(CtrName)
	:-
	check_ctr_file,
	( ctrTbl(CtrName,_) ->
		true
		;
	assert_ctrTbl(CtrName,0),
	flush_dD_buffers(ctrTbl)
	).

export next_ctr_val/2.
next_ctr_val(CtrNam,NextVal)
	:-
	ctrTbl(CtrNam,CurVal),
	NextVal is CurVal + 1,
	update_ctrTbl(ctrTbl(CtrNam,CurVal),ctrTbl(CtrNam,NextVal)),
	flush_dD_buffers(ctrTbl).

export set_ctr_val/2.
set_ctr_val(CtrNam,NewVal)
	:-
	ctrTbl(CtrNam,CurVal),
	update_ctrTbl(ctrTbl(CtrNam,CurVal),ctrTbl(CtrNam,NewVal)),
	flush_dD_buffers(ctrTbl).

export reset_all_ctrs/0.
reset_all_ctrs
	:-
	do_reset.

do_reset
	:-
	ctrTbl(CtrNam,CurVal),
	CurVal \= 0,
	update_ctrTbl(ctrTbl(CtrNam,CurVal),ctrTbl(CtrNam,0)),
	fail.
do_reset
	:-
	flush_dD_buffers(ctrTbl).

/*---------------------------------------------------------
 |	Written by ALS Concept DBMS Intf Tools
 |	93/8/24		8:42:57
 |	Interface to dBaseIII table ctrTbl
 |		-- Hand modified to reflect db3[0,1] changes
 *---------------------------------------------------------*/
/*---------------------------------------------------------
 |	Table is defined by:
 |
 |	defDatabase(dummy_db,
 |   	[table= [name=ctrTbl,
 |          	 fields= [field=[name=ctrNam,type=char(30)],
 |                 		  field=[name=ctrVal,type=int] ] ] ]).
 *---------------------------------------------------------*/

xformArg_local(1,ctrTbl,_A,_B) :- dUatocf(_A,30,_B).

table_access_path(ctrTbl,Path)
	:-
	specific_table_access_path(ctrTbl,Path),
	!.
table_access_path(ctrTbl,'ctrTbl.dbf').

getInfo(opaque(ctrTbl),Buf,ctrTbl(CTRNAM,CTRVAL)) :-
        !, '$uia_peekb'(Buf,0,_A), _A\=42,
        (   '$uia_peek'(Buf,1,30,_B), pad_start(_B,0,30,_C),
            (   _C<30 -> (_C=0 -> CTRNAM='' ; '$uia_peek'(_B,0,_C,CTRNAM))
            ;   CTRNAM=_B)),
        '$uia_peek'(Buf,31,8,_D), name(_D,_E), bufread(_E,CTRVAL,[]).

ctrTbl(CTRNAM,CTRVAL) :-
        table_access_path(ctrTbl,Path),
        access_db3(Path,ctrTbl,ctrTbl(CTRNAM,CTRVAL),user).

putInfo(opaque(ctrTbl),Buf,ctrTbl(CTRNAM,CTRVAL)) :-
        !, '$uia_alloc'(39,Buf),
        (dUatocf(CTRNAM,30,_A), '$uia_poke'(Buf,1,30,_A,0)), name(CTRVAL,_B),
        string_to_uia(_B,_C), dUatonf(_C,8,0,_D,_E), '$uia_poke'(Buf,31,8,_D,0).

dbAssert(ctrTbl(CTRNAM,CTRVAL),
    (   table_access_path(ctrTbl,Path),
        output_db3(Path,ctrTbl,ctrTbl(CTRNAM,CTRVAL),_D,user))).

dbType(ctrTbl(CTRNAM,CTRVAL)) :- !, atom(CTRNAM), number(CTRVAL).

dbRetract(ctrTbl(CTRNAM,CTRVAL),
    (   table_access_path(ctrTbl,Path),
        delete_db3(Path,ctrTbl,ctrTbl(CTRNAM,CTRVAL),user))).

assert_ctrTbl(CTRNAM,CTRVAL) :-
        dbType(ctrTbl(CTRNAM,CTRVAL)), !, 
		table_access_path(ctrTbl,Path),
        output_db3(Path,ctrTbl,ctrTbl(CTRNAM,CTRVAL),_D,user).

retract_ctrTbl(CTRNAM,CTRVAL) :-
        dbType(ctrTbl(CTRNAM,CTRVAL)), !, 
		table_access_path(ctrTbl,Path),
        delete_db3(Path,ctrTbl,ctrTbl(CTRNAM,CTRVAL),user).

det_retract_ctrTbl(CTRNAM,CTRVAL) :-
        dbType(ctrTbl(CTRNAM,CTRVAL)), !, table_access_path(ctrTbl,Path),
        det_delete_db3(Path,ctrTbl,ctrTbl(CTRNAM,CTRVAL),user).

update_ctrTbl(ctrTbl(CTRNAM,CTRVAL),ctrTbl(_A,_B)) :-
        dbType(ctrTbl(CTRNAM,CTRVAL)), dbType(ctrTbl(_A,_B)), !,
        table_access_path(ctrTbl,Path),
        update_db3(Path,ctrTbl,ctrTbl(CTRNAM,CTRVAL),ctrTbl(_A,_B),user).

endmod.  % user


