/*-------------------------------------------------------------*
                    ALS_Drive:ALS TclTk Dev Sys:als_dev:alsdev:alsdir:builtins:db_srctr.pro
               defStruct Type definitions generated from file:
                    ALS_Drive:ALS TclTk Dev Sys:als_dev:alsdev:alsdir:builtins:db_srctr.typ
               by ALS defStruct Type Generator
               Macros written to file: ALS_Drive:ALS TclTk Dev Sys:als_dev:alsdev:alsdir:builtins:db_srctr.mac
 *-------------------------------------------------------------*/

module utilities.
use debugger.
endmod.

module debugger.
use utilities.


%--- dbstr defStruct ---

export access_dbstr/3.
export set_dbstr/3.
access_dbstr(filename,_A,_B) :- arg(1,_A,_B).
set_dbstr(filename,_A,_B) :- mangle(1,_A,_B).

access_dbstr(fcg_num,_A,_B) :- arg(2,_A,_B).
set_dbstr(fcg_num,_A,_B) :- mangle(2,_A,_B).

access_dbstr(winname,_A,_B) :- arg(3,_A,_B).
set_dbstr(winname,_A,_B) :- mangle(3,_A,_B).

access_dbstr(textwin,_A,_B) :- arg(4,_A,_B).
set_dbstr(textwin,_A,_B) :- mangle(4,_A,_B).

access_dbstr(numlines,_A,_B) :- arg(5,_A,_B).
set_dbstr(numlines,_A,_B) :- mangle(5,_A,_B).

access_dbstr(linesizes,_A,_B) :- arg(6,_A,_B).
set_dbstr(linesizes,_A,_B) :- mangle(6,_A,_B).

access_dbstr(invlineindex,_A,_B) :- arg(7,_A,_B).
set_dbstr(invlineindex,_A,_B) :- mangle(7,_A,_B).

access_dbstr(head_tag,_A,_B) :- arg(8,_A,_B).
set_dbstr(head_tag,_A,_B) :- mangle(8,_A,_B).

access_dbstr(call_tag,_A,_B) :- arg(9,_A,_B).
set_dbstr(call_tag,_A,_B) :- mangle(9,_A,_B).

export make_dbstr/1.
make_dbstr(_A) :- _A'=..'[dbstr,_B,_C,_D,_E,0,[],[],0,0].

export make_dbstr/2.
make_dbstr(_A,_B) :-
        struct_lookup_subst(
            [filename,fcg_num,winname,textwin,numlines,linesizes,
                invlineindex,head_tag,call_tag],
            [_C,_D,_E,_F,0,[],[],0,0],_B,_G),
        _A'=..'[dbstr|_G].

export xmake_dbstr/2.
xmake_dbstr(dbstr(_A,_B,_C,_D,_E,_F,_G,_H,_I),[_A,_B,_C,_D,_E,_F,_G,_H,_I]).

endmod.

module utilities.
typeProperties(dbstr,
    [filename,fcg_num,winname,textwin,numlines,linesizes,invlineindex,
        head_tag,call_tag]).
noteOptionValue(dbstr,_A,_B,_C) :- set_dbstr(_A,_C,_B).
endmod.
