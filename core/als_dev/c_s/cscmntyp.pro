/*-------------------------------------------------------------*
                    cscmntyp.pro
               defStruct Type definitions generated from file:
                    cscmntyp.typ
               by ALS defStruct Type Generator
               Macros written to file: cscmntyp.mac
 *-------------------------------------------------------------*/

module utilities.
use socket_comms.
endmod.

module socket_comms.
use utilities.


%--- login_info defStruct ---

export access_login_info/3.
export set_login_info/3.
access_login_info(user_name,_A,_B) :- arg(1,_A,_B).
set_login_info(user_name,_A,_B) :- mangle(1,_A,_B).

access_login_info(password,_A,_B) :- arg(2,_A,_B).
set_login_info(password,_A,_B) :- mangle(2,_A,_B).

export make_login_info/1.
make_login_info(_A) :- functor(_A,login_info,2).

export make_login_info/2.
make_login_info(_A,_B) :-
        struct_lookup_subst([user_name,password],[_C,_D],_B,_E),
        _A'=..'[login_info|_E].

export xmake_login_info/2.
xmake_login_info(login_info(_A,_B),[_A,_B]).

endmod.

module utilities.
typeProperties(login_info,[user_name,password]).
noteOptionValue(login_info,_A,_B,_C) :- set_login_info(_A,_C,_B).
endmod.
