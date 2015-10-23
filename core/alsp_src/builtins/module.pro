/*----------------------------------------------------------------*
 |		module.pro   
 |	Copyright (c) 1989-1992 Applied logic Systems, Inc.
 | 
 |			- module related predicates
  +++++ should be moved to library....
 |
 | Author : Ilyas Cicekli
 | Date   : 2/23/1989
 *----------------------------------------------------------------*/


/*
 * conflict1: Procedure P/A is defined in a module M, and it is exported
 *            by another module N which is in the use list of M.
 *
 *    It is assumed that the calling procedure guarantees that: 
 *       a. "Module" and "UseModule" are modules.
 *       b. "UseModule" is in the use list of "Module".
 */
conflict1(Module,UseModule) :-
    all_procedures(Module,Pred,Arity),
    $exported_proc(UseModule,Pred,Arity),
    nl, swrite("Warning: Procedure "), write(Pred/Arity), 
    swrite(" of module "), write(Module),
    swrite(" is also exported by module "), write(UseModule), 
    fail.
conflict1(_,_).


/*
 * check "conflict1" for all modules and all modules in their use list.
 */
modcheck_conflict1(1) :-
   modules(Module,UseList),
   member(UseModule,UseList),
   conflict1(Module,UseModule),
   fail.

/*
 * check "conflict1" for all modules, and certain given modules.
 */
modcheck_conflict1(2) :-
   modules(Module,UseList),
   member(UseModule,UseList),
   conflict1_usemodule(UseModule),
   conflict1(Module,UseModule),
   fail.

/*
 * check "conflict1" for certain modules and all modules in their use list.
 */
modcheck_conflict1(3) :-
   modules(Module,UseList),
   conflict1_module(Module),
   member(UseModule,UseList),
   conflict1(Module,UseModule),
   fail.

/*
 * check "conflict1" for certain modules and certain modules in their use list.
 */
modcheck_conflict1(4) :-
   modules(Module,UseList),
   member(UseModule,UseList),
   conflict1_module_usemodule(Module,UseModule),
   conflict1(Module,UseModule),
   fail.

modcheck_conflict1(_).



/*
 * conflict2: Procedure P/A is exported by both modules N and K, and
 *            N and K are in the use list of an another module.
 *
 *    It is assumed that the calling procedure guarantees that:
 *       a. "UseModule1" and "UseModule2" are modules, and are different.
 *       b. Both "UseModule1" and "UseModule2" are in the use list of
 *          "Module".
 */

conflict2(Module,UseModule1,UseModule2) :-
    all_procedures(UseModule1,Pred,Arity),
    $exported_proc(UseModule1,Pred,Arity),
    $exported_proc(UseModule2,Pred,Arity),
    nl, swrite("Warning: Procedure "), write(Pred/Arity), 
    swrite(" is exported by both modules "), write(UseModule1), 
    swrite(" and "), write(UseModule2),
    nl, swrite("          and they are in the use list of module "),
    write(Module),
    fail.
conflict2(_,_,_).


/*
 * check "conflict2" for all modules and all pairs of modules in
 * their use list.
 */
modcheck_conflict2(1) :-
   modules(Module,UseList),
   get_pair(UseList,UseModule1,UseModule2),
   conflict2(Module,UseModule1,UseModule2),
   fail.

/*
 * check "conflict2" for given modules and all pairs of modules in
 * their use list.
 */
modcheck_conflict2(2) :-
   modules(Module,UseList),
   conflict2_module(Module),
   get_pair(UseList,UseModule1,UseModule2),
   conflict2(Module,UseModule1,UseModule2),
   fail.

modcheck_conflict2(_).

get_pair([X|L],X,Y) :- member(Y,L).
get_pair([_|L],X,Y) :- get_pair(L,X,Y).



