/*---------------------------------------------------------------*
 *	assoc.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Association list programs
 *---------------------------------------------------------------*/

lookup( [ Entry | Tail ],  Key,  Value)
	:-
	key_of(Entry, Key),  value_of(Entry, Value).

lookup( [ Skip | Tail ],  Key,  Value)
	:-
	lookup( Tail,  Key,  Value).


key_of( trait( Key,  Ignore),  Key ).
value_of( trait( Ignore,  Value),  Value ).

trait_list([trait(bud_scales(valvate), bitternut_hickory),
		trait(bud_color(yellow), bitternut_hickory), 
		trait(bud_scales(valvate), pecan_hickory),
		trait(bud_color(brownish), pecan_hickory), 
		trait(bud_scales(imbricate), pignut_hickory),
		trait(terminal_bud_size(short), pignut_hickory), 
		trait(bud_scales(imbricate), mockernut_hickory),
		trait(terminal_bud_size(large), mockernut_hickory),
		trait(outer_scales(persistent), shellbark_hickory),
		trait(twig_color(orange_brown), shellbark_hickory), 
		trait(bud_scales(imbricate), shagbark_hickory),
		trait(terminal_bud_size(large), shagbark_hickory),
		trait(outer_scales(persistent), shagbark_hickory),
		trait(twig_color(reddish_brown), shagbark_hickory)   ]   ).

testt1(Tree) 
	:-  
	trait_list(TL),
	lookup(TL, terminal_bud_size(large),  Tree).
	
testt2(Trait)
	:-
	trait_list(TL), 
	lookup(TL, Trait,  shellbark_hickory).

hickory(Trait,  Tree) 
	:-  
	trait_list(TL),
	lookup(TL, Trait,  Tree).


insert([ OldEntry | OldTail ],  NewEntry, [ NewEntry,  OldEntry | OldTail ])
	:- 
	key_of( NewKey,  NewEntry),
	key_of( OldKey,  OldEntry),
	preceeds(NewKey,  OldKey).
insert([ OldEntry | OldTail ],  NewEntry,  [ OldEntry  |  NewTail ])
	:-
	insert( OldTail,  NewEntry,  NewTail ).
insert([ ], NewEntry, [ NewEntry ]).

/*------------------------------------------*
   variation which avoids repeatedly extracting the new key

insert(OldList,  NewEntry,  NewList)
	:P
	key_of(NewKey, NewEntry),
	insert0(OldList,  NewKey,  NewEntry,  NewList).

insert0([ OldEntry | OldTail ], NewKey, NewEntry,  [ NewEntry,  OldEntry |  OldTail ])
	:P
	key_of( OldKey,  OldEntry),
	preceeds(NewKey,  OldKey).
insert0([ OldEntry | OldTail ],  NewKey, NewEntry,  [ OldEntry  |  NewTail ])
	:P
	insert0(OldTail,  NewKey,  NewEntry,  NewTail).
insert0([ ], NewKey, NewEnrty, [ NewEntry ]).
 *------------------------------------------*/

/*------------------------------------------*
  Using the patterns in definitions of Tkey_ofU and Tvalue_ofU directly 
   in the definition(s) of insert:

insert(OldList,  trait( Key,  Value),  NewList)
	:P
	insert0(OldList,  NewKey,  NewValue,  NewList).

insert0([ trait( OldKey,  OldValue) | OldTail ],  NewKey,  NewValue,  
		 [ trait( NewKey,  NewValue), trait( OldKey,  OldValue) | OldTail ])
	:P
	preceeds(NewKey,  OldKey).
insert0([ OldEntry | OldTail ],  NewKey,  
		NewValue,  [ OldEntry  |  NewTail ])
	:P
	insert0(OldTail,  NewKey,  NewValue,  NewTail).
insert0([ ], NewKey, NewValue, [ trait (NewKey, NewValue) ]).
 *------------------------------------------*/


key_of(suitability( Key,  Ignore),  Key).
value_of(suitability( Ignore, Value), Value ).

tests1(Evaluation)
	:-
	SuitList = [suitability(mutual_fund, low), 
				suitability(silver_contract, high),
				suitability(hog_belly_contract, high),
				suitablility(ibm, unsuitable) ],
	lookup(SuitList, hog_belly_contract,  Evaluation).

