/*---------------------------------------------------------------*
 *	trees.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Basic tree manipulation predicates
 *---------------------------------------------------------------*/

%% Access predicates for the investment suitability example:

key_of(suitability(Key, _), Key).
value_of(suitability(_, Value), Value).

make_entry(Key, Value, suitability(Key, Value)).

preceeds(Item1,  Item2) 
	:- 
	Item1 @< Item2.

locate(tr(Left, Entry, Right), Key, Value)
	:-
	key_of(Entry, Key),
	value_of(Entry, Value).

locate(tr(Left, Entry, Right), Key, Value)
	:-
	key_of(Entry, EntryKey),
	preceeds(Key, EntryKey),
	locate(Left, Key, Value).

locate(tr(Left, Entry, Right), Key, Value)
	:-
	key_of(Entry, EntryKey),
	preceeds(EntryKey, Key),
	locate(Right, Key, Value).


insert(nil, Key, Value, tr(nil, NewEntry, nil))
	:-
	make_entry(Key, Value, NewEntry).
insert(tr(OldLeft, Entry, Right), Key, Value, tr(NewLeft, Entry, Right))
	:-
	key_of(Entry, EntryKey),
	preceeds(Key, EntryKey),
	insert(OldLeft, Key, Value, NewLeft).
insert(tr(Left, Entry, OldRight), Key, Value, tr(Left, Entry, NewRight))
	:-
	key_of(Entry, EntryKey),
	preceeds(EntryKey, Key),
	insert(OldRight, Key, Value, NewRight).

suitability_list([
suitability(loans(bankers_acceptance),low),
		suitability(savings_account(certificate_of_deposit),high),
		suitability(loans(commercial),low),
		suitability(savings_account(commercial_bank),medium),
		suitability(stock(common),low),
		suitability(bonds(corporate),high),
		suitability(retirement_plan(corporate),high),
		suitability(savings_account(credit_union),medium),
		suitability(futures(gold),low),
		suitability(retirement_plan(ira),high),
		suitability(retirement_plan(keogh),high),
		suitability(mutual_fund(load),high),
		suitability(bonds(municipal),high),
		suitability(mutual_fund(no_load),high),
		suitability(limited_partnership(oil_and_gas),low),
		suitability(stock(preferred),medium),
		suitability(limited_partnership(real_estate),low),
		suitability(savings_account(savings_and_loan),low),
		suitability(retirement_plan(section_401k),high),
		suitability(futures(silver),low),
		suitability(life_contract(single_payment),high),
		suitability(annuity(single_premium),high),
		suitability(bonds(state),high),
		suitability(life_insurance(term),medium),
		suitability(loans(treasury_bills),high),
		suitability(bonds(us_government),high),
		suitability(life_insurance(whole_life),high)    ]).


test(ResultTree)
	:-
	suitability_list(Items),
	do_inserts(Items, nil, ResultTree).

do_inserts([], Tree, Tree).
do_inserts([Item | RestItems], CurrentTree, FinalTree)
	:-
	key_of(Item, Key),
	value_of(Item, Value),
	insert(CurrentTree, Key, Value, NextTree),
	do_inserts(RestItems, NextTree, FinalTree).

testTreePrint :-
	test(Tree),
	show(Tree,0).

show(nil,_).

show(n(Left, Entry, Right), Indent) 
	:-
	NewIndent is Indent + 4,	% calculate subtree indents
	show(Right, NewIndent),

	key_of(Entry, Key), value_of(Entry, Value),
	tab(Indent), write(Key),put(~-),write(Value),nl,	% ~- = ASCII code

	show(Left, NewIndent).



