/*======================================================================
 |                      rows_cols.pro
 |      Copyright (c) 2019 Applied Logic Systems, Inc.
 |              Group: Formatting
 |              DocTitle: columns/1
 |              -- Output columnar rectangular arrays of terms
 |
 |	All examples make use of:
 |	silly_list( [
 |          [abc, rjtir, x, y],
 |          [yrtbv, h8t, x, yacc],
 |          [34, f(4,5,6), some_silly(55,yurht, 123), thru]
 |        ] ).
 *=====================================================================*/

module builtins.

export columns/1.
export columns/2.
export columns/3.

/*!---------------------------------------------------------------------
 |      columns/1
 |      columns(ListOfRows)
 |      columns(+)
 |
 |      - Output ListOfRows as a minimal columnar array of terms
 |
 |      If ListOfRows is a list of lists of terms (Rows), and
 |	if all of the Rows are of the same length, outputs a 
 |      rectangular array of the terms appearing in the
 |	Rows to user_output. The array is split into columns of minimal 
 |	size to contain the appropriate terms, with one character  wide 
 |	blank separators
 |
 | Examples:
 |
 |	?- listing(silly_list).
 |	
 |	% user:silly_list/1
 |	silly_list(
 |	[[abc,rjtir,x,y],
 |	 [yrtbv,h8t,x,yacc],
 |	 [34,f(4,5,6),some_silly(55,yurht,123),thru]]).
 |	
 |	?- listing(t1).
 |	% user:t1/0
 |	t1 :- silly_list(ListOfRows), columns(ListOfRows).
 |	
 |	?- t1.
 |	abc    rjtir     x                         y     
 |	yrtbv  h8t       x                         yacc  
 |	34     f(4,5,6)  some_silly(55,yurht,123)  thru  
 |
 |	?- listing(t1h2).
 |	% user:t1h2/0
 |	t1h2 :- silly_list(ListOfRows), ListOfRows = [Header|Rest], 
 |		columns([u(Header,0'_)|Rest]).
 |	
 |	?- t1h2.
 |	abc    rjtir     x                         y     
 |	___    _____     _                         _     
 |	yrtbv  h8t       x                         yacc  
 |	34     f(4,5,6)  some_silly(55,yurht,123)  thru  	
 *!--------------------------------------------------------------------*/
columns(ListOfRows)
	:-
	    % NumCols is a var, used to check all rows are the same length
	mk_rows_atoms(ListOfRows, NumCols, ListOfListsOfAtoms, 
			ListOfItemsLens, ListOfMinColsWidthsLists),
	final_cols_widths(ListOfMinColsWidthsLists, ColsWidths),
	setup_rows_pads(ListOfItemsLens, ColsWidths, RowsPads),
	S = user_output,
	output_fixed_rows(ListOfListsOfAtoms, ColsWidths, RowsPads, S).

/*!---------------------------------------------------------------------
 |      columns/2
 |      columns(ListOfRows, OutputStream)
 |      columns(+, +)
 |
 |      - Like columns/1, outputting to a given stream
 |
 |      If ListOfRows is a list of lists of terms (Rows), and
 |	if all of the Rows are of the same length, outputs a 
 |      rectangular array of the terms appearing in the
 |	Rows to OutputStream. The array is split into columns of minimal 
 |	size to contain the appropriate terms, with one character  wide 
 |	blank separators.
 |	Note: if arg(2) is passed a list of integers(ColWidths), then
 |      	columns(ListOfRows, ColsWidths user_output)
 |	is invoked.
 *!--------------------------------------------------------------------*/
columns(ListOfRows, ColsWidths)
	:-
	ColsWidths = [_|_],
	!,
       	columns(ListOfRows, ColsWidths, user_output).

columns(ListOfRows, OutputStream)
	:-
	    % NumCols is a var, used to check all rows are the same length
	mk_rows_atoms(ListOfRows, NumCols, ListOfListsOfAtoms, 
			ListOfItemsLens, ListOfMinColsWidthsLists),
	final_cols_widths(ListOfMinColsWidthsLists, ColsWidths),
	setup_rows_pads(ListOfItemsLens, ColsWidths, RowsPads),
	!,
	output_fixed_rows(ListOfListsOfAtoms, ColsWidths, RowsPads, OutputStream).

/*!---------------------------------------------------------------------
 |      columns/3
 |      columns(ListOfRows, ColWidths, OutputStream)
 |      columns(+, +, +) 
 |
 |      - Output ListOfRows as a columnized rectangular array of terms
 |
 |      If ListOfRows is a list of lists of terms (Rows), and
 |	if all of the Rows are of the same length, and if ColWidths
 |	is a list of positive integers of the same length as the 
 | 	common length as the individual lists in ListOfRows, outputs a 
 |      rectangular array of the terms appearing in the Rows to 
 |	OutputStream. The array is split into columns of the size of
 |	the corresponding integer of ColWidths, with one character-wide 
 |	blank separators.  Terms in Rows which are of size > the 
 |	corresponding integer of ColWidths are trucated.
 |
 | Examples:
 |
 |	?- listing(t2).
 |	% user:t2/0
 |	t2 :- silly_list(ListOfRows), columns(ListOfRows,[5,5,5,5]).
 |	
 |	?- t2.
 |	abc   rjtir x     y     
 |	yrtbv h8t   x     yacc  
 |	34    f(4,5 some_ thru  
 |	
 |	?- listing(t2h).
 |	% user:t2h/0
 |	t2h :-  silly_list(ListOfRows),
 |	        columns([u([apple,orange,banana,kiwi],0'=)|ListOfRows],[5,5,5,5]).
 |	
 |	?- t2h.
 |	apple orang banan kiwi  
 |	===== ===== ===== ===== 
 |	abc   rjtir x     y     
 |	yrtbv h8t   x     yacc  
 |	34    f(4,5 some_ thru  
 *!--------------------------------------------------------------------*/
columns(ListOfRows, ColWidths, OutputStream)
	:-
	    % NumCols is a var, used to check all rows are the same length
	fixed_mk_rows_atoms(ListOfRows, ColWidths, NumCols, RowsAtoms, RowsPads),
	!,
	output_fixed_rows(RowsAtoms, ColWidths, RowsPads, OutputStream).

	/* ========= Variable column size support ========= */

mk_rows_atoms([], NumCols, [], [], []).
mk_rows_atoms([u(Row,Code) | RowsTail], NumCols, [RowAtoms, UAtoms | RowsAtomsTail], 
	 	[RowItemsLen, RowItemsLen | RowsItemLensTail], 
		[RowMinColWidths, RowItemsLen | RowsMinColsWidthsTail])
	:-!,
	mk_rows_atoms([Row | RowsTail], NumCols, [RowAtoms | RowsAtomsTail], 
	 		[RowItemsLen | RowsItemLensTail], [RowMinColWidths | RowsMinColsWidthsTail]),
	mk_u_atoms(RowItemsLen, Code, UAtoms).

mk_rows_atoms([Row | RowsTail], NumCols, [RowAtoms | RowsAtomsTail], 
	 [RowItemsLen | RowsItemLensTail], [RowMinColWidths | RowsMinColsWidthsTail])
	:-
	atoms_for_row(Row, 0, RowNumCols, RowAtoms, RowItemsLen, RowMinColWidths),
	(RowNumCols = NumCols ->
		mk_rows_atoms(RowsTail, NumCols, RowsAtomsTail, 
				RowsItemLensTail, RowsMinColsWidthsTail)
		;
		throw(rows_len_error(Row,NumCols, RowNumCols))
	).

atoms_for_row([], RowNumCols, RowNumCols, [], [], []).
atoms_for_row([Item | RowTail], CurNumCols, RowNumCols, [ItemAtom | RowAtomsTail], 
		[ItemLen | RowItemsLen], [MinColWidth | RowMinColWidths])
	:-
	atom_for_cols(Item, ItemAtom, ItemLen, MinColWidth),
	NextNumCols is CurNumCols + 1,
	atoms_for_row(RowTail, NextNumCols, RowNumCols, RowAtomsTail, RowItemsLen, RowMinColWidths).

atom_for_cols(Item, Item, ItemLen, MinColWidth)
	:-
	atom(Item),
	!,
	'$strlen'(Item, ItemLen),
	MinColWidth is ItemLen + 1.

atom_for_cols(Item, ItemAtom, ItemLen, MinColWidth)
	:-
	number(Item),
	!,
	number_codes(Item, ItemCodes),
	atom_codes(ItemAtom, ItemCodes),
	'$strlen'(ItemAtom, ItemLen),
	MinColWidth is ItemLen + 1.

	%% general term:
atom_for_cols(Item, ItemAtom, ItemLen, MinColWidth)
	:-
	term_codes(Item, ItemCodes),
	atom_codes(ItemAtom, ItemCodes),
	'$strlen'(ItemAtom, ItemLen),
	MinColWidth is ItemLen + 1.

final_cols_widths(ListOfMinColsWidthsLists, ColsWidths)
	:-
	ListOfMinColsWidthsLists = [RowMinCols | TailMinColsWidthsLists],
	fcw(TailMinColsWidthsLists, RowMinCols, ColsWidths).

fcw([], ColsWidths, ColsWidths).
fcw([RowMinColsWidths | TailListOfMinColsWidthsLists], CumRowMinCols, ColsWidths)
	:-
	max_vector(RowMinColsWidths, CumRowMinCols, InterColsWidths),
	fcw(TailListOfMinColsWidthsLists, InterColsWidths, ColsWidths).

%%%%%%
	%% Being added to library/arithx1
max_vector([], [], []).
max_vector([Left | LeftTail], [Right | RightTail], [Max | ResultTail])
	:-
	max(Left, Right, Max),
	max_vector(LeftTail, RightTail, ResultTail).


atoms_for_row([], RowNumCols, RowNumCols, [], [], []).
atoms_for_row([Item | RowTail], CurNumCols, RowNumCols, [ItemAtom | RowAtomsTail], 
		[ItemLen | RowItemsLen], [MinColWidth | RowMinColWidths])
	:-
	atom_for_cols(Item, ItemAtom, ItemLen, MinColWidth),
	NextNumCols is CurNumCols + 1,
	atoms_for_row(RowTail, NextNumCols, RowNumCols, RowAtomsTail, RowItemsLen, RowMinColWidths).

mk_u_atoms([], _, []).
mk_u_atoms([ItemLen | RowItemsLen], Code, [UAtom | UAtoms])
	:-
	u_atom(ItemLen, Code, UAtom),
	mk_u_atoms(RowItemsLen, Code, UAtoms).

u_atom(ItemLen, Code, UAtom)
	:-
	n_of(ItemLen, Code, UAtomCodes),
	atom_codes(UAtom, UAtomCodes).



setup_rows_pads([], _, []).
setup_rows_pads([RowItemsLens  | ListOfItemsLensTail], ColsWidths, [RowPads | RowsPadsTail])
	:-
	pads_for_row(RowItemsLens, ColsWidths, RowPads),
	setup_rows_pads(ListOfItemsLensTail, ColsWidths, RowsPadsTail).

pads_for_row([], [], []).
pads_for_row([ItemLen | RowItemsLensTail], [ColWidth | ColsWidthsTail], [ItemPad | RowPadsTail])
	:-
	(ItemLen > ColWidth ->
		ItemPad = 0
		;
		ItemPad is  ColWidth - ItemLen
	),
	pads_for_row(RowItemsLensTail, ColsWidthsTail, RowPadsTail).


%put_code(S, 0'|).


	/* ========= Fixed column size support ========= */

fixed_mk_rows_atoms([], _, NumCols, [], []).

fixed_mk_rows_atoms([u(Row,Code) | RowsTail], ColWidths, NumCols, 
			[RowAtoms, UAtoms | RowsAtomsTail], [RowPads, UPads | RowsPadsTail])
	:-!,
	fixed_mk_rows_atoms([Row | RowsTail], ColWidths, NumCols, 
				[RowAtoms | RowsAtomsTail], [RowPads | RowsPadsTail]),
	fixed_mk_u_atoms(ColWidths, Code, UAtoms, UPads).

fixed_mk_rows_atoms([Row | RowsTail], ColWidths, NumCols, 
			[RowAtoms | RowsAtomsTail], [RowPads | RowsPadsTail])
	:-
	fixed_atoms_for_row(Row, ColWidths, 0, RowNumCols, RowAtoms, RowPads),
	(RowNumCols = NumCols ->
		fixed_mk_rows_atoms(RowsTail, ColWidths, NumCols, RowsAtomsTail, RowsPadsTail)
		;
		throw(rows_len_error(Row,NumCols, RowNumCols))
	).

fixed_atoms_for_row([], [], NumCols, NumCols, [], []).

fixed_atoms_for_row([Item | RowTail], [ColWidth | ColWidthsTail], CurNumCols, RowNumCols, 
			[ItemAtom | RowAtomsTail], [ItemPad | ItemsPadsTail])
	:-
	fixed_atom_for_cols(Item, ItemAtom, ColWidth, ItemPad),
	NextNumCols is CurNumCols + 1,
	fixed_atoms_for_row(RowTail, ColWidthsTail, NextNumCols, RowNumCols, 
				RowAtomsTail, ItemsPadsTail).

fixed_atom_for_cols(Item, ItemAtom, ColWidth, ItemPad)
	:-
	atom(Item),
	!,
	atom_codes(Item, ItemCodes),
	complete_item_atom_pad(ItemCodes, ColWidth, ItemAtom, ItemPad).

fixed_atom_for_cols(Item, ItemAtom, ColWidth, ItemPad)
	:-
	number(Item),
	!,
	number_codes(Item, ItemCodes),
	complete_item_atom_pad(ItemCodes, ColWidth, ItemAtom, ItemPad).


	%% general term:
fixed_atom_for_cols(Item, ItemAtom, ColWidth, ItemPad)
	:-
	term_codes(Item, ItemCodes),
	complete_item_atom_pad(ItemCodes, ColWidth, ItemAtom, ItemPad).


complete_item_atom_pad(ItemCodes, ColWidth, ItemAtom, ItemPad)
	:-
	at_most_n(ItemCodes, ColWidth, TruncItemCs),
	atom_codes(ItemAtom, TruncItemCs),
	length(TruncItemCs, TruncItemLen),
	(TruncItemLen < ColWidth ->
		ItemPad is ColWidth - TruncItemLen
		;
		ItemPad is 0
	).
	
fixed_mk_u_atoms([], _, []).
fixed_mk_u_atoms([ItemLen | RowItemsLen], Code, [UAtom | UAtoms])
	:-
	min(ItemLen, Right, Max),
	u_atom(ItemLen, Code, UAtom),
	fixed_mk_u_atoms(ItemLen, Code, UAtoms).

fixed_mk_u_atoms([], _, [], []).
fixed_mk_u_atoms([ColWidth | ColWidths], Code, [UAtom | UAtoms], [UPad | UPads])
	:-
	u_atom(ColWidth, Code, UAtom),
	UPad is 0,
	fixed_mk_u_atoms(ColWidths, Code, UAtoms, UPads).


fixed_mk_u_atoms([], _, []).
fixed_mk_u_atoms([ItemLen | RowItemsLen], Code, [UAtom | UAtoms])
	:-
	min(ItemLen, Right, Max),
	u_atom(ItemLen, Code, UAtom),
	fixed_mk_u_atoms(ItemLen, Code, UAtoms).


	/* ========= Write out rows ========= */


output_fixed_rows([], _, [], _).
output_fixed_rows([RowAtoms | RowsAtomsTail], ColsWidths, [RowPads | RowsPadsTail], S)
	:-
	output_fixed_cols(RowAtoms, RowPads, ColsWidths, 1, S), 
	nl(S),
	!,
	output_fixed_rows(RowsAtomsTail, ColsWidths, RowsPadsTail, S).

output_fixed_cols([], _, _, _, _).
output_fixed_cols([ItemAtom | ItemAtomsTail], [ItemPad | RowPadsTail], [ColWidth | ColsWidths], CurPos, S)
	:-
	col_fixed_output(ItemAtom, ItemPad, ColWidth, CurPos, NextPos, S),
	output_fixed_cols(ItemAtomsTail, RowPadsTail, ColsWidths, NextPos, S).

col_fixed_output(ItemAtom, ItemPad, ColWidth, CurPos, NextPos, S)
	:-
	NextPos is CurPos + ColWidth + 2,
	write(S, ItemAtom),
	Num is ItemPad + 1,
	putc_n_of(Num, 0' , S).
	
endmod.

/* =================== Samples ===================== */
/* === Not exported. Invoke with builtins:<call> === */

/*
%% Rows data:

best_books( [
['Book',	'Author',	'Original language',	'First published',	'Approximate sales',	'Genre'],
['The Lord of the Rings',	'J. R. R. Tolkien',	'English',	'1954-1955',	'150 million',	'fantasy'],
['The Little Prince',	'Antoine de Saint-Exupery',	'French',	'1943',	'140 million',	'fiction'],
['Harry Potter and the Philosopher\'s Stone',	'J. K. Rowling',	'English',	'1997',	'120 million',	'fantasy'],
['And Then There Were None',	'Agatha Christie',	'English',	'1939',	'100 million',	'mystery'],
['The Hobbit',	'J. R. R. Tolkien',	'English',	'1937',	'100 million',	'fantasy'],
['Dream of the Red Chamber',	'Cao Xueqin',	'Chinese',	'1791',	'100 million',	'family saga']
] ).

silly_list( [
	[abc, rjtir, x, y],
	[yrtbv, h8t, x, yacc],
	[34, f(4,5,6), some_silly(55,yurht, 123), thru]
	] ).

%% Sample routines:

%% Minimal column routines:

bb0 :-
	best_books(BookList),
	columns(BookList).

bb0f :-
	best_books(BookList),
	open('books.txt', write, S),
	columns(BookList, S),
	close(S).

bb0h2 :-
	best_books(BookList),
	BookList = [Header | Rest],
	columns([u(Header,0'=) | Rest]).
bb0h3 :-
	best_books(BookList),
	BookList = [Header | Rest],
	columns([u(Header,0'+) | Rest]).

t1 :- 
	silly_list(ListOfRows),
	columns(ListOfRows).

t1h1 :- 
	silly_list(ListOfRows),
	ListOfRows = [Header | Rest],
	columns([u(Header,0'+) | Rest]).

t1h2 :- 
	silly_list(ListOfRows),
	ListOfRows = [Header | Rest],
	columns([u(Header,0'_) | Rest]).

t1f :-
	silly_list(ListOfRows),
	open('silly.txt', write, S),
	columns(ListOfRows, S),
	close(S).

%% Fixed columns:

bb1f :-
	best_books(BookList),
	open('books_fx.txt', write, S),
	columns(BookList, [20, 20, 20, 10, 10, 10], S),
	close(S).

bb2 :-
	best_books(BookList),
	columns(BookList, [20, 20, 20, 10, 10, 10]).

t2 :- 
	silly_list(ListOfRows),
	write('1234567890123456789012345678901234567890'),nl, 
	columns(ListOfRows, [5,5,5,5]).

t2h :-
	silly_list(ListOfRows),
	write('1234567890123456789012345678901234567890'),nl, 
	columns([u([apple,orange,banana,kiwi],0'=) | ListOfRows], [5,5,5,5]).


t :- t1, t1h1, bb0, bb2, bb0h2, t2, t2h.



test_cols :-
	test_var_columns_file,
	test_var_columns_file_uh,
	test_fxd_columns_file, 
	test_fxd_columns_file_uh,
	remove_file('silly.txt').

test_var_columns_file :-
	silly_list(ListOfRows),
	open('silly.txt', write, S),
	columns(ListOfRows, S),
	close(S),
	grab_lines('silly.txt', SillyColLines),
	T1 = 'abc    rjtir     x                         y     ',
	'$strlen'(T1, T1Len),
	SillyColLines = [L1,L2,L3],
	'$strlen'(L1, L1Len),
	T1Len == L1Len,
	T1 == L1,
	!.
test_var_columns_file :-
	printf(user, 'test_var_columns_file test failed\n', []).


test_var_columns_file_uh :-
	silly_list(ListOfRows),
	ListOfRows = [Header | Rest],
	open('silly.txt', write, S),
	columns([u(Header,0'+) | Rest], S),
	close(S),
	grab_lines('silly.txt', SillyColLines),

	T1 = 'abc    rjtir     x                         y     ',
	'$strlen'(T1, T1Len),
	SillyColLines = [L1,LUH,L2,L3],
	'$strlen'(L1, L1Len),
	T1Len == L1Len,
	T1 == L1,
	TH = '+++    +++++     +                         +     ',
	'$strlen'(TH, THLen),
	'$strlen'(LUH, LUHLen),
	THLen == LUHLen,
	TH == LUH,
	!.

test_var_columns_file_uh :-
	printf(user, 'test_var_columns_file_uh test failed\n', []).



test_fxd_columns_file :-
	silly_list(ListOfRows),
	open('silly.txt', write, S),
	columns(ListOfRows, [5,5,5,5], S),
	close(S),
	grab_lines('silly.txt', SillyColLines),
	T1 = 'abc   rjtir x     y     ',
	'$strlen'(T1, T1Len),
	SillyColLines = [L1,L2,L3],
	'$strlen'(L1, L1Len),
	T1Len == L1Len,
	T1 == L1,
	!.
test_fxd_columns_file :-
	printf(user, 'test_fxd_columns_file test failed\n', []).


test_fxd_columns_file_uh :-
	silly_list(ListOfRows),
	ListOfRows = [Header | Rest],
	open('silly.txt', write, S),
	columns([u(Header,0'+) | Rest], [5,5,5,5], S),
	close(S),
	grab_lines('silly.txt', SillyColLines),

	T1 = 'abc   rjtir x     y     ',
	'$strlen'(T1, T1Len),
	SillyColLines = [L1,LUH,L2,L3],
	'$strlen'(L1, L1Len),
	T1Len == L1Len,
	T1 == L1,
	TH = '+++++ +++++ +++++ +++++ ',
	'$strlen'(TH, THLen),
	'$strlen'(LUH, LUHLen),
	THLen == LUHLen,
	TH == LUH,
	!.

test_fxd_columns_file_uh :-
	printf(user, 'test_fxd_columns_file_uh test failed\n', []).

*/
