/*======================================================================
 |                      rows_cols_examples.pro
 |      Copyright (c) 2019 Applied Logic Systems, Inc.
 |
 |	Demo usage of ~library/rows_cols.pro to create ascii tables.
 *=====================================================================*/

%% Input rows data:

best_books( [
   ['Book',	'Author',	'Original language',	'First published',	'Approximate sales',	'Genre'],
   ['The Lord of the Rings',	'J. R. R. Tolkien',	'English',	'1954-1955',	'150 million',	'fantasy'],
   ['The Little Prince',	'Antoine de Saint-Exupery',	'French',	'1943',	'140 million',	'fiction'],
   ['Harry Potter and the Philosopher\'s Stone',	'J. K. Rowling',	'English',	'1997',	'120 million',	'fantasy'],
   ['And Then There Were None',	'Agatha Christie',	'English',	'1939',	'100 million',	'mystery'],
   ['The Hobbit',	'J. R. R. Tolkien',	'English',	'1937',	'100 million',	'fantasy'],
   ['Dream of the Red Chamber',	'Cao Xueqin',	'Chinese',	'1791',	'100 million',	'family saga']
] ).

%% Sample routines:

%% Minimal column routines:

	%% write table to user output:
bb0 :-
	best_books(BookList),
	columns(BookList).

	%% write table to file books.txt:
bb0f :-
	best_books(BookList),
	open('books.txt', write, S),
	columns(BookList, S),
	close(S).

	%% write table to user output with underlined header:
bb0h2 :-
	best_books(BookList),
	BookList = [Header | Rest],
	columns([u(Header,0'=) | Rest]).

	%% write table to user output with underlined(+) header:
bb0h3 :-
	best_books(BookList),
	BookList = [Header | Rest],
	columns([u(Header,0'+) | Rest]).

%% Fixed size columns (with truncation):

	%% write fixed-size columns table to file books_fx.txt:
bb1f :-
	best_books(BookList),
	open('books_fx.txt', write, S),
	columns(BookList, [20, 20, 20, 10, 10, 10], S),
	close(S).

	%% write fixed-size columns table to user output:
bb2 :-
	best_books(BookList),
	columns(BookList, [20, 20, 20, 10, 10, 10]).

	%% write fixed-size columns table to user output, with 
	%% underlined header:
bb3 :-
	best_books(BookList),
	BookList = [Header | Rest],
	columns([u(Header,0'=) | Rest], [20, 20, 20, 10, 10, 10]).

