/*
 * testeoln.pro		-- test predicates that involve text lines.
 *			   As a side effect, interactions between the tokenizer
 *			   and stream buffer sizes is also tested.
 *
 *	Copyright (c) 1995 Applied Logic Systems, Inc.
 *
 * Author: Chuck Houpt
 * Creation:	12/11/95
 */

test :-
/*
	get_line_test,
	read_line_test,
	read_test,
	write_test,

*/
	simple_writeline_test,
	dump_test,
	simple_readline_test,
	univ_readline_test,
	read_test,
	get_line_test
	.

/*  The test files have the extension .btf, which stands for
 *  "Binary Text File".  These files should NOT undergo any cr/lf/crlf
 *  translation when copied or read from a file server.
 */
file_name(cr, 'readcr.btf').
file_name(lf, 'readlf.btf').
file_name(crlf, 'readcrlf.btf').

find_file(Name, Name) :-
	exists_file(Name).

find_file(Name, FullName) :-
	builtins:searchdir(SDir),
	pathPlusFile(SDir, Name, FullName),	
	exists_file(FullName).

find_file(Name, _) :-
	printf('File Not Found: %t\n', [Name]),
	fail.

file_size(FileName, Size) :-
	file_status(FileName, Status),
	dmember(size=Size, Status).

initial_buffer_size(Size) :-
	file_name(crlf, FileName),
	find_file(FileName, FullPathName),
	file_size(FullPathName, S),
	Size is S + 10, !.

test_size(Pred, Type, FileType, Size, Results) :-
	file_name(FileType, Name),
	find_file(Name, FullName),
	open(FullName, read, S, [bufsize(Size), read_eoln_type(Type)]),
	C =.. [Pred, S, NewResults],
	C,
	close(S),
/*
	heap_status(HeapFree),
	printf('File: %t read_eoln_type: %t Buffer Size: %d HFree: %d\n',
	       [FullName, Type, Size, HeapFree]),
	ttyflush,
*/
	check(Type, FullName, Size, Results, NewResults).

check(Type, Name, Size, Results, Results) :- !.
check(Type, Name, Size, Results, NewResults) :-
	printf('Read Test Failed!\n'),
	printf('File: %t read_eoln_type: %t Buffer Size: %d\n',
	       [Name, Type, Size]),
	printf('Expected:\n'),
	writeq(Results), nl,
	printf('Actually Got:\n'),
	writeq(NewResults), nl, fail.

test_all(_, 9, _, _, _) :- !.
test_all(Pred, Size, Type, FileType, Results) :-
	test_size(Pred, Type, FileType, Size, Results),
	NewSize is Size - 1,
	!, test_all(Pred, NewSize, Type, FileType, Results).

doread(S, X) :- read(S, X).
doread(S, end_of_file).

read_list(S, [X | TheRest]) :-
	doread(S, X),
	X \= end_of_file,
	read_list(S, TheRest).
read_list(S, []).

do_get_line(S, X) :- get_line(S, X).
do_get_line(S, end_of_file).

get_line_list(S, [X | TheRest]) :-
	do_get_line(S, X),
	X \= end_of_file,
	get_line_list(S, TheRest).
get_line_list(S, []).

test_config(cr, cr).
test_config(lf, lf).
test_config(crlf, crlf).
test_config(universal, cr).
test_config(universal, lf).
test_config(universal, crlf).


read_test :-
	printf('Testing read, eoln and buffer interactions...\n'),
	initial_buffer_size(Size),
	test_size(read_list, cr, cr, Size, X),
	!,
	writeq(X), nl,
	test_size(read_list, crlf, crlf, Size, X),
	test_size(read_list, lf, lf, Size, X),
	test_size(read_list, universal, crlf, Size, X),
	test_size(read_list, universal, cr, Size, X),
	test_size(read_list, universal, lf, Size, X),
	printf('Read Test: crlf for buf_sizes 10-%d...\n', [Size]),
	test_all(read_list, Size, crlf, crlf, X),
	printf('Read Test: lf for buf_sizes 10-%d...\n', [Size]),
	test_all(read_list, Size, lf, lf, X),
	printf('Read Test: cr for buf_sizes 10-%d...\n', [Size]),
	test_all(read_list, Size, cr, cr, X),
	printf('Read Test: universal and crlf for buf_sizes 10-%d...\n', [Size]),
	test_all(read_list, Size, universal, crlf, X),
	printf('Read Test: universal and cr for buf_sizes 10-%d...\n', [Size]),
	test_all(read_list, Size, universal, cr, X),
	printf('Read Test: universal and lf for buf_sizes 10-%d...\n', [Size]),
	test_all(read_list, Size, universal, lf, X).


%% line I/O test.

get_line_test :-
	printf('Testing get_line, eoln and buffer interactions...\n'),
	initial_buffer_size(Size),
	test_size(get_line_list, cr, cr, Size, X),
	!,
	writeq(X), nl,
	test_size(get_line_list, crlf, crlf, Size, X),
	test_size(get_line_list, lf, lf, Size, X),
	test_size(get_line_list, universal, crlf, Size, X),
	test_size(get_line_list, universal, cr, Size, X),
	test_size(get_line_list, universal, lf, Size, X),
	printf('Get_Line Test: crlf for buf_sizes 10-%d...\n', [Size]),
	test_all(get_line_list, Size, crlf, crlf, X),
	printf('Get_Line Test: lf for buf_sizes 10-%d...\n', [Size]),
	test_all(get_line_list, Size, lf, lf, X),
	printf('Get_Line Test: cr for buf_sizes 10-%d...\n', [Size]),
	test_all(get_line_list, Size, cr, cr, X),
	printf('Get_Line Test: universal and crlf for buf_sizes 10-%d...\n', [Size]),
	test_all(get_line_list, Size, universal, crlf, X),
	printf('Get_Line Test: universal and cr for buf_sizes 10-%d...\n', [Size]),
	test_all(get_line_list, Size, universal, cr, X),
	printf('Get_Line Test: universal and lf for buf_sizes 10-%d...\n', [Size]),
	test_all(get_line_list, Size, universal, lf, X).



simple_writeline_test :-
	printf('\nCreate three files with different line markers...'), ttyflush,
	open('foo.cr', write, FooCR, [write_eoln_type(cr)]),
	open('foo.lf', write, FooLF, [write_eoln_type(lf)]),
	open('foo.crlf', write, FooCRLF, [write_eoln_type(crlf)]),
	write_lines(FooCR),
	write_lines(FooLF),
	write_lines(FooCRLF),
	close(FooCR),
	close(FooLF),
	close(FooCRLF),
	printf('Done\n').

write_lines(Stream) :- 
	write(Stream, 'test(\'This is a line\').'), nl(Stream),
	write(Stream, 'test(\'And another line'').'), nl(Stream),
	write(Stream, 'test(\'The last line of this test\').'), nl(Stream).


dump_test :-
	printf('\nDump the contents of the three files:\n'),
	open('foo.cr', read, FooCR),
	open('foo.lf', read, FooLF),
	open('foo.crlf', read, FooCRLF),
	dump_file(FooCR),
	dump_file(FooLF),
	dump_file(FooCRLF),
	close(FooCR),
	close(FooLF),
	close(FooCRLF).

dump_file(Stream) :-
	read_bytes(Stream, ByteList),
	name(Atom, ByteList),
	writeq(Atom), nl.

read_bytes(Stream, [Byte | Tail]) :-
	get_byte(Stream, Byte),
	Byte =\= -1,
	!,
	read_bytes(Stream, Tail).
read_bytes(Stream, []).

	
simple_readline_test :-
	printf('\nPrint the contents of the three files as text lines using respective eoln type\n'),
	open('foo.cr', read, FooCR, [read_eoln_type(cr)]),
	open('foo.lf', read, FooLF, [read_eoln_type(lf)]),
	open('foo.crlf', read, FooCRLF, [read_eoln_type(crlf)]),
	read_lines(FooCR),
	read_lines(FooLF),
	read_lines(FooCRLF),
	close(FooCR),
	close(FooLF),
	close(FooCRLF).

univ_readline_test :-
	printf('\nPrint the contents of the three files as text lines using universal type\n'),
	open('foo.cr', read, FooCR, [read_eoln_type(universal)]),
	open('foo.lf', read, FooLF, [read_eoln_type(universal)]),
	open('foo.crlf', read, FooCRLF, [read_eoln_type(universal)]),
	read_lines(FooCR),
	read_lines(FooLF),
	read_lines(FooCRLF),
	close(FooCR),
	close(FooLF),
	close(FooCRLF).
	
read_lines(Stream) :-
	get_line(Stream, L1), writeq(L1), nl,
	get_line(Stream, L2), writeq(L2), nl,
	get_line(Stream, L3), writeq(L3), nl, nl.

