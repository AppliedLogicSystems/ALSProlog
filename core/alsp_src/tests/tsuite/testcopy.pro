/*
 * testcopy.pro		-- test original and new versions of I/O
 *
 */

old_copy(From,To) :-
	pbi_see(From),
	pbi_tell(To),
	pbi_get0(C),
	old_copy0(C),
	pbi_told,
	pbi_seen.

old_copy0(-1) :- !.
old_copy0(C) :- pbi_put(C), pbi_get0(NC), old_copy0(NC).


new_copy(From,To) :-
	open(From,read,FromStream),
	open(To,write,ToStream),
	get_byte(FromStream,C),
	new_copy0(C,FromStream,ToStream),
	close(FromStream),
	close(ToStream).

new_copy0(-1,_,_) :- !.
new_copy0(C,FromStream,ToStream) :-
	put_byte(ToStream,C),
	get_byte(FromStream,NC),
	new_copy0(NC,FromStream,ToStream).


find_file(Name, Name) :-
	exists_file(Name).

find_file(Name, FullName) :-
	builtins:searchdir(SDir),
	pathPlusFile(SDir, Name, FullName),	
	exists_file(FullName).

find_file(Name, _) :-
	printf('File Not Found: %t\n', [Name]),
	fail.

testnew :-
	X is cputime,
	find_file('testmath.pro', FullName),
	new_copy(FullName, foonew),
	Y is cputime-X,
	write('Elapsed Time'=Y),nl.

testold :-
	X is cputime,
	old_copy('/etc/termcap',fooold),
	Y is cputime-X,
	write('Elapsed Time'=Y),nl.
