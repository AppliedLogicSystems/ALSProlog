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

testnew :-
	als_system(SysVars),
	dmember(os=OS,SysVars),
	X is cputime,
	(OS = unix ->
		new_copy('/etc/termcap',foonew) ;
		new_copy('mailbox3:alsp_src:tests:tsuite:testmath.pro',foonew)),
	Y is cputime-X,
	write('Elapsed Time'=Y),nl.

testold :-
	X is cputime,
	old_copy('/etc/termcap',fooold),
	Y is cputime-X,
	write('Elapsed Time'=Y),nl.
