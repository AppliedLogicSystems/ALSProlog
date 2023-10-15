:-['bp_code.pro'].

bp_load
	:-
	sql_clear_table('./bp_data',bp_table),
	Src = './raw.brainy_puzzles.txt',
	grab_lines(Src, Lines),
	load_bp(Lines, 0).

load_bp([], N) :- printf('Loaded %t lines\n', [N]).
load_bp([Line | RestLines], N)
	:-
write(N),put(0'.),
	asplit(Line, 0'@, Q, A),
	insert_db_bp_q4([N, Q, A]),
	M is N+1,
	load_bp(RestLines, M).
	
t1 :-
	retrieve_primary_db_bp(7, DTVal),
	DTVal = [N, Q, A],
	printf('Riddle %t:\nQ:    %t\n\nA:    %t\n\n',[N, Q, A]).
