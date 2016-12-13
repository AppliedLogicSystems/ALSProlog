

module myMod.
%use  app_utils.
export myt/1.

myt(Y) :-
	interleave([a,b,c], r, U),
	list_diff(U, [b], Y).

foo :-
	bar.

endmod.

zipper :-
	zap.
