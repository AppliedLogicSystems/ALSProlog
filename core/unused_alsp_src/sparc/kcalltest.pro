/*
 * kcalltest.pro
 *
 *	Test interaction of PI_rungoal with rest of system
 *
 *
 */



garb :- f(a,b,c,d) = f(a,b,c,d).
heapused(Where) :- 
	pbi_write(Where), pbi_write(:), 
	X is heapused, 
	pbi_write(X),
	pbi_nl.

test1 :- 
	gc,
	heapused(first),
	garb,
	heapused(second),
	curmod(CurMod),
	kcall(CurMod,test1inner),
	heapused(last).

test1inner :-
	garb,
	heapused(innerfirst),
	gc,
	heapused(innerlast).
%	true.


test2 :-
	curmod(CurMod),
	kcall(CurMod,p(X)),	%% should not be able to backtrack into kcall
	write(p(X)),nl,
	fail.
test2.

p(a).
p(b).
p(c).



:- make_gv('_counter').

test3 :-
	set_counter(counter(8)),
	gc,
	heapused(first),
	garb,
	heapused(second),
	curmod(CurMod),
	kcall(CurMod,test3inner),
	heapused(last).


test3inner :- 
	get_counter(counter(0)),
	!.
test3inner :-
	garb,
	get_counter(counter(N)),
	NN is N-1,
	set_counter(counter(NN)),
	gc,
	heapused(inner),
	test3inner.

test4 :- 
	gc,
	heapused(first),
	garb,
	heapused(second),
	curmod(CurMod),
	kcallu(CurMod,test4inner,'Hello World'),
	heapused(last).

test4inner :-
	garb,
	heapused(innerfirst),
	gc,
	heapused(innerlast).

test5 :- 
	garb,
	assert(p(x)),retract(p(_)),!,		%% create code space garbage
	curmod(CurMod),
	kcall(CurMod,test5inner),
	statistics.

test5inner :-
	garb,
	assert(p(x)),retract(p(_)),!,		%% create code space garbage
	statistics,
	collectcode,
	statistics.
