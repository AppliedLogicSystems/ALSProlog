/*
 * This tests the interaction of global variables with retract and the garbage
 * compactor.
 *
 * When run, the values printed from statistics for H, B, and TR should
 * stay the same.  The codespace used should not vary too much either.
 */

main :- assert(count(100)), r.

r :- retract(count(N)), !, write('N'=N),nl, collectcode,  statistics, r(N).

r(0) :- !.
r(N) :- PrevN is N-1, assert(count(PrevN)), r.
