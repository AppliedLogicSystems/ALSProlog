/*
 * pathname1.pro timings for get_cwd vs canonicalize_pathname
 *
 */

test_cwd :-
    RealStart is realtime,
    CpuStart is cputime,
    test_cwd(50),
    CpuDelta is cputime-CpuStart,
    RealDelta is realtime-RealStart,
    write('cputime'=CpuDelta),nl,
    write('real time'=RealDelta),nl.

test_cwd(0) :- !.
test_cwd(N) :-
    NN is N-1,
    get_cwd(_),
    test_cwd(NN).

test_canon :-
    RealStart is realtime,
    CpuStart is cputime,
    test_canon(50),
    CpuDelta is cputime-CpuStart,
    RealDelta is realtime-RealStart,
    write('cputime'=CpuDelta),nl,
    write('real time'=RealDelta),nl.

test_canon(0) :- !.
test_canon(N) :-
    NN is N-1,
    canonicalize_pathname('.',_),
    test_canon(NN).
