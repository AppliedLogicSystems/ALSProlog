/*
 * Counter benchmark:
 *
 * We implement counters in three ways and put them into a repeat-fail loop.
 * The first way uses the database to implement the counter and the second
 * way uses global variables.  We expect the global variable implementation
 * to be faster.  The third way uses mangle in a manner similar to the
 * global variable version, but mangle is faster.
 *
 * The assert/retract version is run by entering bench1.
 * The global variable version is run by entering bench2.
 * The mangle version is run entering bench3.
 * bench4 is a tail recursive version and is included for purposes of
 * comparison.
 *
 * Results: (in seconds)
 *
 *					C O U N T E R   B E N C H M A R K
 *  Machine / Implementation		|     1 |     2	|     3	|     4	|
 * -------------------------------------+-------+-------+-------+-------+
 *  Sun 3/50 (native, v1.3)		|  1569	| 172.1	|  36.5	|  16.8	|
 *  Sun 3/50 (portable, v1.0)		|  2790	| 664.2	| 620.3	| 443.7	|
 *  SPARCstation IPC (native)		| 217.7	|  19.6	|   6.3	|  4.75	|
 *  SPARCstation IPC (portable, v1.0)	| 368.4	|  70.1	|  70.1	|  46.3	|
 *  SPARC ???				| 200.6	|  14.9	|   4.1	|   3.4	|
 *  Motorola 68k (native)		|	|	|	|	|
 *  Motorola 88k (native)		|  1017	|  16.3	|  3.93	|  2.58	|
 *  NeXT				|   234	|  14.9	|  3.44	|  1.66	|
 *  Motorola 88k (native, SVR4)		|*359.4	|  13.8	|  3.89	|  2.75	|
 *
 *	* I consider this timing to be suspect.  It took a hell of a long
 *	  time in real time.
 *
 *
 */

bench :- bench1, bench2, bench3, bench4.
bench1 :- startTime(X), count1, printElapsed(X).
bench2 :- startTime(X), count2, printElapsed(X).
bench3 :- startTime(X), count3, printElapsed(X).
bench4 :- startTime(X), count4, printElapsed(X).

count1 :-
    repeat,
    inc1,
    count1(X),
    printit(X),
%    X > 200000.
    X > 200.


inc1 :- retract(count1(X)), !, Y is X+1, assert(count1(Y)).
count1(0).


count2 :-
    repeat,
    inc2,
    getCounter(X),
    printit(X),
%    X > 200000.
    X > 200.

:- make_gv('Counter').
:- setCounter(0).

inc2 :-
    getCounter(X), !, Y is X+1, setCounter(Y).

count3 :-
    initial_counter(C),
    repeat,
    inc3(C),
    getCounter(C,X),
    printit(X),
%   X > 200000.
    X > 200.

initial_counter(counter(0)).
inc3(C) :- arg(1,C,X), Y is X+1, mangle(1,C,Y).
getCounter(counter(X),X).

count4 :- count4(0).
count4(X) :- 
%	X > 200000, !.
	X > 200, !.
count4(X) :- inc4(X,Y), printit(Y), count4(Y).

inc4(X,Y) :- Y is X+1.

printit(X) :-
    X mod 10 =:= 0,
%    X mod 10000 =:= 0,
    !,
    write(X),nl.
printit(_).

startTime(X) :- X is cputime.
printElapsed(X) :- Y is cputime-X, write('Time'=Y), nl.
