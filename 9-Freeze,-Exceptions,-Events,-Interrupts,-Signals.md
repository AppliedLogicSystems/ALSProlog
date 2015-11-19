##9.1 Freeze

ALS Prolog supports a ‘freeze’ control construct similar to those that appear in
some other prolog systems (See [carlsson] for general information on delay terms and implementation strategies). Using ‘freeze’, one can implement a variety of approaches to co-routining and delayed evaluation.
````
freeze/2
freeze(Var, Goal)
freeze(?, +)
````
In normal usage, Var is an uninstantiated variable which occurs in Goal. When invoked in module M, the call

    freeze(Var, Goal)

behaves as follows:
1.  If Var is instantiated, then M:Goal is executed;

2.  If Var is not instantiated, then the goal freeze(Var, Goal) immediately succeeds, but creates a ‘delay term’ (on the heap) which encodes information
about this goal. If Var becomes instantiated at some point in the future, at
that time, the goal M:Goal is run (with, of course, Var as instantiated).

For example, here is an example of an extremely simple producer-consumer coroutine:
````
pc2 :- freeze(S, produce2(0,S)), consume2(S).
produce2(N, [N | T])
    :- M is N+1,
       write(‘-p-’),
       freeze(T, produce2(M,T)).

consume2([N | T])
    :- write(n=N),nl,
       ((N > 3, 0 is N mod 3) -> gc; true),
       (N < 300 -> consume2(T) ; true).
````
Without the presence of the ‘freeze’ constructs, this program will simply loop in
produce2/2, doing nothing but incrementing the counter and printing ‘-p-’ on the
terminal. However, using the freeze construct, the program ‘alternates’ between
produce2/2 and consume2/1, producing the following behavior on the terminal:
````
?- pc2.
-p-n = 0
-p-n = 1
-p-n = 2
-p-n = 3
-p-n = 4
<...snip...>
-p-n = 296
-p-n = 297
-p-n = 298
-p-n = 299
-p-n = 300
yes.
````
Here is one very simple illustrative example:
````
u :- freeze(W1, silly(W1,yellow)),
     freeze(W2, grump(W2,blue)),
     W2=W1,
     W2 = igloo.

grump(A,B)
    :- write(grump_running(A,B)),nl,flush_output.

silly(A,B)
    :- write(silly_running(A,B)),nl,flush_output.
````
Running u/0 yields:
````
?- u.
silly_running(igloo,yellow)
grump_running(igloo,blue)
yes.
````
Uisng silly and grump from above, here is another example:

u1 :- freeze(W1, silly(W1,yellow)),
      u11(W1).

u11(W1)
    :- freeze(W2, grump(W2,blue)),
       W2=W1,
       u111(W2).

u111(W2)
    :- freeze(W3, grump(W3,purple)),
       W3 = W2,
       u1_4(W3).

u1_4(W3)
    :- W3 = igloo.
       u11(W1).

u111(W1).

u1_4(W3).
````
Runing this produces:
````
?- u1.
silly_running(igloo,yellow)
grump_running(igloo,blue)
grump_running(igloo,purple)
yes.
````
The following example (due to Bill Older) illustrates the interaction of freeze with backtracking:
````
fred(2) :- write(fred(2)),nl.
fred(3) :- write(fred(3)),nl.
fred(4) :- write(fred(4)),nl.
freeze_backtrack
    :- freeze(X, write(thaw(X))), fred(X), fail.
````
The output here is:
````
?- freeze_backtrack.
thaw(2)fred(2)
thaw(3)fred(3)
thaw(4)fred(4)
no.
````
Finally, here is an example (also due to Bill Older) of cascading freezes:
````
fd([], 1).
fd([A | As], B)
    :-!,
    freeze(A, fd(As, B)).

fdtest([A,B,C,D]) 
    :- fd([A], B),
       fd([A,B], C),
       fd([B,C], D).
````
Here are two different uses of fdtest:
````
?- fdtest([A,B,C,D]).
A-> fd([],B),user:fd([B],C) 
B-> fd([C],D)
C= C
D= D
yes.

?- fdtest([A,B,C,D]), A = 5.
A= 5 
B= 1
C= 1 
D= 1
yes. 