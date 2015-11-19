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

##9.2 Exceptions
The exception mechanism of ALS Prolog allows programs to react to extraordinary
circumstances in an efficient and appropriate manner. The most common extraordinary circumstance to be dealt with is errors. Often an error (perhaps inappropriate user input, etc.) is detected deep in the calling sequence of predicates in a program.
The most appropriate reaction on the part of the program may be to return to a much
earlier state. However, if the code is written to support such a return using the ordinary predicate calling mechanisms, the result is often difficult to understand and
has poor effeiciency. The exception mechanism allows the program to mark a point
in its calling state, and to later be able to return directly to this marked point independently of the pending calls between the marked state and the later state. This notion is illustrated in the figure below.

{ADD PICTURE}

Figure. Direct Return to an Earlier State.

This exception mechanism is implemented using two predicates, catch/3 and throw/1.
````
catch/3
catch(WatchedGoal, Pattern, ExceptionGoal)
catch(+, +, +)

throw/1
throw(Term)
throw(+)
````
The two predicates catch/3 and throw/1 provide a more sophisticated controlled
abort mechanism than the primitive builtins catch/2 and throw/0 (although the
former are implemented in terms of the latter, which are described below). catch/
3 is used to mark a state in the sense of the discussion above. We will say that the call

    catch(WatchedGoal, Pattern, ExceptionGoal)

_catches_ a term T if T unifies with Pattern. A call throw(T) is caught by a call
on catch/3 if the argument T is caught by the call on catch/3, and there is no call
on catch/3 between the given calls on catch/3 and throw/1 which also catches T.

If M is the current module for the call on catch/3, then the first argument of catch/3,
WatchedGoal, is run in module M just as if by call/1; i.e., as if
catch/3 were call/1. If there is no subsequent call to throw/1 which is uncaught by an intervening call to catch/3, then the call on catch/3 is exactly like
a call on call/1. However, if 
i) there is a subsequent call to throw/1 whose argument Ball unifies with the second argument of the call on catch/3, and if 
ii) there is no interposed call to catch/3 whose second argument also unifies with
Ball, 
then all computation of the call on the first argumentt, WatchedGoal, is
aborted, and the third argument of the call to catch/3, ExceptionGoal, is run as
a result of the call to throw/1.

When the system executes throw/1, it will behave as if the head of catch/3 failed.
However, instead of backtracking to the most recent choicepoint, the system will
instead backtrack to the state it was in just before the most recent enclosing catch/
3 whose second argument unifies with the argument of the throw/1 call; then the
system will run the corresponding ExceptionGoal. 

catch and throw are dynamically
scoped in that throw/1 must be called somewhere in an execution of WatchedGoal
which is initially invoked by catch/3. If throw/1 is called outside the scope of some
invocation of catch/3 (meaning, with nothing to catch its abort of execution), the
system aborts to the Prolog shell. The figure below illustrates the behavior of these predicates.

{ADD PICTURE}

Figure. Action of catch/3 and throw/1.

Consider the sample code:
````
ct :- catch(c1, p1(X), e(c1,X)).
c1 :- write(c1),nl, catch(c2, p2(X), e(c2,X)).
c2 :- write(c2),nl, catch(c3, p1(X), e(c3,X)).
c3 :- write(’c3-->’), read(Item),
      write(throwing(Item)),nl, 
      throw(Item).
e(H,I) :- printf("Handler %t caught item %t\n",[H,I]).
````
This leads to the following execution behavior on a TTY interface, with the user input indicated in bold.
?- ct.
c1
c2
c3-->**p1(a).**
throwing(p1(a))
Handler c3 caught item a
yes.
?- ct.
c1
c2
c3-->**p2(a).**
throwing(p2(a))
Handler c2 caught item a