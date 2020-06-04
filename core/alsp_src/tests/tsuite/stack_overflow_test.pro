/* Test correct handling of stack overflows. */

% Perform a few times to catch follow-on problems.
test :-
	stack_overflow_test, stack_overflow_test,
	stack_overflow_test, stack_overflow_test.

stack_overflow_test :-
	catch(ints_from(0,L), Error, (write(e=Error), nl, Error = stack_overflow(_))).

ints_from(N,[N|T]) :-
     % write(p:N),nl,
      NN is N+1,
      ints_from(NN,T).
ints_from(_,_).
