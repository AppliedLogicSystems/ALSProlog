%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  Section 8.9 of the ISO Prolog Standard      %
%                                              %
%  Clause creation and destruction             %
%                                              %
%  Tests for asserta/1, assertz/1,             %
%   retract/1, abolish/1                       %
%   ALS supports abolish/2                     %
%                                              %
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%   and to Joe Pedano and John Hallat of       %
%   Saint Joseph's for their work on this      %
%   project.                                   %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   assumes the same database as sec8.8

%:- dynamic(legs/2).

%legs(A,6) :- insect(A).
%legs(A,7) :- A, call(A).

%:- dynamic(insect/1).
%insect(ant).
%insect(bee).

:- dynamic(foo/1).

foo(X) :- call(X) -> call(X).


%:- multifile(validate/0).
validate :- test_89.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_asserta :-
	defined(asserta/1),
	test_true(asserta(legs(octopus,8))),
	test_true(asserta((legs(A,4) :- animal(A)))),        
	test_true(asserta((foo(X) :- X, call(X)))),
        test_true(asserta(new_pred(bar))).
test_asserta :-
	log_nl, log( 'asserta/1 not supported'),
        log_nl.

verify_assertas :-
        defined(asserta/1),
	test_true(legs(octopus, 8)),
        test_true(new_pred(bar)),
        test_val(
                  (clause(legs(A, X), animal(A))), 
                  X, 
                  4).

verify_assertas.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_asserta_errors :-
	defined(asserta/1),
	error_test(asserta(_), type_error(instantiation_error)),
	error_test(asserta(4), type_error(callable, 4)),
	error_test(asserta((foo:-4)), type_error(callable,4)),
	error_test(asserta((atom(_) :- true)), 
		permission_error(modify, static_procedure, atom/1)).
test_asserta_errors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_assertz :-
	defined(assertz/1),
	test_true(assertz(legs(stool,3))),
	test_true(assertz((legs(B,2) :- bird(B)))),
	test_true(assertz((foo(X) :- X, call(X)))),
        test_true(assertz(another_new_pred(bar))).
test_assertz :-
	log_nl, log( 'assertz/1 not supported'),
        log_nl.
verify_assertzs :-
        defined(assertz/1),
	test_true(legs(stool, 3)),
        test_true(another_new_pred(bar)),
        test_val(
                  (clause(legs(B, X), bird(B))), 
                  X, 
                  2).
verify_assertz.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_assertz_errors :-
	defined(assertz/1),
	error_test(assertz(_), instantiation_error),
	error_test(assertz(4), type_error(callable, 4)),
	error_test(assertz((foo:-4)), type_error(callable, 4)),
	error_test(assertz((atom(_) :- true)), 
	    permission_error(modify, static_procedure, atom/1)).
test_assertz_errors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_retract :-
	defined(retract/1),
	test_true(retract(legs(octopus,8))),
	test_false(retract(legs(spider,6))),
	test_true(retract((legs(X,2) :- T))),
	test_true(retract((legs(X,Y) :- Z))),
	test_false(retract((legs(X,Y) :- Z))),
	test_true(retract((foo(C) :- A -> B))).
test_retract :-
	log_nl, log( 'retract/1 not supported'),
        log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_retract_errors :-
	defined(retract/1),
	error_test(retract((X :- in_eec(Y))), instantiation_error),
	error_test(retract((4 :- X)), type_error(callable, 4)),
	error_test(retract((atom(X) :- X == '[]')), 
	    permission_error(modify, static_procedure, atom/1)).

test_retract_errors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_abolish :-
	defined(abolish/1),
	test_true(abolish(foo/2)).

test_abolish :-
        log_nl, log( 'abolish/1 not supported'),
        log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_abolish_errors :-
	defined(aboilsh/1),
	error_test(abolish(foo/_), instantiation_error),
	error_test(abolish(foo), type_error(predicate_indicator, foo)),
	error_test(abolish(foo(_)), 
		type_error(predicate_indicator, foo(_))),
	error_test(abolish(abolish/1), 
	    permission_error(modify, static_procedure, abolish/1)).

test_abolish_errors.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_89 :-
	
	log_nl, log( 'Starting tests for Section 8.9.'), log_nl,
	log_nl, log( 'Starting tests of asserta.'), log_nl,

	test_asserta,

	log_nl, log( 'Tests of asserta completed, testing asserta errors.'), log_nl,

	test_asserta_errors,

	log_nl, log( 'Tests of asserta errors completed, testing assertz.'), log_nl,

	test_assertz,

	log_nl, log( 'Tests of assertz completed, testing assertz errors.'), log_nl,

	test_assertz_errors,

	log_nl, log( 'Tests of assertz errors completed, testing retract.'), log_nl,

	test_retract,
	
	log_nl, log( 'Tests of retract completed, testing retract errors.'), log_nl,

	test_retract_errors,

	log_nl, log('Tests of retract errors completed, testing abolish.'), 
        log_nl,

	test_abolish,

	log_nl, 
        log('Tests of abolish completed, testing abolish errors.'), 
        log_nl,

	test_abolish_errors,

	log_nl, log( 'Tests of abolish errors completed.'), log_nl, 
	log_nl, log( 'All testing completed for Section 8.9.'), 
        log_nl, log_nl, !.
