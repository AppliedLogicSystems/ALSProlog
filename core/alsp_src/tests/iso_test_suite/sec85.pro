%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  Section 8.5 of the ISO Prolog Standard      %
%                                              %
%    Term creation and decomposition           %
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

%:- multifile(validate/0).

validate :-
	test_85.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    Heavily tested functional for copy_term   %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	object(atom).
	object([atom1, atom2, atom3]).
	object(3).
	object(2.71828).
	object(f(a,X)).
	object(f(X,b)).
	object([1,2,3]).
	object([X,Y,Z]).
	object(pred/arity).
	object(pred/N).
	object(X/Y).
	object(a(X,b(Y,Z))).


	copy_term_test1(X) :-
		object(X),
		copy_obj(X).

	copy_obj(X) :- 
		copy_term(X,Y),!,
		verify_copy(X,Y).

	copy_obj(X) :-
		copy_failed(X).


	verify_copy(X,Y) :- 
		X = Y, !.

	verify_copy(X,Y) :- 
		log( 'Term '), log( X), 
		log( ' copy_termed to inequivalent term '),
		write(Y), log_nl.

	copy_failed(X) :-
		log('Term '), log(X),
		log( ' not copied.'), log_nl.

	copy_failed(X,Y) :-
		X = Y, !,
		log( 'Term '), log( Y),
		log( ' not recognised as a copy of '),
		log( X), log_nl.
		
	copy_term_test2(X,Y) :-
		object(X), object(Y),
		copy_obj(X,Y).

	copy_obj(X,Y) :-
		copy_term(X,Y),!,
		verify_copy(X,Y).

	copy_obj(X,Y) :-
		copy_failed(X,Y).


test(1) :-
	copy_term_test1(X),
	fail.
test(1).

test(2) :-
	copy_term_test2(X,Y),
	fail.
test(2).

test_copy_term :-
	catch( (test(1),test(2)), B, unexpected(B)).

unexpected(B) :-
	log( 'Unexpected error '), log(B), 
	log( ' raised.'), log_nl.

test_errors :-
	
	error_test((functor(X,Y,3)), instantiation_error),
	error_test((functor(X,foo,N)), instantiation_error),
	error_test((functor(X,foo,a)), type_error(integer,a)),
	error_test((functor(F,1.5,1)), type_error(atom,1.5)),
	error_test((functor(F,foo(a),1)), type_error(atomic,foo(a))),
	error_test((arg(X,foo(a,b),a)), instantiation_error),
	error_test((arg(1,X,a)), instantiation_error),
	error_test((arg(0,atom,A)), type_error(compound,atom)),
	error_test((arg(0,3,A)), type_error(compound,3)),

	error_test((X=..Y), instantiation_error),
	error_test((X=..[foo,a|Y]), instantiation_error),
	error_test((X=..[foo|bar]), type_error(list,[foo|bar])),
	error_test((X=..[Foo,bar]), instantiation_error),
	error_test((X=..[3,1]), type_error(atom,3)),
	error_test((X=..[1.1,foo]), type_error(atom,1.1)),
	error_test((X=..[a(b),1]), type_error(atom,a(b))),
	error_test((X=..4), type_error(list,4)).

test_functor :-
	
	test_true((functor(foo(a,b,c),foo,3))),
	test_true((functor(foo(a,b,c),X,Y))),
	test_true((functor(X,foo,3))),
	test_true((functor(X,foo,0))),
	test_true((functor(mats(A,B), A, B))),
	test_false((functor(foo(a), foo, 2))),
	test_false((functor(foo(a), fo, 1))),
	test_true((functor(1, X, Y))),
	test_true((functor(X, 1.1, 0))),
	test_true((functor([_|_], '.', 2))),
	test_true((functor([], [], 0))).

test_arg :-

	test_true((arg(1, foo(a,b), a))),
	test_true((arg(1, foo(a,b), X))),
	test_true((arg(1, foo(X,b), a))),
	test_true((arg(1, foo(X,b), Y))),
	test_false((arg(1, foo(a,b), b))),
	test_false((arg(0, foo(a,b), foo))),
	test_false((arg(3, foo(3,4), N))).

test_univ :-

	test_true((foo(a,b) =.. [foo,a,b])),
	test_true((X =.. [foo, a, b])),
	test_true((foo(a,b) =.. L)),
	test_true((foo(X,b) =.. [foo, a, Y])),
	test_true((1 =.. [1])),
	test_false((foo(a,b) =.. [foo, b, a])).

test_copyterm :-

	test_true((copy_term(X,Y))),
	test_true((copy_term(X,3))),
	test_true((copy_term(_,a))),
	test_true((copy_term(a+X,X+b))),
	test_true((copy_term(_,_))),
	test_true((copy_term(X+X+Y,A+B+B))),
	test_false((copy_term(a,b))),
	test_false((copy_term(a+X,X+b),copy_term(a+X,X+b))).

test_85:-

	log('Starting tests for Section 8.5'), log_nl,
	log_nl, log('Test starting for functor.'), 
        log_nl, log_nl,

	test_functor,

	log_nl, 
        log( 'Test of functor finished, starting test of arg'),
        log_nl,

	test_arg,

	log_nl, 
        log( 'Test of arg finished, starting test of univ'), 
        log_nl, 

	test_univ,

	log_nl, 
        log( 'Test of univ finished, starting test of copy_term'),
        log_nl,

	test_copyterm,

	log_nl, 
        log('Testing copy_term more extensively.'), 
        log_nl,

	test_copy_term,

	log_nl, 
        log('Tests of copyterm completed, testing errors secction 8.5'), 
        log_nl, log_nl,

	test_errors,

	log_nl, 
        log( 'Testing of error catching completed.'), log_nl,
	log_nl, log('All tests finished for Section 8.5.'), 
        log_nl, log_nl, !.
