%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   Section 8.2 of the ISO Prolog Standard     %
%                                              %
%   term unification                           %
%       (=)/2, unify_with_occurs_check/2,      %
%       (\=)/2                                 %
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

validate :- test_82.

test_term_unification :-

	defined('='/2),!,
	test_true((1=1)),
	test_true((X=1)),
	test_true((X=Y)),
	test_true((_=_)),
	test_true(((X=Y)=(X=abc))),
	test_true((f(X,def)=f(def,Y))),
	test_false((1=2)),
	test_false((1=1.0)),
	test_false((g(X)=f(f(X)))),
	test_false((f(X,1)=f(a(X)))),
	test_false((f(X, Y, X)=f(a(X), a(Y), Y, 2))).

test_term_unification :-

	log_nl, log('Term unification function not supported.'), log_nl.

test_unify_with_occurs_check :-

	defined(unify_with_occurs_check/2),!,
	test_true(unify_with_occurs_check(1,1)),
	test_val(unify_with_occurs_check(X,1), X, 1),
	test_false(unify_with_occurs_check(Y, a(Y))),
	test_true(unify_with_occurs_check(X,Y)),
	test_true(unify_with_occurs_check(_,_)),
	test_true(unify_with_occurs_check(X,Y)),
	test_true(unify_with_occurs_check(f(X,def),f(def,Y))),
	test_false(unify_with_occurs_check(1, 2)),
	test_false(unify_with_occurs_check(1, 1.0)),
	test_false(unify_with_occurs_check(g(X), f(f(X)))),
	test_false(unify_with_occurs_check(f(X,1), f(a(X)))),
	test_false(unify_with_occurs_check(f(X,Y,X), f(a(X), a(Y), Y, 2))),
	test_false(unify_with_occurs_check(X, a(X))),
	test_false(unify_with_occurs_check(f(X,1), f(a(X),2))),
	test_false(unify_with_occurs_check(f(1,X,1), f(2,a(X),2))),
	test_false(unify_with_occurs_check(f(1,X), f(2,a(X)))),
	test_false(unify_with_occurs_check(f(X,Y,X,1), f(a(X), a(Y), Y, 2))).

test_unify_with_occurs_check :-

	log_nl, log('Unify with occurs check function not supported.'), log_nl.

test_not_prolog_unify :-

	defined('\='/2),!,
	test_false((1 \= 1)),
	test_false((X \= 1)),
	test_false((X \= Y)),
	test_false((_ \= _)),
	test_false((f(X,def) \= f(def, Y))),
	test_true((1 \= 2)),
	test_true((1 \= 1.0)),
	test_true((g(X) \= f(f(X)))),
	test_true((f(X,1) \= f(a(X)))),
	test_true((f(X,Y,X) \= f(a(X),a(Y),Y,2))).

test_not_prolog_unify :-

	log_nl, log('Not prolog unifiable function not supported.'), log_nl.

test_82:-  

	log( 'Starting tests for Section 8.2'), log_nl,
	log_nl, log( 'Testing Prolog unify.'), log_nl,

	test_term_unification,
	
	log_nl, log('Tests of unification complete, testing unify_with_occurs_check.'), 
        log_nl, log_nl, 
	
	test_unify_with_occurs_check,

	log( 'Tests of unify_with_occurs_check completed, testing not Prolog unifiable.'), 
        log_nl, log_nl,

	test_not_prolog_unify,

	log_nl, log( 'Testing of not Prolog unifiable completed.'), log_nl,
	log_nl, log('All testing completed for Section 8.2.'), 
        log_nl, log_nl, !.
