%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   Section 8.10 of the ISO Prolog Standard    %
%                                              %
%   Tests of setof/3, bagof/3, and findall/3   %
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
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile(validate/0).
validate :- test_810.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_findall :-

	defined(findall/3),!,
	test_true(findall(X, (X=1;Y=2), S)),
	test_true(findall(X+Y, (X=1), S)),
	test_true(findall(X, fail, L)),
	test_true(findall(X, (X=1;X=1), S)),
	test_false(findall(X, (X=2;X=1), [1,2])),
	test_true(findall(X, (X=1;X=2), [X,Y])),
	test_val(findall(X, (X=1;X=2), S),S, [1,2]),
	error_test(findall(X,Goal,S), instantiation_error),
	error_test(findall(X,4,S), type_error(callable, 4)).

test_findall :-

	log_nl, write('Findall function not supported.').

test_bagof :-
	
	defined(bagof/3),!,
	test_true(bagof(X,(X=1;X=2), S)),
	test_true(bagof(X,(X=1;X=2), X)),
	test_true(bagof(X,(X=Y;X=Z), S)),
	test_false(bagof(X,fail,S)),
	test_true(bagof(1,(Y=1;Y=2), L)),
	test_true(bagof(f(X,Y), (X=a;Y=b), L)),
	test_true(bagof(X, Y^((X=1,Y=1);(X=2;Y=2)), L)),
	test_true(bagof(X, Y^((X=1;Y=1);(X=2;Y=2)), L)),
	test_true(bagof(X, (Y^(X=1;Y=2);X=3), S)),
	test_true(bagof(X, (X=Y;X=Z;Y=1), S)),
	test_true(bagof(X, a(X,Y), L)),
	test_true(bagof(X, b(X,Y), L)),
	test_val(bagof(X, (X=1;X=2), S), S, [1,2]),
	error_test(bagof(X,Y^Z,L), instatiation_error),
	error_test(bagof(X,1,L), type_error(callable, 1)),
	error_test(bagof(X,4,S), type_error(callable, 4)).

test_bagof :-

	log_nl, log( 'Bagof function not supported.'), log_nl.

test_setof :-
	
	defined(setof/3),!,
	test_true(setof(X,(X=1;X=2),S)),
	test_true(setof(X,(X=1;X=2),X)),
	test_true(setof(X,(X=2;X=1),S)),
	test_true(setof(X,(X=2;X=2),S)),
	test_true(setof(X,(X=Y;X=Z),S)),
	test_false(setof(X,fail,S)),
	test_true(setof(1,(Y=2;Y=1),L)),
	test_true(setof(f(X,Y),(X=a;Y=b),L)),
	test_true(setof(X,Y^((X=1,Y=1);(X=2,Y=2)),S)),
	test_true(setof(X,Y^((X=1;Y=1);(X=2,Y=2)),S)),
	test_true(setof(X,Y^((X=1,Y=1);X=3),S)),
	test_true(setof(X,(X=Y;X=Z;Y=1),S)),
	test_true(setof(X,a(X,Y),L)),
	test_true(setof(X,member(X,[f(U,b),f(V,c)]),L)),
	test_true(setof(X,member(X,[f(U,b),f(V,c)]),[f(a,c),f(a,b)])),
	test_true(setof(X,member(X,[f(b,U),f(c,V)]),[f(b,a),f(c,a)])),
	test_true(setof(X,member(X,[V,U,f(U),f(V)]),L)),
	test_true(setof(X,member(X,[V,U,f(U),f(V)]),[a,b,f(a),f(a)])),
	test_true(setof(X,member(X,[V,U,f(U),f(V)]),[a,b,f(b),f(a)])),
	test_false(setof(X,member(X,[f(b,U),f(c,V)]),[f(b,a),f(c,a)])),
	error_test(setof(X,4,S), type_error(callable, 4)).

test_setof :-

	log_nl, log( 'Setof function not supported.'), log_nl.

/* Support facts for test_bagof  and test_setof */

a(1, f(_)).
a(2, f(_)).

b(1, 1).
b(1, 1).
b(1, 2).
b(2, 1).
b(2, 2).
b(2, 2).

d(1,1).
d(1,2).
d(1,1).
d(2,2).
d(2,1).
d(2,2).

test_810:-

	log_nl, log( 'Beginning tests for Section 8.10'), log_nl,
	log_nl, log( 'Testing findall function.'), log_nl,

	test_findall,

	log_nl, 
        log('Testing of findall completed, testing bagof function.'),
        log_nl,

	test_bagof,

	log_nl, 
        log( 'Testing of bagof completed, testing setof function.'), 
        log_nl,

	test_setof,

	log_nl, log( 'Testing of setof completed.'), log_nl,
	log( 'All testing completed for Section 8.10.'), log_nl.
