%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   Section 8.6 of the ISO Prolog Standard     %
%                                              %
%  Tests of is/2                               %
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
validate :- test_86.

test_is :-
	
	test_true(is(Result, 3+11.0)),
	X = 1 + 2,
	test_true(Y is X * 3),
	test_true(is(3,3)),
	test_false(is(3,3.0)),
	test_false(is(foo,77)),
	error_test(is(77,N),instantiation_error).


test_86 :-
	log('Starting tests for Section 8.6'), 
        log_nl,
	log_nl, log('Testing Is/2'), 
        log_nl,

	test_is,

	log_nl, log('Testing of is complete.'), log_nl,
	log_nl, log('All testing completed for Section 8.6.'), 
        log_nl, log_nl, !.
