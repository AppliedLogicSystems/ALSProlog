%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  Section 8.7 of the ISO Prolog Standard      %
%                                              %
%  Arithmatic Functions                        %
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

%:- mulitifile(validate/0).

validate :- test_87.


test_math_comp :-
    
        test_false(=:=(0,1)),
        test_true(=\=(0,1)),
        test_true(<(0,1)),
        test_false(>(0,1)),
        test_true(=<(0,1)),
        test_true(=:=(1.0,1)),
        test_false(=\=(1.0,1)),
        test_false(<(1.0,1)),
        test_false(>(1.0,1)),
        test_true(>=(1.0,1)),
        test_true(=<(1.0,1)),
        test_true(=:=(3*2,7-1)),
        test_false(=\=(3*2,7-1)),
        test_false(<(3*2,7-1)),
        test_false(>(3*2,7-1)),
        test_true(>=(3*2,7-1)),
        test_true(=<(3*2,7-1)).

test_math_error :-

        error_test(=:=(X,5),instantiation_error),
        error_test(=\=(X,5),instantiation_error),
        error_test(<(X,5),instantiation_error),
        error_test(>(X,5),instantiation_error),
        error_test(>=(X,5),instantiation_error),
        error_test(=<(X,5),instantiation_error).


test_87:-

        log( 'Starting tests for Section 8.7'), log_nl,
        log_nl, log('Testing Mathematical Comparisons'), 
        log_nl,

        test_math_comp,

        log_nl, 
        log('Mathematical Comparisons finished, testing errors.'), 
        log_nl,

        test_math_error,

        log_nl, log( 'Errors finished.'), log_nl,
        log_nl, log('All testing completed for Section 8.7'), 
        log_nl, log_nl, !.
