%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   Section 8.4 of the ISO Prolog Standard     %
%                                              %
%   Term comparison.                           %
%     tests  (@=<)/2, (==)/2, (\==)/2,         %
%         (@<)/2,  (@>)/2, (@>=)/2             %
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
	test_84.

test_term_comparision :-
	
	test_true((1.0 @=< 1)),
	test_true((1.0 @< 1)),
	test_false((1 \== 1)),
	test_true((aardvark @=< zebra)),
	test_true((short @=< short)),
	test_true((short @=< shorter)),
	test_false((short @>= shorter)),
	test_false((foo(a,b) @< north(a))),
	test_true((foo(b) @> foo(a))),
	test_true((foo(a,X) @< foo(b,Y))),
	test_true((X @=< X)),
	test_true((X == X)),
	test_false((X == Y)),
	test_true((_ \== _)),
	test_false((_ == _)).

test_84 :-
	log( 'Starting tests for Section 8.4'), log_nl,
	log_nl, log( 'Beginning tests of term comparisions.'), 
        log_nl,

	test_term_comparision,

	log_nl, 
        log( 'Tests of term comparisions are completed.'), log_nl,
	log_nl, log( 'All testing completed for Section 8.4.'), 
        log_nl, log_nl, !.
