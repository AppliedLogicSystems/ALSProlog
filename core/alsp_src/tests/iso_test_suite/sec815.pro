%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   sec815.pro                                 %
%                   9:40AM  11/1/1996          %
%   logic and control                          %
%      tests for (\+)/1, once/1, and repeat/0  %
%                                              %
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%                                              %
%                                              %
%   May be used freely provided                %
%   acknowledgement is made.                   %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%   and to Joe Pedano and John Hallat of       %
%   Saint Joseph's for their work on this      %
%   project.                                   %
%                                              %
%                                              %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% :- mulitifile(validate/0).
validate :- test_815.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               
% tests for not provable that should not raise errors
%
%

test_not_provable :-
	test_false((\+ (true))),
        test_false((\+ (!))),
        test_true((\+( 4 = 5))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               
% tests for not provable that should  raise errors
%
%

test_not_provable_errors :-
	error_test((\+(3)), type_error(callable, 3)),
        error_test((\+(X)), instantiation_error).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests for once that should not raise errors
%


test_once :-
	test_true(once(!)),
        test_false(once(fail)),
        test_val(setof(X, once((X = 1; X =2)), S), S, [1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests for once that should  raise errors
%

test_once_errors :-
	error_test(once(X), instantiation_error),
        error_test(once(4), type_error(callable, 4)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests for repeat
%
%     repeat does not raise errors
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%
%   auxiliary predicates for testing repeat
%

:- dynamic(last_val/1).

up_to(N) :- repeat,
	repeat_pred(X).
up_to(N):- last_val(N).  

repeat_pred(1).
repeat_pred(2).
repeat_pred(3) :- asserta(last_val(3)),fail.

test_repeat :-
	test_false((repeat, !, fail)),
	test_val(up_to(N), N, 3).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Tests withtest for existence 
%

test_np :-
	defined('\\+'/1)
         ->
        (test_not_provable, test_not_provable_errors)
        ;
        (log_nl, log( '\+ is not supported.'), log_nl). 
test_1nce :-
	defined(once/1)
         ->
        (test_once, test_once_errors)
        ;
        (log_nl, log( 'once is not supported.'), log_nl). 

test_rpt :-
	( defined(repeat/0), !)
         ->
        test_repeat
        ;
        (log_nl, log( 'repeat is not supported.'), log_nl). 


       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  run the tests.
%


test_815 :-  log_nl,
             log( 'testing not provable'),
             log_nl,
             test_np,
             log_nl,
             log( 'testing not provable done, testing once'),
             log_nl,
             test_1nce,
             log_nl,
             log( 'testing once done, testing repeat'),
             log_nl,
             test_rpt,
             log_nl,
             log('testing repeat done.'),
             log_nl,
             log_nl,
             log('testing of section 8.15 done'),
             log_nl,!.
       
	
        
