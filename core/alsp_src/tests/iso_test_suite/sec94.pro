%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    sec94.pro                                 %
%                        12:40PM  5/4/1996     %
%                                              %
%  tests of arithmetic functors only test for  %
%  support of bitwise fucntors.                %
%  (/\)/2, (\/)/2,  (\)/1, <</2, >>/2          %
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% multifile(validate/0).
validate :- test_94.



   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    test the  and function
%

test_and :-
   test_true( X1 is 15 /\ 12),
   test_val(X2 is 10 /\ 12,X2,8),
   error_test(X3 is 3 /\ N ,instantiation_error),
   error_test(X4 is foo/\2 ,type_error(evaluable, foo/0)).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  test the or function
%

test_or :-
   test_true( X1 is 10  \/ 12),
   test_val(X2 is 10 \/  12,X2,14),
   error_test(X3 is 3 /\ N ,instantiation_error),
   error_test(X4 is foo/\2 ,type_error(evaluable, foo/0)).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Test of ones complement
%

test_ones_complement :- 
	test_true(X1 is \10),
        test_val(X2 is \(\10), X2, 10),
        error_test(X3 is \ N ,instantiation_error),
         error_test(X4 is \foo ,type_error(evaluable, foo/0)).
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of shifts.
%

test_shifts :-
     test_true(X1 is 2 << 3),
     test_val(X2  is  16 << 2, X2, 64),
     test_true(X3 is 32 >> 1),
     test_val(X4 is 19 >> 2, X4, 4),
     error_test(X3 is 3 << N ,instantiation_error),
     error_test(X4 is 3<<foo ,type_error(evaluable, foo/0)),
     error_test(X3 is N << 4 ,instantiation_error),
     error_test(X4 is foo<< 3 ,type_error(evaluable, foo/0)),
     error_test(X3 is 3 >> N ,instantiation_error),
     error_test(X4 is 3>>foo ,type_error(evaluable, foo/0)).

test_94 :-
   log_nl, log('testing bitwise  arithmetic functors'),
   log_nl, log_nl,
   log('testing and or and 1s complement '),
   log_nl,
   test_and,
   test_or,
   test_ones_complement,
   log_nl, log('Done testing and or and 1s complement'), 
   log_nl, log('testing shift functions'),
   log_nl,
   test_shifts,
   log_nl, log('Done testing shifts'),
   log_nl,
   log('Done testing section 9.4'),
   log_nl.
