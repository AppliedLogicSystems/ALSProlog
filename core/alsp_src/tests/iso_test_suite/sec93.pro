%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    sec93.pro                                 %
%                        12:40PM  5/4/1996     %
%                                              %
%  tests of power, log, exp and trignometric   %
%   functors only test                         %
%                                              %
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
validate :- test_93.



   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    test the  ** function
%

test_pow :-
   test_true(X1   is 15 ** 2),
   test_val(X2 is 2 ** 3 ,X2 ,8.0).
   error_test(X3 is 3 ** N ,instantiation_error).
   
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  test the trig functions
%

test_trig :-
   test_true( 0.0 is sin(0.0)),
   test_true(1.0 is cos(0.0)),
   test_true(X is atan(1)).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 	test log and exponential
%

test_log_exp :-
      test_true(0.0 is log(1.0)),
      test_true(1.0 is exp(0.0)),
      error_test(X is log(N), instantiation_error),
      error_test(X is log(foo), type_error(evaluable, foo/0)).


test_93 :-
   log_nl, log('testing power  functor'),
   log_nl, log_nl,
   test_pow,
   log('test power done, testing trig functors '),
   log_nl,
   test_trig,
   log_nl, log('Done testing trig functors'), 
   log_nl, log('testing log and exponential functions'),
   log_nl,
   test_log_exp,
   log_nl, log('Done testing log and exponential functions'),
   log_nl,
   log('Done testing section 9.3'),
   log_nl.
