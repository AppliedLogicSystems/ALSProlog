%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   sec91.pro                                  %
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%    Tests of section 9.1 arithmetic           %
%    functions.                                %
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



:- multifile(validate/0).

validate :-
	test_91.

test_91:-
     	log_nl, log( 'Testing Arithmetic Operations + and -'), log_nl,
     
	test_val(X1 is +(7,35),X1,42),
     	test_val(X2 is +(0,+(3,11)),X2,14),
     	test_val(X3 is +(0,3.2+11),X3,14.2000),
     	error_test(X4 is +(77,N),instantiation_error),
     	error_test(X5 is +(foo,77),type_error(evaluable, foo/0)),
     	test_val(X6 is -(7),X6,-7),
     	test_val(X7 is -(3-11),X7,8),
     	test_val(X8 is -(3.2-11),X8,7.8000),
     	error_test(X9 is -(N),instantiation_error),
     	error_test(X10 is -(foo),type_error(evaluable, foo/0)),
     	test_val(X11 is -(7,35),X11,-28),
     	test_val(X12 is -(20,3+11),X12,6),
     	test_val(X13 is -(0,3.2+11),X13,-14.2000),
     	error_test(X14 is -(77,N),instantiation_error),
     	error_test(X15 is -(foo,77),type_error(evaluable, foo/0)),
     	
	log_nl, log('Done Testing Arithmetic Operations +  and -'), log_nl,
     
        log_nl, log( 'Testing Arithmetic Operations * and /'), log_nl,
     	
	test_val(X16 is *(7,35),X16,245),
     	test_val(X17 is *(0,3+11),X17,0),
     	test_val(X18 is *(1.5,3.2+11),X18,21.3000),
     	error_test(X19 is *(77,N),instantiation_error),
     	error_test(X20 is *(foo,77),type_error(evaluable, foo/0)),
     	test_val(X21 is /(7,35),X21,0.2),
     	test_val(X22 is /(7.0,35),X22,0.2),
     	test_val(X23 is /(140,3+11),X23,10.0),
     	test_val(X123 is /(140,3+11),X123,10.0),
     	test_val(X24 is /(20.164,3.2+11),X24,1.42000),
     	test_val(X25 is /(7,-3),X25,-2.3333333333),
     	test_val(X26 is /(-7,3),X26,-2.3333333333),
     	error_test(X27 is /(77,N),instantiation_error),
     	error_test(X28 is /(foo,77),type_error(evaluable, foo/0)),
     	error_test(X29 is /(3,0),evaluation_error(zero_divisor)),

     	log_nl, log( 'Done Testing Arithmatic Operations * and /'), 
        log_nl,
        log_nl, log('Testing Arithmetic Operations mod etc..'), log_nl,

     	test_val(X30 is mod(7,3),X30,1),
     	test_val(X31 is mod(0,3+11),X31,0),
     	test_val(X32 is mod(7,-2),X32,-1),
     	error_test(Y2 is mod(77,N),instantiation_error),
     	error_test(Y3 is mod(foo,77),type_error(evaluable, foo/0)),
     	error_test(Y4 is mod(7.5,2),type_error(integer, mod/2)),
     	error_test(Y5 is mod(7,0),evaluation_error(zero_divisor)),
     	test_val(X33 is floor(7.4),X33,7),
     	test_val(X34 is floor(-0.4),X34,-1),
     	test_val(X35 is round(7.5),X35,8),
     	test_val(X36 is round(7.6),X36,8),
     	test_val(X37 is round(-0.6),X37,-1),
     	error_test(X38 is round(N),instantiation_error),
     	test_val(X39 is ceiling(-0.5),X39,0),
     	test_val(X40 is truncate(-0.5),X40,0),
     	error_test(X41 is truncate(foo),type_error(evaluable, foo/0)),
     	test_val(X42 is float(7),X42,7.0),
     	test_val(X43 is float(7.3),X43,7.3),
     	test_val(X44 is float(5/3),X44,1.6666666666),
     	error_test(X45 is float(N),instantiation_error),
     	error_test(X46 is float(foo),type_error(evaluable, foo/0)),
     	test_val(X47 is abs(7),X47,7),
     	test_val(X48 is abs(3-11),X48,8),
     	test_val(X49 is abs(3.2-11.0),X49,7.8000),
     	error_test(X50 is abs(N),instantiation_error),
     	error_test(X51 is abs(foo),type_error(evaluable, foo/0)),
        log_nl, log( 'Done Testing Elementary Arithmetic Operations'), log_nl, 
        log_nl, !.
     
   
   



