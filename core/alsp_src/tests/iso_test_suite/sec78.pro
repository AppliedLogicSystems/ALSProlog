%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   Section 7.8 of the ISO Prolog Standard     %
%                                              %
%   Cut and common tests                       %
%                                              %
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%   May be used freely provided                %
%   acknowledgement is made.                   %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cut_b(X) :-
	Y = (log( X),X),
	call(Y).

cut_a(1).
cut_a(2).

twice(!) :- log('C ').
twice(true) :- log('Moss ').
goal((twice(_),!)).
goal(log( 'Three ')).

test_common :-

	test_true(true),
	test_false(fail).

test_call :-

	defined(call/1),!,
	test_true(call(!)),
	test_false(call(fail)),
	test_false(call(fail,X)),
	test_false(call(fail,call(1))),

	error_test(cut_b(_),instantiation_error),
	error_test(cut_b(3),type_error(callable,3)),

	test_true((Z=!,call((Z=!,cut_a(X),Z)))),
	test_true(call((Z=!,cut_a(X),Z))),
	
	error_test(call(write(3)),instantiation_error),
	error_test(call(write(3),call(1)),type_error(callable,1)),
	error_test(call(X),instantiation_error),
	error_test(call(1),type_error(callable,1)),
	error_test(call(fail,1),type_error(callable,(fail,1))),
	error_test((call(write(3),1)),type_error(callable,(write(3),1))),
	error_test((call(1,true)),type_error(callable,(1,true))).

test_call :-

	log_nl, log( 'Call function not supported.'), log_nl.

test_cut :-

	defined(!/0),!,
	test_true(!),
	test_false((!,fail;true)),
	test_true((call(!),fail;true)),
        log('Goal:(twice(_), !, write(''Forwards ''), fail)'),log_nl,
        log('Should now write: C forwards'), log_nl,
        test_false((twice(_), !, log('Forwards '), fail)),
        log_nl,
        log('Goal: ((!; write(''No '')),write(''Cut Disjunction ''),fail)'),
        log_nl,
        log( 'Should now write:  Cut Disjunction'), log_nl, 
	test_false(
                   (
                    (!; log('No ')), 
                    log('Cut Disjunction '),
                   fail
                   )
                  ),log_nl,
        log( 'Goal: (twice(_), (write(''No ''); !), write(''Cut ''),fail )'), 
        log_nl, log( 'Should now write: C No cut Cut '), log_nl,
        test_false((
                     twice(_),
                     (log('No '); !), 
                     log('Cut '),
                     fail
                  )), log_nl,
        log('Goal:(twice(_), (!, fail ; write(''No '')))'), log_nl, 
        log('Should now write: C ' ), log_nl,
        test_false((
                     twice(_),
                     (!, fail ; log('No '))
                  )), log_nl,
       log('Goal:(twice(X), call(X),write(''Forwards ''),fail )'), log_nl,
       log('Should now write:  C Forwards Moss Forwards'), log_nl,
       test_false((
                   twice(X), call(X), 
                   log('Forwards '),
                   fail
                  )), log_nl,
       log('Goal: (goal(X), call(X), write(''Forwards ''), fail)'), log_nl,
       log( 'Should now write: C Forwards Three Forwards'), log_nl,
       test_false((
		    goal(X), call(X), log('Forwards '), fail
                 )), log_nl,
       log( 'Goal: (twice(_), \+(\+(!)),write(''Forwards ''),fail)'), log_nl,
       log( 'Should now write:  C Forwards Moss Forwards'), log_nl,
       test_false((
                    twice(_),
                    \+(\+(!)),
                    log( 'Forwards '),
                    fail
                  )), log_nl,
       log( 'Goal: (twice(_),call(!),write(''Forwards ''),fail)'), log_nl,
       log( 'Should now write:  C Forwards Moss Forwards'), log_nl,
       test_false((
                   twice(_),
                   call(!),
                    log( 'Forwards '),
                    fail
                  )), log_nl,
	test_false((X=1,var(X))),
	test_true((var(X1),X1=1)),
	test_true((X2=true,call(X2))).

test_cut :-

	log_nl, log( 'Cut  not supported.'), log_nl.

%   This is  the database used for the examples
% in 7.8.9.4 of ISO/IEC 13211-1

foo(X) :-
   Y is X * 2, throw(test(Y)).
bar(X) :-
   X = Y, throw (Y).
coo(X) :- throw(Y).
car(X) :- X = 1, throw(X).

g7_8:- catch(p7_8, B, log( h2)),
	coo(C).
p7_8.
p7_8 :- throw(b).

%%%  The following are the goals given in the examples

catch_test(1,Y) :-  catch(foo(5), test(Y), true).
catch_test(2,Z) :-  catch(bar(3), Z, true).
catch_test(3,X) :- catch(true, _, 3).
catch_test(4,X) :- catch(true, C, write(demoen)), throw(bla).
catch_catch_test(5, Y) :- catch(car(X), Y, true).
catch_test(6,Y) :- catch(number_chars(X, ['1','a','0']), error(syntax_error,_), fail).
catch_test(7, Y) :- log_nl, log( 'Should now write h1'),
                    log_nl, catch(g7_8, C, log(h1)), log_nl.
catch_test(8,Y) :-  catch(coo(X), Y, true).

report_uncaught(B) :- log( 'Uncaught error: '), log( B), log_nl.

catch_tests :-
	test_true(catch_test(1,Y1)),
	test_true(catch_test(2,Y2)),
        test_true(catch_test(3,Y3)),
	test_true(catch_test(4,Y4)),
        test_true(catch_test(5,Y5)),
	test_true(catch_test(6,Y6)),
        test_true(catch_test(7,Y7)),
	test_true(catch_test(8,Y8)).



% :- multifile(validate/0).
validate :- test_78.

test_78 :- 

	log( 'Starting tests for Section 7.8'), log_nl,
	log_nl, log( 'Beginning tests of common functions.'), log_nl, log_nl,

	test_common,

	log_nl, log('Beginning tests of call.'), log_nl,

	test_call,
	
	log_nl, log( 'Tests of call are completed, beginning tests of cut.'), log_nl,
	
	test_cut,

	log_nl, log( 'Tests of cut completed.'),
        log_nl , 
        log( 'Testing catch and throw'), log_nl, log_nl,
        catch_tests,
        log_nl,
        log( 'Tests of catch and throw completed'),  log_nl, 
        log_nl,
        log_nl , log( 'All testing completed for Section 7.8.'),
        log_nl, ! .
