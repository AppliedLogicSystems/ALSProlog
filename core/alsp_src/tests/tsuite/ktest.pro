/* Kernal Validation Suite - all changes made for the kernal are marked with "KERNAL" */


'$start' :- run_all_tests.

/* KERNAL: provide primitive output */

write(X) :- pbi_debug(X).
nl.

abolish(P/A) :- abolish(P, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%    harness to run all the tests              %
%   the necessary files are loaded by this     %
%   version.   Results are sent to the         %
%   standard output.                           %
%                                              %
%	Date 3:13PM  29/3/1996                 %           
%                                              %
%   May be used freely provided                %
%   acknowledgement is made.                   %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%   and to Joe Pedano and John Hallat of       %
%   Saint Joseph's for their work on this      %
%   project.                                   %
%                                              %
%   To run the tests load this file and        %
%   then type  'run_all_tests' at the  ?-      %
%   prompts. The files                         %
%   chario.txt, charfile.txt, codeio.txt       %
%   termfile.txt and direct2.pro must be       %
%   in the current directory.                  %
%                                              %
%    Last changed:  5 April 1996.              % 
%                                              %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* KERNAL: The kernal can't consult.

:- reconsult(utils_so).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    load all the tests.
%
% each of these contains a clause for the multifile
%  predicate validate/0.
%

:- reconsult(sec74).
:- reconsult(sec78).
:- reconsult(sec82).
:- reconsult(sec83).
:- reconsult(sec84).
:- reconsult(sec85).
:- reconsult(sec86).
:- reconsult(sec87).
:- reconsult(sec88).
:- reconsult(sec89).
:- reconsult(sec810).
:- reconsult(sec811).
:- reconsult(sec812).
:- reconsult(sec813).
:- reconsult(sec814).
:- reconsult(sec815).
:- reconsult(sec816).

:- reconsult(sec91).
% :- reconsult(maxint).   Contians tests that use the flag maxint.
:- reconsult(sec93).
:- reconsult(sec94).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  run_all_tests
%
%

run_all_tests :-
         start_log,
        all_tests,
        end_log.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Failure driven loop to test each section.
%

all_tests :-
	validate,
        fail.
all_tests.
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  Various utility predicates                  %
%  for validation suite.                       %
%                                              %
%  Author  J.P.E. Hodgson                      %
%       Started  19/12/1995                    %
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
%   All messages are written to a the standard %
%   output.                                    %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* KERNAL: Don't need this since this is the default. */
%% :- set_prolog_flag(undefined_predicate, error).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	report_list(+Reason, +List)
%
%  write a list of exceptions 
%

report_list(Reason, []) :-
	log( Reason), log_nl.  

report_list(Reason, List) :-
	log(Reason), log_nl,
      pp_list(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	pp_list(+List) pretty print a list
%

pp_list([]) :-log_nl.
pp_list([H|T]) :-
      log( '     '), log(H),log_nl,
      pp_list(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    make_list(+Length, -List)
%
%  make a list of length Length.
%

make_list(Len, List) :-
       make_list(Len, [], List).

make_list(0, L,L).
make_list(N, Sofar, List) :-
     N > 0, !,
     N1 is N - 1,
     make_list(N1, [X| Sofar], List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     
%      defined(+Predicate/+Arity)
%
%   in case the predicate does not exist
%   replaces current_predicate/1
%


defined(P/A) :-
	make_list(A, L),
        G =.. [P|L],			% bogus goal
	catch((G;true), B, error_is_not(existence_error,B)).  % (G;true) succeeds if G does
                                                              % not throw an error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  error_is_not(+Error, +Thrown)
%  

error_is_not(Error, Thrown) :-
	Thrown =.. [error, StdError | _],
        ( StdError =.. [existence_error |_]  -> fail ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   format for testing a predicate
%
%
%	test_predicate :- defined(Predicate, Arity),
%                         do_tests.
%	test_predicate :- log('The predicate '), log(Predicate),
%                         log(' with arity '), log(Arity), log_nl,
%                         log('is not supported.'), log_nl, log_nl.
%	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	error_test(+Goal, +ExpectedError)
%
%   Calls a copy of Goal and if the ExpectedError
%   is not thrown reports the difference
%
%   ExpectedError contains only the standard required specification
%   so that the thrown error is to be
%       error(ExpectedError, ImpDef).
% 
%   the metacall try(:GCopy, +Goal) reports an unexpected success or failure
%   of a goal that should produce an error.
%


error_test(Goal, ExpectedError) :-
	copy_term(Goal, G),
        copy_term(ExpectedError, E),
	catch(catch(try(G, Goal), error(E,Impdef), true), 
              Thrown, 
              report_difference(Goal, ExpectedError, Thrown)
             ).



try(G, Goal) :-
	call(G), log( 'Unexpected success rather than error in: '), log(Goal),log_nl
        ;
/* KERNAL: Don't report these because its just how ALS Prolog works! */
        %%log('Unexpected failure rather than error in: '),
        %%log(Goal),log_nl.
        true.
	

report_difference(Goal, Expected, Actual):-
	log('Goal: '), log(Goal), log(' attempted.'), 
       log_nl,
        log('Standard Part of Expected Error: '), 
        log( error(Expected,ImpDef)),log_nl,
	log( 'Error Thrown: '), log( Actual),log_nl, 
        log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   test_val(+Goal, -Arg, +Expected_Val)
%
%
%	Test the goal G with "output" Arg to check for
%  Expected value

test_val(Goal, Arg, Expected) :-
       copy_term(Goal/Arg, G/A), 
	catch( (call(G),
                   ( A = Expected -> true
                    ;
                    log( 'Goal: '), log( Goal),log_nl,
                    log( ' gave unexpected value: '), log(  A),   
                   log_nl,
                    log(' expected value: '), log(Expected),  
                   log_nl
                   )
                 ), 
                 B, 
                report_error(Goal,B)
             ), !.
test_val(Goal, Arg, _) :-
	log( 'Unexpected failure of the goal '),
        log( Goal),
        log_nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   
% test_true(+Goal)     % writes if Goal fails
% test_false(+Goal)    % writes if goal succeeds
% 
%

test_true(G) :- copy_term(G,G1), 
                catch(call(G1), 
                      B, 
                      report_error(G,B)
                      ), 
                !.
test_true(G)  :- log( 'Goal: '), log( G),log_nl,
		log( 'failed, when it should have succeeded.'),
               log_nl.

test_false(G) :- copy_term(G,G1), 
                 catch( (call(G1), log( 'Goal:  '), 
                         log(G),log_nl,
	                 log('succeeded when it should fail.'),
                        log_nl),
                        B,
                        report_error(G,B)
                      ), 
                 !.
test_false(G).



report_error(G,B) :-
	log( 'Unexpected error:'), log(B),log_nl,
        log( 'raised by goal: '), log( G), log_nl,
        log( 'that should not raise an error'),
       log_nl, log_nl.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     resume_log(+LogFile) 
%
%   send standard out to the log file with alias log.
%

resume_log(LogFile) :-
	open(Logfile, append, S, [alias(log)]),
        set_output(log).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%      halt_log(+LogFile)
%
%   set output back to standard out 

halt_log(LogFile) :- close(log).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% Open logfile  for consistency.
%

start_log.

end_log.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Log results to standard out..
%

log(Text) :-
	write(Text).

log_nl :-  nl.

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

/* KERNAL - various fixes. */
test_call :-

	defined(call/1),!,
	test_true(call(!)),
	test_false(call(fail)),
	test_false(call((fail,X))),
	test_false(call((fail,call(1)))),

	error_test(cut_b(_),instantiation_error),
	error_test(cut_b(3),type_error(callable,3)),

	test_true((Z=!,call((Z=!,cut_a(X),Z)))),
	test_true(call((Z=!,cut_a(X),Z))),
	
	error_test(call((write(3), X)),instantiation_error),
	error_test(call((write(3),call(1))),type_error(callable,1)),
	error_test(call(X),instantiation_error),
	error_test(call(1),type_error(callable,1)),
	error_test(call((fail,1)),type_error(callable,(fail,1))),
	error_test(call((write(3),1)),type_error(callable,(write(3),1))),
	error_test(call((1;true)),type_error(callable,(1;true))).

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
/* KERNAL: The kernal doesn't have number_chars. */
	%%test_true(catch_test(6,Y6)),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  Section 8.5 of the ISO Prolog Standard      %
%                                              %
%    Term creation and decomposition           %
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
	test_85.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    Heavily tested functional for copy_term   %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	object(atom).
	object([atom1, atom2, atom3]).
	object(3).
	object(2.71828).
	object(f(a,X)).
	object(f(X,b)).
	object([1,2,3]).
	object([X,Y,Z]).
	object(pred/arity).
	object(pred/N).
	object(X/Y).
	object(a(X,b(Y,Z))).


	copy_term_test1(X) :-
		object(X),
		copy_obj(X).

	copy_obj(X) :- 
		copy_term(X,Y),!,
		verify_copy(X,Y).

	copy_obj(X) :-
		copy_failed(X).


	verify_copy(X,Y) :- 
		X = Y, !.

	verify_copy(X,Y) :- 
		log( 'Term '), log( X), 
		log( ' copy_termed to inequivalent term '),
		write(Y), log_nl.

	copy_failed(X) :-
		log('Term '), log(X),
		log( ' not copied.'), log_nl.

	copy_failed(X,Y) :-
		X = Y, !,
		log( 'Term '), log( Y),
		log( ' not recognised as a copy of '),
		log( X), log_nl.
		
	copy_term_test2(X,Y) :-
		object(X), object(Y),
		copy_obj(X,Y).

	copy_obj(X,Y) :-
		copy_term(X,Y),!,
		verify_copy(X,Y).

	copy_obj(X,Y) :-
		copy_failed(X,Y).


test(1) :-
	copy_term_test1(X),
	fail.
test(1).

test(2) :-
	copy_term_test2(X,Y),
	fail.
test(2).

test_copy_term :-
	catch( (test(1),test(2)), B, unexpected(B)).

unexpected(B) :-
	log( 'Unexpected error '), log(B), 
	log( ' raised.'), log_nl.

test_errors :-
	
	error_test((functor(X,Y,3)), instantiation_error),
	error_test((functor(X,foo,N)), instantiation_error),
	error_test((functor(X,foo,a)), type_error(integer,a)),
	error_test((functor(F,1.5,1)), type_error(atom,1.5)),
	error_test((functor(F,foo(a),1)), type_error(atomic,foo(a))),
	error_test((arg(X,foo(a,b),a)), instantiation_error),
	error_test((arg(1,X,a)), instantiation_error),
	error_test((arg(0,atom,A)), type_error(compound,atom)),
	error_test((arg(0,3,A)), type_error(compound,3)),

	error_test((X=..Y), instantiation_error),
	error_test((X=..[foo,a|Y]), instantiation_error),
	error_test((X=..[foo|bar]), type_error(list,[foo|bar])),
	error_test((X=..[Foo,bar]), instantiation_error),
	error_test((X=..[3,1]), type_error(atom,3)),
	error_test((X=..[1.1,foo]), type_error(atom,1.1)),
	error_test((X=..[a(b),1]), type_error(atom,a(b))),
	error_test((X=..4), type_error(list,4)).

test_functor :-
	
	test_true((functor(foo(a,b,c),foo,3))),
	test_true((functor(foo(a,b,c),X,Y))),
	test_true((functor(X,foo,3))),
	test_true((functor(X,foo,0))),
	test_true((functor(mats(A,B), A, B))),
	test_false((functor(foo(a), foo, 2))),
	test_false((functor(foo(a), fo, 1))),
	test_true((functor(1, X, Y))),
	test_true((functor(X, 1.1, 0))),
	test_true((functor([_|_], '.', 2))),
	test_true((functor([], [], 0))).

test_arg :-

	test_true((arg(1, foo(a,b), a))),
	test_true((arg(1, foo(a,b), X))),
	test_true((arg(1, foo(X,b), a))),
	test_true((arg(1, foo(X,b), Y))),
	test_false((arg(1, foo(a,b), b))),
	test_false((arg(0, foo(a,b), foo))),
	test_false((arg(3, foo(3,4), N))).

test_univ :-

	test_true((foo(a,b) =.. [foo,a,b])),
	test_true((X =.. [foo, a, b])),
	test_true((foo(a,b) =.. L)),
	test_true((foo(X,b) =.. [foo, a, Y])),
	test_true((1 =.. [1])),
	test_false((foo(a,b) =.. [foo, b, a])).

test_copyterm :-

	test_true((copy_term(X,Y))),
	test_true((copy_term(X,3))),
	test_true((copy_term(_,a))),
	test_true((copy_term(a+X,X+b))),
	test_true((copy_term(_,_))),
	test_true((copy_term(X+X+Y,A+B+B))),
	test_false((copy_term(a,b))),
	test_false((copy_term(a+X,X+b),copy_term(a+X,X+b))).

test_85:-

	log('Starting tests for Section 8.5'), log_nl,
	log_nl, log('Test starting for functor.'), 
        log_nl, log_nl,

	test_functor,

	log_nl, 
        log( 'Test of functor finished, starting test of arg'),
        log_nl,

	test_arg,

	log_nl, 
        log( 'Test of arg finished, starting test of univ'), 
        log_nl, 

	test_univ,

	log_nl, 
        log( 'Test of univ finished, starting test of copy_term'),
        log_nl,

	test_copyterm,

	log_nl, 
        log('Testing copy_term more extensively.'), 
        log_nl,

	test_copy_term,

	log_nl, 
        log('Tests of copyterm completed, testing errors secction 8.5'), 
        log_nl, log_nl,

	test_errors,

	log_nl, 
        log( 'Testing of error catching completed.'), log_nl,
	log_nl, log('All tests finished for Section 8.5.'), 
        log_nl, log_nl, !.


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  Section 8.8 of the ISO Prolog Standard      %
%                                              %
%  Tests of clause/2 and current_predicate/1   %
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
validate :- test_88.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    Database for tests. Taken from 8.8 and    %
%    8.9 of the standard.                      %
%    the standard ISO/IEC 13211-1              %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(cat/0).
cat.

:- dynamic(dog/0).
dog :- true.

elk(X) :- moose(X).

:- dynamic(legs/2).



legs(A, 6) :- insect(A).
legs(A, 7) :- A, call(A).
:- dynamic(insect/1).
insect(ant).
insect(bee).

:- dynamic(foo/1).
foo(X) :- call(X), call(X).
foo(X) :- call(X) -> call(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_clause  :- 
	
	test_true((clause(cat, true))),
	test_true((clause(dog, true))),
	test_val(clause(legs(I,6), Body), Body, insect(I)),
	test_val(clause(legs(C,7), Body1), Body1, (call(C), call(C))),
	test_true(clause(insect(J), T)),
	test_false(clause(x, Body2)).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_clause_errors :-
	defined(clause/2),
	error_test(clause(_,B), instantiation_error),
	error_test(clause(4,X), type_error(callable, 4)),
	error_test(clause(elk(N), Body), 
		   permission_error(access, private_procedure, elk/1)).
	error_test(clause(atom(_), Body), 
		   permission_error(access, private_procedure, atom/1)).
test_clause_errors :-
         log_nl, log( 'clause/2 not supported.'),
         log_nl, log_nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_current_predicate :-
        defined(current_predicate/1),
	test_true(current_predicate(dog/0)),
	test_false((current_predicate(current_predicate/1))),
	test_true((current_predicate(elk/Arity))),
	test_false((current_predicate(foo/A))),
	test_true((current_predicate(Name/1))),
	error_test(current_predicate(4), type_error(predicate_indicator, 4)).

test_current_predicate :-
	log_nl, log( 'current_predicate/1 not supported.'),
	log_nl, log_nl.

test_88:-

	log('Starting tests for Section 8.8.'), log_nl,
	log_nl, log( 'Testing clause/1.'), log_nl,

	test_clause,

	log_nl, log( 'Testing of clause finished, checking error values.'), 
        log_nl,

	test_clause_errors,

	log_nl, 
        log( 'Testing of error values finished, starting current_predicate tests.'),
        log_nl,
	
	test_current_predicate,

	log( 'Testing completed of current_predicate.'), log_nl,
	log_nl, log( 'All testing completed for Section 8.8.'), 
        log_nl, log_nl, !.
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  Section 8.9 of the ISO Prolog Standard      %
%                                              %
%  Clause creation and destruction             %
%                                              %
%  Tests for asserta/1, assertz/1,             %
%   retract/1, abolish/1                       %
%   ALS supports abolish/2                     %
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

%   assumes the same database as sec8.8

%:- dynamic(legs/2).

%legs(A,6) :- insect(A).
%legs(A,7) :- A, call(A).

%:- dynamic(insect/1).
%insect(ant).
%insect(bee).

:- dynamic(foo/1).

foo(X) :- call(X) -> call(X).


%:- multifile(validate/0).
validate :- test_89.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_asserta :-
	defined(asserta/1),
	test_true(asserta(legs(octopus,8))),
	test_true(asserta((legs(A,4) :- animal(A)))),        
	test_true(asserta((foo(X) :- X, call(X)))),
        test_true(asserta(new_pred(bar))).
test_asserta :-
	log_nl, log( 'asserta/1 not supported'),
        log_nl.

verify_assertas :-
        defined(asserta/1),
	test_true(legs(octopus, 8)),
        test_true(new_pred(bar)),
        test_val(
                  (clause(legs(A, X), animal(A))), 
                  X, 
                  4).

verify_assertas.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_asserta_errors :-
	defined(asserta/1),
	error_test(asserta(_), type_error(instantiation_error)),
	error_test(asserta(4), type_error(callable, 4)),
	error_test(asserta((foo:-4)), type_error(callable,4)),
	error_test(asserta((atom(_) :- true)), 
		permission_error(modify, static_procedure, atom/1)).
test_asserta_errors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_assertz :-
	defined(assertz/1),
	test_true(assertz(legs(stool,3))),
	test_true(assertz((legs(B,2) :- bird(B)))),
	test_true(assertz((foo(X) :- X, call(X)))),
        test_true(assertz(another_new_pred(bar))).
test_assertz :-
	log_nl, log( 'assertz/1 not supported'),
        log_nl.
verify_assertzs :-
        defined(assertz/1),
	test_true(legs(stool, 3)),
        test_true(another_new_pred(bar)),
        test_val(
                  (clause(legs(B, X), bird(B))), 
                  X, 
                  2).
verify_assertz.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_assertz_errors :-
	defined(assertz/1),
	error_test(assertz(_), instantiation_error),
	error_test(assertz(4), type_error(callable, 4)),
	error_test(assertz((foo:-4)), type_error(callable, 4)),
	error_test(assertz((atom(_) :- true)), 
	    permission_error(modify, static_procedure, atom/1)).
test_assertz_errors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_retract :-
	defined(retract/1),
	test_true(retract(legs(octopus,8))),
	test_false(retract(legs(spider,6))),
	test_true(retract((legs(X,2) :- T))),
	test_true(retract((legs(X,Y) :- Z))),
	test_false(retract((legs(X,Y) :- Z))),
	test_true(retract((foo(C) :- A -> B))).
test_retract :-
	log_nl, log( 'retract/1 not supported'),
        log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_retract_errors :-
	defined(retract/1),
	error_test(retract((X :- in_eec(Y))), instantiation_error),
	error_test(retract((4 :- X)), type_error(callable, 4)),
	error_test(retract((atom(X) :- X == '[]')), 
	    permission_error(modify, static_procedure, atom/1)).

test_retract_errors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_abolish :-
	defined(abolish/1),
	test_true(abolish(foo/2)).

test_abolish :-
        log_nl, log( 'abolish/1 not supported'),
        log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_abolish_errors :-
	defined(aboilsh/1),
	error_test(abolish(foo/_), instantiation_error),
	error_test(abolish(foo), type_error(predicate_indicator, foo)),
	error_test(abolish(foo(_)), 
		type_error(predicate_indicator, foo(_))),
	error_test(abolish(abolish/1), 
	    permission_error(modify, static_procedure, abolish/1)).

test_abolish_errors.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_89 :-
	
	log_nl, log( 'Starting tests for Section 8.9.'), log_nl,
	log_nl, log( 'Starting tests of asserta.'), log_nl,

	test_asserta,

	log_nl, log( 'Tests of asserta completed, testing asserta errors.'), log_nl,

	test_asserta_errors,

	log_nl, log( 'Tests of asserta errors completed, testing assertz.'), log_nl,

	test_assertz,

	log_nl, log( 'Tests of assertz completed, testing assertz errors.'), log_nl,

	test_assertz_errors,

	log_nl, log( 'Tests of assertz errors completed, testing retract.'), log_nl,

	test_retract,
	
	log_nl, log( 'Tests of retract completed, testing retract errors.'), log_nl,

	test_retract_errors,

	log_nl, log('Tests of retract errors completed, testing abolish.'), 
        log_nl,

	test_abolish,

	log_nl, 
        log('Tests of abolish completed, testing abolish errors.'), 
        log_nl,

	test_abolish_errors,

	log_nl, log( 'Tests of abolish errors completed.'), log_nl, 
	log_nl, log( 'All testing completed for Section 8.9.'), 
        log_nl, log_nl, !.


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
       
	
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   sec817.pro                                 %
%                                              %
%  code to validate prolog flags.              %
%                                              %
%   Started 10:12AM  20/12/1995                %
%                                              %
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
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   The standard requires the following        %
%   flags                                      %
%                                              %
%   bounded                                    %
%     values:   true or false                  %
%     default: implementation defined          %
%     determines whether integers              %
%	are bounded or not.  not changeable    %
%                                              %	
%   max_integer                                %
%     default: implementation defined          %
%      not changeable                          %
%                                              %
%   min_integer                                %
%     default: implementation defined          %
%      not changeable                          %
%                                              %
%   integer_rounding_function                  %
%      values: down, toward_zero               %
%      default: implementation defined         %
%      not changeable                          %
%                                              %
%   char_conversion                            %
%      values  on, off                         %
%      default on                              %
%      changeable                              %
%                                              %
%   debug                                      %
%     values on, off                           %
%     default off                              %
%     changeable                               %
%                                              %
%   max_arity                                  %
%      default implementation defined          %
%      not changeable                          %
%                                              %
%   flag_unknown                               %
%      values  error, fail, warning            %
%      default error                           %
%      changeable                              %
%                                              %
%   double_quotes                              %
%      values: chars, codes, atom              %
%      default implmentation defined           %
%      changeable                              %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   flags are manipulated by  two predicates   %
%                                              %
%   set_prolog_flag(+Flag, @Value)             %
%   current_prolog_flag(?Flag, ?Value)         %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

required_flags([bounded, max_integer, min_integer, 
                integer_rounding_function, char_conversion,
                debug, max_arity, unknown, double_quotes]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Test to see which flags are present
%   and which are aditional
%

/* KERNAL: collect_flags replace the call to setof below. */

find_flags :-
      /* KERNAL: setof is not in the kernal, so do it with a recusive function. */
      %%setof(Flag, Val ^ current_prolog_flag(Flag, Val), Flags),
      fail,
      required_flags(Rqd),
      log('Implemented flags:' ), log(Flags),log_nl,
      log('Required Flags: ' ), log(Rqd), log_nl .
      
find_flags :-
	log('No flags implemented'), log_nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  test of halt/1 
%


test_halt1 :-
	test_true(halt(7)).


% multifile(validate/0).

validate :-
	test_817.


test_817 :-
        log('testing flags'), log_nl,
	find_flags,
        log_nl,
        /* KERNAL: don't test halt right now
        log('testing  halt/1'), log_nl,
        test_halt1,
        log_nl,
        */
        log('All tests completed').
        
       
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



% :- multifile(validate/0).

validate :-
	test_91.

test_91:-
     	log_nl, log( 'Testing Arithmetic Operations + and -'), log_nl,
     
	test_val(X1 is +(7,35),X1,42),
     	test_val(X2 is +(0,+(3,11)),X2,14),
     	test_val(X3 is +(0,3.2+11),X3,14.2000),
     	error_test(X4 is +(77,N),instantiation_error),
     	error_test(X5 is +(foo,77),type_error),
     	test_val(X6 is -(7),X6,-7),
     	test_val(X7 is -(3-11),X7,8),
     	test_val(X8 is -(3.2-11),X8,7.8000),
     	error_test(X9 is -(N),instantiation_error),
     	error_test(X10 is -(foo),type_error),
     	test_val(X11 is -(7,35),X11,-28),
     	test_val(X12 is -(20,3+11),X12,6),
     	test_val(X13 is -(0,3.2+11),X13,-14.2000),
     	error_test(X14 is -(77,N),instantiation_error),
     	error_test(X15 is -(foo,77),type_error),
     	
	log_nl, log('Done Testing Arithmetic Operations +  and -'), log_nl,
     
       log_nl, log( 'Testing Arithmetic Operations * and /'), log_nl,
     	
	test_val(X16 is *(7,35),X16,245),
     	test_val(X17 is *(0,3+11),X17,0),
     	test_val(X18 is *(1.5,3.2+11),X18,21.3000),
     	error_test(X19 is *(77,N),instantiation_error),
     	error_test(X20 is *(foo,77),type_error),
     	test_val(X21 is /(7,35),X21,0),
     	test_val(X22 is /(7.0,35),X22,0.2000),
     	test_val(X23 is /(140,3+11),X23,10),
     	test_val(X123 is /(140,3+11),X123,10.0),
     	test_val(X24 is /(20.164,3.2+11),X24,14.2000),
     	test_val(X25 is /(7,-3),X25,-2.3333),
     	test_val(X26 is /(-7,3),X26,-2.3333),
     	error_test(X27 is /(77,N),instantiation_error),
     	error_test(X28 is /(foo,77),type_error),
     	error_test(X29 is /(3,0),evaluation_error),

     	log_nl, log( 'Done Testing Arithmatic Operations * and /'), 
        log_nl,
         log_nl, log('Testing Arithmetic Operations mod etc..'), log_nl,

     	test_val(X30 is mod(7,3),X30,1),
     	test_val(X31 is mod(0,3+11),X31,0),
     	test_val(X32 is mod(7,-2),X32,-1),
     	error_test(Y2 is mod(77,N),instantiation_error),
     	error_test(Y3 is mod(foo,77),type_error),
     	error_test(Y4 is mod(7.5,2),type_error),
     	error_test(Y5 is mod(7,0),evaluation_error),
     	test_val(X33 is floor(7.4),X33,7),
     	test_val(X34 is floor(-0.4),X34,-1),
     	test_val(X35 is round(7.5),X35,8),
     	test_val(X36 is round(7.6),X36,8),
     	test_val(X37 is round(-0.6),X37,-1),
     	error_test(X38 is round(N),instantiation_error),
     	test_val(X39 is ceiling(-0.5),X39,0),
     	test_val(X40 is truncate(-0.5),X40,0),
     	error_test(X41 is truncate(foo),type_error),
     	test_val(X42 is float(7),X42,7.0),
     	test_val(X43 is float(7.3),X43,7.3),
     	test_val(X44 is float(5/3),X44,1.0),
     	error_test(X45 is float(N),instantiation_error),
     	error_test(X46 is float(foo),type_error),
     	test_val(X47 is abs(7),X47,7),
     	test_val(X48 is abs(3-11),X48,8),
     	test_val(X49 is abs(3.2-11.0),X49,7.8000),
     	error_test(X50 is abs(N),instantiation_error),
     	error_test(X51 is abs(foo),instantiation_error),
        log_nl, log( 'Done Testing Elementary Arithmetic Operations'), log_nl, 
        log_nl, !.



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
      error_test(X is log(foo), instantiation_error).


test_93 :-
   log_nl, log('testing power  functor'),
   log_nl, log_nl,
   test_pow,
   log('test pwer done, testing trig functors '),
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
   error_test(X4 is foo/\2 ,type_error(integer, foo)).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  test the or function
%

test_or :-
   test_true( X1 is 10  \/ 12),
   test_val(X2 is 10 \/  12,X2,14),
   error_test(X3 is 3 /\ N ,instantiation_error),
   error_test(X4 is foo/\2 ,type_error(integer, foo)).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Test of ones complement
%

test_ones_complement :- 
	test_true(X1 is \10),
        test_val(X2 is \(\10), X2, 10),
        error_test(X3 is \ N ,instantiation_error),
         error_test(X4 is \foo ,type_error(integer, foo)).
   


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
     error_test(X4 is 3<<foo ,type_error(integer, foo)),
     error_test(X3 is N << 4 ,instantiation_error),
     error_test(X4 is foo<< 3 ,type_error(integer, foo)),
     error_test(X3 is 3 >> N ,instantiation_error),
     error_test(X4 is 3>>foo ,type_error(integer, foo)).

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
