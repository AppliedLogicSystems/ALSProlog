%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%     Directive tests.                         %
%     File: sec74.pro                          %
%                                              %
%	1:22PM  8/1/1996                       %
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
%   Required directives are:                   %
%                                              %
%    dynamic/1, multifile/1, discontiguous/1,  %
%   op/3, char_conversion/2, initialization/1, %
%   include/1, ensure_loaded/1 and             %
%   set_prolog_flag/2                          %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- resume_log('valid.txt').     %  These are needed to capture errors thrown by the directives.

:- dynamic(d7_4/1).
:- multifile(mf7_4/1).     % other clauses in direct2.pro
:- discontiguous(dis7_4/1).
:- op(25, xfx, and).
:- char_conversion('&', '%').
:- initialization(go7_4).
:- include('include').  % contains the predicate included(here).
:- ensure_loaded('include').       % error_test/2
:- set_prolog_flag(unknown, error).

% :- halt_log(log).

d7_4(d1).

dis7_4(dis1).
dis7_4_1(disa1).

mf7_4(mf2).

A and B :- A, B.

dis7_4(dis2).
dis7_4_1(disa2).     % should not compile.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   test of dynamic.
%

test_dyn :-
	assertz(d7_4(d2)),
        error_test(assertz(dis7_4(3)), permission_error(modify, static_procedure, dis7_4)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   test of multifile.
%

test_multi :-
	test_val(setof(X, mf7_4(X), S), S, [mf1, mf2]).

test_op :-
	test_val((d7_4(X) and dis7_4(Y)), [X,Y], [d1, dis1]).

test_char_conv :-
	test_val(char_conversion('&', X), X, '%').
test_flag :-
	error_test(unknown, undefined_predicate).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   perform the tests.
%

go_74:-  log( 'Testing directives Section 7.4'),
      log_nl,
      test_dyn,
      test_multi,
      test_op,
      test_char_conv,
      test_flag,
      log( 'Done testing section 7.4.'),
      log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   validate is included in case initialize/1 does not work.
%  Note that is has to be written to run just once.
% 
:- multifile(validate/0).
validate :-  test_74.

test_74 :-   
      log( 'Testing directives Section 7.4'),
      log_nl,
      reconsult(direct2),
      test_dyn,
      test_multi,
      test_op,
      test_char_conv,
      test_flag,
      log( 'Done testing section 7.4.'),
      log_nl, !.	 
