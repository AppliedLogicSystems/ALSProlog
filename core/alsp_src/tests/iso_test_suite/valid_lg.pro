%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%   valid_lg.pro harness to run all the tests  %
%   the necessary files are loaded by this     %
%   version.    Results are written to a log   %
%   file 'logfile.txt'.                        %
%                                              %
%	Date 3:13PM  29/3/1996                 %           
%                                              %
%   May be used freely provided                %
%   acknowledgement is made.                   %
%                                              %
%   Thanks to Ken Bowen of ALS for support,    %
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- reconsult(utils_lg).


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
%:- reconsult(maxint).
:- reconsult(sec93).
:- reconsult(sec94).


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
        
	
