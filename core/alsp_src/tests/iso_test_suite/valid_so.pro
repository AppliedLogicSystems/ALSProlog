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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    load all the tests.
%
% each of these contains a clause for the multifile
%  predicate validate/0.
%

init_files :-
	consult('utils_so.pro'),
	consult('sec74.pro'),
	consult('sec78.pro'),
	consult('sec82.pro'),
	consult('sec83.pro'),
	consult('sec84.pro'),
	consult('sec85.pro'),
	consult('sec86.pro'),
	consult('sec87.pro'),
	consult('sec88.pro'),
	consult('sec89.pro'),
	consult('sec810.pro'),
	consult('sec811.pro'),
	consult('sec812.pro'),
	consult('sec813.pro'),
	consult('sec814.pro'),
	consult('sec815.pro'),
	consult('sec816.pro'),

	consult('sec91.pro'),
	consult('maxint.pro'),
	consult('sec93.pro'),
	consult('sec94.pro').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  run_all_tests
%
%

run_all_tests :-
	init_files,
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
        
	
