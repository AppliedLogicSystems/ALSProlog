%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  sec811.pro                                  %
%                      19 January 1996         %
%                                              %
%   Stream selection and control               %
%   tests for the predicates                   %
%   current__input/1, curretn_output/1,        %
%   set_input/1, set_output/1,                 %
%   open/3, open/4, close/1, close/2,          %
%   flush_output/1, flush_output/0,            %
%   stream_property/2,                         %
%   at_end_of_stream/0, at_end_of_stream/1,    %
%   set_stream_position/2                      %
%                                              %   
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
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


%:- multifile(validate/0).

validate :- test_811.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   The files 'charfile.txt' and 'termfile.txt'
%   are assumed to exist. 
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   tests of current_input that should not throw errors
%  
%   Since this requires opening other files the tests assume that
%   open/3 works. If it doesn't then those parts
%   of the test will throw errors.
%

test_current_input :-
	test_true(current_input(Stdin)),
        current_input(Stdin),
        open('charfile.txt', read, S),
        set_input(S),
        test_true(current_input(S)),
        test_false(current_input(Stdin)),
        get_char(_),
        close(S),
        test_true(current_input(Stdin)),
        test_false((current_output(Stdout), current_input(Stdout))).
        




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of current_input that should throw errors
%
%  


test_current_input_errors :-
	error_test(current_input(fred), domain_error(stream, fred)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   tests of current_output that should not throw errors
%  
%   Since this requires opening other files the tests assume that
%   open/3 works. If it doesn't then those parts
%   of the test will throw errors.
%

test_current_output :-
	test_true(current_output(Stdout)),
        current_output(Stdout),
        open('outchar.txt', write, S),
        set_output(S),
        test_true(current_output(S)),
        test_false(current_output(Stdout)),
        put_char('a'),
        close(S),
        test_true(current_output(Stdout)),
        test_false((current_input(Stdin), current_output(Stdin))) .
        




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of current_output that should throw errors
%
%  


test_current_output_errors :-
	error_test(current_output(fred), domain_error(stream, fred)).
        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   tests of open/4 that should not throw errors.
%   these do little more than opne read or write and then close
%    
%

test_open :-
	test_true((
                   open('charfile.txt', read, S1, [type(text)]),
                   get_char(S1,X),
                   close(S1)
                  )),
	test_true((
                   open('charfile.txt', read, S2, [type(binary)]),
                   get_code(S2,Y),
                   close(S2)
                  )),
        test_true((
                   open('charfile.txt', read, S3, [type(text), alias(fred)]),
                   get_char(fred, X),
                   close(fred)
                  )).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of open/3 and  open/4 that should throw errors
%
%

test_open_errors :-
	error_test(open('notafile.txt', read, A1),
                   existence_error(source_sink, 'notafile.txt')),
        error_test(open('charfile.txt', read/write, A2),
                   type_error(atom, read/write)),
        error_test(open(X, append,A3), instantiation_error),
        error_test(open('charfile.txt', read, A4, [type(text) | _]),
                   instantiation_error),
        error_test(open('charfile.txt', read, A5, [type(text), eof(action)]),
                   domain_error(stream_option, eof(action))).
 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    tests of close/1 and close/2 that should not throw errors
%  only the actions not already used with open are tried.
%

test_close :-
	test_true((
	          current_output(Stdout),
                  close(Stdout)
                 )),
        test_true((
                 current_output(S1),
                 close(S1, [force(false)])
                  )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of close/1 and close/2 that should throw errors
%
%

test_close_errors :-
	error_test((current_output(Stdout),close(Stdout, notalist)),
                   type_error(list, notalist)),
        error_test(close(not/stream/alias),
                 domain_error(stream_or_alias, not/stream/alias)), 
        error_test(close(fred), existence_error(stream, fred)),
        error_test(close(A), instantiation_error).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   tests of flush_output/0 and /1 that should not raise errors
%
%   These just test to see if the prdicates are present
%


test_flush_output :-
	test_true(flush_output),
        test_true((
                   open('validn.out', append, S),
                   write(S,'testing flush_output'),
                   flush_output(S),
                   close(S)
                 )).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   tests of flush_output/0 and /1 that should  raise errors
%
%

test_flush_output_errors :-
	error_test(flush_output(X), instantiation_error),
        error_test(
                    (current_input(S), flush_output(S)),
                    permission_error(output, stream, S)
                  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests of stream property that should not raise errors.
%
%

test_stream_property :-
	test_true(stream_property(S, output)),    % only care that it succeeds
        open('charfile.txt', read, S1),
        open('validn.out', append, S2),
        test_true((stream_property(S1, file_name('charfile.txt')),
                   stream_property(S2, file_name('validn.out'))
                 )), 
        close(S1), close(S2).           
                 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests of stream property that should raise errors.
%
%

test_stream_property_errors :-
	error_test(stream_property(not/stream, Y), domain_error(stream, not/stream)),
        error_test(stream_property(S,not_a_stream_property), 
                   domain_error(stream_property, not_a_stream_property)).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests  of at_end_of_stream/0,1 that should not raise errors
%

test_at_end_of_stream :-
        open('charfile.txt', read, S3),
        get_char(S3, X),
        test_false((
                    at_end_of_stream(S3)
                   )), 
        close(S3).
               




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests  of at_end_of_stream/0,1 that should raise errors
%
  
test_at_end_of_stream_errors :-
		error_test(at_end_of_stream(not/stream), 
                 domain_error(stream_or_alias, not/stream)).     
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of set_stream_position/2 that should not raise errors
%
%

test_set_stream_position :-
	open('charfile.txt', read, S, [reposition(true)]),
        stream_property(S, position(Start)),
        get_char(S,X), get_char(S,Y),
        test_true(set_stream_position(S, Start)),
        test_val(get_char(S,Z), Z, X),
        close(S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of set_stream_position/2 that should  raise errors
%
%
 
test_set_stream_position_errors :-
	open('charfile.txt', read, S, [reposition(false)]),
        error_test(set_stream_position(S, end_of_stream(at)),
                   permission_error(reposition, stream, S)),
        close(S),
	open('charfile.txt', read, S1, [reposition(true)]),
        stream_property(S1, position(Start)),
        error_test(set_stream_position(X, Start), 
                   instantiation_error),
        error_test(set_stream_position(S1, Any),
                   instantiation_error),
         close(S1).

     
        
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  run all the tests.
%

test_811 :-
        log_nl, log( 'testing at_end_of_stream'), log_nl,
	test_eos,
        log_nl, log( 'testing at_end_of_stream done '), log_nl,
        log_nl, log( 'testing current_input'), log_nl,
	test_cip,
        log_nl, log( 'testing current_input done '), log_nl,
        log_nl, log( 'testing current_output'), log_nl,
	test_cop,
        log_nl, log( 'testing current_output done '), log_nl,
        log_nl, log( 'testing open'), log_nl,
	test_ope,
        log_nl, log( 'testing open done '), log_nl,
        log_nl, log( 'testing close'), log_nl,
	test_cl,
        log_nl, log( 'testing close done '), log_nl,
        log_nl, log( 'testing flush_output'), log_nl,
	test_fl_out,
        log_nl, log( 'testing flush_output done '), log_nl,
        log_nl, log( 'testing stream_property'), log_nl,
	test_sp,
        log_nl, log( 'testing stream_property done '), log_nl,
        log_nl, log( 'testing set_stream_position'), log_nl,
	test_ssp,
        log_nl, log( 'testing set_stream_position done '), log_nl,
        log( 'testing section 8.11 done'), log_nl,log_nl, !. 

test_cip :- 
  
   defined(current_input/1), !, 
     
      test_current_input, test_current_input_errors.
test_cip:-
     log_nl, log( 'current_input/1 not supported'), log_nl.
    
      
test_cop:-
    defined(current_output/1), !, 
    test_current_output, test_current_output_errors.

test_cop:- 
    log_nl, log( 'current_output/1 not supported'), log_nl.
   

test_ope :- 
   defined(open/3), !,
   test_open,test_open_errors.

test_ope :-
    log_nl, log( 'open/3 not supported'), log_nl.

test_cl :- 
    defined(close/1), !,
    test_close,test_close_errors.

test_cl :-
    log_nl, log( 'close/1 not supported'), log_nl.

test_fl_out :-
    defined(flush_output/1), !, 
     test_flush_output, test_flush_output_errors.

test_fl_out:-
    log_nl, log( 'flush_output/1 not supported'), log_nl.

test_sp :-
   defined(stream_property/2)
    -> 
   (test_stream_property, test_stream_property_errors)
    ;
    (log_nl, log( 'stream_property/2 not supported'), log_nl).

% can't test for end_of_stream being defined 
%  in the usual way since it will try to read from Current input

test_eos :-
     catch((at_end_of_stream(not/alias);true), B, error_is_not(existence_error,B)),
     test_at_end_of_stream,test_at_end_of_stream_errors.

test_eos:-     
     log_nl, log( 'at_end_of_stream not supported'), log_nl.

test_ssp :-
    defined(set_stream_position/2)
    ->
     (test_set_stream_position, test_set_stream_position_errors).
  
