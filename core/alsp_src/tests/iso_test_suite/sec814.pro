%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  sec814.pro           6 Feb 1996             %
%                                              %
%     Tests of read_term/3, read_term/2,       %
%   read/1, read/2, write_term/3,              %
%   write_term/2, write/1, write/2,            %
%   writeq/2, writeq/2, write_canonical/2,     %
%   write_canonical/1, op/3, current_op/3,     %
%   char_conversion/2                          %
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
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% :- multifile(validate/0).

validate :- test_814.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Link tests of read and write 
%
%    open a file write to it, close it read back
%

write_read1(TermOut, File, TermIn) :-
	open(File, write, Sout),
        set_output(Sout),
        write(TermOut), write('. '),
        close(Sout),
        open(File, read, Sin),
        set_input(Sin),
        read(TermIn),
        close(Sin).

write_read2(TermOut, File, TermIn) :-
	open(File, write, Sout),
        write(Sout, TermOut), write('. '),
        close(Sout),
        open(File, read, Sin),
        read(Sin, TermIn),
        close(Sin).
%  And now for terms that must be quoted to read.

writeq_read1(TermOut, File, TermIn) :-
	open(File, write, Sout),
        set_output(Sout),
        writeq(TermOut), write(Sout, '. '),
        close(Sout),
        open(File, read, Sin),
        set_input(Sin),
        read(TermIn),
        close(Sin).

writeq_read2(TermOut, File, TermIn) :-
	open(File, write, Sout),
        writeq(Sout, TermOut), write(Sout, '. '),
        close(Sout),
        open(File, read, Sin),
        read(Sin, TermIn),
        close(Sin).

%  Canonical
write_canonical_read1(TermOut, File, TermIn) :-
	open(File, write, Sout),
        set_output(Sout),
        write_canonical(TermOut), write(Sout, '. '),
        close(Sout),
        open(File, read, Sin),
        set_input(Sin),
        read(TermIn),
        close(Sin).

write_canonical_read2(TermOut, File, TermIn) :-
	open(File, write, Sout),
        write_canonical(Sout, TermOut), write(Sout, '. '),
        close(Sout),
        open(File, read, Sin),
        read(Sin, TermIn),
        close(Sin).
    
%%  _term forms

write_read_term2(TermOut, File, TermIn,WriteOptions, ReadOptions) :-
	open(File, write, Sout),
	set_output(Sout),
        write_term(TermOut, WriteOptions), write( '. '),
        close(Sout),
        open(File, read, Sin),
		set_ionput(Sin),
        read_term( TermIn, ReadOptions),
        close(Sin).

write_read_term3(TermOut, File, TermIn, WriteOptions, ReadOptions) :-
	open(File, write, Sout),
        write_term(Sout, TermOut, WriteOptions), write(Sout, '. '),
        close(Sout),
        open(File, read, Sin),
        read_term(Sin, TermIn,ReadOptions),
        close(Sin).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of read_term/1,2,3 write_term/1,2,3 etc that
%     should not throw errors.
%
%

test_read_write_term :-
	test_true(write_read1(foo(bar, baz), 'termio.tmp', foo(bar, baz))),
        test_val(write_canonical_read2(a * b, 'termio.tmp', C), C, *(a,b)),
        test_true(writeq_read1('The Prolog Standard', 'termio.tmp','The Prolog Standard' )),
        test_val(write_read_term3(3 + X, 'termio.tmp', TermIn, 
                                  [ignore_ops(true)], [variables(Varlist)]),
                 TermIn/VarList, +(3,Y)/[Y]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of  read_term/1,2,3 write_term/1,2,3 etc that
%     should  throw errors.
%
%

test_read_write_term_errors :-
	error_test(read(X,Term1), instantiation_error),
        error_test(read(fred, Char2), existence_error(stream, fred)),
        error_test(read(nostream/noalias, Char3), 
                     domain_error(stream_or_alias,nostream/noalias)),
        open('termfile.txt', read, S1, [alias(inputstream)]),
        error_test(write(inputstream, 'a'), permission_error(output, stream, inputstream)),
        open('out.tmp', write, S2, [alias(outputstream)]),
        error_test(read(outputstream, Y), permission_error(input, stream, outputstream)),
        close(S1), close(S2).

   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Tests of op/3 and curent_op/3 that should not produce errors
%
%
%    Write a term to the file, make the new operator, read it back
%

rw_op(Left, Op, Right, ReadIn) :-
	open('opfile.tmp', write, Sout),
        write(Sout, Left), write(Sout, ' '),
		write(Sout, Op), write(Sout, ' '),
		write(Sout, Right), write(Sout, ' .'),
        close(Sout),
        open('opfile.tmp', read, Sin),
        op(30, xfx , Op),
        read(Sin, ReadIn),
        op(0, xfx, Op),		 % restore status quo ante
        close(Sin).


test_op :-
	test_true(rw_op(a, because, b, because(a,b))),
        test_true((op(35, fx, ++), current_op(35, fx, ++))),
        test_false((op(0, fx, ++), current_op(X, fx, ++))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	tests of op/3 and current_op/3 that should throw errors
%

test_op_errors :-
	error_test(op(2000, xfx, a), domain_error(operator_priority, 2000)),
        error_test(current_op(200, afy, X), domain_error(operator_specifier, afy)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Test of char_conversion/2 that should not throw errors
%
  

rw_char_conver(CharOut, ConvertTo, ReadIn) :-
	open('charconv.tmp', write, Sout),
        put_char(Sout, CharOut),
        close(Sout),
        char_conversion(CharOut, ConvertTo),
        open('charconv.tmp', read, Sin),
        get_char(Sin, ReadIn),
        char_conversion(CharOut, CharOut),
        close(Sin).

test_char_conversion :-
	test_true(rw_char_conver(a,b,b)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Test of char_conversion/2 that should  throw errors
%

test_char_conversion_errors :-
	error_test(char_conversion(X,a), instantiation_error).


char_conversion_test :-
	defined(char_conversion/2),
        test_char_conversion,
        test_char_converison_errors.
char_conversion_test :-
	write('char_conversion/2 is not supported'), nl.
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Amalgams with test for suport.
%

test_rw_term :-
	(defined(read_term/3), defined(write_term/3))
        ->
        (test_read_write_term,test_read_write_term_errors)
        ;
        (log_nl, log( ' read_term/write_term  not supported.'), log_nl).

test_op3 :-
         defined(op/3)
         ->
         (test_op, test_op_errors)
         ;
         (log_nl, log( ' op/3  not supported.'), log_nl).

	   
test_cc :-
         defined(char_conversion/2)
         ->
         char_conversion_test
         ;
         (log_nl, log( 'char_conversion/2 not supported.'), log_nl).



test_814 :-
        log_nl,
        log( 'Testing read and write of terms'),
        log_nl, log_nl,
	test_rw_term,
        log_nl,
        log( 'Testing read and write of terms done, testing op/3'),
        log_nl, log_nl,
        test_op3,
        log_nl,
        log( 'Testing op/3 done, testing char_conversion'),
        log_nl, log_nl,
        test_cc,
        log_nl,
        log( 'Testing char_conversion done.'),
        log_nl, log_nl,
        log( 'Testing of section 8.14 done. '),
        log_nl, log_nl, !.

	

    
