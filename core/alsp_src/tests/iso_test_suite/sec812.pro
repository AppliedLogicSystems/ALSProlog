%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  sec812.pro          1 Feb 1996              %
%                                              %
%     Tests of get_char/2, get_char/1,         %
%   get_code/2, get_code/1, peek_char/2,       %
%   peek_char/1, peek_code/2,peek_code/1,      %
%   put_char/2, put_char/1, put_code/2,        %
%   put_code/1, nl, nl/1.                      %
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



%:- mulitifile(validate/0).

validate :- test_812.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Link tests of get and put  chars
%
%    open a file write to it, close it read back
%

rw_char1(CharOut, File, CharIn) :-
	open(File, write, Sout),
        set_output(Sout),
        put_char(CharOut),
        close(Sout),
        open(File, read, Sin),
        set_input(Sin),
        get_char(CharIn),
        close(Sin).

rw_char2(CharOut, File, CharIn) :-
	open(File, write, Sout),
        put_char(Sout,CharOut),
        close(Sout),
        open(File, read, Sin),
        get_char(Sin, CharIn),
        close(Sin).
rw_char2a(CharOut, File, CharIn) :-
        open(File, write, Sout, [alias(out)]),
        put_char(out,CharOut),
        close(Sout),
        open(File, read, Sin, [alias(in)]),
        get_char(in, CharIn),
        close(Sin).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of get_char/1,2  and put_char/1,2 that
%     should not throw errors.
%
%

test_get_put_char :-
	test_true(rw_char1('a', 'chario.tmp', 'a')),
        test_val(rw_char2('b', 'chario.tmp', C), C, 'b'),
        test_true(rw_char2a('z', 'chario.tmp', 'z')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of get_char/1,2  and put_char/1,2 that
%     should  throw errors.
%
%

test_get_put_char_errors :-
	error_test(get_char(X,Char1), instantiation_error),
        error_test(get_char(fred, Char2), existence_error(stream, fred)),
        error_test(get_char(nostream/noalias, Char3), 
                     domain_error(stream_or_alias,nostream/noalias)),
        open('charfile.txt', read, S1, [alias(inputstream)]),
        error_test(put_char(inputstream, 'a'), permission_error(output, stream, inputstream)),
        open('out.tmp', write, S2, [alias(outputstream)]),
        error_test(get_char(outputstream, Y), permission_error(input, stream, outputstream)),
        close(S1), close(S2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	roll these two together and test for support of get and put char
%       use of arity 2 vrsions should faorce errors.
%       rather than some intput.

test_gp_chars :-
	defined(get_char/2), defined(put_char/2),!,
        test_get_put_char, test_get_put_char_errors.
test_gp_chars :-
       
         log_nl, log( 'get_char /put_char not supported'), log_nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     tests of get and put  code
%
%    open a file write to it, close it read back
%

rw_code1(CodeOut, File, CodeIn) :-
	open(File, write, Sout),
        set_output(Sout),
        put_code(CodeOut),
        close(Sout),
        open(File, read, Sin),
        set_input(Sin),
        get_code(CodeIn),
        close(Sin).

rw_code2(CodeOut, File, CodeIn) :-
	open(File, write, Sout),
        put_code(Sout,CodeOut),
        close(Sout),
        open(File, read, Sin),
        get_code(Sin, CodeIn),
        close(Sin).
rw_code2a(CodeOut, File, CodeIn) :-
        open(File, write, Sout, [alias(out)]),
        put_code(out,CodeOut),
        close(Sout),
        open(File, read, Sin, [alias(in)]),
        get_code(in, CodeIn),
        close(Sin).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of get_code/1,2  and put_code/1,2 that
%     should not throw errors.
%
%

test_get_put_code :-
	test_true(rw_code1(66, 'codeio.tmp', 66)),
        test_val(rw_code2(67, 'codeio.tmp', C), C, 67),
        test_true(rw_code2a(90, 'codeio.tmp', 90)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Tests of get_code/1,2  and put_code/1,2 that
%     should  throw errors.
%
% 

test_get_put_code_errors :-
        error_test(get_code(X,Code1), instantiation_error),
        error_test(get_code(fred, Code2), existence_error(stream, fred)),
        error_test(get_code(nostream/noalias, Code3), 
                     domain_error(stream_or_alias,nostream/noalias)),
        open('charfile.txt', read, S1, [alias(inputstream)]),
        error_test(put_code(inputstream, 77), permission_error(output, stream, inputstream)),
        open('out.tmp', write, S2, [alias(outputstream)]),
        error_test(get_code(outputstream, Y), permission_error(input, stream, outputstream)),
        close(S1), close(S2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	roll these two together and test for support of get and put code
%

test_gp_codes :-
	(defined(get_code/2), defined(put_code/2))
        ->
        (test_get_put_code, test_get_put_code_errors)
       ;
        ( log_nl, log( 'get_code /put_code not supported'), log_nl).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Tests of peek_char
%
%    open a file write to it, close it 
%     open to read, 
%    peek then read. 
%

wpr_char1(CharOut, File, CharPeeked, CharIn) :-
	open(File, write, Sout),
        set_output(Sout),
        put_char(CharOut),
        close(Sout),
        open(File, read, Sin),
        set_input(Sin), 
        peek_char(CharPeeked),
        get_char(CharIn),
        close(Sin).

wpr_char2(CharOut, File, CharPeeked, CharIn) :-
	open(File, write, Sout),
        put_char(Sout,CharOut),
        close(Sout),
        open(File, read, Sin),
        peek_char(Sin, CharPeeked),
        get_char(Sin, CharIn),
        close(Sin).
wpr_char2a(CharOut, File, CharPeeked, CharIn) :-
        open(File, write, Sout, [alias(out)]),
        put_char(out,CharOut),
        close(Sout),
        open(File, read, Sin, [alias(in)]),
        peek_char(in, CharPeeked),
        get_char(in, CharIn),
        close(Sin).



  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of peek_char/1,2   that
%     should not throw errors.
%
%

test_peek_char :-
	test_true(wpr_char1('a', 'peekchar.txt', 'a', 'a')),
        test_true(wpr_char2('b', 'peekchar.txt', 'b', 'b')),
        test_true(wpr_char2a('c', 'peekchar.txt', 'c', 'c')).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of peek_char/1,2 that
%     should  throw errors.
%
%

test_peek_char_errors :-
	error_test(peek_char(X,Char1), instantiation_error),
        error_test(peek_char(fred, Char2), existence_error(stream, fred)),
        error_test(peek_char(nostream/noalias, Char3), 
                     domain_error(stream_or_alias,nostream/noalias)),
        open('peekfile.txt', write, S1, [alias(outputstream)]),
        error_test(peek_char(outputstream, X), permission_error(input, stream, outputstream)),
        close(S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Tests of peek_code
%
%    open a file write to it, close it 
%     open to read, 
%    peek then read. 
%

wpr_code1(CodeOut, File, CodePeeked, CodeIn) :-
	open(File, write, Sout),
        set_output(Sout),
        put_code(CodeOut),
        close(Sout),
        open(File, read, Sin),
        set_input(Sin), 
        peek_code(CodePeeked),
        get_code(CodeIn),
        close(Sin).

wpr_code2(CodeOut, File, CodePeeked, CodeIn) :-
	open(File, write, Sout),
        put_code(Sout,CodeOut),
        close(Sout),
        open(File, read, Sin),
        peek_code(Sin, CodePeeked),
        get_code(Sin, CodeIn),
        close(Sin).
wpr_code2a(CodeOut, File, CodePeeked, CodeIn) :-
        open(File, write, Sout, [alias(out)]),
        put_code(out,CodeOut),
        close(Sout),
        open(File, read, Sin, [alias(in)]),
        peek_code(in, CodePeeked),
        get_code(in, CodeIn),
        close(Sin).



  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of peek_code/1,2   that
%     should not throw errors.
%
%

test_peek_code :-
	test_true(wpr_code1(67, 'peekchar.txt', 67, 67)),
        test_true(wpr_code2(56, 'peekchar.txt', 56, 56)),
        test_true(wpr_code2a(101, 'peekchar.txt', 101, 101)).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of peek_char/1,2 that
%     should  throw errors.
%
%

test_peek_code_errors :-
	error_test(peek_code(X,Code1), instantiation_error),
        error_test(peek_code(fred, Code2), existence_error(stream, fred)),
        error_test(peek_code(nostream/noalias, Code3), 
                     domain_error(stream_or_alias,nostream/noalias)),
        open('peekfile.txt', write, S1, [alias(outputstream)]),
        error_test(peek_code(outputstream, X), permission_error(input, stream, outputstream)),
        close(S1).

   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	roll these  together and test for support of peek_char
%

test_pk_chars :-
	defined(peek_char/2)
        ->
        (test_peek_char, test_peek_char_errors)
       ;
        ( log_nl, log( 'peek_char not supported'), log_nl).

test_pk_codes :-
	defined(peek_code/2)
        ->
        (test_peek_code, test_peek_code_errors)
       ;
        ( log_nl, log( 'peek_code not supported'), log_nl).


   


      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	run the tests
%


test_812 :- 
        log_nl, log( 'Testing get and put chars.'),
        log_nl, log_nl,
        test_gp_chars,
        log_nl,
        log( 'tests of char io done, testing code io.'),
        log_nl, log_nl,
	test_gp_codes,
        log_nl,
        log( 'tests of code io done, testing peeks.'),
        log_nl, log_nl,
        test_pk_chars,
        test_pk_codes,
        log_nl,
        log( 'tests of peeks done.'),
        log_nl, 
        log( 'tests of section 8.12 done.'),
        log_nl,!.

    
