%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%  sec813.pro           2 Feb 1996             %
%                                              %
%     Tests of get_byte/2, get_byte/1,         %
%    peek_byte/2,  peek_byte/1,                %
%   put_byte/2, put_byte/1,                    %
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



% multifile(validate/0).

validate :- test_813.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Link tests of get and put  chars
%
%    open a file write to it, close it read back
%

rw_byte1(ByteOut, File, ByteIn) :-
	open(File, write, Sout, [type(binary)]),
        set_output(Sout),
        put_byte(ByteOut),
        close(Sout),
        open(File, read, Sin, [type(binary)]),
        set_input(Sin),
        get_byte(ByteIn),
        close(Sin).

rw_byte2(ByteOut, File, ByteIn) :-
	open(File, write, Sout, [type(binary)]),
        put_byte(Sout,ByteOut),
        close(Sout),
        open(File, read, Sin, [type(binary)]),
        get_byte(Sin, ByteIn),
        close(Sin).
rw_byte2a(ByteOut, File, ByteIn) :-
        open(File, write, Sout, [alias(out), type(binary)]),
        put_byte(out,ByteOut),
        close(Sout),
        open(File, read, Sin, [alias(in), type(binary)]),
        get_byte(in, ByteIn),
        close(Sin).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of get_byte/1,2  and put_byte/1,2 that
%     should not throw errors.
%
%

test_get_put_byte :-
         test_true(rw_byte1(110, 'byteio.tmp', 110)),
         test_val(rw_byte2(45, 'byteio.tmp', C), C, 45),
         test_true(rw_byte2a(67, 'byteio.tmp', 67)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of get_byte/1,2  and put_byte/1,2 that
%     should  throw errors.
%
%

test_get_put_byte_errors :-
	error_test(get_byte(X,B1), instantiation_error),
        error_test(get_char(fred, B2), existence_error(stream, fred)),
        error_test(get_char(nostream/noalias, B3), 
                     domain_error(stream_or_alias,nostream/noalias)),
        open('charfile.txt', read, S1, [alias(inputstream),type(binary)]),
        error_test(put_byte(inputstream,55 ), permission_error(output, stream, inputstream)),
        open('out.tmp', write, S2, [alias(outputstream),type(binary)]),
        error_test(get_byte(outputstream, Y), permission_error(input, stream, outputstream)),
        close(S1), close(S2).
        

   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Tests of peek_byte
%
%    open a file write to it, close it 
%     open to read, 
%    peek then read. 
%

wpr_byte1(ByteOut, File, BytePeeked, ByteIn) :-
	open(File, write, Sout, [type(binary)]),
        set_output(Sout),
        put_byte(ByteOut),
        close(Sout),
        open(File, read, Sin, [type(binary)]),
        set_input(Sin), 
        peek_byte(BytePeeked),
        get_byte(ByteIn),
        close(Sin).

wpr_byte2(ByteOut, File, BytePeeked, ByteIn) :-
	open(File, write, Sout,[type(binary)]),
        put_byte(Sout,ByteOut),
        close(Sout),
        open(File, read, Sin, [type(binary)]),
        peek_byte(Sin, BytePeeked),
        get_byte(Sin, ByteIn),
        close(Sin).
wpr_byte2a(ByteOut, File, BytePeeked, ByteIn) :-
        open(File, write, Sout, [alias(out), [type(binary)]]),
        put_byte(out,ByteOut),
        close(Sout),
        open(File, read, Sin, [alias(in),[type(binary)]]),
        peek_byte(in, BytePeeked),
        get_byte(in, ByteIn),
        close(Sin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    amalgamate byte iotests and test for support
%

test_gp_byte :-
	(defined(get_byte/2), defined(put_byte/2))
        ->
        (test_get_put_byte,test_get_put_byte_errors)
        ;
        (log_nl, log( 'get_byte/put_byte not supported.'), log_nl).

  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of peek_byte/1,2   that
%     should not throw errors.
%
%

test_peek_byte :-
	test_true(wpr_byte1(212, 'peekbyte.bin', 212, 212)),
        test_true(wpr_byte2(45, 'peekbyte.bin', 45, 45)),
        test_true(wpr_byte2a(108, 'peekbyte.bin', 108, 108)).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%     Tests of peek_byte/1,2 that
%     should  throw errors.
%
%

test_peek_byte_errors :-
	error_test(peek_byte(X,B1), instantiation_error),
        error_test(peek_byte(fred, B2), existence_error(stream, fred)),
        error_test(peek_byte(nostream/noalias, Char3), 
                     domain_error(stream_or_alias,nostream/noalias)),
        open('peekfile.bin', write, S1, [alias(outputstream), type(binary)]),
        error_test(peek_byte(outputstream, X), permission_error(input, stream, outputstream)),
        close(S1).

validate_peek_byte :-
        defined(peek_byte/2),
        test_peek_byte,
        test-peek_byte_errors.
validate_peek_byte :-
	log_nl,log( 'peek_byte is not supported.'), log_nl.

       
	

test_813 :-
       log_nl,
       log( 'testing byte io.'),
       log_nl, log_nl,
       test_gp_byte,
       log_nl, log( 'testing byte io done, testing peek_byte.'),
       log_nl,
       validate_peek_byte,
       log_nl,
       log('testing  peek_byte done.'),
       log_nl,
       log('tests of  section 8.13 done.'),
       !.		


       

    
