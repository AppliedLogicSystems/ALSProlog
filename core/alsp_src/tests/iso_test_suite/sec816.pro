%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    sec816.pro                                %
%                        10:23AM  11/1/1996    %
%                                              %
%  tests of atom handling predicates           %
%   atom_length/2, atom_concat/3,              %
%   sub_atom/5, atom_chars/2, atom_codes/2,    %
%  char_code/2, numberchars/2, number_codes/2. %
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
validate :-
	test_816.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_length that should not raise errors
%
%

test_atom_length :-
	test_val(atom_length(fred, L), L, 4),
        test_val(atom_length('',Z), Z, 0),
        test_false(atom_length(scarlet, 5)),
        test_true(atom_length('Use Prolog', 10)).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_length that should  raise errors
%
%        

test_atom_length_errors :-
	error_test(atom_length(X, 4), instantiation_error),
        error_test(atom_length(1.23, 4), type_error(atom, 1.23)),
        error_test(atom_length(atom, '4'), type_error(integer, '4')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence test.
%

test_al :-	
        defined(atom_length/2)
        ->
        (test_atom_length,test_atom_length_errors)
        ;
        ( log_nl, log( 'atom_length/2 is not supported'), log_nl).


       
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_concat that should not raise errors
%
%

test_atom_concat :-
	test_val(atom_concat('hello', ' world', S), S, 'hello world'),
        test_val(atom_concat(X,' world', 'hello world'), X, 'hello'),
        test_false(atom_concat('small', ' world', 'hello world')),
        test_true(atom_concat(P,Q, 'hello world')).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_concat that should  raise errors
%
%    incomplete
%      

test_atom_concat_errors :-
	error_test(atom_concat(hello, V1, V2), instantiation_error).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence.
%

test_ac :-	
        defined(atom_concat/3)
        ->
        (test_atom_concat,test_atom_concat_errors)
        ;
        ( log_nl, log( 'atom_concat/3 is not supported'), log_nl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of sub_atom that should not raise errors
%
%

test_sub_atom :-
	test_val(sub_atom(abracadabra, 0, 5, _, S1), S1, abrac),
        test_val(sub_atom(abracadabra, _, 5, 0, S2), S2, dabra),
        test_val(sub_atom(abracadabra,B,2,A, ab), B/A, 0/9),
        test_val(setof(X, Y^Z^sub_atom(ab,Y,Z,_, X), ['', 'a', 'ab', 'b'])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of sub_atom that should  raise errors
%
%    incomplete
%      

test_sub_atom_errors :-
	error_test(subatom(A, B,C,D,E), instantiation_error),
        error_test(subatom(abracadabra, -1, 3, 2, S), 
                  domain_error(not_less_than_zero, -1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence.
%

test_sa :-	
        defined(subatom/5)
        ->
        (test_sub_atom,test_sub_atom_errors)
        ;
        ( log_nl, log( 'subatom/5 is not supported'), log_nl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_chars that should not raise errors
%
%

test_atom_chars :-
	test_val(atom_chars('', L), L, []),
        test_val(atom_chars([], M), M, ['[', ']']),
        test_true(atom_chars('ant', ['a', 'n', 't'])),
        test_false(atom_chars('soap', ['s', 'o', 'p'])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_chars that should  raise errors
%
%

test_atom_chars_errors :-
	error_test(atom_chars(X,Y), instantiation_error).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence.
%

test_a_chars :-	
        defined(atom_chars/2)
        ->
        (test_atom_chars,test_atom_chars_errors)
        ;
        ( log_nl, log( 'atom_chars/2 is not supported'), log_nl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_codes that should not raise errors
%
%

test_atom_codes :-
	test_val(atom_codes('', L), L, []),
        test_val(atom_codes([], M), M, [0'[, 0']]),
        test_true(atom_codes('ant', [0'a, 0'n, 0't])),
        test_false(atom_chars('soap', [0's, 0'o, 0'p])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of atom_codes that should  raise errors
%
%

test_atom_codes_errors :-
	error_test(atom_codes(X,Y), instantiation_error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence.
%

test_a_codes :-	
        defined(atom_codes/2)
        ->
        (test_atom_codes,test_atom_codes_errors)
        ;
        ( log_nl, log( 'atom_codes/2 is not supported'), log_nl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of char_code that should not raise errors
%
%

test_char_code :-
        test_true(char_code('a', Code)),
        test_true(char_code(B, 77)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of char_code that should  raise errors
%
%  Asummes < 10000 char codes
%

test_char_code_errors :-
	error_test(char_code(X,Y), instantiation_error),
        error_test(char_code('ab', Y), type_error(character, 'ab')),
        error_test(char_code(Char, 9999), representation_error(character_code)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence.
%

test_c_code :-	
        defined(char_code/2)
        ->
        (test_char_code,test_char_code_errors)
        ;
        ( log_nl, log( 'char_code/2 is not supported'), log_nl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of number_chars that should not raise errors
%
%  incomplete

test_number_chars :-
	test_val(number_chars(33, L), L, ['3', '3']),
        test_val(number_chars(A, [-, '2','5']), A, -25),
        test_true(number_chars(3.14159, Pi)),
        test_true(number_chars(15, ['0', 'x', 'f'])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of number_chars that should  raise errors
%
%

test_number_chars_errors :-
	error_test(number_chars(X,Y), instantiation_error),
        error_test(number_chars(ab, Y), type_error(number, ab)),
        error_test(number_chars(Z, notlist), type_error(list, notlist)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence.
%

test_n_chars :-	
        defined(number_chars/2)
        ->
        (test_number_chars,test_number_chars_errors)
        ;
        ( log_nl, log( 'number_chars/2 is not supported'), log_nl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of number_codes that should not raise errors
%
%  incomplete

test_number_codes :-
	test_val(number_codes(33, L), L, [0'3, 0'3]),
        test_val(number_codes(A, [0'-, 0'2,0'5]), A, -25),
        test_true(number_codes(3.14159, Pi)),
        test_true(number_codes(15, [0'0, 0'x, 0'f])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Tests of number_codes that should  raise errors
%
%

test_number_codes_errors :-
	error_test(number_codes(X,Y), instantiation_error),
        error_test(number_codes(ab, Y), type_error(number, ab)),
        error_test(number_codes(Z, notlist), type_error(list, notlist)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   with existence.
%

test_n_codes :-	
        defined(number_codes/2)
        ->
        (test_number_codes,test_number_codes_errors)
        ;
        ( log_nl, log( 'number_codes/2 is not supported'), log_nl).
test_816 :-
        log_nl,
        log( 'testing atom_length/2'),
        log_nl,
        test_al,
         log_nl,
        log( 'testing atom_length/2 done, testing atom_concat/3'),
        log_nl,
	test_ac,
         log_nl,
        log( 'testing atom_concat/2 done,testing subatom/5'),
        log_nl,
	test_sa,
        log_nl,
        log( 'testing subatom/5 done,testing atom_chars/2'),
        log_nl,
        test_a_chars,
        log_nl,
        log( 'testing atom_chars/2 done,testing atom_codes/2'),
        log_nl,
        test_a_codes,
        log_nl,
        log( 'testing atom_codes/2 done,testing char_code/2'),
        log_nl,
        test_c_code,
        log_nl,
        log( 'testing char_code/2 done,testing number_chars/2'),
        log_nl,
        test_n_chars,
        log_nl,
        log( 'testing number_chars/2 done,testing number_codes/2'),
        log_nl,
        test_n_codes,
        log_nl,
        log( 'testing number_codes/2 done'),
        log_nl,
        log( 'testing section 8.16 done'),
        log_nl, !.
