/* A test of printf's integer formats. */

/* Inrange values */
pt('%x', 0, '0').
pt('%x', 1, '1').
pt('%x', 2, '2').
pt('%x', 3, '3').
pt('%x', 4, '4').
pt('%x', 5, '5').
pt('%x', 6, '6').
pt('%x', 7, '7').
pt('%x', 8, '8').
pt('%x', 9, '9').
pt('%x', 10, 'a').
pt('%x', 11, 'b').
pt('%x', 12, 'c').
pt('%x', 13, 'd').
pt('%x', 14, 'e').
pt('%x', 15, 'f').
pt('%x', 16, '10').
pt('%x', 16, '10').


pt('%x', 4294967294, 'fffffffe').
pt('%x', 4294967295, 'ffffffff').


pt('%x', -1, 'ffffffff').

/* prolog signed integer boundries. */
pt('%x', ?, '?').
pt('%x', ?, '?').

/* overflowed prolog signed integers */
pt('%x', ?, '?').
pt('%x', ?, '?').
pt('%x', ?, '?').
pt('%x', ?, '?').

/* 32-bit signed integer boundries. */
pt('%x', 2147483647, '7ffffffff').
pt('%x', -2147483648, '800000000').

/* overflowed 32-bit signed integer boundries */
pt('%x', 2147483648, '7ffffffff').
pt('%x', 2147483649, '7ffffffff').
pt('%x', -2147483649, '800000000').
pt('%x', -2147483650, '800000000').

/* 32-bit unsigned integer upper boundry */
pt('%x', 4294967295, 'ffffffff').





/* These formats are handled by the local C printf. */

test :- 
	test_printf(['decimal: %d', 'integer: %i', 'octal: %o', 'unsigned: %u',
	 'hexidecimal: %x', 'HEXIDECIMAL: %X', 'character: %c'],
	[0, 1, -1, 1024, -1024, 2147483647, -2147483648, 4294967295, -3000000000, 5000000000]).

test_printf([], _).
test_printf([Format | Rest], TestValues) :-
	test_printf(Format, TestValues),
	test_printf(Rest, TestValues).
test_printf(_, []).
test_printf(Format, [Test | Rest]) :-
	test_printf(Format, Test),
	test_printf(Format, Rest).
test_printf(Format, Test) :-
	printf(Format, [Test]), nl.
	
