/* A test of printf's integer formats. */

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
	
