


ta :- test_arithx1.

test_arithx1 :-
	test_max,
	test_min,
	test_maximum,
	test_minimum,
	test_sumlist,
	test_prodlist,
	test_sum_squares,
	test_sum_square_diffs,
	test_max_vector,
	test_min_vector.


test_max :-
	max(3,92,M1),
	M1 == 92,
	max(3,-92,M2),
	M2 == 3,
	max(-6,-9,M3),
	M3 == -6,
	max(23.89, 23.87,M4),
	M4 == 23.89,
	!.
	
test_max :-
	printf(user, 'test_max test failed\n', []).


test_min :-
	min(3,92,M1),
	M1 == 3,
	min(3,-92,M2),
	M2 == -92,
	min(-6,-9,M3),
	M3 == -9,
	min(23.89, 23.87,M4),
	M4 == 23.87,
	!.
	
test_min :-
	printf(user, 'test_min test failed\n', []).

test_maximum :-
	maximum([3.4, 5, -6.78, 123.4456], M1),
	M1 == 123.4456,
	maximum([-3.4, -5, -6.78, -123.4456], M2),
	M2 == -3.4,
	!.

test_maximum :-
	printf(user, 'test_maximum test failed\n', []).

test_minimum :-
	minimum([3.4, 5, -6.78, 123.4456], M1),
	M1 == -6.78,
	minimum([-3.4, -5, -6.78, -123.4456], M2),
	M2 == -123.4456,
	!.

test_minimum :-
	printf(user, 'test_minimum test failed\n', []).

test_sumlist :-
	sumlist([3.4, 5, -6.78, 123.4456], M1),
	M1 == 125.0656,
	sumlist([-3.4, -5, -6.78, -123.4456], M2),
	M2 == -138.6256,
	!.
	
test_sumlist :-
	printf(user, 'test_sumlist test failed\n', []).

test_prodlist :-
	prodlist([3.4, 5, -6.78, 123.4456], M1),
	R1 is M1 - (- 14228.33986),
	R1 < 0.0001,
	prodlist([0.24, 1.6, 8, 23.67654], M2),
	R2 = M2 - 72.73433088,
	R2 < 0.0001,
	!.
	
test_prodlist :-
	printf(user, 'test_prodlist test failed\n', []).

test_sum_squares :-
	sum_squares([3,4], M1),
	M1 == 25,
	sum_squares([88.98,23.6], M2),
	M2 == 8474.4004,
	!.

test_sum_squares :-
	printf(user, 'test_sum_squares test failed\n', []).


test_sum_square_diffs :-
	XList = [4.3, 24.88], 
	YList = [5.12, 20.44],
	sum_square_diffs(XList, YList, Result),
	D is (Result - 20.386),
	D < 0.0001,
	!.

test_sum_square_diffs :-
	printf(user, 'test_sum_square_diffs test failed\n', []).

test_max_vector :-
	Left = [3, 4.5, 88.9],
	Right = [1.2, 8.44, 100.9],
	max_vector(Left, Right, Result),
	Result == [3,8.44,100.9],
	!.

test_max_vector :-
	printf(user, 'test_max_vector test failed\n', []).

test_min_vector :-
	Left = [3, 4.5, 88.9],
	Right = [1.2, 8.44, 100.9],
	min_vector(Left, Right, Result),
	Result == [1.2,4.5,88.9],
	!.

test_min_vector :-
	printf(user, 'test_min_vector test failed\n', []).

