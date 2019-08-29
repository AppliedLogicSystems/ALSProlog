:- [test].

test_arithx1 :-
	test([
	test_max,
	test_min,
	test_maximum,
	test_minimum,
	test_sumlist,
	test_prodlist,
	test_sum_squares,
	test_sum_square_diffs,
	test_max_vector,
	test_min_vector,
	true]).

test_max :-
	max(3,92,M1),
	M1 == 92,
	max(3,50-92,M2),
	M2 == 3,
	max(-6,-7*9,M3),
	M3 == -6,
	max(23.89, 23.87,M4),
	M4 == 23.89.
	

test_min :-
	min(3,92,M1),
	M1 == 3,
	min(3*1,-0-92,M2),
	M2 == -92,
	min(-6*(-5),-9,M3),
	M3 == -9,
	min(23.89, 23.87,M4),
	M4 == 23.87.
	
test_maximum :-
	maximum([3.4, 2+3, -6*1.78, 123.4456], M1),
	M1 == 123.4456,
	maximum([-3.4, -5, -6.78, -123.4456], M2),
	M2 == -3.4.

test_minimum :-
	minimum([3.4, 5*5, -6*1.78, 9+123.4456], M1),
	M1 == -10.68,
	minimum([-3.4+9, -5*6, -6.78, -123.4456], M2),
	M2 == -123.4456.

test_sumlist :-
	sumlist([3.4, 5*5, -6*1.78, 9+123.4456], M1),
	M1==150.1656,
	sumlist([-3.4+9, -5*6, -6.78, -123.4456], M2),
	M2 == -154.6256.
	
test_prodlist :-
	prodlist([3.4, 5*5, -6*1.78, 9+123.4456], M1),
	M1 - (-120234.1157) < 0.0001,
	prodlist([-3.4+9, -5*6, -6.78, -123.4456], M2),
	M2 - (-140609.4762) < 0.0001.
	
test_sum_squares :-
	sum_squares([3,4], M1),
	M1 == 25,
	sum_squares([88 + 0.98, 4.3*6.77], M2),
	M2 == 8764.890721.

test_sum_square_diffs :-
	XList = [4.3*1, 4+20.88], 
	YList = [5.12+2.3, 3*20.44],
	sum_square_diffs(XList, YList, Result),
	Result - 1337.608 < 0.0001.

test_max_vector :-
	Left = [3, 4.1 + 0.4, 88.9],
	Right = [0.6 * 2, 8.44, 100.9],
	max_vector(Left, Right, Result),
	Result == [3,8.44,100.9].

test_min_vector :-
	Left = [3, 4.5, 88.9],
	Right = [1.2, 8.44, 100.9],
	min_vector(Left, Right, Result),
	Result == [1.2,4.5,88.9].

