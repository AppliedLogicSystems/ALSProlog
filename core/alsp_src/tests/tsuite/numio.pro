/*
 * numio.pro		-- test out get_number/3, put_number/3,
 *			   and reading, writing, and seeking to the
 *			   same stream.
 *
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation:	3/18/93
 */

test :-
	open('numio.tmp',read_write,_,[type(binary),bufsize(16),alias(foo)]),

	pn(double,19.3),	%% 0-7
	pn(short, 20),		%% 8-9
	pn(long, 0x01020304),	%% 10-13
	pn(long, 0x10203040),	%% 14-17
	pn(byte, 12),		%% 18
	pn(double, -44.99),	%% 19-26
	pn(long, 0x11223344),	%% 27-30
	pn(double, 123.456),	%% 31-38
	pn(float, 1.5),		%% 39-42
	pn(long, 0x12345678),	%% 43-46

	stream_position(foo, _, 0),

	gn(double,19.3),	%% 0-7
	gn(short, 20),		%% 8-9
	gn(long, 0x01020304),	%% 10-13
	gn(long, 0x10203040),	%% 14-17
	gn(byte, 12),		%% 18
	gn(double, -44.99),	%% 19-26
	gn(long, 0x11223344),	%% 27-30
	gn(double, 123.456),	%% 31-38
	gn(float, 1.5),		%% 39-42
	gn(long, 0x12345678),	%% 43-46

	point(a),
	stream_position(foo, _, 14),

	gn(long, 0x10203040),	%% 14-17
	pn(byte, 21),		%% 18
	gn(double, -44.99),	%% 19-26
	gn(long, 0x11223344),	%% 27-30

	point(b),
	stream_position(foo, _, 10),

	pn(ulong, 0x98765432),	%% 10-13
	gn(long, 0x10203040),	%% 14-17
	gn(byte, 21),		%% 18
	pn(double, 123e-12),	%% 19-26
	gn(long, 0x11223344),	%% 27-30

	point(c),
	stream_position(foo, _, 0),

	gn(double,19.3),	%% 0-7
	gn(short, 20),		%% 8-9
	gn(ulong, 0x98765432),	%% 10-13
	gn(long, 0x10203040),	%% 14-17
	gn(byte, 21),		%% 18
	gn(double, 123e-12),	%% 19-26
	gn(long, 0x11223344),	%% 27-30
	gn(double, 123.456),	%% 31-38
	gn(float, 1.5),		%% 39-42
	gn(long, 0x12345678),	%% 43-46

	pn(float, 3.25),	%% 47-50
	pn(float, 9.125),	%% 51-54
	pn(byte, 95),		%% 55
	pn(ulong, 0xabcdef01),	%% 56-59
	pn(long, 0x00112233),	%% 60-63
	pn(byte, 22),		%% 64
	pn(double, 123.45e12),	%% 65-72
	pn(short, 16385),	%% 73-74

	point(d),
	stream_position(foo, _, 39),

	pn(float, 33.75),	%% 39-42
	pn(ulong, 0x87654321),	%% 43-46
	gn(float, 3.25),	%% 47-50
	gn(float, 9.125),	%% 51-54
	gn(byte, 95),		%% 55
	gn(ulong, 0xabcdef01),	%% 56-59
	gn(long, 0x00112233),	%% 60-63
	gn(byte, 22),		%% 64
	gn(double, 123.45e12),	%% 65-72
	gn(short, 16385),	%% 73-74

	point(e),
	stream_position(foo, _, 31),

	gn(double, 123.456),	%% 31-38
	gn(float, 33.75),	%% 39-42
	gn(ulong, 0x87654321),	%% 43-46
	gn(float, 3.25),	%% 47-50
	pn(long, 0x11002200),	%% 51-54
	pn(byte, 98),		%% 55

	point(f),
	stream_position(foo, _, 75),

	pn(double, 12.5),	%% 75-82
	pn(short, 987),		%% 83-84
	pn(double, -9999999.5),	%% 85-92
	pn(short, 789),		%% 93-94
	pn(double, 101010.65),	%% 95-102
	pn(byte, 11),		%% 103
	pn(ulong, 0xacbacbaa),	%% 104-107

	point(g),
	stream_position(foo, _, 27),

	gn(long, 0x11223344),	%% 27-30
	gn(double, 123.456),	%% 31-38
	gn(float, 33.75),	%% 39-42
	gn(ulong, 0x87654321),	%% 43-46
	gn(float, 3.25),	%% 47-50
	gn(long, 0x11002200),	%% 51-54
	gn(byte, 98),		%% 55
	gn(ulong, 0xabcdef01),	%% 56-59
	gn(long, 0x00112233),	%% 60-63
	gn(byte, 22),		%% 64
	gn(double, 123.45e12),	%% 65-72
	gn(short, 16385),	%% 73-74
	gn(double, 12.5),	%% 75-82
	gn(short, 987),		%% 83-84
	gn(double, -9999999.5),	%% 85-92
	gn(short, 789),		%% 93-94
	gn(double, 101010.65),	%% 95-102
	gn(byte, 11),		%% 103
	gn(ulong, 0xacbacbaa),	%% 104-107

	point(h),

	close(foo),
	remove_file('numio.tmp'),
	write('Test numio.pro completed successfully.'),nl.

pn(Size,Val) :-
	put_number(foo,Size,Val),
	!.
pn(Size,Val) :-
	write(put_number(foo,Size,Val)), write(' failed.'),nl,fail.

gn(Size,Val) :-
	get_number(foo,Size,Val2),
	!,
	gneq(Size, Val, Val2).
gn(Size,Val) :-
	write(get_number(foo,Size,Val)), write(' failed.'),nl,fail.

gneq(Size,Val,Val) :- !.
gneq(Size,Val1,Val2) :-
	write(getnumber(foo,Size,Val1)),
	write(' got '), write(Val2), write(' instead'),
	nl,fail.

point(X) :- printf('\npoint %t\n',[X]).

