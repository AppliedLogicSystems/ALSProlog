%
% Creation:	Craig Thornley
%		April 27, 1989
%
%	Copyright (c) 1989 Motorola, Inc.
%
% vectors.pro	contains vector primitive builtins
%
% Modified:	when		who	what
%		June 12, 1989	CT	added vsplit, vr_split & vc_split
%
%


% REAL VECTOR PRIMITIVES

	% make_rvector(L,V) make list L of real numbers into a vector V

make_rvector(L,V) :-
	make_vector(L,V).




	% vr_arg(N,V,A)	A is unified with the Nth real number of vector V

vr_arg(N,V,A) :-
	varg(N,V,A).


	% vr_arg_first(N,V,Vout) Vout is a real vector consisting of the
	% first N reals of vector V.

vr_arg_first(N,V,Vout) :-
	varg_first(N,V,Vout).


	% vr_arg_last(N,V,Vout) Vout is a real vector consisting of the last
	% N reals of vector V.

vr_arg_last(N,V,Vout) :-
	varg_last(N,V,Vout).


	% vr_length(V,L) L is the number of reals in vector V.

vr_length(V,L) :-
	vlength(V,L).



	% vr_mag2(V,Sr) Sr is the scalar real result of the sum of the 
	% squares of each element in real vector V.

%vr_mag2(V,Sr) :-
%	vrr_dot(V,V,Sr).



	% vr_mag(V,Sr) Sr is the square root of the sum of the squares of
	% each element in real vector V.

vr_mag(V,Sr):-
	vrr_dot(V,V,Sr1),
	Sr is sqrt(Sr1).






% COMPLEX VECTOR PRIMITIVES


	% make_cvector(CList,V) given a list of complex numbers CList,
	% make it into a vector, V.  A complex number is a dotted pair of
	% reals or integers [r|i].  The resultant vector will be a contiguous
	% list of double precision numbers (regardless of whether the numbers
	% can be represented as integers or not. Thus, for each complex number
	% 4 words are required to store it in the vector.

make_cvector(CList,V) :-
	make_cvector2(CList,List),
	make_vector(List,V).
make_cvector2([],[]).
make_cvector2([[R|I]|Tail],[R,I|NewTail]) :-
	make_cvector2(Tail,NewTail).




	% vc_arg(N,V,C) the Nth complex number of complex vector V is 
	% complex number C.

vc_arg(N,V,[R|I]) :-
	N2 is N * 2,
	varg(N2,V,I),
	N3 is N2 - 1,
	varg(N3,V,R).




	% vc_arg_first(N,V,Vout) Vout is a complex vector consisting of the
	% first N complex numbers of vector V.  

vc_arg_first(N,V,Vout) :- 
	N2 is N * 2, 
	varg_first(N2,V,Vout).  



	% vc_arg_last(N,V,Vout) Vout is a complex vector consisting of the last
	% N complex numbers of complex vector V.

vc_arg_last(N,V,Vout) :-
	N2 is N * 2,
	varg_last(N2,V,Vout).


	% vc_length(V,L) L is the number of complex numbers in vector V.

vc_length(V,L) :-
	vlength(V,L1),
	L is L1/2.



	% vc_mag2(V,Sr) Sr is the scalar real result of the sum of the 
	% squares of each element in real vector V.

vc_mag2(V,Sr) :-
	vcc_dot(V,V,Sr).



	% vc_mag(V,Sr) Sr is the square root of the sum of the squares of
	% each element in real vector V.

vc_mag(V,Sr):-
	vcc_dot(V,V,Sr1),
	Sr is sqrt(Sr1).


	% vsplit(+V,-V1,-V2) if V has an even number of elements the first
	% half of the elements of V will be put into a new vector V1 and
	% the last half will be put into the new vector V2.

vsplit(V,V1,V2) :-
	vlength(V,N),
	N2 is N / 2,
	integer(N2),!,		% will fail if not even number
	varg_first(N2,V,V1),
	varg_last(N2,V,V2).

vsplit(V,_,_) :-
	write('vector error: cannot split odd vector'),nl,fail.


vr_split(V,V1,V2) :-
	vsplit(V,V1,V2).

vc_split(V,V1,V2) :-
	vlength(V,N),
	Mod is N mod 4,
	Mod == 0,!,
	vsplit(V,V1,V2).

vc_split(V,_,_) :-
	write('vector error: cannot split odd vector'),nl,fail.
