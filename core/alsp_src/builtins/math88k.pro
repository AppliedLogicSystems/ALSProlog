%=========================================================================================
%
%  Creation:    Craig Thornley
%		April 27, 1989
%
%  Copyright (c) 1989 Motorola, Inc.
%
%  blt_mth88.pro	contains vector primitive builtins
%
%  Modified:	when		who	what
%		89/06/12	CT	added vsplit, vr_split & vc_split
%               92/02/27	SD      corrected vc_mag and vc_mag2
%		92/03/02	SM	add more primitives (including scalar), rename
%					file, and make these predicates builtins
%		92/03/20	SM	add SD's lsplit/3
%
%=========================================================================================

module builtins.

%-----------------------------------------------------------------------------------------
%
%  REAL VECTOR PRIMITIVES
%
%-----------------------------------------------------------------------------------------

export make_rvector/2.

%
%  A list L of real-valued numbers is converted to a corresponding real-valued vector V.
%

make_rvector(L,V) :- make_vector(L,V).


export vr_arg/3.

%
%  A is unified with the Nth real-valued component of real-valued vector V.
%

vr_arg(N,V,A) :- varg(N,V,A).


export vr_arg_first/3.

%
%  Vout is a real-valued vector composed of the first N components of the real-valued
%  vector V. 
%

vr_arg_first(N,V,Vout) :- varg_first(N,V,Vout).


export vr_arg_last/3.

%
%  Vout is a real-valued vector composed of the last N components of the real-valued
%  vector V.
%

vr_arg_last(N,V,Vout) :- varg_last(N,V,Vout).


export vr_length/2.

%
%  L is the number of components in the real-valued vector V.
%

vr_length(V,L) :- vlength(V,L).



export vr_mag/2.

%
%  Sr is the non-negative real-valued scalar representing the magnitude of the real-valued
%  vector V.
%

vr_mag(V,Sr):- vrr_dot(V,V,Sr1), psqrt(Sr1,Sr).



%-----------------------------------------------------------------------------------------
%
%  COMPLEX VECTOR PRIMITIVES
%
%  V is the vector representation of the complex-valued list CList.
%  A complex number is a dotted pair of reals or integers, {R|I].
%  Vector V will be a contiguous list of double precision floating point numbers.
%  Thus, each complex number requires 4 words of storage.
%
%-----------------------------------------------------------------------------------------

export make_cvector/2.

make_cvector(CList,V) :- make_cvector2(CList,List), make_vector(List,V).

make_cvector2([],[]).
make_cvector2([[R|I]|Tail],[R,I|NewTail]) :- make_cvector2(Tail,NewTail).


export vc_arg/3.

%
%  The Nth component of the complex-valued vector V is the dotted pair [R|I].
%

vc_arg(N,V,[R|I]) :-
	N2 is N * 2,
	varg(N2,V,I),
	N3 is N2 - 1,
	varg(N3,V,R).


export vc_arg_first/3.

%
%  Vout is the complex-valued vector composed of the first N components of the complex-
%  valued vector V.
%

vc_arg_first(N,V,Vout) :- N2 is N * 2, varg_first(N2,V,Vout).  


export vc_arg_last/3.

%
%  Vout is the complex-valued vector composed of the last N components of the complex-
%  valued vector V.
%

vc_arg_last(N,V,Vout) :- N2 is N * 2, varg_last(N2,V,Vout).


export vc_length/2.

%
%  L is the number of components in the complex-valued vector V.
%

vc_length(V,L) :- vlength(V,L1), L is L1/2.


export vc_mag2/2.

%
%  Sr is the non-negative real-valued scalar representing the squared magnitude of the 
%  complex-valued vector V.
%
%  Correction: replaced  vcc_dot(V,V,Sr) 
%                    by  vcjc_dot(V,V,[Sr|_])                                - SD 92/02/27
%

vc_mag2(V,Sr) :- vcjc_dot(V,V,[Sr|_]).                       


export vc_mag/2.

%
%  Sr is the non-negative real-valued scalar representing the magnitude of the complex-
%  valued vector V.
%
%  Correction: replaced  vcc_dot(V,V,Sr1), psqrt(Sr1,Sr) 
%                    by  vcjc_dot(V,V,[R|_]), psqrt(R,Sr)                    - SD 92/02/27

vc_mag(V,Sr) :- vcjc_dot(V,V,[R|_]), psqrt(R,Sr).



%-----------------------------------------------------------------------------------------
%
%  SPLITTING REAL AND COMPLEX VECTORS
%
%  If vector V has an even number of components, the first half will be put into vector
%  V1 and the second half will be put into vector V2. Otherwise, the call will fail.
%
%-----------------------------------------------------------------------------------------

vsplit(V,V1,V2) :-
	vlength(V,N),
	N2 is N / 2,
	integer(N2),!,		                            % will fail if not even number
	varg_first(N2,V,V1),
	varg_last(N2,V,V2).

vsplit(V,_,_) :- write('vector error: cannot split odd vector.'),nl,fail.


export vr_split/3.

vr_split(V,V1,V2) :- vsplit(V,V1,V2).

export vc_split/3.

vc_split(V,V1,V2) :-
	vlength(V,N),
	Mod is N mod 4,
	Mod == 0,!,
	vsplit(V,V1,V2).

vc_split(V,_,_) :- write('vector error: cannot split odd vector'),nl,fail.


% lsplit(+L,-Top, -Bot) splits list L into two halves, Top and Bot.  If L has an odd number
% of elements, lsplit/3 fails.
% Note that in this Prolog implementation, L=[] is allowed, whereas with vectors there is no
% such thing (lsplit/4 also fails for L=[]).


export lsplit/3.

lsplit(X,T,B) :-
	lsplit2(X,X,T,B).

lsplit2([],B,[],B).
lsplit2([_,_|Xs],[Y|Ys],[Y|T],B) :-
	lsplit2(Xs,Ys,T,B).


% lsplit/4(+Len,+L,-Top,-Bot) is an analogue to lsplit/3.  lsplit/4 is coded in 88k
% assembler.
% Given L and its length Len, split L into Top and Bot.  lsplit/4 behaves slightly differently
% than lsplit/3; while lsplit/4 fails when Len is not bound to an even positive integer,
% it does not check the actual length of the list.  So lsplit/4 will "split" an odd list but 
% Top and Bot will not be true halves, obviously.


endmod.

