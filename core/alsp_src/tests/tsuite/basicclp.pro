b(0,X,Y) :- X::real(4,7),Y::real, {Y==X+1}.

b(1,X,Y) :- X::real(0,3), {Y is X*(X+1),Y==2}.

b(2,X,Y) :- X::real(1,3), {Y**2==X}.
b(a2,X,Y) :- [X,Y]::real(1,3), {Y**2==X}.

b(3,X,Y) :- [X,Y]::real, {1==X + 2*Y, Y - 3*X==0}.
b(4,X,Y) :- {1==X + 2*Y, Y - 3*X==0}.

b(5,X,Y) :- X::real(2,5),{Y == X**3}.
b(a5,X,Y) :- X::real(4,9),Y::real, {Y**2 == X}.
b(b5,X,Y) :- X::real(4,9),Y::real, {Y**2 == X, Y>0}.
b(c5,X,Y) :- X::real(4,9),Y::real(0,50000), {Y**2 == X}.


b(6,X,Y) :- { X**3 + Y**3 ==2*X*Y, X**2 + Y**2==1, X>=0}.

b(7,X,Y) :- [X,Y]::real(0,1000), {X is 4.5, 9 == (4 ** Y)}.
b(8,X) :- X::real(0,1000), {9 == (3 ** X)}.

b(9,X,Y) :- X::real(1,4), Y::real, {Y == exp(X)}.


b(10,X) :- X::real(0,1), {0==35*X**256 -14*X**17 + X}, solve(X).

b(11,X) :- X::real(0,100), { 0 == (2*X + 17.6)*(4.3*X - 34) }, solve(X).



	/*-------------------------------------
	 | Elementary tests for {}:
	 *------------------------------------*/

g1 :- {foobar(X,Y)}, X=hi, write(bong),nl,flush_output,Y=there.

g2 :- {foobar(X,Y)}, X=hi, write(bong),nl,flush_output,fail.
g2 :- write(g2_cl2),nl.

g3 :-
	{foobar(X,Y)}, 
	X=hi(Boy), 
	write(bing),nl,flush_output,
	Y = there,
	write(bong),nl,flush_output,
	Boy = silly.
	
g4 :-
	{foobar(X,Y)}, 
	X=hi(Boy), 
	write(bing),nl,flush_output,
	Y = there,
	write(bong),nl,flush_output.
	
