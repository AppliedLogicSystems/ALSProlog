
%
% THESE ARE EXAMPLES FROM THE PAPER:
%           Programming in CLP(BNR) 
%               Benhamou & Older
%           presented at PPCP '93
%  
%  Only those involving real intervals are included here.
%  This file uses the new V3.5 syntax.


b(1,X,Y) :- X::real(1,3), {Y**2==X}.
/*
    _X = [1.0000, 3.0001],
    _Y = [-1.7321, 1.7321]
*/

b(2,X,Y) :- [X,Y]::real, {1==X + 2*Y, Y - 3*X==0}. % explicit declarations
b(a2,X,Y) :- [X,Y]::real(0,10), {1==X + 2*Y, Y - 3*X==0}. % explicit declarations
/*
    _Y = [0.42857, 0.42858],
    _X = [0.14285, 0.14286]
*/

b(3,X,Y) :- {1==X + 2*Y, Y - 3*X==0}.   % implicit declarations
b(a3,X,Y) :- [X,Y]::real(-10,10),{1==X + 2*Y, Y - 3*X==0}.   % implicit declarations
/*
    _Y = [0.42857, 0.42858],
    _X = [0.14285, 0.14286]
*/

b(4,X,Y) :- [X,Y]::real(0.5,10),{X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }. % correct answer but may indicate bug
b(a4,X,Y) :- [X,Y]::real(0,5),[X,Y]::real(0.5,10),{X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }. % correct answer but may indicate bug
/*
    _X = [1.0966, 1.0967],
    _Y = [1.9486, 1.9487]
*/

b(5,X) :-  X::real(0,1), {0==35*X**256 -14*X**17 + X}, solve(X).
/*
    _X = [0.0000, 2.1020e-44] ;

    _X = [0.84794, 0.84795] ;

    _X = [0.99584, 0.99585]
*/

b(6, X,Y) :- { X**3 + Y**3 ==2*X*Y, X**2 + Y**2==1, X>=0}, solve(X).
b(a6, X,Y) :- [X,Y]::real(-10,10),{ X**3 + Y**3 ==2*X*Y, X**2 + Y**2==1, X>=0}, solve(X).
/*
    _X = [0.39104, 0.39106],
    _Y = [-0.92038, -0.92036] ;

    _X = [0.44977, 0.44980],
    _Y = [0.89313, 0.89315] ;

    _X = [0.89309, 0.89316],
    _Y = [0.44974, 0.44987] ;
*/

% version 4.3  (68020 + 68882)

b(7,X,Y) :-X::real(1,3), {Y**2==X}. 
/*
    ?- [_H527 :: real(1, 3), {_H541 ** 2 == _H527}]
       where [_H527 : real(1.0, 3.0),
              _H541 : real(-1.73205080756888, 1.73205080756888)]. 
YES
*/

b(8,X,Y) :-  [X,Y]::real, {1==X + 2*Y, Y - 3*X==0}. % explicit declarations
b(a8,X,Y) :-  [X,Y]::real(-100,100), {1==X + 2*Y, Y - 3*X==0}. % explicit declarations
/*
    ?- [[_H950, _H945] :: real,
        {1 == _H950 + 2 * _H945, _H945 - 3 * _H950 == 0}]
       where [_H945 : real(-89.3024169850972, 90.15955984224),
              _H950 : real(-179.31911968448, 179.604833970194)]. 
YES
?- interval_control(_,_,_).
?- interval_control(10000, 8, 1). 
    ?- interval_control(10000, 8, 1). 
YES

?- [X,Y]::real, {1==X + 2*Y, Y - 3*X==0}. % explicit declarations
    ?- [[_H950, _H945] :: real,
        {1 == _H950 + 2 * _H945, _H945 - 3 * _H950 == 0}]
       where [_H945 : real(0.428571428571429, 0.428571428571429),
              _H950 : real(0.142857142857143, 0.142857142857143)]. 
YES
?- {1==X + 2*Y, Y - 3*X==0}. 
    ?- {1 == _H853 + 2 * _H848, _H848 - 3 * _H853 == 0}
       where [_H848 : real(0.428571428571429, 0.428571428571429),
              _H853 : real(0.142857142857143, 0.142857142857143)]. 
YES
*/

b(9,X,Y) :- {X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }.
b(a9,X,Y) :- [X,Y]::real(-100,100),{X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }.
/*
    ?- {_H813 >= 0,
        _H818 >= 0,
        tan(_H813) == _H818,
        ((_H813 ** 2) + (_H818 ** 2)) == 5}
       where [_H813 : real(0.0, 2.23606797749979),
              _H818 : real(0.0, 2.23606797749979)]. 
YES
*/

b(10,Y) :- Y is tan(1.09).
b(a10,Y) :- Y::real(0,10),Y is tan(1.09).
/*
    ?- 1.91709182160686 is tan(1.09). 
YES
*/

b(11,X,Y) :- {X>=0,Y>=0,  X**2 + Y**2 == 5 }. 
b(a11,X,Y) :- [X,Y]::real(-10,10),{X>=0,Y>=0,  X**2 + Y**2 == 5 }. 
/*
    ?- {_H567 >= 0, _H572 >= 0, _H567 ** 2 + _H572 ** 2 == 5}
       where [_H567 : real(0.0, 2.23606797749979),
              _H572 : real(0.0, 2.23606797749979)]. 
YES
*/


b(12,X,Y) :- [X,Y]::real(0,_), {5>=X**2, 5>=Y**2,Y==tan(X)}.
b(a12,X,Y) :- [X,Y]::real(0,100), {5>=X**2, 5>=Y**2,Y==tan(X)}.
/*
    ?- [[_H922, _H917] : real(0, _H931),
        {5 >= _H922 ** 2, 5 >= _H917 ** 2, _H917 == tan(_H922)}]
       where [_H917 : real(0.0, 2.23606797749979),
              _H922 : real(0.0, 2.23606797749979)]. 
YES
*/

b(14, Y) :-  { tan(2.236)==Y}.
b(a14, Y) :-  Y::real(-10,10),{ tan(2.236)==Y}.
/*
    ?- {tan(2.236) == _H502}
       where [_H502 : real(-1.27473488083765, -1.27473488083765)]. 
YES
*/

b(20, X, Y) :- [X,Y]::real(0.0,0.5), {Y == X**2 }.

b(21, X, Y) :- [X,Y]::real(0.0,8.0), {Y == X**2, X==Y }, solve(X).

b(22, X, Y) :- [X,Y]::real(0.0,8.0), {Y == X*X, X==Y}, solve(X).
