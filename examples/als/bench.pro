/*----------------------------------------------------------------*
 |	bench.pro
 |	Copyright (c) 1986-2015 by Applied Logic Systems
 |	A Symbolic Equation Solver
 |		-- equation solving benchmark
 |      Author:  Keith Hughes
 |
 | solve0/3 is used to solve simple algebraic equations using meta-level
 | inference.  Running main will run this solver for a fairly complex
 | equation and report on the time taken by the solver.  main is also
 | invoked by autotest.pro.
 |
 | Examples:
 |
 | ?- solve(y=m*x+b, x, Answer).
 |
 | ?- solve(((2^(cos(x)^2)*2^(sin(x)^2))^sin(x))^cos(x)=2^(1/4), x, Answer).
 |
 *----------------------------------------------------------------*/
:- op(200,xfy,'^').

X = X.

main :-
        Y is cputime,
        solve0(((2^ (cos(x)^2)*2^ (sin(x)^2))^sin(x))^cos(x)=2^ (1/4),x,Ans),
        Z is cputime,
        DeltaTime is Z-Y,
        write(Ans),nl,
        write(time=DeltaTime),nl.

contains(X,X) :- !.

contains(X,Y) :- atomic(Y),!,fail.

contains(X,Struct) :-
   functor(Struct,_,A),!,
   contains1(X,Struct,0,A).

contains1(_,S,A,A) :- !, fail.
contains1(X,S,A,_) :- 
   E is A + 1, arg(E,S,Foo),
   contains(X,Foo).
contains1(X,S,A,W) :-
   E is A + 1,!,
   contains1(X,S,E,W).

count(X,Term,C) :-
  count(X,Term,0,C).

count(X,X,N,EN) :-
  EN is N + 1,!.

count(_,A,N,N) :-
  atomic(A),!.

count(X,Struct,N,EN) :-
   functor(Struct,_,Arity),!,
   count1(X,Struct,0,Arity,N,EN).

count1(X,_,Arity,Arity,N,N) :- !.
count1(X,Struct,CN1,CN2,N,EN) :-
   CN3 is CN1 + 1,
   arg(CN3,Struct,S),
   count(X,S,N,N1),
   count1(X,Struct,CN3,CN2,N1,EN).

singleoccur(X,Term) :-
   count(X,Term,1).

nooccur(X,Term) :-
   count(X,Term,0).

position(X,X,[0]).
position(X,Term,[]) :- atomic(Term).
position(X,Term,Pos) :-
   functor(Term,_,Arity),
   position1(X,Term,0,Arity,1,Pos).
position(_,_,[]).

position1(X,Term,Arity,Arity,_,_) :- !,fail.
position1(X,Term,CN1,CN3,N,[N|L]) :-
   CN2 is CN1 + 1,
   arg(CN2,Term,S),
   N1 is N + 1,
   position2(X,S,N1,L).
position1(X,Term,CN1,CN3,N,L) :-
   CN2 is CN1 + 1,
   N1 is N + 1,
   position1(X,Term,CN2,CN3,N1,L).

position2(X,X,N,[]).
position2(_,A,_,_) :-
   atomic(A),!,fail.
position2(X,Struct,N,NR) :-
   functor(Struct,_,Arity),
   position1(X,Struct,0,Arity,1,NR).

isolate([],Ans,TidyAns) :- !,
   tidy(Ans,TidyAns).
isolate([0],Ans,Ans) :- !.
isolate([R|S],Eqn,Ans) :- !,
   isolax(R,Eqn,New,Cond),!,
   isolate(S,New,Ans).


collect(X,Old=Oth,New=Oth) :-
   leastDom(X,Old,LOld,RHS,NNew),
   collax(U,LHS,RHS),
   match(LOld,LHS),
   contains(X,U), tidy(NNew,New).

attract(X,Old=Oth,New=Oth) :-
   leastDom(X,Old,LOld,RHS,NNew),
   attrax(UList,LHS,RHS),
   match(LOld,LHS),
   checklist(contains(X),UList),
   tidy(NNew,New).

checklist(_,[]) :- !.
checklist(contains(X),[L|E1]) :-
   contains(X,L),!,
   checklist(contains(X),E1).

solveList([H|T],Unknown,[Answer1|Rest]) :-
   solve(H,Unknown,Answer1),
   solveList0(T,Unknown,Rest).

solveList(Equation,Unknown,Answer) :- 
   % not(Equation = [_|_]),
   Equation \= [_|_],
   solve(Equation,Unknown,Answer).

solveList0([],_,[]) :- !.
solveList0([H|T],Unknown,[Answer1|Rest]) :-
   solve(H,Unknown,Answer1),
   solveList0(T,Unknown,Rest).

solve(LHS=RHS,X,Ans) :-
   canonical(LHS-RHS=0,Equation),
   tidy(Equation,CleanEquation1),!,
   solve0(CleanEquation1,X,CleanEquation2),
   tidy(CleanEquation2,Ans).

solve0(Eqn=Oth,X,Ans) :-
   singleoccur(X,Eqn),
   position(X,Eqn,List),!,
   isolate(List,Eqn=Oth,Ans).

solve0(Eqn,X,Ans) :-
   collect(X,Eqn,New),!,
   solve0(New,X,Ans).

solve0(Eqn,X,Ans) :-
   attract(X,Eqn,New),
   closeness(X,Eqn,EC), closeness(X,New,NC),
   EC > NC, !,
   solve0(New,X,Ans).

isDom(X,Y) :- atomic(Y),!,atomic(X),X=Y.
isDom(X,Unary) :-
   functor(Unary,_,1),!,fail.
isDom(X,Term) :-
   functor(Term,_,Arity),
   isDom(X,Term,0,Arity).

isDom(_,_,End,End) :- !.
isDom(X,Term,Now,End) :-
   Pos is Now + 1,!,arg(Pos,Term,XTerm),
   contains(X,XTerm),
   isDom(X,Term,Pos,End).

leastDom(X,Term,Term,Q,Q) :-
   isDom(X,Term).
leastDom(_,Term,_,_,_) :-
   atomic(Term),!,fail.
leastDom(X,Term,Least,R,Re) :-
   functor(Term,Name,Arity),
   functor(Re,Name,Arity),
   leastDom(X,Term,Least,R,Re,0,Arity).
leastDom(_,_,_,_,_,End,End) :- !, fail.
leastDom(X,Term,Least,R,Re,Now,End) :-
   Pos is Now + 1,
   arg(Pos,Term,STerm), arg(Pos,Re,SRe),
   leastDom(X,STerm,Least,R,SRe),
   fixDom(Term,Re,Pos,End).
leastDom(X,Term,Least,R,Re,Now,End) :-
   Pos is Now + 1,
   arg(Pos,Term,STerm), arg(Pos,Re,STerm),
   leastDom(X,Term,Least,R,Re,Pos,End).

fixDom(_,_,End,End) :- !.
fixDom(Term,Re,Now,End) :-
   Pos is Now + 1,
   arg(Pos,Term,STerm), arg(Pos,Re,STerm),
   fixDom(Term,Re,Pos,End).

closeness(X,X,0) :- !.
closeness(_,Y,0) :- atomic(Y),!.
closeness(X,Term,Count) :-
   leastCover(X,Term,Least),
   functor(Least,_,Arity),
   depth(1,X,Least,0,Arity,List,[]), addList(List,0,Count).
closeness(_,_,0).

depth(_,_,_,End,End,List,List) :- !.
depth(Depth,X,Term,Now,End,[Depth|Rest],EList) :-
   Pos is Now + 1, arg(Pos,Term,X),!,
   depth(Depth,X,Term,Pos,End,Rest,EList).
depth(Depth,X,Term,Now,End,List,EList) :-
   Pos is Now + 1, arg(Pos,Term,Foo),
   atomic(Foo),!,
   depth(Depth,X,Term,Pos,End,List,EList).
depth(Depth,X,Term,Now,End,List,EList) :-
   Pos is Now + 1, arg(Pos,Term,STerm),
   functor(STerm,_,NArity), NDepth is Depth + 1,!,
   depth(NDepth,X,STerm,0,NArity,List,List1),
   depth(Depth,X,Term,Pos,End,List1,EList).

addList([],Count,Count) :- !.
addList([Num|Rest],OldCount,EndCount) :-
   NewCount is OldCount + Num,!,
   addList(Rest,NewCount,EndCount).

leastCover(_,Y,_) :- atomic(Y),!,fail.
leastCover(X,Term,Least) :-
   functor(Term,_,Arity),!,
   leastCover(X,Term,Least,0,Arity).

leastCover(_,_,_,End,End) :- !, fail.
leastCover(X,Term,Least,Now,End) :-
   Pos is Now + 1,
   arg(Pos,Term,STerm),
   contains(X,STerm),!,
   lc_semi(X,Term,STerm,Pos,End,Least), !.
leastCover(X,Term,Least,Now,Ens) :-
   Pos is Now + 1,!,
   leastCover(X,Term,Least,Pos,End).

   lc_semi(X,Term,STerm,Pos,End,Term) :- leastCover0(X,Term,Pos,End).
   lc_semi(X,Term,STerm,Pos,End,Least) :- leastCover(X,STerm,Least).

leastCover0(_,_,End,End) :- !, fail.
leastCover0(X,Term,Now,End) :-
   Pos is Now + 1,
   arg(Pos,Term,STerm),!,
   lc0_semi(X,STerm,Term,Pos,End).

   lc0_semi(X,STerm,Term,Pos,End) :- contains(X,STerm), !.
   lc0_semi(X,STerm,Term,Pos,End) :- leastCover0(X,Term,Pos,End).

dmatch(X,Y) :-
   match(X,Y),!.

match(X,X) :- !.
match(X,Y) :- atomic(X), !, fail.
match(X,Y) :- atomic(Y), !, fail.

match(X,Y) :-
   makebag(X,BagX),
   makebag(Y,BagY),!, % if can make bag, don't need below (???)
   compbag(BagX,BagY).
match(X,Y) :-
   functor(X,Name,Arity),
   functor(Y,Name,Arity),
   matchArgs(X,Y,0,Arity).

matchArgs(_,_,End,End) :- !.
matchArgs(X,Y,Now,End) :-
   Pos is Now + 1,
   arg(Pos,X,XP),
   arg(Pos,Y,YP),
   match(XP,YP),
   matchArgs(X,Y,Pos,End).

makebag(X,X) :- atomic(X), !.
makebag(X,X) :- var(X),!.
makebag(X*Y,timesbag(List)) :- !,
   timesbag(X,Y,List,[]).
makebag(X+Y,addbag(List)) :-
   addbag(X,Y,List,[]).

addbag(X,Y,List,EList) :- !,
   addbag1(X,List,List1),
   addbag1(Y,List1,EList).

addbag1(X,[X|Rest],Rest) :-
   var(X),!.
addbag1(X+Y,List,EList) :- !,
   addbag1(X,List,List1),
   addbag1(Y,List1,EList).
addbag1(X,[X|Rest],Rest).

timesbag(X,Y,List,EList) :- !,
   timesbag1(X,List,List1),
   timesbag1(Y,List1,EList).

timesbag1(X,[X|Rest],Rest) :- var(X),!.
timesbag1(X*Y,List,EList) :- !,
   timesbag1(X,List,List1),
   timesbag1(Y,List1,EList).
timesbag1(X,[X|Rest],Rest).

compbag(timesbag(X),timesbag(Y)) :- !,
   complist(times,X,Y).
compbag(addbag(X),addbag(Y)) :- !,
   complist(add,X,Y).

complist(_,X,X).
complist(Place,[Var],List) :-
   var(Var),!,
   fixlist(Place,Var,List).
complist(Place,List,[Var]) :-
   var(Var),!,
   fixlist(Place,Var,List).
complist(Type,X,Y) :-
   memList(X,MemX,RestX),
   memList(Y,MemY,RestY),
   match(MemX,MemY),
   complist(Type,RestX,RestY).

memList([Item|RestList],Item,RestList).
memList([BeginList|EndList],Item,[BeginList|RestList]) :-
   memList(EndList,Item,RestList).

fixlist(_,Last,[Last]) :- !.
fixlist(times,A*B,[A|Rest]) :- !,
   fixlist(times,B,Rest).
fixlist(add,A+B,[A|Rest]) :- !,
   fixlist(add,B,Rest).
%
% these two procedures prepare things to be beat upon, or clean things up
% afterwards. they are written to exhaustively apply rewrite rules until
% no more will work.
%

canonical(Old,Old) :-
   atomic(Old),!.
canonical(Old,New) :-
   canonicalax(TOld,Inter),
   dmatch(Old,TOld),
   canonical(Inter,New). % apply these babies til we can't ride 'em no more
canonical(Old,New) :-
   functor(Old,Name,Arity),
   functor(Inter,Name,Arity),
   canonical(Old,Inter,0,Arity),
   funky2(Old,Inter,New). 
canonical(Old,Old).

funky2(Old,Old,Old) :- !.
funky2(_,Inter,New) :-
   canonical(Inter,New).

canonical(_,_,End,End) :- !.
canonical(Old,Temp,Now,End) :-
   Pos is Now + 1,
   arg(Pos,Old,Canonical1),
   arg(Pos,Temp,Canonical2),!,
   canonical(Canonical1,Canonical2),
   canonical(Old,Temp,Pos,End).

tidy(Old,Old) :-
   atomic(Old),!.
tidy(U=V,true) :-
   match(U,V),!.
tidy(V,U) :-
   match(V,W+0),!,
   tidy(W,U).
tidy(V,0) :- 
   match(V,U + (-U)),!.
tidy(V,0) :-
   match(V,_*0),!.
tidy(V,W) :-
   match(V,1*U),!,
   tidy(U,W).
tidy(U/1,W) :- !,
   tidy(U,W).
tidy(0/U,0) :- !.
tidy(-(-U),V) :- !,
   tidy(U,V).
tidy(-0,0) :- !.
tidy(1^_,1) :- !.
tidy(log(X,Y),1) :-
   match(X,Y),!.
tidy(ln(e),1).
tidy(U^1,W) :- !,
   tidy(U,W).
tidy(_^0,1) :- !.
tidy(V,1) :-
   match(V,cos(X)^2+sin(X)^2),!.
tidy(Old,New) :-
   functor(Old,Name,Arity),
   functor(Inter,Name,Arity),
   tidy(Old,Inter,0,Arity),
   funky1(Old,Inter,New).
tidy(Old,Old).

funky1(Old,Old,Old) :- !.
funky1(_,Inter,New) :-
   tidy(Inter,New).

tidy(_,_,End,End) :- !.
tidy(Old,Temp,Now,End) :-
   Pos is Now + 1,
   arg(Pos,Old,Tidy1),
   arg(Pos,Temp,Tidy2),!,
   tidy(Tidy1,Tidy2),
   tidy(Old,Temp,Pos,End).

%
% the axioms used by the above
%


canonicalax(-(X*Y),(-X)*Y).
canonicalax(-(X+Y),(-X) + (-Y)).
canonicalax(X-Y,X + (-Y)).

isolax(1,-U=V,U= (-V),true).
isolax(1,V + U = W, V = W + (-U), true).
isolax(2,V + U = W, U = W + (-V), true).
isolax(1,V * U = W, V = W / U, true).
isolax(2,V * U = W, U = W / V, true).
isolax(1,V / U = W, V = W * U, true).
isolax(2,V / U = W, U = V / W, true).
isolax(1,X ^ Y = Z, X = Z ^ (1 / Y), true).
isolax(2,Y ^ X = Z, X = log(Y,Z), true).
isolax(1,log(Base,Y) = Exp, Base = Y ^ (1 / Exp), true).
isolax(2,log(Base,Y) = Exp, Y = Base^Exp,true).
isolax(1,ln(Y) = Exp, Y = e^Exp,true).
isolax(1,sin(U)=V,U=n0*180+ (-1)^N*arcsin(V), arbint(N)).
isolax(1,cos(U)=V,U=n0*180+arccos((-1)^N*V), arbint(N)).
isolax(1,tan(U)=V,U=arctan(V),true).
isolax(1,arcsin(U)=V,U=sin(V),true).
isolax(1,arccos(U)=V,U=cos(V),true).
isolax(1,arctan(U)=V,U=tan(V),true).
isolax(1,sinh(U)=V,U=arcsinh(V),true).
isolax(1,cosh(U)=V,U=arccosh(V),true).
isolax(1,tanh(U)=V,U=arctanh(V),true).
isolax(1,arcsinh(U)=V,U=sinh(V),true).
isolax(1,arccosh(U)=V,U=cosh(V),true).
isolax(1,arctanh(U)=V,U=tanh(V),true).

collax(W,U*W+V*W,(U+V)*W).
collax(U,(U+W)* (U+ (-W)),(U^2 + (-(W^2)))).
collax(W,(U+W)* (U+ (-W)),(U^2 + (-(W^2)))).
collax(U,log(U,X)+log(U,Y),log(U,X*Y)).
collax(U,sin(U)*cos(U),sin(2*U)*2^ (-1)).
collax(U,U+U*V,U* (1+V)).
collax(U,U*U,U^2).
collax(U,U+U,U*2).
collax(U,U*U^N,U^ (N+1)).
collax(U,U^M*U^N,U^ (M+N)).
collax(U,2*sin(U)*cos(U),sin(2*U)).
collax(U,sin(U)*cos(V)+cos(U)*sin(V),sin(U+V)).
collax(U,sin(U)*cos(V)-cos(U)*sin(V),sin(U-V)).
collax(U,cos(U)*cos(V)-sin(U)*sin(V),cos(U+V)).
collax(U,cos(U)*cos(V)+sin(U)*sin(V),cos(U-V)).

attrax([U,V],U*W+V*W,(U+V)*W).
attrax([U,V],W^U*W^V,W^ (U+V)).
attrax([U,V],log(W,U)+log(W,V),log(W,U*V)).
attrax([U,V],ln(U)+ln(V),ln(U*V)).
attrax([U,V],ln(U)+ (-ln(V)),ln(U/V)).
attrax([V,W],(U^V)^W,U^ (V*W)).
attrax([U,V],U^ (V*W),(U^V)^W).
attrax([U,V],(W+U)+V,W+ (U+V)).
attrax([U,V],(W*U)*V,W* (U*V)).
attrax([U,V],sin(U)*cos(V)+cos(U)*sin(V),sin(U+V)).
attrax([U,V],sin(U)*cos(V)-cos(U)*sin(V),sin(U-V)).
attrax([U,V],cos(U)*cos(V)-sin(U)*sin(V),cos(U+V)).
attrax([U,V],cos(U)*cos(V)+sin(U)*sin(V),cos(U-V)).
