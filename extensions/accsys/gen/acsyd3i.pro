module builtins.

export a_dDfldno/8.
a_dDfldno(A,B,C,D,E,F,G,H) :-
	acc0(0,A,B,C,D,E,F,G,H).
export a_dUnktoa/6.
a_dUnktoa(A,B,C,D,E,F) :-
	acc1(0,A,B,C,D,E,F).
export a_dDapprec/3.
a_dDapprec(A,B,C) :-
	acc2(0,A,B,C).
export a_dUdftodk/3.
a_dUdftodk(A,B,C) :-
	acc3(0,A,B,C).
export a_dUnktod/3.
a_dUnktod(A,B,C) :-
	acc4(0,A,B,C).
export a_dUnftod/3.
a_dUnftod(A,B,C) :-
	acc5(0,A,B,C).
export a_dDrecsta/3.
a_dDrecsta(A,B,C) :-
	acc6(0,A,B,C).
export a_dDrclrec/3.
a_dDrclrec(A,B,C) :-
	acc6(1,A,B,C).
export a_dDrmrec/3.
a_dDrmrec(A,B,C) :-
	acc6(2,A,B,C).
export a_dDdelrec/3.
a_dDdelrec(A,B,C) :-
	acc6(3,A,B,C).
export a_dDreccnt/3.
a_dDreccnt(A,B,C) :-
	acc6(4,A,B,C).
export a_dDbuffs/3.
a_dDbuffs(A,B,C) :-
	acc7(0,A,B,C).
export a_dUleap/2.
a_dUleap(A,B) :-
	acc8(0,A,B).
export a_dDfields/2.
a_dDfields(A,B) :-
	acc8(1,A,B).
export a_dDreclen/2.
a_dDreclen(A,B) :-
	acc8(2,A,B).
export a_dDflush/2.
a_dDflush(A,B) :-
	acc8(3,A,B).
export a_dDclose/2.
a_dDclose(A,B) :-
	acc8(4,A,B).
export a_dUdkto3i/4.
a_dUdkto3i(A,B,C,D) :-
	acc9(0,A,B,C,D).
export a_dUdfto3i/4.
a_dUdfto3i(A,B,C,D) :-
	acc9(1,A,B,C,D).
export a_dUdtonf/5.
a_dUdtonf(A,B,C,D,E) :-
	acc10(0,A,B,C,D,E).
export a_dUnftonk/5.
a_dUnftonk(A,B,C,D,E) :-
	acc11(0,A,B,C,D,E).
export a_dUatonf/5.
a_dUatonf(A,B,C,D,E) :-
	acc12(0,A,B,C,D,E).
export a_dU3itodk/5.
a_dU3itodk(A,B,C,D,E) :-
	acc13(0,A,B,C,D,E).
export a_dU3itodf/5.
a_dU3itodf(A,B,C,D,E) :-
	acc13(1,A,B,C,D,E).
export a_dDgetrec/4.
a_dDgetrec(A,B,C,D) :-
	acc14(0,A,B,C,D).
export a_dDupdrec/4.
a_dDupdrec(A,B,C,D) :-
	acc14(1,A,B,C,D).
export a_dDinsrec/4.
a_dDinsrec(A,B,C,D) :-
	acc14(2,A,B,C,D).
export a_dUdtonk/3.
a_dUdtonk(A,B,C) :-
	acc15(0,A,B,C).
export a_dUtoday/3.
a_dUtoday(A,B,C) :-
	acc16(0,A,B,C).
export a_dUexpnm/3.
a_dUexpnm(A,B,C) :-
	acc17(0,A,B,C).
export a_dUatocf/3.
a_dUatocf(A,B,C) :-
	acc18(0,A,B,C).
export a_dDopen/4.
a_dDopen(A,B,C,D) :-
	acc19(0,A,B,C,D).
export a_dversion/1.
a_dversion(Value) :- 
	acc_gv(0,Value).
export a_d_report/1.
a_d_report(Value) :- 
	acc_gv(1,Value).
export a_dretcode/1.
a_dretcode(Value) :- 
	acc_gv(2,Value).
export a_d_blksiz/1.
a_d_blksiz(Value) :- 
	acc_gv(3,Value).
export a_d_request/1.
a_d_request(Value) :- 
	acc_gv(4,Value).
export a_d_recno/1.
a_d_recno(Value) :- 
	acc_gv(5,Value).

endmod.     %  builtins
