module builtins.

export a_dDfldnm/8.
a_dDfldnm(A,B,C,D,E,F,G,H) :-
	acc0(0,A,B,C,D,E,F,G,H).
export a_dDfldno/8.
a_dDfldno(A,B,C,D,E,F,G,H) :-
	acc1(0,A,B,C,D,E,F,G,H).
export a_dUleap/2.
a_dUleap(A,B) :-
	acc2(0,A,B).
export a_dTclose/2.
a_dTclose(A,B) :-
	acc2(1,A,B).
export a_dDfields/2.
a_dDfields(A,B) :-
	acc2(2,A,B).
export a_dDreclen/2.
a_dDreclen(A,B) :-
	acc2(3,A,B).
export a_dDflush/2.
a_dDflush(A,B) :-
	acc2(4,A,B).
export a_dDclose/2.
a_dDclose(A,B) :-
	acc2(5,A,B).
export a_dTmemuse/2.
a_dTmemuse(A,B) :-
	acc3(0,A,B).
export a_dTcreat/2.
a_dTcreat(A,B) :-
	acc3(1,A,B).
export a_dTchkmm/2.
a_dTchkmm(A,B) :-
	acc3(2,A,B).
export a_dTmemosz/4.
a_dTmemosz(A,B,C,D) :-
	acc4(0,A,B,C,D).
export a_dUdtonk/3.
a_dUdtonk(A,B,C) :-
	acc5(0,A,B,C).
export a_dUtoday/3.
a_dUtoday(A,B,C) :-
	acc6(0,A,B,C).
export a_dUexpnm/3.
a_dUexpnm(A,B,C) :-
	acc7(0,A,B,C).
export a_dUatocf/3.
a_dUatocf(A,B,C) :-
	acc8(0,A,B,C).
export a_dDgetrec/4.
a_dDgetrec(A,B,C,D) :-
	acc9(0,A,B,C,D).
export a_dDupdrec/4.
a_dDupdrec(A,B,C,D) :-
	acc9(1,A,B,C,D).
export a_dDinsrec/4.
a_dDinsrec(A,B,C,D) :-
	acc9(2,A,B,C,D).
export a_dDopen/4.
a_dDopen(A,B,C,D) :-
	acc10(0,A,B,C,D).
export a_dDcreat/4.
a_dDcreat(A,B,C,D) :-
	acc10(1,A,B,C,D).
export a_dUnftod/3.
a_dUnftod(A,B,C) :-
	acc11(0,A,B,C).
export a_dUnktod/3.
a_dUnktod(A,B,C) :-
	acc12(0,A,B,C).
export a_dUdftodk/3.
a_dUdftodk(A,B,C) :-
	acc13(0,A,B,C).
export a_dDcopy/3.
a_dDcopy(A,B,C) :-
	acc13(1,A,B,C).
export a_dDrecsta/3.
a_dDrecsta(A,B,C) :-
	acc14(0,A,B,C).
export a_dDrclrec/3.
a_dDrclrec(A,B,C) :-
	acc14(1,A,B,C).
export a_dDrmrec/3.
a_dDrmrec(A,B,C) :-
	acc14(2,A,B,C).
export a_dDdelrec/3.
a_dDdelrec(A,B,C) :-
	acc14(3,A,B,C).
export a_dDreccnt/3.
a_dDreccnt(A,B,C) :-
	acc14(4,A,B,C).
export a_dTopen/3.
a_dTopen(A,B,C) :-
	acc15(0,A,B,C).
export a_dDbuffs/3.
a_dDbuffs(A,B,C) :-
	acc15(1,A,B,C).
export a_dDapprec/3.
a_dDapprec(A,B,C) :-
	acc16(0,A,B,C).
export a_dU3itodk/5.
a_dU3itodk(A,B,C,D,E) :-
	acc17(0,A,B,C,D,E).
export a_dU3itodf/5.
a_dU3itodf(A,B,C,D,E) :-
	acc17(1,A,B,C,D,E).
export a_dUatonf/5.
a_dUatonf(A,B,C,D,E) :-
	acc18(0,A,B,C,D,E).
export a_dUnftonk/5.
a_dUnftonk(A,B,C,D,E) :-
	acc19(0,A,B,C,D,E).
export a_dUdtonf/5.
a_dUdtonf(A,B,C,D,E) :-
	acc20(0,A,B,C,D,E).
export a_dUdkto3i/4.
a_dUdkto3i(A,B,C,D) :-
	acc21(0,A,B,C,D).
export a_dUdfto3i/4.
a_dUdfto3i(A,B,C,D) :-
	acc21(1,A,B,C,D).
export a_dDdate/5.
a_dDdate(A,B,C,D,E) :-
	acc22(0,A,B,C,D,E).
export a_dUnktoa/6.
a_dUnktoa(A,B,C,D,E,F) :-
	acc23(0,A,B,C,D,E,F).
export a_dTgetmm/6.
a_dTgetmm(A,B,C,D,E,F) :-
	acc23(1,A,B,C,D,E,F).
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
