/*==================================================================*
 |				int_net.c
 |			Copyright (c) 1995 Applied Logic Systems, Inc.
 |			Portions Copyright (c) 1995  BellNorthern Research Ltc.
 |
 |		Interval network maintance functions
 |
 |	Date:	December, 1995
 |	Author: Ken Bowen
 |		--Derived from BNR code
 *==================================================================*/
#include "defs.h"

#ifdef CONSTRDEBUG
int debug_constr_flag = 0;

void	
debugconstr()
{
	if (debug_constr_flag == 0)
		debug_constr_flag = 1;
	else
		debug_constr_flag = 0;
}

#endif /* CONSTRDEBUG */


#if defined(INTCONSTR)

#include "intrv.h"
#include "freeze.h"     
#include "intrv_pr.h"

	/*--- From intaux.c ---*/
extern int    extract_bds   PARAMS( (PWord *, int, fp *, fp *) );
extern PWord  get_intvl_tm	PARAMS((PWord *, int));

	/*--- From bmeta.c ---*/
extern int trailed_mangle0	PARAMS((PWord,PWord,int,PWord,int));

void update_propagate	PARAMS((double,double,PWord,int,PWord,PWord));
int ilnk_net			PARAMS ((void));

	/*--- From wam.c ---*/
extern void bind_int_unfreeze	PARAMS((PWord *,int *,double));

/*----------------------------------------------------*
 |	BLT("$iter_link_net",  5, ilinknet, "_ilinknet"), 
 *----------------------------------------------------*/

	/* Declare the global queue, with types: */
PWord qhead, qend;
int   qheadt, qendt;

	/* Vars for args to the primitives are globals: */
double zl,zh,xl,xh,yl,yh,ul,uh,vl,vh;
int status = 0;

	/* Include the (generated) code for the primitives: */
#include "intrv.c"

/*-----------------------------------------------------------------*
 |	ilinknet()
 |
 |	- C/Prolog boundary wrapper for '$iterate_link_net'
 |
 |	Pulls args off the Prolog stack and sets up the (first)
 |	primitive operation on the queue, and then calls ilnk_net()
 |	to drive the iteration over the queue.
 *-----------------------------------------------------------------*/
int
ilinknet()
{
	PWord OpCd, Z, X, Y, Goal;
	int OpCdt,Zt,Xt,Yt,Goalt;

	w_get_An(&OpCd, &OpCdt, 1);
	w_get_An(&Z, &Zt, 2);
	w_get_An(&X, &Xt, 3);
	w_get_An(&Y, &Yt, 4);
	w_get_An(&Goal, &Goalt, 5);

/*printf("ArgTypes: OpCdt=%d Zt=%d Xt=%d Yt=%d Goalt=%d\n",OpCdt,Zt,Xt,Yt,Goalt); 
printf("Z=%x X=%x, Y=%x Goal=%x\n",Z,X,Y,Goal); */

	/* No need to check OpCd, because we create it. */	

	if ((Zt != WTP_INTEGER) && ((Zt != WTP_REF) || (!CHK_DELAY(Z))))
		FAIL;
	if ((Xt != WTP_INTEGER) && ((Xt != WTP_REF) || (!CHK_DELAY(X))))
		FAIL;
	if ((Yt != WTP_INTEGER) && ((Yt != WTP_REF) || (!CHK_DELAY(Y))))
		FAIL;

	/*-----------------------------------------------------------------*
		The queue looks like this:

		qhead --> G1:  pop(OpCd1,Z1,X1,Y1,P1)
				                          | 
                                          V
		          G2:  pop(OpCd2,Z2,X2,Y2,P2)
				                          | 
                                          V
                                          ...
				                          | 
                                          V
		qend  --> Gn:  pop(OpCdn,Zn,Xn,Yn,Pn)

	  Each new entry is added at the end of the queue, by mangling 
	  the "Link" slot (arg 6) in the pop structure;  this is also
	  used as a "boolean" to tell whether or not a goal has been added
	  to the queue: if the link slot is an unbound variable, it is NOT 
	  currently on the queue, while if the link slot \= 0, it IS on 
	  the queue; 
	  Note that for the Pn link slot, we will set Pn to be Prolog 
	  integer 0, hence not an unbound variable, but something we 
	  can recognize as the end of the queue directly in the link.
	
	  When any pop structure is removed from the queue, the link slot
	  (arg 6) is mangled back to being an unbound variable.
	
	  When the last item is removed from the queue, we set 
	  qhead = qend = 0.
	 *-----------------------------------------------------------------*/

	qhead  = Goal;
	qheadt = Goalt;
	qend   = Goal;
	qendt  = Goalt;
	w_install_argn(Goal, 6, 0, WTP_INTEGER);

	if (ilnk_net())
		SUCCEED;
	else
		FAIL;
}

#define UNLINK_ALL { while (qheadt != WTP_INTEGER) { \
		w_get_argn(&next, &nextt, qhead, 6); \
		w_install_unbound_argn(qhead, 6); \
		qhead = next; \
		qheadt = nextt; } }

/*-----------------------------------------------------------------*
 |	ilnk_net()
 |
 |	- drives the iteration of the network over the queue
 *-----------------------------------------------------------------*/
void disp_infp	PARAMS((int,PWord,PWord,int,double,double,PWord,int,double,double,PWord,int,double,double));

void note_changes	PARAMS((void));

int
ilnk_net()
{
	PWord OpCd, Z, X, Y, Goal, next;
	int   OpCdt,Zt,Xt,Yt,Goalt,nextt;
	PWord IntrvTm;

	while (qheadt != WTP_INTEGER) 
	{
		status = 0;
/* printf(">>>>>>>ilnk_net-TOP WHILE: qhead=%x qend=%x <<<<<<<<<<\n",qhead,qend); */

			/* queue points at the first pop structure: */
		w_get_argn(&OpCd, &OpCdt, qhead, 1);
		w_get_argn(&Z, &Zt, qhead, 2);
		w_get_argn(&X, &Xt, qhead, 3);
		w_get_argn(&Y, &Yt, qhead, 4);
		w_get_argn(&Goal, &Goalt, qhead, 5);

		extract_bds((PWord *)Z, Zt, &zl, &zh);
		extract_bds((PWord *)X, Xt, &xl, &xh);
		extract_bds((PWord *)Y, Yt, &yl, &yh);

/*printf(">>>B-switch: OpCd=%d Goal=%x qhead=%d qend=%d\n",OpCd,Goal,qhead,qend);*/

#ifdef CONSTRDEBUG
	if (debug_constr_flag > 0)
		disp_infp(0,OpCd,Z,Zt,zl,zh,X,Xt,xl,xh,Y,Yt,yl,yh);
#endif

		switch (OpCd) {
			case 0 :	{ i_unequal(); break;}
			case 1 :	{ i_equal(); break;}
			case 2 : 	{ i_greatereq(); break;}
			case 3 :	{ i_higher(); break;}
			case 4 :	{ i_add(); break;}
			case 5 :	{ i_begin_tog(); break;}
			case 6 : 	{ i_finish_tog(); break;}
			case 7 :	{ i_inf(); break;}
			case 8 :	{ i_j_less(); break;}
			case 9 :	{ i_k_equal(); break;}
			case 10 :	{ i_lub(); break;}
			case 11 :	{ i_mul(); break;}
			case 12 :	{ i_narrower(); break;}
			case 13 :	{ i_or(); break;}
			case 14 :	{ i_pow_odd(); break;}
			case 15 :	{ i_qpow_even(); break;}
			case 16 :	{ i_rootsquare(); break;}
			case 17 :	{ i_vabs(); break;}
			case 18 :	{ i_wrap(); break;}
			case 19 :	{ i_xp(); break;}
			case 20 :	{ i_cos(); break;}
			case 21 :	{ i_sin(); break;}
			case 22 :	{ i_tan(); break;}
			default :	break;
		}

#ifdef CONSTRDEBUG
	if (debug_constr_flag > 0)
		disp_infp(1,OpCd,Z,Zt,zl,zh,X,Xt,xl,xh,Y,Yt,yl,yh);
#endif

	/* ----------------------------------------------------------------
	   if nothing changed, we're done with this queue element;
	   otherwise, there is work to do:
	   For each interval:

	   1. check consistency of the interval; fail if it is inconsistent;
		  -- need to reset the link arg of each qelemnt before we FAIL;
	   
	   2. the interval is consistent; first check whether it has collapsed 
		  into a point interval, & if so, rebind the variable it (the
		  frozen interval) is bound to, and raise the appropriate 
		  unfreeze interrupt;

	   3.  the interval is consistent, and is not a point interval;
		   update the 
		   , and propagate to the operations which use 
		   these intervals (put those ops on the queue); 
	 * ---------------------------------------------------------------- */

		if (status != 0)
		{
		if (status & ~link) { /* something changed */
			/* -------------------
			 |		z
         	 * ------------------- */
			unflip(z);
			if (z_changed & status) {
				if (zl > zh) {		/* z inconsistent */
						printf("z_changed & inconsistent:zl=%g zh=%g\n",zl,zh);  
					UNLINK_ALL;
					FAIL;
				}
					/* put boolean update here */
	   				/* -------------------------------------------------------*
					 | IntrvTm should be intvl(ProType,Var,UsedBy,L,U,UIA); 
					 | Below, if Z is bound (Zt != WTP_UNBOUND), we assume
					 | that Z is a point, which means failure here, because if
					 | it is already a point, it can't change....
					 *--------------------------------------------------------*/
				if (Zt == WTP_UNBOUND) 
					IntrvTm = (PWord)get_intvl_tm((PWord *)Z, Zt);
				else
					FAIL;
	   				/* -------------------------------------------------------*
					 |  End points have coalesced, so change (bind) the
					 |  underlying variable to a point.
					 *--------------------------------------------------------*/
				if (zl == zh) {
						printf("z_changed->point:zl=%g zh=%g\n",zl,zh);  
					bind_int_unfreeze((PWord *)Z,&Zt,zl);
				}
	   				/* -------------------------------------------------------*
					 *--------------------------------------------------------*/
				update_propagate(zl, zh, Z, Zt, IntrvTm, Goal);

			}	/*  z changed */

				/* -------------------
				 |		x
         		 * ------------------- */
			unflip(x);
			if (x_changed & status) {
				if (xl > xh) {
						printf("x_changed & inconsistent:xl=%g xh=%g\n",xl,xh);  
					UNLINK_ALL;
					FAIL;
				}
				if (xl == xh) {
						printf("x_changed->point:xl=%g xh=%g\n",xl,xh);  
					bind_int_unfreeze((PWord *)X,&Xt,xl);
				}
				if (Xt == WTP_UNBOUND)   
					IntrvTm = (PWord)get_intvl_tm((PWord *)X, Xt);
				update_propagate(xl, xh, X, Xt, IntrvTm, Goal);

			}	/* x changed */

				/* -------------------
				 |		y
         		 * ------------------- */
			unflip(y);
			if (y_changed & status) {
				if (yl > yh) {
						printf("y_changed & inconsistent:yl=%g yh=%g\n",yl,yh);  
					UNLINK_ALL;
					FAIL;
				}
				if (yl == yh) {
						printf("y_changed->point:yl=%g yh=%g\n",yl,yh);  
					bind_int_unfreeze((PWord *)Y,&Yt,yl);
				}
				if (Yt == WTP_UNBOUND)   
					IntrvTm = (PWord)get_intvl_tm((PWord *)Y, Yt);
				update_propagate(yl,yh,Y,Yt,IntrvTm,Goal);

			}	/* y changed */

#ifdef CONSTRDEBUG
	if (debug_constr_flag > 0)
		note_changes();
#endif
			}	/* (status & ~link) -- something changed */


		/* ------------------------------------------------------
		   Note that next/ntextt already holds the (pointer) from
		   the link slot (arg 6) of qhead; so all we need to do
		   now is set the link slot of qhead to be unbound (since 
		   it is being disconnected from the queue), and then set
		   qhead to next 
         * ------------------------------------------------------ */
		}	/* status != 0 */

		w_get_argn(&next, &nextt, qhead, 6);
		w_install_unbound_argn(qhead, 6);
		qhead = next;
		qheadt = nextt;


		} /* while */ 

		SUCCEED;

} /* ilinknet */

/*-----------------------------------------------------------------*
 |	update_propagate(L,H,Var,Type,IntrvTm,Goal)
 |
 |	- updates interval & propagates changes
 |
 |	Parameters:
 |	L,H			-- new lower/upper bds for the interval;
 |	Var,Type	-- the (actual) variable and its type;
 |	IntrvTm		-- the interval structure for this variable;
 |	Goal		-- the goal [pop(#N,...)] we computed this time
 |
 |	1.  Updates the values for L/H in IntrvTm;
 |	2.  Puts all operations (other than Goal) which are on the
 |		UsedBy list of Var onto the queue.
 *-----------------------------------------------------------------*/
void
update_propagate(L,H,Var,Type,IntrvTm,Goal)
	double L,H;
	PWord IntrvTm, Var, Goal;
	int Type;
{
	PWord UList,   LHead;
	int   UList_t, LHead_t;
	PWord IntUIA,   Link; 
	int   IntUIA_t, Linkt; 

	if (Type == WTP_INTEGER)
		return;

	/* ----
	   IntrvTm should be intvl(Type,Var,UsedBy,L,U,UIA);
	   First update the interval bounds  in the UIA: 
	 *----  */

/* printf("Enter: update_propagate:L=%g H=%g Intrv=%x \n", L, H, IntrvTm); */

	w_uia_alloc(&IntUIA, &IntUIA_t, (size_t)UIA_DIMENSION);
	w_uia_poke(IntUIA, (int) UIA_FIRST_POS, (UCHAR *) &L, sizeof (double));
	w_uia_poke(IntUIA, (int) UIA_SECOND_POS, (UCHAR *) &H, sizeof (double));

	trailed_mangle0(UIA_POSITION, IntrvTm, WTP_STRUCTURE, IntUIA, WTP_UIA);

/*
	w_get_argn(&IntUIA, &IntUIA_t, IntrvTm, UIA_POSITION);
	w_uia_poke(IntUIA, 0, (UCHAR *) &L, sizeof (double));
	w_uia_poke(IntUIA, 8, (UCHAR *) &H, sizeof (double));
*/

		/* Extract the UsedBy List from IntrvTm: */
	w_get_argn(&UList, &UList_t, IntrvTm, USED_BY_POSITION);

	while ( UList_t != WTP_SYMBOL ) 
	{
		w_get_car(&LHead,&LHead_t,UList);
		w_get_argn(&Link, &Linkt, LHead, 6);
		if (Linkt == MTP_UNBOUND)
		{
			/* ---------------------------------------------------------*
				The link slot of LHead is unbound, thus it is not on 
				the queue; Add LHead to Queue:  
				At start:                             |
					       						      v
			          	qend =  pop(KOpCd,KZ,KX,KY,KG,0)  

			          	LHead = pop(GOpCd,GZ,GX,GY,G, _)

				At end:                               |
							       				      v
			                  	pop(KOpCd,KZ,KX,KY,KG,_)  
				       							      |
			       								      v
	           	 qend = LHead = pop(GOpCd,GZ,GX,GY,G, 0)
		 	 * ---------------------------------------------------------*/

/* printf("Updateprop: adding:LHead=%x qhead=%x qend=%x\n",LHead,qhead,qend); */

			w_install_argn(qend, 6, LHead, WTP_STRUCTURE);
			qend = LHead;
			w_install_argn(qend, 6, 0, WTP_INTEGER);
		}
		w_get_cdr(&UList,&UList_t,UList);

	} /* while */
/* printf("Exit Updateprop: qhead=%x qend=%x\n",qhead,qend); */

} /* update_propagate */


void
bind_int_unfreeze(V,Vt,pv)
	PWord *V;
	int *Vt;
	double pv;
{
}





void disp_vv	PARAMS((PWord,int,double,double));

void
disp_infp(BA,OpCd,Z,Zt,zl,zh,X,Xt,xl,xh,Y,Yt,yl,yh)
	PWord OpCd, Z, X, Y;
	int   BA,Zt,Xt,Yt;
	double zl,zh,xl,xh,yl,yh;
{
	if (BA == 0) 
		printf(">Bef[");
	else
		printf("<Aft[");

		switch (OpCd) {
			case 0 :	{ printf("!="); break;}
			case 1 :	{ printf("= "); break;}
			case 2 : 	{ printf(">="); break;}
			case 3 :	{ printf("hg"); break;}
			case 4 :	{ printf("+ "); break;}
			case 5 :	{ printf("bt"); break;}
			case 6 : 	{ printf("ft"); break;}
			case 7 :	{ printf("in"); break;}
			case 8 :	{ printf("j<"); break;}
			case 9 :	{ printf("k="); break;}
			case 10 :	{ printf("lb"); break;}
			case 11 :	{ printf("* "); break;}
			case 12 :	{ printf("nw"); break;}
			case 13 :	{ printf("or"); break;}
			case 14 :	{ printf("^3"); break;}
			case 15 :	{ printf("^2"); break;}
			case 16 :	{ printf("rt"); break;}
			case 17 :	{ printf("va"); break;}
			case 18 :	{ printf("wr"); break;}
			case 19 :	{ printf("xp"); break;}
			case 20 :	{ printf("cs"); break;}
			case 21 :	{ printf("si"); break;}
			case 22 :	{ printf("tn"); break;}
			default :	break;
		}
	printf("] Z");
	disp_vv(Z,Zt,zl,zh);
	printf(" X");
	disp_vv(X,Xt,xl,xh);
	printf(" Y");
	disp_vv(Y,Yt,yl,yh);
	printf("\n");
}

void
disp_vv(V,Vt,vl,vh)
	PWord V;
	int   Vt;
	double vl,vh;
{
	if (Vt == WTP_UNBOUND)
		printf("[_%lu][%g,%g]", (long)(((PWord *) V) - wm_heapbase), vl,vh);
	else if (Vt == WTP_INTEGER)
		printf("[i][%g,%g]",vl,vh);
	else if (Vt == WTP_STRUCTURE)
		printf("[s][%g,%g]",vl,vh);
	else 
		printf("[?][%g,%g]",vl,vh);
}

void
note_changes()
{
	if (z_changed & status) 
		printf("z_changed:zl=%22.17g zh=%22.17g diff=%22.17g\n",zl,zh,zh-zl);
	if (x_changed & status) 
		printf("x_changed:xl=%22.17g xh=%22.17g diff=%22.17g\n",xl,xh,xh-xl);
	if (y_changed & status) 
		printf("y_changed:yl=%22.17g yh=%22.17g diff=%22.17g\n",yl,yh,yh-yl);
}

#endif /* defined(INTCONSTR) */
