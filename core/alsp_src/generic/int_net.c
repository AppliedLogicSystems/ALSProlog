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

#ifdef INTCONSTR

void disp_infp
PARAMS((int,PWord,PWord,int,double,double,int,PWord,int,double,double,int,PWord,int,double,double,int));

void note_changes	PARAMS((void));

void setup_4_boole	PARAMS((double,double,int,long *));

#ifdef DEBUGSYS
int debug_constr_flag = 0;

void	debugconstr	PARAMS( (void) );

void	
debugconstr()
{
	if (debug_constr_flag == 0)
		debug_constr_flag = 1;
	else
		debug_constr_flag = 0;
}

#endif /* DEBUGSYS */



#include "intrv.h"
#include "freeze.h"     
#include "intrv_pr.h"

	/*--- From intaux.c ---*/
extern int	  extract_bds   PARAMS( (PWord *, int, fp *, fp *, int *) );
int	  extract_bool_bds PARAMS( (PWord *, int, int, long *) ); 
extern PWord  get_intvl_tm	PARAMS((PWord *, int));

extern int op_anynot[], op_bothnot[], op_conjunction[];
extern int op_disjunction[], op_exclusiveor[], op_negation[];

	/*--- From bmeta.c ---*/
extern int trailed_mangle0	PARAMS((PWord,PWord,int,PWord,int));

void update_propagate	PARAMS((double,double,PWord,int,PWord,PWord,int));
int ilnk_net			PARAMS ((void));

	/*--- From wam.c ---*/
extern void bind_point_unfreeze	PARAMS((PWord *,int *,double,int));

int ilinkq	PARAMS ((PWord,PWord,PWord,PWord,PWord,int,int,int,int));
	/*----------------------------------------------------*
 	 |	GLOBAL VARIABLES
 	 *----------------------------------------------------*/

	/*--- Declare the global queue, with types: ---*/
PWord qhead, qend;
int   qheadt, qendt;

int status = 0;

	/*--- Vars for args to the primitives: ---*/
double zl,zh,xl,xh,yl,yh,ul,uh,vl,vh;
int  zpt,xpt,ypt;			/* Interval types: REALKIND, INTEGERKIND, BOOLEANKIND */

	/*--- Vars for args to boolean primitives: ---*/

long booleanInput;
long booleanOutput;

	/*--- Vars for constraint statistics: */
int ncstrs= 0; 
int nprops= 0; 
int niters= 0;  		/* number of primops: this time; */
int itermax= MAXITERS;  /* max number of primops before failure; */

	/*----------------------------------------------------*
 	 |	BLT("$iter_link_net",  5, ilinknet, "_ilinknet"), 
 	 *----------------------------------------------------*/

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
	int holdme;

/* printf("Enter ilinknet\n"); */

	w_get_An(&OpCd, &OpCdt, 1);
	w_get_An(&Z, &Zt, 2);
	w_get_An(&X, &Xt, 3);
	w_get_An(&Y, &Yt, 4);
	w_get_An(&Goal, &Goalt, LINK_POSITION);

	holdme = ilinkq(OpCd, Z, X, Y, Goal, Zt,Xt,Yt,Goalt);

/* printf("EXIT ilinknet\n"); */

	return holdme;
}


int
ilinkq(OpCd, Z, X, Y, Goal, Zt,Xt,Yt,Goalt)
	PWord OpCd, Z, X, Y, Goal;
	int Zt,Xt,Yt,Goalt;
{
		/* Increment count of # times constraint system was called from outside: */
	ncstrs += 1; 

	/*------------------------------------------------*
	 |	Check incoming Z,X,Y for correct types      
	 |	No need to check OpCd, because we create it.
	 *------------------------------------------------*/

	OK_CSTR_ARG(Z) 
	OK_CSTR_ARG(X) 
	OK_CSTR_ARG(Y) 

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
	  the "Link" slot (arg LINK_POSITION) in the pop structure;  this is also
	  used as a "boolean" to tell whether or not a goal has been added
	  to the queue: if the link slot is an unbound variable, it is NOT 
	  currently on the queue, while if the link slot \= 0, it IS on 
	  the queue; 
	  Note that for the Pn link slot, we will set Pn to be Prolog 
	  integer 0, hence not an unbound variable, but something we 
	  can recognize as the end of the queue directly in the link.
	
	  When any pop structure is removed from the queue, the link slot
	  (arg LINK_POSITION) is mangled back to being an unbound variable.
	
	  When the last item is removed from the queue, we set 
	  qhead = qend = 0.
	 *-----------------------------------------------------------------*/

	qhead  = Goal;
	qheadt = Goalt;

	qend   = Goal;
	qendt  = Goalt;
	w_install_argn(Goal, LINK_POSITION, 0, WTP_INTEGER);

	niters = 0;
	if (ilnk_net())
	{
		
		nprops += niters;
		SUCCEED;
	}
	else
	{
		nprops += niters;
		FAIL;
	}
}

/*-----------------------------------------------------------------*
 |	MACROS for ilnk_net()
 *-----------------------------------------------------------------*/
	/* -------------------------------------------------------*
	 | 	"Below, if Z is bound (Zt != WTP_UNBOUND), we assume
	 | 	"that Z is a point, which means failure here, because if
	 | 	"it is already a point, it can't change....
	 | Otherwise, set IntrvTm to intvl(ProType,Var,UsedBy,L,U,UIA); 
	 *--------------------------------------------------------*/
#define POINT_CHECK(V,Vt) { if (Vt == WTP_UNBOUND)\
			IntrvTm = (PWord)get_intvl_tm((PWord *)V, Vt);\
		else\
			FAIL;\
		}

#define UNLINK_ALL { while (qheadt != WTP_INTEGER) { \
		w_get_argn(&next, &nextt, qhead, LINK_POSITION); \
		w_install_unbound_argn(qhead, LINK_POSITION); \
		qhead = next; \
		qheadt = nextt; } }

	/* -------------------------------------------------------*
	 |  Check for end points coalescing; if so, change (bind)
	 |  V,VT,the underlying variable to a point:
	 *--------------------------------------------------------*/

#define UPDATE_ITEM(V,VT,VL,VH, VPT) { 						\
	if ((VPT == INTEGERKIND) || (VPT == BOOLEANKIND)) 		\
			/* Round endpoints of integer/boolean 			\
			   intervals inward to nearest ints: */			\
	{	int_fp new_int;										\
		new_int = ceiling(VL);								\
		if (VL NE new_int) {								\
			status |= redonode;								\
			VL = new_int;									\
		}													\
		new_int = floor(VH);								\
		if (VH NE new_int) {								\
			status |= redonode;								\
			VH = new_int;									\
		} 													\
	} 		/* VPT == INTEGERKIND */						\
		if (VL == VH)										\
			bind_point_unfreeze((PWord *)V,&VT,VL,VPT);		\
	 	update_propagate(VL, VH, V, VT, IntrvTm, Goal, VPT);\
	}

#define NCONSTRMACROS 1

#ifndef NCONSTRMACROS

#define DO_UPDATE(V,VT,vv,V_CHANGED,VL,VH,VPT) unflip(vv);		\
		if (V_CHANGED & status) {								\
			if (VL > VH) {										\
				UNLINK_ALL;										\
				FAIL;											\
			}													\
	 		/*If Z is bound (Zt != WTP_UNBOUND), Z must be a 	\
			  point,& thus fail here, since if it is a point, 	\
			  it can't change*/									\
			POINT_CHECK(V,VT)									\
			UPDATE_ITEM(V,VT,VL,VH, VPT)						\
			}

#define UPDATE_BOOLEAN(V,VT,VL,VH) { long tt;					\
		switch(status & (VL ## change + VH ## change))			\
			{/* boolean becomes a fixed point.  				\
				If both bounds change, then fail*/				\
				case (VL ## change):							\
					tt = booleanOne;							\
					break;										\
				case (VH ## change):							\
					tt = booleanZero;							\
					break;										\
				default:										\
					FAIL;										\
			}													\
			bind_point_unfreeze((PWord *)V,&VT,(double)tt,0);	\
			}

#else /* not-NCONSTRMACROS------use functions instead, for debugging --------*/

void unflip_z	PARAMS((void));
void unflip_x	PARAMS((void));
void unflip_y	PARAMS((void));

void
unflip_z()
{
	if (status & zflip) {
		int_fp t = zl; 
		status ^= zflip; 
		zl = (zh NE 0.0) ? (- zh) : 0.0; 
		zh = (t NE 0.0) ? (- t) : 0.0; 
		switch (status & (zlchange | zhchange)) {
			case zlchange: 
			case zhchange: 
					/*Flips the zlchange & zhchange:*/  
				status ^= (zlchange | zhchange); 
		}	/* switch */
	}
}  /* unflip_z */

void
unflip_x()
{
	if (status & xflip) {
		int_fp t = xl;
		status ^= xflip;
		xl = (xh NE 0.0) ? (- xh) : 0.0;
		xh = (t NE 0.0) ? (- t) : 0.0;
		switch (status & (xlchange | xhchange)) {
			case xlchange:
			case xhchange:
					/*Flips the xlchange & xhchange:*/  
				status ^= (xlchange | xhchange);
		} /* switch */
	}
}	/* unflip_x */

void
unflip_y()
{
	if (status & yflip) {
		int_fp t = yl;
		status ^= yflip;
		yl = (yh NE 0.0) ? (- yh) : 0.0;
		yh = (t NE 0.0) ? (- t) : 0.0;
		switch (status & (ylchange | yhchange)) {
			case ylchange:
			case yhchange:
					/*Flips the ylchange & yhchange:*/  
				status ^= (ylchange | yhchange);
		} /* switch */ 
	}
}	/* unflip_y */

#define DO_UPDATE(V,VT,vv,V_CHANGED,VL,VH,VPT) unflip_ ## vv ();	\
		if (V_CHANGED & status) {									\
			if (VL > VH) {											\
				UNLINK_ALL;											\
				FAIL;												\
			}														\
	 		/* If Z is bound (Zt != WTP_UNBOUND), Z must be 		\
			   a point, & thus fail here, since if it is a point, 	\
			   it can't change */									\
			POINT_CHECK(V,VT)										\
			UPDATE_ITEM(V,VT,VL,VH, VPT)							\
		}

/*----
#define boolean_z       0x01        ---booleanInput and booleanOutput
#define boolean_zdef    0x02        ---booleanInput
#define boolean_zchg    0x02        ---booleanOutput
 ----*/

int update_boolean_z	PARAMS(( PWord, int, long *, PWord, PWord ));
int update_boolean_x	PARAMS(( PWord, int, long *, PWord, PWord ));
int update_boolean_y	PARAMS(( PWord, int, long *, PWord, PWord ));

int
update_boolean_z(Z,Zt,bO,Goal,IntrvTm)
	PWord Z, Goal, IntrvTm;
	int Zt;
	long *bO;
{ 
	int tt;
	if ((Zt == WTP_UNBOUND) && ((*bO & boolean_zchg) > 0)) 
	{
		/* boolean becomes a fixed point.  
		   If both bounds change, then fail*/

		switch ((*bO & boolean_z) > 0) {		
			case booleanZero:
						tt = booleanZero;
						break;
			case booleanOne:
						tt = booleanOne;
						break;
			default:
						FAIL;
		}
		IntrvTm = (PWord)get_intvl_tm((PWord *)Z, Zt);
		update_propagate((double)tt, (double)tt, Z, Zt, IntrvTm, Goal, BOOLEANKIND);
		bind_point_unfreeze((PWord *)Z,&Zt,(double)tt,0);
	}
	SUCCEED;
}

int
update_boolean_x(X,Xt,bO,Goal,IntrvTm)
	PWord X, Goal, IntrvTm;
	int Xt;
	long *bO;
{ 
	long tt;

	if ((Xt == WTP_UNBOUND) && ((*bO & boolean_xchg) > 0))  
	{
		/* boolean becomes a fixed point.  
		   If both bounds change, then fail*/

		switch ((*bO & boolean_x) > 0) {		
			case booleanZero:
						tt = booleanZero;
						break;
			case booleanOne:
						tt = booleanOne;
						break;
			default:
						FAIL;
		}
		IntrvTm = (PWord)get_intvl_tm((PWord *)X, Xt);
		update_propagate((double)tt, (double)tt, X, Xt, IntrvTm, Goal, BOOLEANKIND);
		bind_point_unfreeze((PWord *)X,&Xt,(double)tt,0);
	}
	SUCCEED;
}

int
update_boolean_y(Y,Yt,bO,Goal,IntrvTm)
	PWord Y, Goal, IntrvTm;
	int Yt;
	long *bO;
{ 
	long tt;
	if ((Yt == WTP_UNBOUND) && ((*bO & boolean_ychg) > 0)) 
	{
		/* boolean becomes a fixed point.  
			   If both bounds change, then fail*/

		switch ((*bO & boolean_y) > 0) {		
			case booleanZero:
						tt = booleanZero;
						break;
			case booleanOne:
						tt = booleanOne;
						break;
			default:
						FAIL;
		}
		IntrvTm = (PWord)get_intvl_tm((PWord *)Y, Yt);
		update_propagate((double)tt, (double)tt, Y, Yt, IntrvTm, Goal, BOOLEANKIND);
		bind_point_unfreeze((PWord *)Y,&Yt,(double)tt,0);
	}
	SUCCEED;
}

#endif		/* ------- NCONSTRMACROS ------- */

/*-----------------------------------------------------------------*
 |	ilnk_net()
 |
 |	- drives the iteration of the network over the queue
 *-----------------------------------------------------------------*/

int
ilnk_net()
{
	PWord OpCd, Z, X, Y, Goal, next;
	int   OpCdt,Zt,Xt,Yt,Goalt,nextt;
	PWord IntrvTm = (PWord)NULL;

	while (qheadt != WTP_INTEGER) 
	{
		if (niters > itermax)
		{
			printf("Iteration bound exceeded: niters=%d\n", niters);
			FAIL;
		}
/* printf("m=%lx\n",(unsigned long) wm_TR - (unsigned long) wm_H); */

		status = 0;

			/* queue points at the first pop structure: */
		w_get_argn(&OpCd, &OpCdt, qhead, 1);
		w_get_argn(&Z, &Zt, qhead, 2);
		w_get_argn(&X, &Xt, qhead, 3);
		w_get_argn(&Y, &Yt, qhead, 4);
		w_get_argn(&Goal, &Goalt, qhead, LINK_POSITION);

			/*-----------------------------------------------
			 | Note: This branch simply distinguishes the
			 | stricly boolean operations (e.g., AND/OR);
			 | However, some operators < FIRSTBOOLEAN are
			 | also boolean operations (equal, greatereq,etc),
			 | and hence appropriate provision for this fact
			 | must be made in the upper branch here.
			 |	  In the call,
			 |	extract_bds(DelVar, DelVar_t, LB, UB, IKind)
			 | IKind is output, and is bound to the kind of
			 | the variable: BOOLEANKIND, INTEGERKIND, REALKIND.
			 *-----------------------------------------------*/
					/* Setup for real or integer operation */
			if (!extract_bds((PWord *)Z, Zt, &zl, &zh, &zpt))
				{ /* NEED TO MAKE THIS RAISE AN ERROR: */
				FAIL;
				}
			if (!extract_bds((PWord *)X, Xt, &xl, &xh, &xpt))
				{ /* NEED TO MAKE THIS RAISE AN ERROR: */
				FAIL;
				}
			if (!extract_bds((PWord *)Y, Yt, &yl, &yh, &ypt))
				{ /* NEED TO MAKE THIS RAISE AN ERROR: */
				FAIL;
				}
		if ((int)OpCd >= FIRSTBOOLEAN) {
			booleanInput  = 0;
			booleanOutput = 0;
			setup_4_boole(yl,yh,0,&booleanInput);
			setup_4_boole(xl,xh,1,&booleanInput);
			setup_4_boole(zl,zh,2,&booleanInput);
		}

#ifdef DEBUGSYS
	if (debug_system[CSTRPRIM])
		disp_infp(0,OpCd,Z,Zt,zl,zh,zpt,X,Xt,xl,xh,xpt,Y,Yt,yl,yh,ypt);
#endif

			/* Increment count of number of primitive constraint operations: */

		niters += 1;

		switch ((int)OpCd) {
			case 0 :	{ i_unequal(); 		break;}
			case 1 :	{ i_equal(); 		break;}
			case 2 : 	{ i_greatereq(); 	break;}
			case 3 :	{ i_higher(); 		break;}
			case 4 :	{ i_add(); 			break;}
			case 5 :	{ i_begin_tog(); 	break;}
			case 6 : 	{ i_finish_tog(); 	break;}
			case 7 :	{ i_inf(); 			break;}
			case 8 :	{ i_j_less(); 		break;}
			case 9 :	{ i_k_equal(); 		break;}
			case 10 :	{ i_lub(); 			break;}
			case 11 :	{ i_mul(); 			break;}
			case 12 :	{ i_narrower(); 	break;}
			case 13 :	{ i_or(); 			break;}
			case 14 :	{ i_pow_odd();		break;}
			case 15 :	{ i_qpow_even();	break;}
			case 16 :	{ i_rootsquare();	break;}
			case 17 :	{ i_vabs(); 		break;}
			case 18 :	{ i_wrap(); 		break;}
			case 19 :	{ i_xp(); 			break;}
			case 20 :	{ i_cos(); 			break;}
			case 21 :	{ i_sin(); 			break;}
			case 22 :	{ i_tan(); 			break;}
			case 23 :	{ booleanOutput=op_conjunction[booleanInput]; break;}
			case 24 :	{ booleanOutput=op_disjunction[booleanInput]; break;}
			case 25 :	{ booleanOutput=op_anynot[booleanInput];      break;}
			case 26 :	{ booleanOutput=op_bothnot[booleanInput];     break;}
			case 27 :	{ booleanOutput=op_exclusiveor[booleanInput]; break;}
			case 28 :	{ booleanOutput=op_negation[booleanInput];    break;}
			default :	break;
		}

#ifdef DEBUGSYS
	if (debug_system[CSTRPRIM])
		disp_infp(1,OpCd,Z,Zt,zl,zh,zpt,X,Xt,xl,xh,xpt,Y,Yt,yl,yh,ypt);
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
		if (((int)OpCd < FIRSTBOOLEAN) && (status != 0))
		{
		if (status & ~link) { /* something changed */
		
#ifdef DEBUGSYS
	if (debug_system[CSTRCHNG]) note_changes();
#endif

			DO_UPDATE(Z,Zt,z,z_changed,zl,zh,zpt)       

			DO_UPDATE(X,Xt,x,x_changed,xl,xh,xpt)	

			DO_UPDATE(Y,Yt,y,y_changed,yl,yh,ypt)	

			}	/* (status & ~link) -- something changed */

		/* ------------------------------------------------------
		   Note that the (pointer) from the link slot (arg LINK_POSITION) of 
		   qhead points to the next element in the queue; so all we 
		   need to do is to extract this pointer into nextt, and
		   then (normally) set the link slot of qhead to be unbound 
		   (since it is being disconnected from the queue), and then 
		   set qhead to next 

		   However, when status & redonode = 1, we must hook the
		   element pointed at by qhead in at the end of the for
		   further processing during the present cycle (because we
		   explicitly changed one or both of its endpoint values
		   [during integer rounding].  Recall that:

				qend  --> Gn:  pop(OpCdn,Zn,Xn,Yn,Pn)

			where Pn = 0.  Hence, in this case we need to change 
			slot LINK_POSITION of the item pointed at by qend to hold a pointer
			to (the item pointed at by qhead), and then change slot
			LINK_POSITION of qhead to 0 (ie, make it WTP_UNBOUND);

		   Note that next/ntextt already holds the (pointer) from
		   the LINK_POSITION of qhead.
         * ------------------------------------------------------ */
		}		/* OpCd >= FIRSTBOOLEAN && status != 0 */
		else if (((int)OpCd >= FIRSTBOOLEAN) && (boolean_chg & booleanOutput)) {
#ifndef NCONSTRMACROS
			UPDATE_BOOLEAN(V,VT,VL,VH)
#else
			update_boolean_z(Z,Zt,&booleanOutput, Goal, IntrvTm);
			update_boolean_x(X,Xt,&booleanOutput, Goal, IntrvTm);
			update_boolean_y(Y,Yt,&booleanOutput, Goal, IntrvTm);
#endif /* NCONSTRMACROS */
		}

		w_get_argn(&next, &nextt, qhead, LINK_POSITION);

			/* Must arrange for redo-testing for booleans: */
		if (((status && redonode) != 0) && ( nextt != WTP_INTEGER) ) {
				w_install_argn(qend, LINK_POSITION, qhead, WTP_STRUCTURE);
				qend = qhead;
				w_install_argn(qend, LINK_POSITION, 0, WTP_INTEGER);

				qhead = next;
				qheadt = nextt;
		}
		else {
			w_install_unbound_argn(qhead, LINK_POSITION);
			qhead = next;
			qheadt = nextt;
		}

	} /* while */

	SUCCEED;

} /* ilinknet */

/*-----------------------------------------------------------------*
 |	update_propagate(L,H,Var,Type,IntrvTm,Goal,IKind)
 |
 |	- updates interval & propagates changes
 |
 |	Parameters:
 |	L,H			-- new lower/upper bds for the interval;
 |	Var,Type	-- the (actual) variable and its Prolog type;
 |	IntrvTm		-- the interval structure for this variable;
 |	Goal		-- the goal [pop(#N,...)] we computed this time
 |	IKind		-- the kind of interval (BOOLEANKIND, INTEGERKIND, REALKIND)
 |
 |	1.  Updates the values for L/H in IntrvTm;
 |	2.  Puts all operations (other than Goal) which are on the
 |		UsedBy list of Var onto the queue.
 |  Note that Type is one of:
 |		WTP_INTEGER, WTP_REAL, WTP_STRUCTURE, and that the
 |	distinction between WTP_REAL and WTP_STRUCTURE is determined
 |	by whether DoubleType is #defined.
 |
 |	Note that Type and IKind are generally quite different.
 *-----------------------------------------------------------------*/

extern void plain_bind PARAMS((PWord, PWord));


void
update_propagate(L,H,Var,Type,IntrvTm,Goal, IKind)
	double L,H;
	PWord IntrvTm, Var, Goal;
	int Type, IKind;
{
	PWord UList,   LHead;
	int   UList_t, LHead_t;
	PWord IntUIA,   Link; 
	int   IntUIA_t, Linkt; 
	PWord *UIAPtr; 

#ifdef DEBUGSYS
	if (debug_system[CSTRUPDT]) {
		printf("Enter: update_propagate:L=%g H=%g K=%d Type=%d Intrv=%x \n", 
						L, H, IKind, Type, (int)IntrvTm);
		pbi_cptx();
		printf("CurP(=B)=%0x SPB =%0x\n", (int)wm_B, (int)chpt_SPB(wm_B) );
/*		pbi_walk_cps(); */
		}
#endif /* DEBUGSYS */

/*	if (IKind != BOOLEANKIND) {   */
	/* ------------------------------------------------------------*
	   Now update the interval endpoints in the interval structure;
	   IF (the interval endpoints uia) is more recent than
		  the latest CP
	   THEN 
			update the interval structure destructively in place
	   ELSE

	   -- Make a new UIA struct with the new values, and do a
		  trailed mangle of the new UIA in for the old.

	   First, MUST do a check to make sure there is enough space on
	   the heap for the new UIA;

	   THEN
	   IntrvTm should be intvl(Type,Var,UsedBy,L,U,UIA);
	   First create a physically new uia, and install the interval 
	   bounds and kind in this new UIA: 
	 *-------------------------------------------------------------*/

	/* CHANGED BY SPIRO BELOW */ 
#if 1
	if (0) {
#else
	if ((long)IntrvTm > (long)chpt_SPB(wm_B) ) {
#endif
#if 0
		printf("CASE#1\n");
#endif
		w_get_argn(&IntUIA, &IntUIA_t, IntrvTm, UIA_POSITION);
		w_uia_poke(IntUIA, (int) UIA_FIRST_POS,  (UCHAR *) &L, sizeof (double));
		w_uia_poke(IntUIA, (int) UIA_SECOND_POS, (UCHAR *) &H, sizeof (double));

	} else {
#if 0
		printf("CASE#2\n");
#endif
		w_uia_alloc(&IntUIA, &IntUIA_t, (size_t)UIA_DIMENSION);
/*
                wm_H = (PWord *) MMK_UIAVAL(IntUIA);
                UIAPtr = wm_H;
		wm_H += 1;
*/

		w_uia_poke(IntUIA, (int) UIA_FIRST_POS,  (UCHAR *) &L, sizeof (double));
		w_uia_poke(IntUIA, (int) UIA_SECOND_POS, (UCHAR *) &H, sizeof (double));
		w_uia_poke(IntUIA, (int) UIA_THIRD_POS,  (UCHAR *) &IKind, sizeof (long));

		/* ------------------------------------------------------------*
			Now trailed-mangle the new UIA into the UIA position in
			the interval structure:
	 	 *-------------------------------------------------------------*/
/* printf("SPIRO: trailed mangle\n"); */
printf("inet_call_tm: %d %x %d %x %d\n", (int)UIA_POSITION, (int)IntrvTm, 
			(int)WTP_STRUCTURE, (int)IntUIA, (int)WTP_UIA);     
		trailed_mangle0((int)UIA_POSITION, IntrvTm, (int)WTP_STRUCTURE, IntUIA, (int)WTP_UIA);     
	}

#ifdef DEBUGSYS
	if (debug_system[CSTRUPTM]) {
		printf("UPDATE_PROP-TM:[L=%g H=%g K=%d] intuia=%0x wm_H=%0x wm_HB=%0x\n",
							L,H,IKind,(int)(PWord)(IntUIA + wm_heapbase),(int)wm_H,(int)wm_HB);
		pbi_cptx();
	}
#endif /* DEBUGSYS */


/*	}	 IKind != BOOLEANKIND  */

		/*--------------------------------------------------------------* 
		 |  FROM HERE ON, APPLIES TO ALL OF BOOLEANS, INTEGERS, & REALS: 
		 *--------------------------------------------------------------*/

		/* Extract the UsedBy List from IntrvTm: */
	w_get_argn(&UList, &UList_t, IntrvTm, USED_BY_POSITION);
/* printf("PROP-BEGIN:UList_t=%d UList=%x IntrvTm=%x\n",UList_t,(int)UList,(int)IntrvTm); */

		/* Iterate down the list of operation nodes which use IntrvTm,
		   putting each of them on the processing queue: */
	while ( UList_t != WTP_SYMBOL ) 
	{
		w_get_car(&LHead,&LHead_t,UList);
		if (LHead != Goal) {
		w_get_argn(&Link, &Linkt, LHead, LINK_POSITION);
							/* printf("Top-while:UList_t=%d UList=%x Linkt=%d
							   Link=%x\n",UList_t,(int)UList,Linkt,(int)Link); */
		if (Linkt == MTP_UNBOUND)
		{
			/* ---------------------------------------------------------*
				The link slot of LHead is unbound, thus it is not
				currently on the queue; Add LHead to Queue:  
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

#ifdef DEBUGSYS
						if (debug_system[CSTRUPAD])
							printf("Updateprop: adding:LHead=%x qhead=%x qend=%x\n",
											(int)LHead,(int)qhead,(int)qend);
#endif /* DEBUGSYS */

			w_install_argn(qend, LINK_POSITION, LHead, WTP_STRUCTURE);
			qend = LHead;
			w_install_argn(qend, LINK_POSITION, 0, WTP_INTEGER);
		}
		}
		w_get_cdr(&UList,&UList_t,UList);
								/* if (UList_t == WTP_SYMBOL) 
									printf("Bot-while:UList_t=%d UList=%x \n",
										UList_t,(int)UList); */

	} /* while */

		 			/* The boolean is [0, 1] & L = H (= 0 or 1) */
/*
	if ((IKind == BOOLEANKIND) && (Type == 0) && (L == H)) {
			plain_bind(Var, MMK_INT(L));
	}
*/
#ifdef DEBUGSYS
			if (debug_system[CSTRUPXT])
				printf("Exit Updateprop: qhead=%x qend=%x\n",
									(int)qhead,(int)qend);
#endif /* DEBUGSYS */

} /* update_propagate */


void disp_vv	PARAMS((PWord,int,double,double,int));

void
disp_infp(BA,OpCd,Z,Zt,dzl,dzh,dzpt,X,Xt,dxl,dxh,dxpt,Y,Yt,dyl,dyh,dypt)
	PWord OpCd, Z, X, Y;
	int   BA,Zt,Xt,Yt;
	double dzl,dzh,dxl,dxh,dyl,dyh;
	int  dzpt,dxpt,dypt;
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
			case 23 :	{ printf("& "); break;}
			case 24 :	{ printf("| "); break;}
			case 25 :	{ printf("~a"); break;}
			case 26 :	{ printf("~b"); break;}
			case 27 :	{ printf("xo"); break;}
			case 28 :	{ printf("~ "); break;}
			default :	break;
		}
	printf("] Z");
	disp_vv(Z,Zt,dzl,dzh,dzpt);
	printf(" X");
	disp_vv(X,Xt,dxl,dxh,dxpt);
	printf(" Y");
	disp_vv(Y,Yt,dyl,dyh,dypt);
	printf("\n");
}

void
disp_vv(V,Vt,VL,VH,VPT)
	PWord V;
	int   Vt, VPT;
	double VL,VH;
{

	if (VPT==BOOLEANKIND)
	{
		printf("{b}");
		if (Vt == WTP_UNBOUND)
			printf("[_%lu][0,1]", (long)(((PWord *) V) - wm_heapbase));
		else if (Vt == WTP_INTEGER)
			printf("[i][%ld]",V);
		else 
			printf("[?][]");
	}
	else
	{
		if (VPT==REALKIND)
			printf("{r}");
		else if (VPT==INTEGERKIND)
			printf("{i}");
		else 
			printf("{?}");

		if (Vt == WTP_UNBOUND)
			printf("[_%lu][%g,%g]", (long)(((PWord *) V) - wm_heapbase), VL,VH);
		else if (Vt == WTP_INTEGER)
			printf("[i][%g,%g]",VL,VH);
		else if (Vt == WTP_STRUCTURE)
			printf("[s][%g,%g]",VL,VH);
		else 
			printf("[?][%g,%g]",VL,VH);
	}
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


/*--------------------------------------------------------
 	extract_bool_bds(DelVar, DelVar_t, Which)

	DelVar   is a (pointer to) an interval variable; 
	DelVar_t is the Prolog (impl) type of DelVar
	Which    is 0 (y), 1 (x), or 2 (z) - this tells us
			 whether we are extracting from y, x, or z:

	extracts the boolean interval bounds from DelVar and
	places them in the global variable booleanInput

	if DelVar is bound to a boolean integer (0, 1), uses 
	this value...
 *-------------------------------------------------------*/

int
extract_bool_bds(DelVar, DelVar_t, Which, bI)
	PWord *DelVar;
	int DelVar_t, Which;
	long *bI;
{
		/*-------------------------------*
		 |	Which: 0 (y), 1 (x), 2 (z) 
		 *-------------------------------*/

	if (DelVar_t == WTP_INTEGER)
			/* Boolean is definite */
	{ 
		if ((int)DelVar == 0)
			/* Boolean is definite & Value = 0 */
		{
			switch ((int)Which) {
			case 0:
					/* no change to boolean_y */
				*bI = *bI + boolean_ydef;
				break;
			case 1:
					/* no change to boolean_x */
				*bI = *bI + boolean_xdef;
				break;
			case 2:
					/* no change to boolean_z */
				*bI = *bI + boolean_zdef;
				break;
			default:
					/* NEED TO MAKE THIS RAISE AN ERROR: */
				FAIL;
			}
	  		SUCCEED;
		}
		else if ((int)DelVar == 1)
			/* Boolean is definite & Value = 1 */
		{
			switch ((int)Which) {
			case 0:
				*bI += boolean_y;
				*bI += boolean_ydef;
				break;
			case 1:
				*bI += boolean_x;
				*bI += boolean_xdef;
				break;
			case 2:
				*bI += boolean_z;
				*bI += boolean_zdef;
				break;
			default:
					/* NEED TO MAKE THIS RAISE AN ERROR: */
				FAIL;
			}
	  		SUCCEED;
		}
		else
					/* NEED TO MAKE THIS RAISE AN ERROR: */
			FAIL;
	}
	else if ((DelVar_t == WTP_UNBOUND) && (CHK_DELAY(DelVar)))
			/* Boolean is indefinite;
			   Nothing to set in booleanInput */
	  	SUCCEED;

	else
					/* NEED TO MAKE THIS RAISE AN ERROR: */
		FAIL;
}


void setup_4_boole(L,H,Which,bI)
	double L,H;
	int Which;
	long *bI;
{
	if ((L==0) && (H==0)) {
			switch ((int)Which) {
			case 0:
					/* no change to boolean_y */
				*bI = *bI + boolean_ydef;
				break;
			case 1:
					/* no change to boolean_x */
				*bI = *bI + boolean_xdef;
				break;
			case 2:
					/* no change to boolean_z */
				*bI = *bI + boolean_zdef;
				break;
			default:
					/* NEED TO MAKE THIS RAISE AN ERROR: */
				/* FAIL; */
				break;
			}
	} else if ((L==1) && (H==1)) {
			switch ((int)Which) {
			case 0:
				*bI += boolean_y;
				*bI += boolean_ydef;
				break;
			case 1:
				*bI += boolean_x;
				*bI += boolean_xdef;
				break;
			case 2:
				*bI += boolean_z;
				*bI += boolean_zdef;
				break;
			default:
					/* NEED TO MAKE THIS RAISE AN ERROR: */
				/* FAIL; */
				break;
			}
	} /* if ((L==0) && (H==1)) -- nothing to change in bI */
}


	/*--- Vars for constraint statistics: */

int reset_cstr_ctrs PARAMS (( void ));

int
reset_cstr_ctrs()
{
	 ncstrs= 0; 
	 nprops= 0; 

	 SUCCEED;
}

int get_cstr_ctrs_vals PARAMS (( void ));

int
get_cstr_ctrs_vals()
{
	PWord c1, c2;
	int   c1t, c2t;

	w_get_An(&c1, &c1t, 1);
	w_get_An(&c2, &c2t, 2);

	if ((w_unify(c1, c1t, ncstrs, WTP_INTEGER))
		&& (w_unify(c2, c2t, nprops, WTP_INTEGER) ))
		SUCCEED;
	else
		FAIL;
}

int set_max_iters_val PARAMS (( void ));

int
set_max_iters_val()
{
	PWord val;
	int   tp;

	w_get_An(&val, &tp, 1);
	if (tp != WTP_INTEGER)
		FAIL;
	if (val <= 0)
		FAIL;

	itermax = val;

	SUCCEED;
}

#endif /* defined(INTCONSTR) */

