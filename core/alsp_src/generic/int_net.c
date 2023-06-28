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


void setup_4_boole	(double,double,int,long *);

#ifdef DEBUGSYS
int debug_constr_flag = 0;

void	debugconstr	(void);

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
extern int	  extract_bds   (PWord *, int, fp *, fp *, int *);
int	  extract_bool_bds (PWord *, int, int, long *); 
extern PWord  get_intvl_tm	(PWord *, int);

extern int op_anynot[], op_bothnot[], op_conjunction[];
extern int op_disjunction[], op_exclusiveor[], op_negation[];

	/*--- From bmeta.c ---*/
extern int trailed_mangle0	(PWord,PWord,int,PWord,int);

void update_propagate	(double,double,PWord,int,int);
int ilnk_net			(void);

	/*--- From wam.c ---*/
extern void bind_point_unfreeze	(PWord *,int *,double,int);

int ilinkq	(PWord,PWord,PWord,PWord,PWord,int,int,int,int);
	/*----------------------------------------------------*
 	 |	GLOBAL VARIABLES
 	 *----------------------------------------------------*/

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


/*-----------------------------------------------------------------*
 |	MACROS 
 *-----------------------------------------------------------------*/
#define DO_UPDATE(V,VT,vv,V_CHANGED,VL,VH,VPT) unflip_ ## vv ();	\
		if (V_CHANGED & status) {									\
			if (VL > VH) {											\
				FAIL;												\
			}														\
	 		/* If Z is bound (Zt != WTP_UNBOUND), Z must be 		\
			   a point, & thus fail here, since if it is a point, 	\
			   it can't change */									\
			POINT_CHECK(V,VT)										\
			UPDATE_ITEM(V,VT,VL,VH, VPT)							\
		}

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

int do_update_z	(PWord, int);
int do_update_x	(PWord, int);
int do_update_y	(PWord, int);


/***** GLOBALS:
double zl,zh,xl,xh,yl,yh,ul,uh,vl,vh;
int  zpt,xpt,ypt;			
*****/

int
do_update_z(Z,Zt)
	PWord Z; 
	int Zt;
{
	if (z_changed & status) {
		if (zl > zh) {
			FAIL;
		}
	 		/* If Z is bound (Zt != WTP_UNBOUND), Z must be
			   a point, & thus fail here, since if it is a point,
			   it can't change */
		if (Zt != WTP_UNBOUND)
			FAIL;

		if ((zpt == INTEGERKIND) || (zpt == BOOLEANKIND))
				/* Round endpoints of integer/boolean 
				   intervals inward to nearest ints: */
		{	int_fp new_int;
			new_int = ceiling(zl);
			if (zl NE new_int) {
				status |= redonode;
				zl = new_int;
			}
			new_int = floor(zh);
			if (zh NE new_int) {
				status |= redonode;
				zh = new_int;
			}
		} 		/* END: zpt == INTEGERKIND || zpt == BOOLEANKIND */
		if (zl == zh)
/*			bind_point_unfreeze((PWord *)Z,&Zt,zl,zpt);   */
			status |= zcoalesce ;
		update_propagate(zl, zh, Z, Zt, zpt);
	}
	SUCCEED;
}

int
do_update_x(X,Xt)
	PWord X; 
	int Xt;
{
	if (x_changed & status) {
		if (xl > xh) {
			FAIL;
		}
	 		/* If X is bound (Xt != WTP_UNBOUND), X must be
			   a point, & thus fail here, since if it is a point,
			   it can't change */
		if (Xt != WTP_UNBOUND)
			FAIL;

		if ((xpt == INTEGERKIND) || (xpt == BOOLEANKIND))
				/* Round endpoints of integer/boolean 
				   intervals inward to nearest ints: */
		{	int_fp new_int;
			new_int = ceiling(xl);
			if (xl NE new_int) {
				status |= redonode;
				xl = new_int;
			}
			new_int = floor(xh);
			if (xh NE new_int) {
				status |= redonode;
				xh = new_int;
			}
		} 		/* END: xpt == INTEGERKIND || xpt == BOOLEANKIND */
		if (xl == xh)
/*			bind_point_unfreeze((PWord *)X,&Xt,xl,xpt);   */
			status |= xcoalesce ;
		update_propagate(xl, xh, X, Xt, xpt);
	}
	SUCCEED;
}

int
do_update_y(Y,Yt)
	PWord Y; 
	int Yt;
{
	if (y_changed & status) {
		if (yl > yh) {
			FAIL;
		}
	 		/* If Y is bound (Yt != WTP_UNBOUND), Y must be
			   a point, & thus fail here, since if it is a point,
			   it can't change */
		if (Yt != WTP_UNBOUND)
			FAIL;

		if ((ypt == INTEGERKIND) || (ypt == BOOLEANKIND))
				/* Round endpoints of integer/boolean 
				   intervals inward to nearest ints: */
		{	int_fp new_int;
			new_int = ceiling(yl);
			if (yl NE new_int) {
				status |= redonode;
				yl = new_int;
			}
			new_int = floor(yh);
			if (yh NE new_int) {
				status |= redonode;
				yh = new_int;
			}
		} 		/* END: ypt == INTEGERKIND || ypt == BOOLEANKIND */
		if (yl == yh)
/*			bind_point_unfreeze((PWord *)Y,&Yt,yl,ypt);		*/
			status |= ycoalesce ;
		update_propagate(yl, yh, Y, Yt, ypt);
	}
	SUCCEED;
}

void unflip_z	(void);
void unflip_x	(void);
void unflip_y	(void);

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

/*----
#define boolean_z       0x01        ---booleanInput and booleanOutput
#define boolean_zdef    0x02        ---booleanInput
#define boolean_zchg    0x02        ---booleanOutput
 ----*/

int update_boolean_z	( PWord, int, long *);
int update_boolean_x	( PWord, int, long *);
int update_boolean_y	( PWord, int, long *);

int
update_boolean_z(Z,Zt,bO)
	PWord Z;
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
		update_propagate((double)tt, (double)tt, Z, Zt, BOOLEANKIND);
		bind_point_unfreeze((PWord *)Z,&Zt,(double)tt,0);
	}
	SUCCEED;
}

int
update_boolean_x(X,Xt,bO)
	PWord X;
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
		update_propagate((double)tt, (double)tt, X, Xt, BOOLEANKIND);
		bind_point_unfreeze((PWord *)X,&Xt,(double)tt,0);
	}
	SUCCEED;
}

int
update_boolean_y(Y,Yt,bO)
	PWord Y;
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
		update_propagate((double)tt, (double)tt, Y, Yt, BOOLEANKIND);
		bind_point_unfreeze((PWord *)Y,&Yt,(double)tt,0);
	}
	SUCCEED;
}


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

extern void plain_bind (PWord, PWord);


void
update_propagate(L,H,V,Vt,IKind)
	double L,H;
	PWord V;
	int Vt, IKind;
{
	PWord IntrvTm, IntUIA;
	int   IntUIA_t; 

#ifdef DEBUGSYS
	if (debug_system[CSTRUPDT]) {
		printf("Enter: update_propagate:L=%g H=%g K=%d Type=%d \n", 
						L, H, IKind, Vt);
		}
#endif /* DEBUGSYS */

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
	   IntrvTm should be intvl(Vt,Var,UsedBy,L,U,UIA);
	   First create a physically new uia, and install the interval 
	   bounds and kind in this new UIA: 
	 *-------------------------------------------------------------*/

	IntrvTm = (PWord)get_intvl_tm((PWord *)V, Vt);

	/* CHANGED BY SPIRO BELOW */ 
#if 0
	if ((long)IntrvTm > (long)chpt_SPB(wm_B) ) {
		w_get_argn(&IntUIA, &IntUIA_t, IntrvTm, UIA_POSITION);
		w_uia_poke(IntUIA, (int) UIA_FIRST_POS,  (UCHAR *) &L, sizeof (double));
		w_uia_poke(IntUIA, (int) UIA_SECOND_POS, (UCHAR *) &H, sizeof (double));

	} else {
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
		trailed_mangle0(UIA_POSITION, IntrvTm, WTP_STRUCTURE, IntUIA, WTP_UIA); 
#if 0
	}
#endif

#ifdef DEBUGSYS
	if (debug_system[CSTRUPTM]) {
		printf("UPDATE_PROP-TM:[L=%g H=%g K=%d] intuia=%0x wm_H=%0x wm_HB=%0x\n",
							L,H,IKind,(int)(PWord)(IntUIA + wm_heapbase),(int)wm_H,(int)wm_HB);
		pbi_cptx();
	}
#endif /* DEBUGSYS */


} /* update_propagate */







void disp_vv	(PWord,int,double,double,int);
void disp_infp
(int,PWord,PWord,int,double,double,int,PWord,int,double,double,int,PWord,int,double,double,int);
void note_changes	(void);

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
	printf(" status=%d", status);
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

int reset_cstr_ctrs ( void );

int
reset_cstr_ctrs()
{
	 ncstrs= 0; 
	 nprops= 0; 

	 SUCCEED;
}

int get_cstr_ctrs_vals ( void );

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

int set_max_iters_val ( void );

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

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

	/*   x_int_op(OpCd,Z,X,Y,Status,LinkFlag),  */

int
x_int_op()
{
	PWord OpCd, Z, X, Y, Stat;
	int OpCdt,Zt,Xt,Yt,Statt;


printf("Enter x_int_op\n");

	status = 0;
	w_get_An(&OpCd, &OpCdt, 1);
	w_get_An(&Z, &Zt, 2);
	w_get_An(&X, &Xt, 3);
	w_get_An(&Y, &Yt, 4);
	w_get_An(&Stat, &Statt, 5);

	OK_CSTR_ARG(Z) 
	OK_CSTR_ARG(X) 
	OK_CSTR_ARG(Y) 

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

disp_infp(0,OpCd,Z,Zt,zl,zh,zpt,X,Xt,xl,xh,xpt,Y,Yt,yl,yh,ypt);

	if ((int)OpCd >= FIRSTBOOLEAN) {
		booleanInput  = 0;
		booleanOutput = 0;
		setup_4_boole(yl,yh,0,&booleanInput);
		setup_4_boole(xl,xh,1,&booleanInput);
		setup_4_boole(zl,zh,2,&booleanInput);
	}

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

disp_infp(1,OpCd,Z,Zt,zl,zh,zpt,X,Xt,xl,xh,xpt,Y,Yt,yl,yh,ypt);

		if (((int)OpCd < FIRSTBOOLEAN) && (status != 0))
		{
		if (status & ~link) { /* something changed */
		
			do_update_z(Z,Zt);
			do_update_x(X,Xt);
			do_update_y(Y,Yt);

			}	/* (status & ~link) -- something changed */
		}		/* OpCd >= FIRSTBOOLEAN && status != 0 */
		else if (((int)OpCd >= FIRSTBOOLEAN) && (boolean_chg & booleanOutput)) {
			update_boolean_z(Z,Zt,&booleanOutput);
			update_boolean_x(X,Xt,&booleanOutput);
			update_boolean_y(Y,Yt,&booleanOutput);
		}

printf("x_int_op: status=%d\n", status);

	if (w_unify(Stat, Statt, status, WTP_INTEGER))
		SUCCEED;
	else
		FAIL;
}



#endif /* defined(INTCONSTR) */
