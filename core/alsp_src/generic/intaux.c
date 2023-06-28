/*==================================================================*
 |				intaux.c
 |			Copyright (c) 1995 Applied Logic Systems, Inc.
 |			Portions Copyright (c) 1995  BellNorthern Research Ltc.
 |
 |		Supporting functions for (generated) intrv.c (intrv.h)
 *==================================================================*/
#include "defs.h"
#include "intrv.h"
#if defined(INTCONSTR)
#include "winter.h"
#include "freeze.h"

#if defined(__MWERKS__) && defined(__INTEL__) 
void noop(void)
{

}
#endif

/*--------------------------------------------------------
	"API" functions for the UIA structure holding the
	actual interval endpoints; these are for interfaceing
	to the Prolog level.

	LOWERINF, UPPERINF are (new) tokens created in the
	base system, and referring to '-inf', '+inf', resp.

	set_intrv_endpoints(UIA, LowerDesc, UpperDesc)
	set_intrv_endpoint_lower(UIA, LowerDesc)
	set_intrv_endpoint_upper(UIA, UpperDesc)
	set_intrv_type(UIA, TypeDesc)

	get_intrv_endpoints(UIA, LowerDesc, UpperDesc)
	get_intrv_endpoint_lower(UIA, LowerDesc)
	get_intrv_endpoint_upper(UIA, UpperDesc)
	get_intrv_type(UIA, TypeDesc)

		{Which = 0 (lower), 1 (upper)}:
	is_ieee_inf(UIA, Which)   
	set_ieee_inf(UIA, Which)   
	set_ieee_inf_both(UIA)   

 *-------------------------------------------------------*/



extern double sym_f_cnst[];
extern PWord	deref_2( PWord );

PWord get_intvl_tm(PWord *, int);

/*--------------------------------------------------------
 	PWord *get_intvl_tm(DelVar, DelVar_t)

	DelVar should be a frozen (delay) variable; get_intvl_tm
	first extracts the delay structure associated with
	DelVar, and then extracts the interval structure 
	contained in the delay structure, returning
	(a pointer to) this interval structure.
 *-------------------------------------------------------*/
PWord get_intvl_tm(DelVar, DelVar_t)
	PWord *DelVar; 
	int DelVar_t;
{
	PWord DrT, CstrTm, functor;
	int CstrTm_t;

	if ((DelVar_t == WTP_UNBOUND) && (CHK_DELAY((PWord *)DelVar)))
	{
	DrT = deref_2((PWord)DelVar);
	w_get_argn(&CstrTm, &CstrTm_t, (PWord)((PWord *)DrT-1), 4);

		/* CstrTm is now the delay term from DelVar;
		   but it might be a compound (comma) interval delay
		   term, so we need to pull out the leftmost component */

	w_get_functor(&functor, CstrTm);
	while(TK_COMMA == functor )
	{
		w_get_argn(&CstrTm, &CstrTm_t, CstrTm, 1);
		w_get_functor(&functor, CstrTm);
	}
	/* CstrTm should be intvl(Type,Var,UsedBy,UIA) */

	return(CstrTm);
	}
	else
		FAIL;
}	/* get_intvl_tm */


/*--------------------------------------------------------
 	extract_bds(DelVar, DelVar_t, LB, UB, IKind)

	DelVar is a (pointer to) an interval variable; if
	this variable is bound to an integer, returns the
	integer in both LB, UB (-> integer point interval).
	If DelVar is an unbound interval variable (it is frozen),
	extracts the values of the end points of the interval,
	and binds them to (the vars pointed at by) LB, UB
	IKind is the Kind of interval; if DelVar is actually
	bound, it is determined by this (but we use the
	"Kind" values: BOOLEANKIND, INTEGERKIND, REALKIND.
	If DelVar is an unbound delay variable which is in
	fact an interval variable, then IKind is extracted from
	the 3rd slot in the UIA structure holding the end points
	and interval kind.
 *-------------------------------------------------------*/

int
extract_bds(PWord *, int, fp *, fp *, int *);

int
extract_bds(DelVar, DelVar_t, LB, UB, IKind)
	PWord *DelVar;
	int DelVar_t, *IKind;
	fp *LB, *UB;
{
	PWord *Intvl, *IntUIA; 
	int IntUIA_t;

		/* The variable is actually bound to something: */
	if (DelVar_t == WTP_INTEGER)
	{ 
		*LB = (double)(int)DelVar; 
	  	*UB = (double)(int)DelVar; 
		*IKind = INTEGERKIND;
	  	SUCCEED;
	}
#ifdef DoubleType
	else if (DelVar_t == WTP_DOUBLE)
	{ 
		w_get_double(LB, *DelVar);
		*UB = *LB;
		*IKind = REALKIND;
	  	SUCCEED;
	}
#endif
	else if (DelVar_t == WTP_SYMBOL)
	{
		int_fp t;

		initfpu();
		switch ((int)DelVar) {
			FPCCASE(t)
		}
		prev(t);
		*UB = t;
		*LB = -t;
		resetfpu();
		*IKind = REALKIND;
		SUCCEED;
	}
	else if (DelVar_t == WTP_STRUCTURE)
	{ 
		PWord functor, vv;
		int   arity, tt;
		double uu=0;
		int i;

		w_get_arity(&arity, (PWord)DelVar);
		w_get_functor(&functor, (PWord)DelVar);
			/* Must have: arity == 4 && functor == TK_DDOUBLE */
		if ((arity != 4) || (functor != TK_DDOUBLE))
			FAIL;

		for (i = 0; i < 4; i++) {
		    w_get_argn(&vv, &tt, (PWord)DelVar, i + 1);
			*(((short *) &uu)+ i) = (short) vv;
		}
    	*LB = uu;
    	*UB = uu;
		*IKind = REALKIND;
	  	SUCCEED;
	}
		/* The variable is NOT actually bound to something,
		   but is a delay variable:  */
	else if ((DelVar_t == WTP_UNBOUND) && (CHK_DELAY(DelVar)))
	{
 		Intvl = (PWord *)get_intvl_tm(DelVar, DelVar_t);
		w_get_argn((long *)&IntUIA, &IntUIA_t, (long)Intvl, UIA_POSITION);

		if (IntUIA_t == WTP_UIA) {
				/* real or integer interval term */
			w_uia_peek((long)IntUIA, UIA_FIRST_POS,  (UCHAR *) LB, sizeof (double));
			w_uia_peek((long)IntUIA, UIA_SECOND_POS, (UCHAR *) UB, sizeof (double));
			w_uia_peek((long)IntUIA, UIA_THIRD_POS,  (UCHAR *) IKind, sizeof (long));
		}
		else
		{	
			/* If we get here, the variable is a boolean interval var
			   which is not bound to anything (ie, 0 or 1), and so is indefinite: */

			*LB = 0; *UB = 1; *IKind = BOOLEANKIND;
		}

	  	SUCCEED;
	}
	else
		FAIL;

} /* extract_bds */

extern fp i_next(fp *);
extern fp i_prev(fp *);


	/* Prototypes */
int	pbi_fuzz(void);

	/*---------------------------------------------*
	 | pbi_fuzz()
	 |
	 | Input: fpv = floating point value	
	 | Outputs:
	 |		ub = next greater floating point value
	 |		lb = next lower floating point value
	 *---------------------------------------------*/

int 
pbi_fuzz()
{
	PWord fpv, Sgn, v2, v3, vv, v2o, v3o;
	int fpvt, Sgn_t, t2, t3, tt, t2o, t3o;
	fp ub, lb;
	int_fp t;
   	double dblval;
	int i;

    w_get_An(&fpv, &fpvt, 1);
    w_get_An(&Sgn,	&Sgn_t, 2);
    w_get_An(&v2, &t2, 3);
    w_get_An(&v3, &t3, 4);

/*	ISA_DOUBLE( fpv_in, fpvt )  */

#ifdef DoubleType
    if ( fpvt == WTP_DOUBLE)
	    w_get_double(&dblval, fpv);
	else
		FAIL;
#else  /* not-DoubleType */
    if ( fpvt == WTP_STRUCTURE )
    {
    	PWord functor;
    	int arity;

	w_get_arity(&arity, fpv);
	w_get_functor(&functor, fpv);
	if (arity == 4 && functor == TK_DDOUBLE) {
		for (i = 0; i < 4; i++) {
		    w_get_argn(&vv, &tt, fpv, i + 1);
		    *(((short *) &dblval) + i) = (short) vv;
		}
	}
    	else
	    FAIL;
    }
    else if ( fpvt == WTP_SYMBOL ) {
		switch ((int)fpv) {
			FPCCASE(dblval)
		}
	}
    else
		FAIL;
#endif /* DoubleType */

	if ((int)Sgn < 0) { dblval = -dblval; }

	initfpu();
	t = dblval;       /* setup for next(t);  */
	next(t);
	ub = t;
	t = dblval; /* prev(t);  */
	prev(t);
	lb = t;
	resetfpu();

#ifndef DoubleType
	w_mk_term(&v2o, &t2o, (PWord) TK_DDOUBLE, 4);
	for (i = 0; i < 4; i++)
	    w_install_argn(v2o, i + 1, (PWord) (*(((short *) &lb) + i)), WTP_INTEGER);
#else
		w_mk_double(&v2o, &t2o, lb);
#endif

#ifndef DoubleType
	w_mk_term(&v3o, &t3o, (PWord) TK_DDOUBLE, 4);
	for (i = 0; i < 4; i++)
	    w_install_argn(v3o, i + 1, (PWord) (*(((short *) &ub) + i)), WTP_INTEGER);
#else
		w_mk_double(&v3o, &t3o, ub);
#endif

    if (w_unify(v2, t2, v2o, t2o) && w_unify(v3, t3, v3o, t3o))
	SUCCEED;
    else
	FAIL;
}


/* -------------------- vv Dummy for now vv -------------------- */

void iaerror(void);

void iaerror()
{
}

void deact(void);

void deact()
{
}

/* -------------------- ^^ Dummy for now ^^ -------------------- */

fp int_round(fp);

	/* -- Round double to nearest longint -- */
fp int_round(num)
	fp num;
{
	fp ffl;

	ffl = floor(num);

	if ((num - ffl) <= 0.5)
		return(ffl);
	else
		return(ffl+1);
}


fp i_next(x)
	fp *x;
{
	

	if (*x GE 0.0) {
		if (++(((fpoverlay *)x)->l[SECOND]) EQ 0) 
			++(((fpoverlay *)x)->l[FIRST]); 
		} 
	else if (*x LT 0.0) { 
		if (--(((fpoverlay *)x)->l[SECOND]) EQ -1) 
			--(((fpoverlay *)x)->l[FIRST]); 
		}
	return(*x);
}

fp i_prev(x)	 
	fp *x;
{
	if (*x GT 0.0) { 
		if (--(((fpoverlay *)x)->l[SECOND]) EQ -1) 
			--(((fpoverlay *)x)->l[FIRST]); 
		} 
	else if (*x LT 0.0) { 
		if (++(((fpoverlay *)x)->l[SECOND]) EQ 0) 
			++(((fpoverlay *)x)->l[FIRST]); 
		} 
	else { /* x EQ 0.0 */ 
		((fpoverlay *)x)->l[SECOND] = 1; 
		((fpoverlay *)x)->l[FIRST] = 0x80000000; 
		}
	return(*x);
}


	/********************************************************
	 |	BOOLEAN STUFF
	 ********************************************************/

/*---------------------------------------------------*
 |	Boolean operator tables
 *---------------------------------------------------*/

int op_anynot[64] = {0x00,0x80,0x3c,0x00,0x80,0x80,0x80,0x80,
					 0x43,0x80,0xff,0x40,0x00,0x80,0x30,0x20,
					 0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					 0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					 0x43,0x80,0xff,0x40,0x80,0x80,0x80,0x80,
					 0x03,0x80,0xff,0x00,0x03,0x80,0xff,0x00,
					 0x00,0x80,0x0c,0x08,0x80,0x80,0x80,0x80,
					 0x03,0x80,0xff,0x00,0x02,0x80,0x00,0xff};

int op_bothnot[64] = {0x00,0x80,0x00,0x28,0x80,0x80,0x80,0x80,
					  0x00,0x80,0x30,0x20,0x42,0x80,0x40,0xff,
					  0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					  0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					  0x00,0x80,0x0c,0x08,0x80,0x80,0x80,0x80,
					  0x03,0x80,0xff,0x00,0x02,0x80,0x00,0xff,
					  0x42,0x80,0x40,0xff,0x80,0x80,0x80,0x80,
					  0x02,0x80,0x00,0xff,0x02,0x80,0x00,0xff};

int op_conjunction[64]={0x00,0x80,0x00,0x3c,0x80,0x80,0x80,0x80,
						0x42,0x80,0x40,0xff,0x00,0x80,0x20,0x30,
					 	0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					 	0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					 	0x42,0x80,0x40,0xff,0x80,0x80,0x80,0x80,
					 	0x02,0x80,0x00,0xff,0x02,0x80,0x00,0xff,
					 	0x00,0x80,0x08,0x0c,0x80,0x80,0x80,0x80,
					 	0x02,0x80,0x00,0xff,0x03,0x80,0xff,0x00};

int op_disjunction[64]={0x00,0x80,0x28,0x00,0x80,0x80,0x80,0x80,
						0x00,0x80,0x20,0x30,0x43,0x80,0xff,0x40,
						0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
						0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
						0x00,0x80,0x08,0x0c,0x80,0x80,0x80,0x80,
						0x02,0x80,0x00,0xff,0x03,0x80,0xff,0x00,
						0x43,0x80,0xff,0x40,0x80,0x80,0x80,0x80,
						0x03,0x80,0xff,0x00,0x03,0x80,0xff,0x00};

int op_exclusiveor[64]={0x00,0x80,0x00,0x00,0x80,0x80,0x80,0x80,
						0x00,0x80,0x20,0x30,0x00,0x80,0x30,0x20,
						0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
						0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
						0x00,0x80,0x08,0x0c,0x80,0x80,0x80,0x80,
						0x02,0x80,0x00,0xff,0x03,0x80,0xff,0x00,
						0x00,0x80,0x0c,0x08,0x80,0x80,0x80,0x80,
						0x03,0x80,0xff,0x00,0x02,0x80,0x00,0xff};

int op_negation[64]={0x00,0x80,0x0c,0x08,0x80,0x80,0x80,0x80,
		    		 0x03,0x80,0xff,0x00,0x02,0x80,0x00,0xff,
					 0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					 0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,
					 0x00,0x80,0x0c,0x08,0x80,0x80,0x80,0x80,
					 0x03,0x80,0xff,0x00,0x02,0x80,0x00,0xff,
					 0x00,0x80,0x0c,0x08,0x80,0x80,0x80,0x80,
					 0x03,0x80,0xff,0x00,0x02,0x80,0x00,0xff};


#endif /* defined(INTCONSTR) */
