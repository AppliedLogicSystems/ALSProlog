/*==================================================================*
 |				intaux.c
 |			Copyright (c) 1995 Applied Logic Systems, Inc.
 |			Portions Copyright (c) 1995  BellNorthern Research Ltc.
 |
 |		Supporting functions for (generated) intrv.c (intrv.h)
 *==================================================================*/

#include "intrv.h"
#if defined(INTCONSTR)
#include "freeze.h"

extern PWord	deref_2		PARAMS(( PWord ));

PWord *get_intvl_tm		PARAMS((PWord *, int));

/*--------------------------------------------------------
 	PWord *get_intvl_tm(DelVar, DelVar_t)

	DelVar should be a frozen (delay) variable; get_intvl_tm
	first extracts the delay structure associated with
	DelVar, and then extracts the interval structure 
	contained in the delay structure, returning
	(a pointer to) this interval structure.
 *-------------------------------------------------------*/
PWord *get_intvl_tm(DelVar, DelVar_t)
	PWord *DelVar; 
	int DelVar_t;
{
	PWord DrT, *CstrTm, functor;
	int *CstrTm_t;

/* printf("in get_intvl_tm:DelVar=%x\n",DelVar); */

	if ((DelVar_t == WTP_UNBOUND) && (CHK_DELAY((PWord *)DelVar)))
	{
	DrT = deref_2((PWord)DelVar);
	w_get_argn((PWord *)&CstrTm, (PWord *)&CstrTm_t, (PWord)((PWord *)DrT-1), 4);

		/* CstrTm is now the delay term from DelVar;
		   but it might be a compound (comma) interval delay
		   term, so we need to pull out the leftmost component */

	w_get_functor(&functor, (PWord)CstrTm);
	while(TK_COMMA == functor )
	{
		w_get_argn((PWord *)&CstrTm, (PWord *)&CstrTm_t, (PWord)CstrTm, 1);
		w_get_functor(&functor, (PWord)CstrTm);
	}
	/* CstrTm should be intvl(Type,Var,UsedBy,UIA) */

/* printf("out get_intvl_tm=%x\n",CstrTm); */

	return(CstrTm);
	}
	else
printf("get_intvl_tm:Non-delay: %x \n",DelVar);
		FAIL;
}	/* get_intvl_tm */


/*--------------------------------------------------------
 	extract_bds(DelVar, DelVar_t, LB, UB)

	DelVar is a (pointer to) an interval variable; if
	this variable is bound to an integer, returns the
	integer in both LB, UB (-> integer point interval).
	If DelVar is an unbound interval variable (it is frozen),
	extracts the values of the end points of the interval,
	and binds them to (the vars pointed at by) LB, UB
 *-------------------------------------------------------*/

int extract_bds	PARAMS( (PWord *, int, fp *, fp *) );

int
extract_bds(DelVar, DelVar_t, LB, UB)
	PWord *DelVar;
	int DelVar_t;
	fp *LB, *UB;
{
	PWord *Intvl, *IntUIA; 
	int IntUIA_t;
    double pval;
    int   valtype;

	if (DelVar_t == WTP_INTEGER)
	{ 
		*LB = (double)(int)DelVar; 
	  	*UB = (double)(int)DelVar; 
	  	SUCCEED;
	}
	else if ((DelVar_t == WTP_UNBOUND) && (CHK_DELAY(DelVar)))
	{
 		Intvl = get_intvl_tm(DelVar, DelVar_t);
		w_get_argn(&IntUIA, &IntUIA_t, Intvl, UIA_POSITION);
		w_uia_peek(IntUIA, 0, (UCHAR *) LB, sizeof (double));
		w_uia_peek(IntUIA, 8, (UCHAR *) UB, sizeof (double));
	}
	else
		FAIL;

} /* extract_bds */


extern fp i_next  PARAMS((fp *));
extern fp i_prev  PARAMS((fp *));

	/* Prototypes */
int	pbi_fuzz	PARAMS((void));

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
	PWord fpv, v2, v3, vv, v2o, v3o;
	int fpvt, t2, t3, tt, t2o, t3o;
	fp ub, lb;
	int_fp t;
    	double dblval;
	int i;

    w_get_An(&fpv, &fpvt, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

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
    else
	FAIL;
#endif /* DoubleType */

	initfpu();
	t = dblval;       /* setup for next(t);  */
/* printf("i_next(t): bef:t=%20.16f ",t); */
	next(t);
	ub = t;
/* printf(" aft:t=%20.16f ub=%20.16f \n",t,ub); */
	t = dblval; /* prev(t);  */
/* printf("i_prev(t): bef:t=%20.16f ",t); */
	prev(t);
	lb = t;
/* printf("aft: t=%20.16f lb=%20.16f \n",t,lb); */
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


/* Dummy for now */

void iaerror	PARAMS( (void) );

void iaerror()
{
}

void deact	PARAMS( (void) );

void deact()
{
}

double round	PARAMS( (double) );

double round(num)
	double num;
{
	return(num);
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







#endif /* defined(INTCONSTR) */
