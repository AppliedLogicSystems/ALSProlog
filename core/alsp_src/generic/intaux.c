/*==================================================================*
 |				intaux.c
 |			Copyright (c) 1995 Applied Logic Systems, Inc.
 |			Portions Copyright (c) 1995  BellNorthern Research Ltc.
 |
 |		Supporting functions for (generated) intrv.c (intrv.h)
 *==================================================================*/
	/* Drop following if/when this file is included in intrv.c */
#include "intrv.h"
#if defined(INTCONSTR)
#include "freeze.h"



extern PWord	deref_2		PARAMS(( PWord ));

/*--------------------------------------------------------
 	extract_bds(IntStruct, ZL, ZH)

	IntStruct is a (pointer to) an interval delay structure;
	extracts the values of the end points of the interval,
	and binds them to (the vars pointed at by) ZL, ZH
 *-------------------------------------------------------*/


void extract_bds	PARAMS( (PWord *, fp *, fp *) );

void
extract_bds(IntStruct, ZL, ZH)
	PWord *IntStruct; /* IntStruct_t == WTP_UNBOUND) */
	fp *ZL, *ZH;
{
	PWord DrT, *CstrTm, functor;
	int *CstrTm_t;
	PWord ZLV, ZHV;
	int ZLV_t, ZHV_t;
#ifndef DoubleType
	PWord vv;
	int arity, tt, i;
#endif

/* printf("!+extract_bds:IntStruct=%x\n",(int)IntStruct); */

		/* Test that we are really getting a delay var */
	if (CHK_DELAY((PWord *)IntStruct))
	{
		DrT = deref_2((PWord)IntStruct);
		w_get_argn((PWord *)&CstrTm, (PWord *)&CstrTm_t, (PWord)((PWord *)DrT-1), 4);

			/* CstrTm is now the delay term from IntStruct;
				but it might be a compound (comma) interval delay
				term, so we need to pull out the leftmost component */

		w_get_functor(&functor, (PWord)CstrTm);
		while(TK_COMMA == functor )
		{
			w_get_argn((PWord *)&CstrTm, (PWord *)&CstrTm_t, (PWord)CstrTm, 1);
            w_get_functor(&functor, (PWord)CstrTm);
		}
		
			/* CstrTm should be intvl(Type,Var,_,L,U)
				-- maybe we should test?? */
             
		w_get_argn(&ZLV, &ZLV_t, (PWord)CstrTm, 4);

#ifndef DoubleType
	GET_DBL_VAL(ZLV_t, ZLV, ZL);
#else
		if (ZLV_t == WTP_DOUBLE)
			w_get_double(&ZL, ZLV);
		else
			iaerror();
#endif

		w_get_argn(&ZHV, &ZHV_t, (PWord)CstrTm, 5);

#ifndef DoubleType
	GET_DBL_VAL(ZHV_t, ZHV, ZH);
#else
		if (ZHV_t == WTP_DOUBLE)
			w_get_double(&ZH, ZHV);
#endif

/* printf("extracted: L=%g  H=%g\n",(fp)*ZL, (fp)*ZH); */

	}
	else
			/* Change to exception later */
		printf("Error: Non-delay var passed to extract_bds\n");
}


void change_bound	PARAMS((PWord *, fp *, int));

void change_bound(IntStruct, PtrFP, Which)
	PWord *IntStruct;
	fp *PtrFP;
	int Which;
{
	PWord DrT, *CstrTm, functor, rval;
	int *CstrTm_t,i,rtag;

		DrT = deref_2((PWord)IntStruct);
		w_get_argn((PWord *)&CstrTm, (PWord *)&CstrTm_t, (PWord)((PWord *)DrT-1), 4);

			/* CstrTm is now the delay term from IntStruct;
				but it might be a compound (comma) interval delay
				term, so we need to pull out the leftmost component */

		w_get_functor(&functor, (PWord)CstrTm);
		while(TK_COMMA == functor )
		{
			w_get_argn((PWord *)&CstrTm, (PWord *)&CstrTm_t, (PWord)CstrTm, 1);
			w_get_functor(&functor, (PWord)CstrTm);
		}
		
			/* CstrTm should be intvl(Type,Var,_,L,U)
				-- maybe we should test?? */

#ifndef DoubleType
	w_mk_term(&rval, &rtag, (PWord) TK_DDOUBLE, 4);
	for (i = 0; i < 4; i++)
	    w_install_argn(rval, i + 1, (PWord) (*(((short *) PtrFP) + i)), WTP_INTEGER);
#else
		w_mk_double(&rval, &rtag, *PtrFP);
#endif
		w_install_argn((PWord)CstrTm, (Which?5:4), rval, rtag);

}


fp i_next  PARAMS((fp *));
fp i_prev  PARAMS((fp *));

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
