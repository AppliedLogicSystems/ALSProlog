/*=============================================================*
 |		freeze.c   
 | Copyright (c) 1995 by Applied Logic Systems
 |
 |		-- Most machinery for implementing freeze
 |
 | Program Author:  Ken Bowen
 | Creation:  04/18/95
 | 	Based on design by K. Buettner:  
 |		Delayed Evaluation -- How to do in on the SUN
 *=============================================================*/

#include "defs.h"

#ifdef FREEZE

#include "wintcode.h"
#include "module.h"
#include "icodegen.h"
#include "cutmacro.h"
#include "compile.h"
#include "freeze.h"

int	pbi_delay		PARAMS(( void ));
int	pbi_is_delay_var	PARAMS(( void ));
int	update_chpt_slots	PARAMS(( PWord ));
int	pbi_clct_tr		PARAMS(( void ));
int 	pbi_del_tm_for		PARAMS(( void ));

/*---------------------------------------------------------------*
 | pbi_delay()
 |
 | creates a delay term on the heap, given 3 input args from
 | the prolog heap:
 *---------------------------------------------------------------*/

int
pbi_delay()
{
	PWord *dv,m,g,vv,rdt,*one,*two;
	int dvt,mt,gt,vvt,rdtt;

#ifdef DEBUGFREEZE
pbi_cptx();
printf("enter delay----wm_H=%x--TK_DELAY=%x-----------------\n",
			(int)wm_H,TK_DELAY);
#endif

    w_get_An((PWord *)&dv, &dvt, 1);
    w_get_An(&m, &mt, 2);
    w_get_An(&g, &gt, 3);
    w_get_An(&rdt, &rdtt, 4);
#ifdef DEBUGFREEZE
printf("   >incoming var: %x[_%lu]\n",(int)*dv,
					(long)(((PWord *) *dv) - wm_heapbase));
#endif
	
/*
#ifdef	BigStruct
	one = (PWord *)((PWord *)((PWord *)wm_H + 1) + 1); 
	two = (PWord *)((PWord *)((PWord *)wm_H + 1) + 2); 
#else
	one = (PWord *)((PWord *)wm_H + 1); 
	two = (PWord *)((PWord *)wm_H + 2); 
#endif
*/
		/* 1st two args of the delay term: */
	one = (PWord *)((PWord *)wm_H + 1); 
	w_install(one,(int)one,MTP_UNBOUND); 
	two = (PWord *)((PWord *)wm_H + 2); 
	w_install(two,(int)two,MTP_UNBOUND);

		/* make the delay term: */
	w_mk_term(&vv, &vvt, TK_DELAY, 4);
		/* the 2nd arg (=two) is ok -- it's unbound;
		   install the module and goal args: */
	w_install_argn(vv, 3, m,mt);
	w_install_argn(vv, 4, g,gt);  

	/* Now we have to bind the incoming argument on
	   which the freeze was executed (dv) to the
	   newly created unbound variable, one.  Since the
	   incoming argument dv is older than the newly 
	   allocated location one, normal practice would be
	   to install a pointer in one --> dv;  however, the
	   operation of our delay mechanism requires that 
	   the binding be installed the other way 'round:
			dv --> one
	   So we cannont call on unify [_w_unify(dv, one) ] 
	   even though this would handle the wake-up
	   problems we have to solve below.

	   The correct binding is accomplished by:
			w_install((PWord *)dv,(int)one,MTP_UNBOUND);
	  */


	/*
	   If dv was an ordinary unbound variable, thie is
	   all we would have to do.  However, dv might itself 
	   be an already created delay variable; e.g., we 
	   could be the second freeze of this example:
			freeze(X, foo1(X)),
			freeze(X, foo2(X)),

	   So we have to worry about the case in which
	   dv is already a delay var; otherwise, nothing
	   extra has to be done.

	   When both dv and one are delay vars, if the
	   binding of them together wenth thru the normal
	   mechanism, eventually we would reach the point
	   where we recognized that we had two uninstatiated
	   delay vars which were being bound together. Since
	   we must cope with the special cases of intervals
	   (as opposed to non-interval delay vars), we
	   simply call into Prolog on '$combine_dvars'/2
	   [from blt_frez.pro] which is designed to do 
	   exactly this.


	   Note that we can assume that dv is uninstatiated,
	   due to the test performed by freeze/3 in
	   blt_frez.pro.
	 */

	w_install(dv,(int)one,MTP_UNBOUND);

	update_chpt_slots((PWord)wm_H);

		/* return the delay term in the 4th arg: */
	if (w_unify(rdt, rdtt, vv, vvt))
	{
#ifdef DEBUGFREEZE
pbi_cptx();
printf("exit delay---wm_H=%x--real_dv=%x[_%lu]-------\n", 
					(int)wm_H,  (int)one,
					(long)(((PWord *) one) - wm_heapbase));
#endif
		SUCCEED;
	}
	else
		FAIL;
}

int
pbi_is_delay_var()
{
    PWord *dv;
    int dvt;

    w_get_An((PWord *)&dv, &dvt, 1);

    if (dvt != WTP_UNBOUND)
	FAIL;

    if (CHK_DELAY(dv))
	SUCCEED;
    else
	FAIL;
}


	/*-------------------------------------------------*
	 | Sweep the trail/cp stack, from top to first choice point,
	 | collecting the active delay var terms;
	 *-------------------------------------------------*/

PWord	deref_2		PARAMS(( PWord ));

PWord
deref_2(w)
    register PWord w;
{
    register PWord x,w2;

	w2 = w;
    while (M_ISVAR(w) && (x = M_VARVAL(w)) != w)
		{w2 = w; w = x;}

    return w2;
}

int
pbi_clct_tr()
{
	PWord **CurT,*Back1,*Forw1;
	PWord BStop,v1,clctv,DrT;
	int t1,cvt;

    w_get_An(&v1, &t1, 1);
	BStop = (PWord) wm_B;
	w_mk_sym(&clctv,&cvt,TK_NIL);  

#ifdef DEBUGFREEZE
	printf("clct: wm_TR=%x BStop (=B) =%x\n",(int)wm_TR,(int)BStop);
#ifdef TRAILVALS
	for (CurT = (PWord **)wm_TR+1; CurT < (PWord **)BStop; (PWord *)CurT += 2)  
#else
	for (CurT = (PWord **)wm_TR; CurT < (PWord **)BStop; CurT += 1)  
#endif
	{
		printf("%x-[%x]", (int)CurT,(int)*CurT);
			/* Delay term not allowed to be resettable: */
		if (CHK_DELAY(*CurT))
		{
			DrT = deref_2(*CurT);
			printf("Delay VAR! <%x | %x> ",(int)(**CurT),(int)deref_2(*CurT));
			Forw1 = (PWord *)DrT + 1;
			if (M_ISVAR(*Forw1) && (M_VARVAL(*Forw1) == (PWord)Forw1)) 
				{
				printf(" - Active\n");
				Back1 = (PWord *)DrT-1;
				w_install_argn((int)Back1, 2, clctv, cvt);
				w_install(&clctv, (int)Back1, WTP_STRUCTURE);
				cvt = WTP_STRUCTURE;
				}
			else
				printf(" - InActive\n");
		/*		break;    */
		}
		else
			printf("\n");
	}
#else  /* no-DEBUGFREEZE */
#ifdef TRAILVALS
	for (CurT = (PWord **)wm_TR+1; CurT < (PWord **)BStop; CurT += 2) 
#else
	for (CurT = (PWord **)wm_TR; CurT < (PWord **)BStop; CurT += 1) 
#endif
	{
		if (CHK_DELAY(*CurT))
		{
			DrT = deref_2((PWord)*CurT);
			Forw1 = (PWord *)DrT + 1;
			if (M_ISVAR(*Forw1) && (M_VARVAL(*Forw1) == (PWord)Forw1)) 
			{
				Back1 = (PWord *)DrT-1;
				w_install_argn((int)Back1, 2, clctv, cvt);
				w_install(&clctv, (int)Back1, WTP_STRUCTURE);
				cvt = WTP_STRUCTURE;
			}
			else
				break;
		}
	}
#endif
	if (w_unify(v1, t1, clctv, cvt))
		SUCCEED;
	else
		FAIL;
}

int
pbi_del_tm_for()
{
	PWord *Back1, v1,v2,DrT,tms;
	int t1,t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

	if (CHK_DELAY((PWord *)v1))
	{
		DrT = deref_2((PWord)v1);
		Back1 = (PWord *)DrT-1;
		w_install(&tms, (int)Back1, WTP_STRUCTURE);
		if (w_unify(v2, t2, tms, WTP_STRUCTURE))  
			SUCCEED;
		else
			FAIL;
	}
	else
		FAIL;
}

int
update_chpt_slots(hval)
	PWord hval;
{
  PWord *CurP, *Stop;

	CurP = (PWord *) wm_B;
	Stop = (PWord *)wm_trailbase;

		/* see chpt.h for choice-point macros */
	while (CurP != 0) {
		chpt_HB(CurP) = (PWord *)hval;
		CurP = (PWord *)chpt_B(CurP);
	}
	wm_HB = (PWord *)hval;
	SUCCEED;
}

	/*---------------------------------------------------------------
	 | combin_dels(r,f)
	 |
	 | Effectively performs:
	 |		w_rungoal(builtins, '$combine_dvars'(r,f), WTP_STRUCTURE)
	 |
	 |  '$combine_dvars'/2 is defined in builtins/blt_frez.pro
	 |  Note that f should be the senior (older) of the two vars.
	 *--------------------------------------------------------------*/
void
combin_dels(r,f)
	PWord r, f;
{
	PWord mod,   goal,   cdfctr;
	int   mod_t, goal_t, cdf_t;

#ifdef DEBUGFREEZE
printf("combin_dels:r=_%lu f=_%lu \n",
			(long)(((PWord *) r) - wm_heapbase),
			(long)(((PWord *) f) - wm_heapbase)); 
#endif

	w_mk_sym(&mod,&mod_t,TK_BUILTINS);  

	w_mk_sym(&cdfctr, &cdf_t, TK_CMBVARS);
	w_mk_term(&goal, &goal_t, cdfctr, 2);

	w_install_argn(goal, 1, r, WTP_UNBOUND);
	w_install_argn(goal, 2, f, WTP_UNBOUND);

	w_rungoal(mod, goal, goal_t);
}



/*========================================================================*
                               DEBUGGING FUNCTIONS
 *========================================================================*/

int	pbi_walk_cps	PARAMS(( void ));
void	disp_heap_item	PARAMS(( PWord * ));
int	pbi_swp_tr		PARAMS(( void ));
int	disp_heap 		PARAMS(( void ));

	/*-------------------------------------------------*
	 | Walk the choice point stack,from newest to
     	 | oldest, printing out the choice points;
	 *-------------------------------------------------*/

int
pbi_walk_cps()
{
  PWord *CurP, *Stop;

	CurP = (PWord *) wm_B;
	Stop = (PWord *)wm_trailbase;
	printf("wm_TR=%x  Init CurP (=B) =%x  wm_TRbase=%x\n",
						(int)CurP,(int)wm_TR,(int)Stop);

		/* see chpt.h for choice-point macros */
	while (CurP != 0) {
		printf("curP=%x  prevB=%x  spb=%x  hb=%x  nxtc= %x\n",
				(int)CurP, (int)chpt_B(CurP), (int)chpt_SPB(CurP),
				(int)chpt_HB(CurP), (int)chpt_NextClause(CurP)  );

	CurP = (PWord *)chpt_B(CurP);
	}
	SUCCEED;
}

	/*-------------------------------------------------*
	  Print out current values of WAM registers;
	 *-------------------------------------------------*/
int
pbi_cptx()
{
	printf("Tr_b= %x  B= %x  TR= %x  H= %x  HB= %x  H_b= %x\n",
			(int)wm_trailbase,(int)wm_B,(int)wm_TR,
			(int)wm_H,(int)wm_HB,(int)wm_heapbase);
	SUCCEED;
}

	/*-------------------------------------------------*
	 | Display an individiual heap entity
	 *-------------------------------------------------*/

void
disp_heap_item(CurT)
  PWord *CurT;
{
  PWord Tagg, CTagg, *STRADDR;
  int FID;
  char *FSt;

    	Tagg =  MTP_TAG( *CurT );
		printf("%lx - (%lx)[%d]", (long)CurT,(long)*CurT,(int)Tagg);
    	switch (Tagg) {
		case MTP_UNBOUND:
			CTagg = M_VARVAL(*CurT);
			if (CTagg == (PWord)CurT)
				printf("unbound\n");
			else
				printf("Ref->%x\n",(int)CTagg);
			break;
		case MTP_STRUCT:
			printf("structure:");

			STRADDR = MSTRUCTADDR(*CurT);
			FID = MFUNCTOR_TOKID(*STRADDR);
			if (FID < tok_table_size() )
			{
				FSt = toktable[FID].tkname;
				printf("(%x)-fctr=tokid(%d) %s/%d\n",(int)STRADDR,
								(int)FID,FSt,(int)MFUNCTOR_ARITY(*STRADDR));
			}
			else
				printf("-weird sym(as fctr:%x/%x)\n",FID,MFUNCTOR_ARITY(*CurT));
			break;
		case MTP_LIST:
			printf("list\n");
			break;
		case MTP_CONST:
		{
			CTagg = (int)MTP_CONSTTAG( (*CurT ) );
			printf("constant: (%d)",(int)CTagg);
			switch (CTagg) {
			case MTP_INT:
				printf("-integer=%d\n",(int)MINTEGER(*CurT));
				break;
			case MTP_SYM:
				if (MSYMBOL((*CurT)) < tok_table_size() )
					printf("-symbol=%d/%s\n",MSYMBOL((*CurT)),
							(int)TOKNAME(MSYMBOL((*CurT))));
				else
					printf("-weird sym(as fctr:%x/%x)\n",
							(int)MFUNCTOR_TOKID(*CurT),(int)MFUNCTOR_ARITY(*CurT));
				break;
			case MTP_FENCE:
				printf("-fence\n");
				break;
			case MTP_UIA:
				printf("-uia\n");
				break;
			default:
				printf("-unknown constant\n");
			}
		}
			break;
		default:
			printf("unknown quantity: %d\n",(int)Tagg);
		}
}

	/*-------------------------------------------------*
	 | Sweep the trail/cp stack, from top to first choice point,
	 | displaying each entry and the item it references
	 *-------------------------------------------------*/

int
pbi_swp_tr(void)
{
	PWord **CurT, *Back1, BStop, TrS;

	BStop = (PWord) wm_B;
	TrS = (PWord) wm_TR;
	printf("trail:wm_TR=%lx -> BStop (=B) =%lx\n",(long)TrS,(long)BStop);

#ifdef TRAILVALS
	for (CurT = (PWord **)wm_TR+1; CurT < (PWord **)BStop; CurT += 2)
#else
	for (CurT = (PWord **)wm_TR; CurT < (PWord **)BStop; CurT += 1)
#endif
	{
		printf("%lx[%lx]->", (long)CurT,(long)(1 & (long)CurT) );
		disp_heap_item(*CurT);
		Back1 = (*CurT)-1;
		if ((MFUNCTOR_TOKID(*Back1) == TK_DELAY) &&
				(MFUNCTOR_ARITY(*Back1) == 4))
		{
			printf("Delay VAR!");
			printf("         ");
			disp_heap_item(*Back1);
		}
#ifdef TRAILVALS
		printf("  +>%lx[%lx]->", (long)(CurT-1),(long)(1 & (long)(CurT-1)) );
		disp_heap_item(*(CurT+1));
#else
#endif
	}
	SUCCEED;
}




	/*-------------------------------------------------*
	 | Sweep the heap from newest backwards,
	 | displaying entries
	 *-------------------------------------------------*/

int
disp_heap()
{
    PWord v1,v2;
    int   t1,t2,start,stop;
	PWord *CurA;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    if (t1 != WTP_INTEGER || t2 != WTP_INTEGER)
		FAIL;

		/* If v1,v2 > 0, need:  wm_H >= v1 >= v2 >=  wm_heapbase */
	if (v1 == 0)
		start = (int)wm_H;
	else if (wm_H < (PWord *)v1  || v1 < 0)
		FAIL;
	else 
		start = (int)v1;

	if (v2 == 0) 
		stop = (int)wm_heapbase+1;
	else if (v2 < 0)
	{
		stop = start + (int)v2;
		if (stop < (int)wm_heapbase)
			FAIL;
	}
	else if ( (v1 < v2) || (((PWord *)v2) < wm_heapbase) )
		FAIL;
	else
		stop = v2;

printf("Heap display: %x --> %x\n",(int)start,(int)stop);


	for (CurA = (PWord *)start; CurA >= (PWord *)stop; CurA -= 1)
		disp_heap_item(CurA);   

	SUCCEED;
}
#endif /* FREEZE */
