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

void	pbi_delay			PARAMS(( void ));
void	update_chpt_slots	PARAMS(( PWord ));
void	pbi_clct_tr			PARAMS(( void ));
void	pbi_collect_thawed	PARAMS(( void ));

/*---------------------------------------------------------------*
 | pbi_delay()
 |
 | creates a delay term on the heap, given 3 input args from
 | the prolog heap:
 *---------------------------------------------------------------*/

void
pbi_delay()
{
	PWord dv,m,g,vv,rdt,*one,*two;
	int dvt,mt,gt,vvt,rdtt;

#ifdef DEBUGFREEZE
pbi_cptx();
printf("enter delay----wm_H=%x--TK_DELAY=%x-----------------\n",
			(int)wm_H,TK_DELAY);
#endif

    w_get_An(&dv, &dvt, 1);
    w_get_An(&m, &mt, 2);
    w_get_An(&g, &gt, 3);
    w_get_An(&rdt, &rdtt, 4);
	
/*
#ifdef	BigStruct
	one = (PWord *)((PWord *)((PWord *)wm_H + 1) + 1); 
	two = (PWord *)((PWord *)((PWord *)wm_H + 1) + 2); 
#else
	one = (PWord *)((PWord *)wm_H + 1); 
	two = (PWord *)((PWord *)wm_H + 2); 
#endif
*/
	one = (PWord *)((PWord *)wm_H + 1); 
	two = (PWord *)((PWord *)wm_H + 2); 

	w_mk_term(&vv, &vvt, TK_DELAY, 4);

	w_install(one,(int)one,MTP_UNBOUND); 
	w_install((PWord *)dv,(int)one,MTP_UNBOUND);
	w_install(two,(int)two,MTP_UNBOUND);
	w_install_argn(vv, 3, m,mt);
	w_install_argn(vv, 4, g,gt);  

	update_chpt_slots((PWord)wm_H);

	w_unify(rdt, rdtt, vv, vvt);

#ifdef DEBUGFREEZE
printf("exit delay----wm_H=%x--real_dv=%x---------\n", (int)wm_H,(int)one);
#endif

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

void
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
	for (CurT = (PWord **)wm_TR; CurT < (PWord **)BStop; CurT += 1)
	{
		printf("%x-", (int)CurT);
		if (CHK_DELAY(*CurT))
		{
			DrT = deref_2(*CurT);
			printf("[%x]Delay VAR! <%x | %x> ",
						(int)*CurT,(int)(**CurT),(int)deref_2(*CurT));
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
				break;
		}
		else
			printf("\n");
	}
#else
	for (CurT = (PWord **)wm_TR; CurT < (PWord **)BStop; CurT += 1)
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
	w_unify(v1, t1, clctv, cvt);
}
	


void
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
}

	/*---------------------------------------------------------------
	 | combine_delays(r,f)
	 |
	 | r,f are known (uninstatiated) variables being bound together,
     | and both are already known delay variables;  r is the newer
	 | variable  -- a pointer to f is being installed in r;
	 |
	 | Let the delay terms be:
     |		'$delay'(r, _, _, Gr)
     |		'$delay'(f, _, _, Gf)
     | Then this function locates these terms (1 back from the var....),
     | creates a term (Gf, Gr) [= H] [in that order] on the heap,
     | mangles H into the 4th arg of both delay terms, and sweeps the
     | choice points setting them up to the new top of heap.
	 *--------------------------------------------------------------*/
void
combine_delays(r,f)
	PWord r, f;
{
	PWord *rG,*fG,Conj,*one,*two;
	int ConjT;

	rG = (PWord *)((PWord *)r+3);
	fG = (PWord *)((PWord *)f+3);
	
/*
#ifdef	BigStruct
	one = (PWord *)((PWord *)((PWord *)wm_H + 1) + 1); 
	two = (PWord *)((PWord *)((PWord *)wm_H + 1) + 2); 
#else
	one = (PWord *)((PWord *)wm_H + 1); 
	two = (PWord *)((PWord *)wm_H + 2); 
#endif  
*/
	one = (PWord *)((PWord *)wm_H + 1); 
	two = (PWord *)((PWord *)wm_H + 2); 

	w_mk_term(&Conj, &ConjT, TK_COMMA, 2);
	*one = *fG;
	*two = *rG;
	
	*rG = MMK_STRUCTURE(Conj);
	*fG = MMK_STRUCTURE(Conj);

	*(PWord *)((PWord *)r+1) = (PWord)(PWord *)((PWord *)f+1);

	update_chpt_slots((PWord)wm_H);

#ifdef DEBUGFREEZE
	printf("combine:r=%x f=%x rG=%x fG=%x Conj=%x c1=%x  wm_H=%x\n",
			(int)r,(int)f,(int)rG,(int)fG,(int)Conj,
			(int)((PWord *)(((PWord *)Conj + 1) + 1)),(int)wm_H);
#endif
}

void	
pbi_collect_thawed()
{
  	PWord *CurP, *Stop, *CurSt;
	PWord **CurT,*Back1,*Forw1;
	PWord BStop,v1,u,l;
	int t1,cvt,ut,lt;

    w_get_An(&v1, &t1, 1);

	CurP = (PWord *) wm_B;
	CurSt = (PWord *)wm_TR;
	Stop = (PWord *)wm_trailbase;
	BStop = (PWord) CurP;

	w_mk_sym(&l,&lt,TK_NIL);  

	while (CurP != 0){
#ifdef DEBUGFREEZE
		printf("curP=%x  \n", (int)CurP );  
		for (CurT = (PWord **)CurSt; CurT < (PWord **)CurP; CurT += 1)
		{
			printf("%x-", (int)CurT);    
			if (CHK_DELAY(*CurT))
			{
				printf("%x-[%x]th-Delay VAR! <%x | %x> ",
						(int)CurT,(int)*CurT,(int)(**CurT),(int)deref_2(*CurT));
				Forw1 = (*CurT) + 1;
				if (M_ISVAR(*Forw1) && (M_VARVAL(*Forw1) == (PWord)Forw1)) 
					printf(" - Active\n");
				else
				{
					printf(" - Dead\n");
					w_mk_list(&u,&ut);
					Back1 = (*CurT)-1;
	    			w_install_car(u, Back1, WTP_STRUCTURE);
	    			w_install_cdr(u, l, lt);
					l = u;
					lt = ut;
				}
			}
			else
				printf("\n");
#else
		for (CurT = (PWord **)CurSt; CurT < (PWord **)CurP; CurT += 1)
		{
			if (CHK_DELAY(*CurT))
			{
				Forw1 = (*CurT) + 1;
				if (!M_ISVAR(*Forw1) || (M_VARVAL(*Forw1) != (PWord)Forw1)) 
				{
					w_mk_list(&u,&ut);
							/* adjust for BIGSTRUCTURE: */
					Back1 = (*CurT)-1;
	    			w_install_car(u, (PWord)Back1, WTP_STRUCTURE);
	    			w_install_cdr(u, l, lt);
					l = (PWord)u;
					lt = (int)ut;
				}
			}
#endif
		}
		CurSt= (PWord *)(CurP + chpt_SIZE);
		CurP = (PWord *)chpt_B(CurP);
	}
	w_unify(v1, t1, l, lt);
}

/*========================================================================*
                               DEBUGGING FUNCTIONS
 *========================================================================*/

void	pbi_walk_cps	PARAMS(( void ));
void	disp_heap_item	PARAMS(( PWord * ));
void	pbi_swp_tr		PARAMS(( void ));
int		disp_heap 		PARAMS(( void ));

	/*-------------------------------------------------*
	 | Walk the choice point stack,from newest to
     | oldest, printing out the choice points;
	 *-------------------------------------------------*/

void
pbi_walk_cps()
{
  PWord *CurP, *Stop;

	CurP = (PWord *) wm_B;
	Stop = (PWord *)wm_trailbase;
	printf("wm_TR=%x  Init CurP (=B) =%x  wm_TRbase=%x\n",(int)CurP,(int)wm_TR,(int)Stop);

		/* see chpt.h for choice-point macros */
	while (CurP != 0) {
		printf("curP=%x  prevB=%x  spb=%x  hb=%x  nxtc= %x\n",
				(int)CurP, (int)chpt_B(CurP), (int)chpt_SPB(CurP),
				(int)chpt_HB(CurP), (int)chpt_NextClause(CurP)  );

	CurP = (PWord *)chpt_B(CurP);
	}
}

	/*-------------------------------------------------*
	  Print out current values of WAM registers;
	 *-------------------------------------------------*/
void
pbi_cptx()
{
	printf("Tr_b= %x  B= %x  TR= %x  H= %x  HB= %x  H_b= %x\n",
			(int)wm_trailbase,(int)wm_B,(int)wm_TR,
			(int)wm_H,(int)wm_HB,(int)wm_heapbase);
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
		printf("%x - (%x)", (int)CurT,(int)*CurT);
    	switch (Tagg) {
		case MTP_UNBOUND:
			CTagg = M_VARVAL(*CurT);
			if (CTagg == (PWord)CurT)
				printf("unbound\n");
			else
				printf("Ref->%x\n",CTagg);
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
			printf("constant: (%d)",CTagg);
			switch (CTagg) {
			case MTP_INT:
				printf("-integer=%d\n",MINTEGER(*CurT));
				break;
			case MTP_SYM:
				if (MSYMBOL((*CurT)) < tok_table_size() )
					printf("-symbol=%d/%s\n",MSYMBOL((*CurT)),
							TOKNAME(MSYMBOL((*CurT))));
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
			printf("unknown quantity: %d\n",Tagg);
		}
}

	/*-------------------------------------------------*
	 | Sweep the trail/cp stack, from top to first choice point,
	 | displaying each entry and the item it references
	 *-------------------------------------------------*/

void
pbi_swp_tr()
{
	PWord **CurT, *Back1, BStop;

	BStop = (PWord) wm_B;
	printf("BStop (=B) =%x\n",BStop);

	for (CurT = (PWord **)wm_TR; CurT < (PWord **)BStop; CurT += 1)
	{
		printf("%x->", (int)CurT);
		disp_heap_item(*CurT);
		Back1 = (*CurT)-1;
		if ((MFUNCTOR_TOKID(*Back1) == TK_DELAY) &&
				(MFUNCTOR_ARITY(*Back1) == 4))
			printf("Delay VAR!");
		printf("         ");
		disp_heap_item(Back1);
	}
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
		stop = (int)wm_heapbase;
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
