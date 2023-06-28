/*===================================================================*
 |		bmeta.c   
 |	Copyright (c) 1985 by Kevin A. Buettner
 |	Copyright (c) 1986-1993 by Applied Logic Systems
 |
 |		-- prolog builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | 06/28/85 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K. Buettner -- IBM PC conversion
 *===================================================================*/
#include "defs.h"
#include "module.h"
#include "icodegen.h"

#ifdef CMeta

int
pbi_true()
{
    SUCCEED;
}

int
pbi_equal()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (w_unify(v1, t1, v2, t2))
		SUCCEED;
    else
		FAIL;

}	/* pbi_equal */

int
pbi_arg()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord functor;
    int   arity;
    PWord argval;
    int   argtype;

    w_get_An(&v1, &t1, 1);	/* Get argument number  */
    w_get_An(&v2, &t2, 2);	/* Get structure or list */
    w_get_An(&v3, &t3, 3);	/* Get argument to unify with */

    switch (t1) {
    case WTP_UNBOUND:
    	PERR_INSTANTIATION(find_token((UCHAR *)"arg"), 3);
    	break;
    case WTP_INTEGER:
    	if (v1 < 0)
    	   PERR_DOMAIN(find_token((UCHAR *)"arg"), 3, find_token((UCHAR *)"not_less_than_zero"), v1, t1);
    	break;
    default:
        PERR_TYPE(find_token((UCHAR *)"arg"), 3, TK_INTEGER, v1, t1);
    }
    
    switch (t2) {
	case WTP_STRUCTURE:
	    w_get_functor(&functor, v2);
	    w_get_arity(&arity, v2);
	    if (v1 > arity || v1 < 1)
			FAIL;

	    w_get_argn(&argval, &argtype, v2, (int) v1);

	    break;

	case WTP_LIST:
	    if (v1 == 1)
			w_get_car(&argval, &argtype, v2);
	    else if (v1 == 2)
			w_get_cdr(&argval, &argtype, v2);
	    else
			FAIL;

	    break;

	case WTP_UNBOUND:
	    PERR_INSTANTIATION(find_token((UCHAR *)"arg"), 3);
    	break;
	default:
	    PERR_TYPE(find_token((UCHAR *)"arg"), 3, find_token((UCHAR *)"compound"), v2, t2);
    }

 	if (w_unify(argval, argtype, v3, t3))
		SUCCEED;
   	else
		FAIL;

} /* pbi_arg */

#ifndef BigStruct
#define w_get_argaddr(addr,s,argn,arity)  (addr = (PWord *)s + argn)

#else  /* BigStruct */
#define w_get_argaddr(addr,s,argn,arity)  \
	{if (arity < ESCAPE_ARITY) addr = (PWord *)s + argn; else addr=(PWord *)s+(argn+1);}

#endif /* BigStruct */

#define w_get_caraddr(addr,list)    (addr = (PWord *)list)
#define w_get_cdraddr(addr,list)    (addr = (PWord *)list + 1)

int
pbi_mangle()
{				/* mangle(ArgN,Struct,Arg) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    int   arity;
    PWord *argaddr;
    PWord *b;
    PWord *newv3 = NULL;	/* stifle -Wall */

    w_get_An(&v1, &t1, 1);	/* Get argument number  */
    w_get_An(&v2, &t2, 2);	/* Get structure or list to be mangled*/
    w_get_An(&v3, &t3, 3);	/* Get new argument to mangle into the struct or list */

    if ((t1 != WTP_INTEGER) || (t3 == WTP_UNBOUND))
	FAIL;

    switch (t2) {
	case WTP_STRUCTURE:
	    w_get_arity(&arity, v2);
	    if (v1 > arity || v1 < 1)
		FAIL;
	    w_get_argaddr(argaddr, v2, (int) v1, arity);
	    break;

	case WTP_LIST:
	    if (v1 == 1)
		w_get_caraddr(argaddr, v2);
	    else if (v1 == 2)
		w_get_cdraddr(argaddr, v2);
	    else
		FAIL;
	    break;

	default:
	    FAIL;
    }

    w_install(argaddr, v3, t3);	/* mangle the new argument */

    /*
     * if the object is an integer, or a symbol, we are done.
     * Otherwise the object is a list, a structure, or an uia.
     * In that case, if the object is above the slot in the heap
     * we have to update "HB" registers and HB values in choice
     * points.
     */
    /*--------------------------------------------------------------------------*
     | if the object is an integer, or a symbol, we are done, because they
	 | occupy only one word, and so are stored in the location argaddr;
     | Otherwise the object is a list, a structure, or an uia.
     | In that case, we have to worry that the new pointer we just have
	 | installed in argaddr will never become a dangling pointer.  This could
	 | happen if: a) the object (a list,struct,uia) pointed at by v3 is above 
	 | (more recent than) the slot (argaddr) in the heap, and
	 | b) at some point under backtracking, the top of heap pointer were
	 | set to a location __BETWEEN__ the location of v3 and the slot argaddr.
	 | This can happen only if the heap backtrack (hb)  value of some choicepoint 
	 | points to such an inbetween value.  At the moment, v3 is at the top of
	 | the heap (wm_H).  So we only have to worry about choicepoints whose hb
	 | value points to some point above (more recent than) argaddr, since any
	 | hb value of a cp which points to lower (older than) values than argaddr
	 | is not in between v3 and argaddr.  So we can avoid this happening by
     | changing all "HB" registers and the hb values in choice points whose
	 | hb value is pointing above argaddr.  We update these to point at the
	 | top of heap, w_H.
     *--------------------------------------------------------------------------*/


    if ((t3 != WTP_INTEGER) && (t3 != WTP_SYMBOL)) {

	switch (t3) {
	    case WTP_LIST:
	    case WTP_STRUCTURE:
		newv3 = (PWord *) MBIAS(v3);
		break;
	    case WTP_UIA:
		newv3 = (PWord *) MBIAS(M_FIRSTUIAWORD(v3));
		break;
	}

	if (argaddr < newv3) {
	    /*
	     * The object is a list, a structure or an uia, and,
	     * the object is above the slot
	     */
	    gv_setcnt++;
	    wm_HB = wm_H;
	    b = wm_B;
	    while (b != (PWord *) 0 && (PWord) chpt_HB(b) >= (PWord) argaddr) {
		chpt_HB(b) = wm_H;
		chpt_SPB(b) = (PWord *) (((long) chpt_SPB(b)) & ~3);
		b = chpt_B(b);
	    }
	}
    }
    SUCCEED;

}

extern void	disp_heap_item	( PWord * );

#ifdef TRAILVALS

int trailed_mangle0	(PWord,PWord,int,PWord,int);

int
pbi_trailed_mangle(void)
{				/* trailed_mangle(ArgN,Struct,Arg) */
    PWord v1, v2, v3;
    int   t1, t2, t3;

    w_get_An(&v1, &t1, 1);	/* Get argument number  */
    w_get_An(&v2, &t2, 2);	/* Get structure or list */
    w_get_An(&v3, &t3, 3);	/* Get argument to mangle */

    if ((t1 != WTP_INTEGER) || (t3 == WTP_UNBOUND))
	FAIL;

	return trailed_mangle0(v1,v2,t2,v3,t3);

}

/*---------------------------------------------------------------
 |  trailed_mangle0(v1,v2,t2,v3,t3)
 |	v1 - integer giving arg pos in v2 to replace;
 |	v2 - structure/list in which to replace arg;
 |	t2 - type of v2;
 |	v3 - incoming thing to put into arg v1 of term v2
 |	t3 - type of v3
 |
 |	Does the mangle and double-word trails the change;
 |	word #1 of the trail pair points to the position
 |	in v2 which was changed; word #2 in the pair points
 |	to the old value of that position.
 |-------
 | The call from int_net.c:
 |
 |	trailed_mangle0(UIA_POSITION, IntrvTm, WTP_STRUCTURE, IntUIA, WTP_UIA); 
 *---------------------------------------------------------------*/

int
trailed_mangle0(v1,v2,t2,v3,t3)
     PWord v1, v2, v3;
     int       t2, t3;
{				
  int   arity;
  PWord *argaddr;
  PWord *b;
  PWord *newv3 = NULL, oldarg = 0, *newoldarg = NULL;	/* stifle -Wall */
  int oldargt;

	if (t3 == 5) 
    printf("IN_tm:v1=%d v2=%x,t2=%d,v3=%x,t3=%d\n", (int)v1,(int)v2,(int)t2,(int)v3,(int)t3);

  switch (t2) {
  case WTP_STRUCTURE:
    w_get_arity(&arity, v2);
    if (v1 > arity || v1 < 1)
      FAIL;
/*    w_get_argn(&oldarg, &oldargt, v2, (int) v1);  */
    w_get_argaddr(argaddr, v2, (int) v1, arity);
    w_get(&oldarg, &oldargt, *argaddr);
    break;

  case WTP_LIST:
    if (v1 == 1) {
      w_get_car(&oldarg, &oldargt, v2);
      w_get_caraddr(argaddr, v2);
    }
    else if (v1 == 2) {
      w_get_cdr(&oldarg, &oldargt, v2);
      w_get_cdraddr(argaddr, v2);
    }
    else
      FAIL;
    break;

  default:
    FAIL;
  }
  /* argaddr is the location to be modified;
     Need to trail this location and move its
     value onto the trail before we modify it
     */

  /*
    printf("tr_mg: wm_TR=%0x   \n",(int)wm_TR);
    printf("tr_mg: wm_TR-1=%0x   argaddr=%0x\n",(int)(wm_TR-1),(int)argaddr);
    printf("tr_mg: wm_TR-2=%0x   *argaddr=%0x  ",(int)(wm_TR-2),(int)*argaddr);
    pbi_cptx();
    disp_heap_item(argaddr); 
    */

  /* Copy the old value onto the right place on the trail: */

  /* CHANGED BY SPIRO (???)
    Must use *argaddr and NOT oldarg below because 
    we must preserve the data with tag and all
    */
  *(((PWord *)wm_TR)-2) = *argaddr;

  /*
    printf("Compare: *argaddr=%0x [%0x]  oldarg=%0x \n",
    (int)*argaddr, (int)(wm_heapbase + *argaddr), (int)oldarg);
    */

  /* Trail this argument location: */

  *(((PWord *)wm_TR)-1) = (PWord)argaddr;

  /*
    printf("tr_mg: AFTER TRAIL\n");
    printf("tr_mg: wm_TR=%0x   \n",(int)wm_TR);
    printf("tr_mg: wm_TR-1=%0x [%0x]\n",(int)(wm_TR-1),(int)*(wm_TR-1));
    printf("tr_mg: wm_TR-2=%0x [%0x]\n",(int)(wm_TR-2),(int)*(wm_TR-2));
    pbi_cptx();
    */

  wm_TR = (PWord *)wm_TR - 2;

  w_install(argaddr, v3, t3);	 /* mangle the new argument */
  
  /*
    printf("--mangle done --\n");
    pbi_cptx();
    disp_heap_item(argaddr); 
    */
  /*---------------------------------------------------------------------*
    | non-TRAILVALS case:
    | if the object is an integer, or a symbol, we are done, because they
    | occupy only one word, and so are stored in the location argaddr;
    | Otherwise the object is a list, a structure, or an uia.
    | In that case, we have to worry that the new pointer we just have
    | installed in argaddr will never become a dangling pointer.  This could
    | happen if: a) the object (a list,struct,uia) pointed at by v3 is above 
    | (more recent than) the slot (argaddr) in the heap, and
    | b) at some point under backtracking, the top of heap pointer were
    | set to a location __BETWEEN__ the location of v3 and the slot argaddr.
    | This can happen only if the heap backtrack (hb)  value of 
    | some choicepoint 
    | points to such an inbetween value.  At the moment, v3 is at the top of
    | the heap (wm_H).  So we only have to worry about choicepoints whose hb
    | value points to some point above (more recent than) argaddr, since any
    | hb value of a cp which points to lower (older than) values than argaddr
    | is not in between v3 and argaddr.  So we can avoid this happening by
    | changing all "HB" registers and the hb values in choice points whose
    | hb value is pointing above argaddr.  We update these to point at the
    | top of heap, w_H.
    |
    | #ifdef TRAILVALS case:
    | All of the above applies, but we must also assure that the old value
    | in the arg slot (living at argaddr when we started) is preserved;
    | again, if it is an integer or symbol, we are ok; but if it is a 
    | list, structure, or uia, we must also worry about it just like newv3;
    *---------------------------------------------------------------------*/

  if ((t3 != WTP_INTEGER) && (t3 != WTP_SYMBOL)) {
    /* The new object v3 is a list, a structure or an uia */
    switch (t3) {
    case WTP_LIST:
    case WTP_STRUCTURE:
      newv3 = (PWord *) MBIAS(v3);
      break;
    case WTP_UIA:
      newv3 = (PWord *) MBIAS(M_FIRSTUIAWORD(v3));
      break;
    }

    if ((oldargt != WTP_INTEGER) && (oldargt != WTP_SYMBOL)) {
      /* The old object oldarg is a list, a structure or an uia */
      switch (oldargt) {
      case WTP_LIST:
      case WTP_STRUCTURE:
	newoldarg = (PWord *) MBIAS(oldarg);
	break;
      case WTP_UIA:
	newoldarg = (PWord *) MBIAS(M_FIRSTUIAWORD(oldarg));
	break;
      }
      if ((argaddr < newv3) || (argaddr < newoldarg)) {
	/* Either the new object v3 or the old object oldarg is above the slot */
	/*
	  printf("-*-TM:CPupd:wm_HB(old)=%0x wm_H=%0x [argaddr=%0x newv3=0%x newoldarg=%0x]\n",
                                                    wm_HB,wm_H,argaddr,newv3,newoldarg);
	  */
	gv_setcnt++;
	wm_HB = wm_H;
	b = wm_B;
	while (b != (PWord *) 0 && (PWord) chpt_HB(b) >= (PWord) argaddr) {
	  chpt_HB(b) = wm_H;
	  chpt_SPB(b) = (PWord *) (((long) chpt_SPB(b)) & ~3);
	  b = chpt_B(b);
	}
      }
    } /* oldarg is an integer or symbol */

    else if (argaddr < newv3) {
      /* The new object v3 is above the slot */
      /*
	printf("*TM:CPupd:wm_HB(old)=%0x wm_H=%0x [argaddr=%0x newv3=0%x]\n",wm_HB,wm_H,argaddr,newv3);
	*/
      gv_setcnt++;
      wm_HB = wm_H;
      b = wm_B;
      while (b != (PWord *) 0 && (PWord) chpt_HB(b) >= (PWord) argaddr) {
	chpt_HB(b) = wm_H;
	chpt_SPB(b) = (PWord *) (((long) chpt_SPB(b)) & ~3);
	b = chpt_B(b);
      }
    }
  }	/* ((t3 != WTP_INTEGER) && (t3 != WTP_SYMBOL)) */
  /*
    printf("Out-TM: wm_H=%0x wm_HB=%0x\n",wm_H,wm_HB);
    */

  SUCCEED;

}
#endif

int
pbi_functor()
{				/* functor(Struct,F,A)  */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord f;
    int   a;

    w_get_An(&v1, &t1, 1);	/* get arguments        */
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    /*
     * Switch on the type of the first argument
     */

    switch (t1) {
	case WTP_UIA:
	    if (!force_uia(&v1, &t1)) {
		FAIL;
		break;
	    }
	    /* fall thru to WTP_SYMBOL case */
	case WTP_SYMBOL:
	case WTP_INTEGER:
	    if (w_unify(v1, t1, v2, t2) &&
		w_unify((PWord) 0, WTP_INTEGER, v3, t3))
		SUCCEED;
	    else
		FAIL;
	    break;
	
	case WTP_STRUCTURE:
	    w_get_functor(&f, v1);
	    w_get_arity(&a, v1);

	    if (f == TK_DDOUBLE && a == 4) {
		if (w_unify(v1, t1, v2, t2) &&
		    w_unify((PWord) 0, WTP_INTEGER, v3, t3))
		   SUCCEED;
		else
		   FAIL;
	    } else if (w_unify(f, WTP_SYMBOL, v2, t2) &&
		w_unify((PWord) a, WTP_INTEGER, v3, t3))
		SUCCEED;
	    else
		FAIL;

	    break;

	case WTP_UNBOUND:
	    if (t3 == WTP_INTEGER && v3 == 0 && t2 == WTP_INTEGER) {
	    	if (w_unify(v1, t1, v2, t2)) SUCCEED;
	    	else FAIL;
	    } else if (t3 == WTP_INTEGER && v3 == 0 && t2 == WTP_STRUCTURE) {
		w_get_functor(&f, v2);
		w_get_arity(&a, v2);

		if (f == TK_DDOUBLE && a == 4) {
	    	    if (w_unify(v1, t1, v2, t2)) SUCCEED;
	    	    else FAIL;
	    	} else FAIL;
	    	
	    } else if (t3 == WTP_INTEGER  && v3 >= 0 && force_uia(&v2, &t2)) {
		PWord s;
		int   t;
		int   i;

/* Creation of lists and structures is fixed. -- Ilyas & Raman 5/16/91 */

		if (v2 == TK_DOT && v3 == 2) {
		    w_mk_list(&s, &t);
		    w_install_unbound_car(s);
		    w_install_unbound_cdr(s);
		}
		else if (v3 == 0) {
		    s = v2;
		    t = WTP_SYMBOL;
		}
		else {
		    w_mk_term(&s, &t, v2, (int) v3);
		    for (i = 1; i <= v3; i++) {
			w_install_unbound_argn(s, i);
		    }
		}

		if (w_unify(v1, t1, s, t))
		    SUCCEED;
		else
		    FAIL;

	    }
	    else {
	    	if (t2 == WTP_UNBOUND || t3 == WTP_UNBOUND)
	    	    PERR_INSTANTIATION(TK_FUNCTOR, 3);
	    	else if (t2 != WTP_UIA && t2 != WTP_SYMBOL && t2 != WTP_INTEGER) {
	    	    if (t2 == WTP_STRUCTURE) {
			w_get_functor(&f, v2);
	    		w_get_arity(&a, v2);

	   		if (f == TK_DDOUBLE && a == 4 && t3 == WTP_INTEGER && v3 > 0)
	   		    PERR_TYPE(TK_FUNCTOR, 3, TK_ATOM, v2, t2);
	   		else PERR_TYPE(TK_FUNCTOR, 3, find_token((UCHAR *)"atomic"), v2, t2);
	    	    } else PERR_TYPE(TK_FUNCTOR, 3, find_token((UCHAR *)"atomic"), v2, t2);
	    	} else if (t3 != WTP_INTEGER)
	    	    PERR_TYPE(TK_FUNCTOR, 3, TK_INTEGER, v3, t3);
	    	else if ((t2 == WTP_INTEGER || t2 == WTP_STRUCTURE) && t3 == WTP_INTEGER && v3 != 0)
	    	    PERR_TYPE(TK_FUNCTOR, 3, TK_ATOM, v2, t2);
	    	else if (t3 == WTP_INTEGER && v3 < 0)
	    	    PERR_DOMAIN(TK_FUNCTOR, 3, find_token((UCHAR *)"not_less_than_zero"), v3, t3);
	    	    
	    	else
	    	    FAIL;
	    }
	    break;

	case WTP_LIST:
	    if (w_unify((PWord) TK_DOT, WTP_SYMBOL, v2, t2) &&
		w_unify((PWord) 2, WTP_INTEGER, v3, t3))
		SUCCEED;
	    else
		FAIL;
	    break;

	default:
	    FAIL;
	    break;

    }
}

int
pbi_identical()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (wm_identical(v1, t1, v2, t2))
	SUCCEED;
    else
	FAIL;
}

int
pbi_unidentical()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (wm_identical(v1, t1, v2, t2))
	FAIL;
    else
	SUCCEED;
}

int
pbi_eq()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == t2 && v1 == v2)
	SUCCEED;
    else
	FAIL;
}

int
pbi_noneq()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == t2 && v1 == v2)
	FAIL;
    else
	SUCCEED;
}


int
pbi_var()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_UNBOUND)
	SUCCEED;
    else
	FAIL;
}

int
pbi_nonvar()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 != WTP_UNBOUND)
	SUCCEED;
    else
	FAIL;
}

int
pbi_integer()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_INTEGER)
	SUCCEED;
    else
	FAIL;
}

int
pbi_float()
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

#ifdef DoubleType
    if (t == WTP_DOUBLE)
	SUCCEED;
#else  /* DoubleType */
    if (t == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_arity(&arity, v);
	w_get_functor(&functor, v);

	if (arity == 4 && functor == TK_DDOUBLE)
	    SUCCEED;
	else
	    FAIL;
    }
#endif /* DoubleType */
    else
	FAIL;
}

int
pbi_number()
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

#ifdef DoubleType
    if (t == WTP_DOUBLE)
	SUCCEED;
#else  /* DoubleType */
    if (t == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_arity(&arity, v);
	w_get_functor(&functor, v);

	if (arity == 4 && functor == TK_DDOUBLE)
	    SUCCEED;
	else
	    FAIL;
    }
#endif /* DoubleType */
    else if (t == WTP_INTEGER)
	SUCCEED;
    else
	FAIL;
}

int
pbi_atom()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_SYMBOL || ta1 == WTP_UIA)
	SUCCEED;
    else
	FAIL;
}

int
pbi_atomic()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_SYMBOL || ta1 == WTP_INTEGER || ta1 == WTP_UIA)
	SUCCEED;
#ifdef DoubleType
    else if (ta1 == WTP_DOUBLE)
	SUCCEED;
#else  /* DoubleType */
    else if (ta1 == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_arity(&arity, va1);
	w_get_functor(&functor, va1);

	if (arity == 4 && functor == TK_DDOUBLE)
	    SUCCEED;
	else
	    FAIL;
    }
#endif /* DoubleType */
    else
	FAIL;
}

int
pbi_compound(void)
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    switch (ta1) {
    case WTP_STRUCTURE:
    	{
#ifdef DoubleType
	    SUCCEED;
#else
	    PWord functor;
	    int   arity;

	    w_get_arity(&arity, va1);
	    w_get_functor(&functor, va1);

	    if (arity == 4 && functor == TK_DDOUBLE) FAIL;
	    else SUCCEED;
	}
#endif
	break;
    case WTP_LIST:
    	SUCCEED;
    	break;
    default:
    	FAIL;
    	break;
    }
}
#endif /* CMeta */


/*
 * pbi_findterm seeks a term on the heap
 */

int
pbi_findterm()
{				/* $findterm(F,A,Pos, Term,NewPos) */
    PWord f, a, p, t, n;
    int   ft, at, pt, tt, nt;
    PWord functor;
    PWord *hpos;

    w_get_An(&f, &ft, 1);
    w_get_An(&a, &at, 2);
    w_get_An(&p, &pt, 3);
    w_get_An(&t, &tt, 4);
    w_get_An(&n, &nt, 5);

    if (!xform_uia(&f, &ft) || at != WTP_INTEGER || pt != WTP_INTEGER || p < 0)
	FAIL;

    functor = MMK_FUNCTOR(f, a);
    for (hpos = wm_heapbase + p; hpos < wm_H; hpos++) {
	if (*hpos == functor) {
	    if (w_unify((PWord) hpos, WTP_STRUCTURE, t, tt) &&
	    w_unify((long) (hpos - wm_heapbase) + a + 1, WTP_INTEGER, n, nt))
		SUCCEED;
	    else
		FAIL;
	}
    }

    FAIL;
}
