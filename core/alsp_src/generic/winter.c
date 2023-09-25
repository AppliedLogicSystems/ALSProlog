/*===========================================================*
 |			winter.c                     
 |		Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |			-- interface between abstract machine and C
 |
 | Author: Kevin A. Buettner
 | Creation: 2/25/87
 | Revision History:
 | 10/26/94 - C. Houpt -- Various char* casts.
 *===========================================================*/
#include "defs.h"
#include "wintcode.h"

/* WAM variables that must be set for execution */

PWord wm_normal = DEFAULT_SAFETY;

/*
 * Following four variables (wm_safety, wm_trigger, wm_regidx and
 * wm_interrupt_caught) should be next to each other so that they can
 * be in same page(hopefully, at worst in two pages). These variables
 * are used by interrupt handlers and we are going to lock the page
 * where these variables leave (in DOS environment).            -- Ilyas 5/20/91
 */
#ifndef WM_SAFETY_REG_HOOK
PWord wm_safety = DEFAULT_SAFETY;
#endif	/* WM_SAFETY_REG_HOOK */
PWord wm_trigger = -1;
#if 0
//int   wm_regidx = 0;
#endif
PWord wm_interrupt_caught = 0;


PWord wm_in_Prolog = 0;

PWord wm_spying = 0;


/*//Code *wm_cutaddr;*/
/*//Code *wm_overcode;*/	/* This is the Prolog code run during an*/
/*//				 * exception*/
/*//				 */
#ifdef KERNAL
PWord *wm_regs[10][NumWAMRegs];
#else
/*//PWord *wm_regs[100][NumWAMRegs];*/
#endif /* KERNAL */

#ifdef MacOS
/* Since the Mac WAM is not allowed to use the A5 register, we store
 * the H pointer in a2 and keep the Fail Addr in a memory location, which
 * is declared here.
 */
long *Fail;

#endif /* MacOS */

#if 0
//PWord *wm_heapbase;
//PWord *wm_trailbase;
//PWord *wm_stackbot;
#ifdef MacOS
//PWord *wm_stackbot_safety;
#endif /* MacOS */

//PWord *wm_gvfreelist;
//PWord *wm_gvbase;
//int   gv_setcnt;		/* number of times gv_set run since last gc */
#endif

#define round(x,s) ((((x)-1) & ~(long)((s)-1)) + (s))

PWord	deref		( PWord );

#ifdef DEBUG
static int valid_pword(PWord p)
{
	switch (MTP_TAG(p)) {
	case MTP_UNBOUND:
	case MTP_STRUCT:
	case MTP_LIST:
		if (((long *)p > wm_SP && (long *)p < wm_H)) return 1;
		else return 0;
		break;
	default:
		return 1;
		break;
	}
}
#endif

/*
 * Assumption: PWords are 32 bits wide
 */

PWord
deref(PWord w)
{
    PWord x;

	ASSERT(valid_pword(w));

    while (M_ISVAR(w) && (x = M_VARVAL(w)) != w) {
		w = x;
		ASSERT(valid_pword(w));
	}

    return w;
}

void
w_get(PWord *vp, int  *tp, PWord in)
{

    in = deref(in);
    switch ((int) MTP_TAG(in)) {
	case MTP_UNBOUND:
	    *tp = WTP_UNBOUND;
	    *vp = MVAR(in);
	    break;
	case MTP_LIST:
	    *tp = WTP_LIST;
	    *vp = (PWord) MLISTADDR(in);
	    break;
	case MTP_STRUCT:
	    *tp = WTP_STRUCTURE;
	    *vp = (PWord) MSTRUCTADDR(in);
	    break;
#ifdef MTP_CONST
	case MTP_CONST:
	    switch (MTP_CONSTTAG(in)) {
#endif
		case MTP_INT:
		    *tp = WTP_INTEGER;
		    *vp = MINTEGER(in);
		    break;
		case MTP_SYM:
		    *tp = WTP_SYMBOL;
		    *vp = MSYMBOL(in);
		    break;
		case MTP_UIA:
		    *tp = WTP_UIA;
		    *vp = (PWord) MUIA(in);
		    break;
#ifdef MTP_CONST
		default:
		    fatal_error(FE_IN_WGET1, MTP_CONSTTAG(in));
		    break;
	    }
	    break;
#endif
#ifdef DoubleType
	case MTP_DOUBLE:
	    *tp = WTP_DOUBLE;
	    *vp = (PWord) MDOUBLEADDR(in);
	    break;
#endif
	default:
	    fatal_error(FE_IN_WGET2, MTP_TAG(in));
    }
}

void
w_install(PWord *addr, PWord val, int tag)
{
    switch (tag) {
	case WTP_UNBOUND:
	    *addr = MMK_VAR(val);
	    break;
	case WTP_LIST:
	    *addr = MMK_LIST(val);
	    break;
	case WTP_STRUCTURE:
	    *addr = MMK_STRUCTURE(val);
	    break;
	case WTP_INTEGER:
	    *addr = MMK_INT(val);
	    break;
	case WTP_SYMBOL:
	    *addr = MMK_SYM(val);
	    break;
	case WTP_UIA:
	    *addr = MMK_UIA(val);
	    break;
#ifdef DoubleType
	case WTP_DOUBLE:
	    *addr = MMK_DOUBLE(val);
	    break;
#endif /* DoubleType */

	default:
	    fatal_error(FE_IN_WINSTALL, tag);
    }

	ASSERT(valid_pword(*addr));
}

void
w_install_argn(PWord s, int n, PWord v, int t)
{
#ifdef BigStruct
    if (MFUNCTOR_ARITY(*((PWord *) s)) == ESCAPE_ARITY) {
		s = (PWord) (((PWord *) s) + 1);
    }
#endif
    w_install(((PWord *) s) + n, v, t);
}

void
w_install_unbound_argn(PWord s, int n)
{
#ifdef BigStruct
    if (MFUNCTOR_ARITY(*((PWord *) s)) == ESCAPE_ARITY) {
		s = (PWord) (((PWord *) s) + 1);
    }
#endif
    w_install(((PWord *) s) + n, (PWord)(((PWord *) s) + n), WTP_UNBOUND);
}

void
w_get_argn(PWord *rval, int *rtag, PWord s, int argn)
{
#ifdef BigStruct
    if (MFUNCTOR_ARITY(*((PWord *) s)) == ESCAPE_ARITY) {
		s = (PWord) (((PWord *) s) + 1);
    }
#endif
    w_get(rval, rtag, *(((PWord *) s) + argn));
}

void
w_mk_term(PWord *rval, int *rtag, PWord func, int arity)
{
    register PWord *p;

    p = wm_H;
    *rval = (PWord) p;
    *rtag = WTP_STRUCTURE;
#ifdef	BigStruct
    if (arity >= ESCAPE_ARITY) {
		wm_H += arity + 2;
		*p = MMK_FUNCTOR(func, ESCAPE_ARITY);
		*(p + 1) = MMK_INT(arity);
    }
    else {
		wm_H += arity + 1;
		*p = MMK_FUNCTOR(func, arity);
    }
#else  /* BigStruct */
    wm_H += arity + 1;
    *p = MMK_FUNCTOR(func, arity);
#endif /* BigStruct */
}

void
w_get_functor(PWord *rfunc, PWord saddr)
{
    *rfunc = MFUNCTOR_TOKID(*((PWord *) saddr));
}

void
w_get_arity(int *rarity, PWord saddr)
{
    *rarity = MFUNCTOR_ARITY(*((PWord *) saddr));
#ifdef BigStruct
    if (*rarity == ESCAPE_ARITY) {
		*rarity = MINTEGER(*(((PWord *) saddr) + 1));
    }
#endif /* BigStruct */
}

void
w_mk_list(PWord *rval, int *rtag)
{
    *rval = (PWord) wm_H;
    wm_H += 2;
    *rtag = WTP_LIST;
}

/*
 * w_mk_unbound(rval,rtag)
 *
 */

void
w_mk_unbound(PWord *rval, int *rtag)
{
    register PWord *p;

    p = wm_H++;
    *p = (PWord) MMK_VAR(p);
    *rval = (PWord) p;
    *rtag = WTP_UNBOUND;
}

/*
 * w_get_An(rval,rtag,n)
 *
 *      rval and rtag are pointers to longs where the value and tag of
 *      argument n should be stored.
 */

void
w_get_An(PWord *rval, int  *rtag, int n)
{
    w_get(rval, rtag, *(wm_SP + n + 1));
}

/*
 *
 * w_get_uianame(buf,uia,size)
 *
 *      Puts a copy of the null terminated string pointed at (indirectly)
 *      by the integer uia.  It is the programmers responsibility to provide
 *      a buf that is big enough.
 */

UCHAR *
w_get_uianame(register UCHAR *s, PWord uia, register int size)
{
    UCHAR *buf = s;
    register UCHAR *t;

    t = (UCHAR *) M_FIRSTUIAWORD(uia);

    if (size <= 0)
	return 0;

    while (size-- && (*s++ = *t++)) ;

    if (*(s - 1)) {
	*(s - 1) = 0;
	return 0;
    }
    else
	return buf;
}

/*
 * w_mk_uia(rval, rtag, str)
 *
 *      rval and rtag are pointers to longs where the value and tag of the
 *      uia created out of str will be stored.
 *
 * I decided to trade having fast comparison of UIAs for slightly slower
 * output of UIAs, hence the somewhat messy code.
 */
void
w_mk_uia(PWord *rval, int  *rtag, register UCHAR *str)
{
    register UCHAR *t;
    register int i = 0;

    *rval = (PWord) MMK_UIAVAL(wm_H);
    *rtag = WTP_UIA;

    t = (UCHAR *) (wm_H + 1);
    while ((*t++ = *str++))
	i++;
    i = sizeof (PWord) - (i % sizeof (PWord));
    while (i--)
	*t++ = (UCHAR) 0;
    i = (PWord *) t - wm_H;

    *wm_H = MMK_FENCE(i);
    wm_H += i;
    *wm_H++ = MMK_FENCE(i);
}


/*
 * w_mk_len_uia(rval, rtag, str, len)
 *
 *      This function is defined from w_mk_uia() above.  The only difference is that
 *      the length of str is explicitly passed, so str does not have to be 0 terminated.
 */
void
w_mk_len_uia(PWord *rval, int *rtag, register UCHAR *str, register size_t len)
{
    register UCHAR *t;
    register int i = len;

    *rval = (PWord) MMK_UIAVAL(wm_H);
    *rtag = WTP_UIA;

    t = (UCHAR *) (wm_H + 1);
    while (len--) *t++ = *str++;
    i = sizeof (PWord) - (i % sizeof (PWord));
    while (i--)
	*t++ = (UCHAR) 0;
    i = (PWord *) t - wm_H;

    *wm_H = MMK_FENCE(i);
    wm_H += i;
    *wm_H++ = MMK_FENCE(i);
}


/*
 * w_mk_uia_in_place(rval, rtag, str)
 *
 * same as w_mk_uia except that the uia string is already
 * in the heap at the correct position. We simply
 * have to adding padding and fences.
 */

void
w_mk_uia_in_place(PWord *rval, int *rtag, register UCHAR *str)
{
    register UCHAR *t;
    register int i;

    *rval = (PWord) MMK_UIAVAL(wm_H);
    *rtag = WTP_UIA;

    i = strlen((char *)str);
    t = &str[i];
    i = sizeof (PWord) - (i % sizeof (PWord));
    while (i--)
	*t++ = (UCHAR) 0;
    i = (PWord *) t - wm_H;

    *wm_H = MMK_FENCE(i);
    wm_H += i;
    *wm_H++ = MMK_FENCE(i);
}

/*
 * w_uia_alloc(rval, rtag, size)
 *
 *      rval and rtag are pointers to longs where the value and tag of the
 *      uia will be stored.
 */
void
w_uia_alloc(PWord *rval, int *rtag, size_t size)
{
    register PWord *t;
#ifdef __LP64__
    register size_t i;
#else
	register int i;
#endif

    *rval = (PWord) MMK_UIAVAL(wm_H);
    *rtag = WTP_UIA;

    t = (PWord *) (wm_H + 1);
#ifdef __LP64__
    i = (size + sizeof (PWord)) / sizeof(PWord);	/* uia size in long words */
#else
    i = (size + sizeof (PWord)) >> 2;
#endif
    while (i--)
	*t++ = (PWord) 0;

    i = (PWord *) t - wm_H;

/*
	if ( ((unsigned long)wm_TR - (unsigned long)((unsigned long)wm_H +i +1))
			< (unsigned long) wm_normal ) {
	printf("w_uia_alloc: wm_TR=%x wm_H=%x wm_safety=%x wm_normal=%x\n",
				(int)wm_TR, (int)wm_H, (int)wm_safety, (int)wm_normal);
	}
*/

    *wm_H = MMK_FENCE(i);
    wm_H += i;
    *wm_H++ = MMK_FENCE(i);

}

/*
 * w_uia_clip(uia,clipsize)
 */
int
w_uia_clip(PWord uia, int clipsize)
{
    int   size;
    int   i;
    UCHAR *t;
    UCHAR *tend;
    UCHAR *t2end;
    UCHAR *t2start;

    t = (UCHAR *) M_FIRSTUIAWORD(uia);
    size = (int) M_UIASIZE(uia);

    if (clipsize > size)
	return (0);

    if (clipsize < (size - (int) sizeof (PWord))) {

	/* Clip the UIA */
	tend = t + clipsize;
	i = sizeof (PWord) - (clipsize % sizeof (PWord));
	while (i--)
	    *tend++ = (UCHAR) 0;
	i = ((int) ((PWord *) tend - (PWord *) t)) + 1;		/* fence
								 * value
								 */
	*(((PWord *) t) - 1) = MMK_FENCE(i);
	*((PWord *) tend) = MMK_FENCE(i);

	/* Clean rest of the UIA */
	t2start = tend + sizeof (PWord);
	t2end = t + size;
	if ((t2end - t2start) > (int) sizeof (PWord)) {
	    /* Make another uia */
	    i = (int) ((PWord *) t2end - (PWord *) t2start);	/* fence
								 * value
								 */
	    *((PWord *) t2start) = MMK_FENCE(i);
	    *((PWord *) t2end) = MMK_FENCE(i);
	}
	else {
	    /* Make integer(s) */
	    *((PWord *) t2start) = MMK_INT(0);
	    if (t2start != t2end)
		*((PWord *) t2end) = MMK_INT(0);
	}
    }

    return (1);
}

/*
 * w_uia_peek(uia,off,val,valsize)
 */
int
w_uia_peek(PWord uia, int off, register UCHAR *val, register int valsize)
{
    int   size;
    register UCHAR *t;

    t = (UCHAR *) M_FIRSTUIAWORD(uia);
    size = (int) M_UIASIZE(uia);
    if (off >= 0 && (off + valsize) <= size) {
	t += off;
	while (valsize--)
	    *val++ = *t++;
	return (1);
    }
    else
	return (0);
}

/*
 * w_uia_poke(uia,off,val,valsize)
 */
int
w_uia_poke(PWord uia, int off, register UCHAR *val, register int valsize)
{
    int   size;
    register UCHAR *t;

    t = (UCHAR *) M_FIRSTUIAWORD(uia);
    size = (int) M_UIASIZE(uia);
    if (off >= 0 && (off + valsize) < size) {
	t += off;
	while (valsize--)
	    *t++ = *val++;
	return (1);
    }
    else
	return (0);
}

/*
 * w_uia_peeks(uia,off,val,valsize)
 */
int
w_uia_peeks(PWord uia, int off, register UCHAR *val, register int valsize)
{
    int   size;
    register UCHAR *t;

    t = (UCHAR *) M_FIRSTUIAWORD(uia);
    size = (int) M_UIASIZE(uia);
    if (off >= 0 && off < size) {
	t += off;
	if ((off + valsize) > size)
	    valsize = size - off;
	while (valsize && (*val++ = *t++))
	    valsize--;
	if (valsize == 0)
	    *val = (UCHAR) 0;
	return (1);
    }
    else
	return (0);
}

/*
 * w_uia_pokes(uia,off,val)
 */
int
w_uia_pokes(PWord uia, int off, register UCHAR *val)
{
    int   size;
    register UCHAR *t;
    register int valsize;

    t = (UCHAR *) M_FIRSTUIAWORD(uia);
    size = (int) M_UIASIZE(uia);
    if (off >= 0 && off < (size - 1)) {
	t += off;
	valsize = (size - 1) - off;
	while (valsize-- && (*t++ = *val++)) ;
	return (1);
    }
    else
	return (0);
}

#ifdef DoubleType

void
w_mk_double(PWord *rval, int *rtag, double dbl)
{
#ifdef COERCE2INTS
    if (dbl == floor(dbl) && MINPROLOGINT <= dbl && dbl <= MAXPROLOGINT) {
	*rval = (int) floor(dbl);
	*rtag = WTP_INTEGER;
    }
    else 
#endif
	{
	long *h;

	*rval = (long) wm_H;
	*rtag = WTP_DOUBLE;
	h = wm_H;
	wm_H += 4;
	*h = *(h + 3) = MMK_FENCE(3);
	*(h + 1) = *(int *) &dbl;
	*(h + 2) = *(((int *) &dbl) + 1);
    }

}

void
w_get_double(double *dbl, PWord ptr)
{
    *(long *) dbl = *(((PWord *)ptr) + 1);
    *(((long *) dbl) + 1) = *(((PWord *)ptr) + 2);
}
#else
void
w_mk_double(PWord *rval, int  *rtag, double dbl)
{
	int   i;

	w_mk_term(rval, rtag, (PWord) TK_DDOUBLE, 4);
	for (i = 0; i < 4; i++)
	    w_install_argn(*rval, i + 1, (PWord) (*(((short *) &dbl) + i)), WTP_INTEGER);
}

void
w_get_double(double *dbl, PWord ptr)
{
	PWord v;
	int i,t;
	
	for (i = 0; i < 4; i++) {
	   	w_get_argn(&v, &t, ptr, i + 1);
	   	*(((short *) dbl) + i) = (short) v;
	}
}

#endif /* DoubleType */


int
w_unify(PWord v1, int t1, PWord v2, int t2)
{
    PWord a1, a2;

    w_install(&a1, v1, t1);
    w_install(&a2, v2, t2);

    return _w_unify(a1, a2);

}

int
w_rungoal(PWord mod, PWord gv, int gt)
{
    PWord a1, a2;
    dbprot_t odbrs;
    int status;

    w_install(&a1, mod, WTP_SYMBOL);
    w_install(&a2, gv, gt);


    odbrs = w_dbprotect(DBRS_RUNABLE);
    status = wm_rungoal(a1, a2);
    (void) w_dbprotect(odbrs);

    return status;
}
