/*=====================================================================*
 |		winter.h
 |	Copyright (c) 1987-1994 Applied Logic Systems, Inc.
 |
 |		(Low level) machine / C interface include file
 |
 | Author: Kevin A. Buettner
 | Created: 11/25/87
 | Revision History:
 *=====================================================================*/

#ifndef _WINTER_H_INCLUDED_
#define _WINTER_H_INCLUDED_ 1

#ifdef __cplusplus
extern "C" {
#endif


#ifdef KERNAL
#define DEFAULT_SAFETY		400			/* bytes	*/

#define DEFAULT_HEAP_SIZE	0x001000	/* long words */
#define DEFAULT_STACK_SIZE	0x001000	/* long words */

#define	MIN_ICBUFSIZE		0x001000	/* 64k code words */
#define MAX_ICBUFSIZE		0x100000	/* 1M code words */

#else		/* ----------- NON-KERNAL --------------------------*/
#define DEFAULT_SAFETY		98304		/* bytes	*/

#define DEFAULT_HEAP_SIZE	0x040000	/* long words (1 MBytes) */
#define DEFAULT_STACK_SIZE	0x040000	/* long words (1 MBytes) */

#define	MIN_ICBUFSIZE		0x010000	/* 64k code words */
#define MAX_ICBUFSIZE		0x100000	/* 1M code words */

#endif /* KERNAL */



/*
 * NumWAMRegs are the number of shadow locations provided for in the wm_regs
 * structure.  Although we don't use all of them, we pick a small power of
 * 2 in order to get fast multiplication in the assembly language portions of
 * the code.
 */

#define NumWAMRegs		16


/*
 * The 80386 has a limited register set so it was necessary to actually put
 * the "B" register in memory.  Here it is.
 */

#ifdef arch_i386
extern PWord *wm_b_reg;
#endif


/*
 * wm_normal is the value that wm_safety will normally contain when prolog
 * is executing.  This value will initially be set to DEFAULT_SAFETY.  At
 * some point, the compiler will change this value if it detects a clause
 * which will potentially use more heap than the amount provided for by
 * DEFAULT_SAFETY.
 */

//extern PWord wm_normal;


/*
 * Following four variables (wm_safety, wm_trigger, wm_regidx and 
 * wm_interrupt_caught) should be next to each other so that they can
 * be in same page(hopefully, at worst in two pages). These variables 
 * are used by interrupt handlers and we are going to lock the page 
 * where these variables leave (in DOS environment). 		-- Ilyas 5/20/91
 */

#ifdef WM_SAFETY_REG_HOOK
WM_SAFETY_REG_HOOK;
#endif

//extern PWord wm_safety;
//extern PWord wm_trigger;
#if 0
/*//extern int   wm_regidx;*/
#endif
//extern PWord wm_interrupt_caught;

//extern PWord wm_in_Prolog;

/*//extern Code *wm_overcode;*/

//extern PWord wm_spying;

#if 0
/*//extern PWord *wm_regs[][NumWAMRegs];*/

/*//extern PWord *wm_heapbase;*/
/*//extern PWord *wm_trailbase;*/
/*//extern PWord *wm_stackbot;*/
#ifdef MacOS
/*//extern PWord *wm_stackbot_safety;*/
#endif

/*//extern PWord *wm_gvfreelist;*/
/*//extern PWord *wm_gvbase;*/
/*//extern int  gv_setcnt;*/
#endif

/*//extern Code *wm_cutaddr;*/


#include "wintidx.h"

#if 0
//#define wm_B	(wm_regs[wm_regidx][wm_B_idx])
//#define wm_HB	(wm_regs[wm_regidx][wm_HB_idx])
//#define wm_SPB	(wm_regs[wm_regidx][wm_SPB_idx])
//#define wm_E	(wm_regs[wm_regidx][wm_E_idx])
//#define wm_TR	(wm_regs[wm_regidx][wm_TR_idx])
//#define wm_H	(wm_regs[wm_regidx][wm_H_idx])
//#define wm_SP	(wm_regs[wm_regidx][wm_SP_idx])
//#define wm_FAIL (wm_regs[wm_regidx][wm_FAIL_idx])
#endif

/*
 * The following are not functions.
 */

#ifdef Portable
/*//extern Code *wm_fail;*/
/*//extern Code *wm_trust_fail;*/
/*//extern Code *wm_panic;*/
#else
extern	int	wm_fail		( void );
extern	int	wm_trust_fail	( void );
#endif /* Portable */

/*@[3.1]@------------------------------------------------------------------
 * To the builtins, we wish to present a machine independent representation
 * of the prolog objects and their tags.  Hence the WTP tags below.
 *--------------------------------------------------------------------------*/

#define WTP_REF         0               /* Reference        */
#define WTP_UNBOUND     0               /* Unbound Variable */
#define WTP_LIST        1               /* List             */
#define WTP_STRUCTURE   2               /* Structure        */
#define WTP_SYMBOL      3               /* Symbol           */
#define WTP_INTEGER     4               /* Signed integer   */
#define WTP_UIA	        5               /* UnInterned Atom	*/

	/*--------------------------------------------------------------
	 |  This is defined even when DoubleType is undefined, for used
	 |	in the third arg of do_is/3 and 4th arg of make_numberx/4.
	 *-------------------------------------------------------------*/
/* #ifdef	DoubleType */
#define WTP_DOUBLE      6               /* Floating point number */
/* #endif  DoubleType */


/*
 * Structure access macros
 */

#define w_get_car(rval,rtag,laddr) w_get(rval,rtag,* (PWord *) laddr)
#define w_get_cdr(rval,rtag,laddr) w_get(rval,rtag,* (((PWord *) laddr)+1))
#define w_install_car(l,v,t)       w_install((PWord *) l,v,t)
#define w_install_cdr(l,v,t)       w_install(((PWord *) l)+1,v,t)
#define w_mk_int(rval,rtag,ival)   (*(rval)=(PWord)(ival),*(rtag)=WTP_INTEGER)
#define w_mk_sym(rval,rtag,tokid)  (*(rval)=(PWord)(tokid),*(rtag)=WTP_SYMBOL)

#define w_install_unbound_car(l) 	w_install((PWord *)l,(PWord)((PWord *)(l)),WTP_UNBOUND)
#define w_install_unbound_cdr(l)    w_install(((PWord *)l)+1,(PWord)(((PWord *)l)+1),WTP_UNBOUND)

/*
 * Declarations and prototypes for functions defined in winter.c
 */

extern	void	w_get_pe (PE,  PWord *, int *, PWord );
#define		w_get(a,b,c)	w_get_pe(hpe,a,b,c)
extern	void	w_install_pe (PE,  PWord *, PWord, int );
#define		w_install(a,b,c)	w_install_pe(hpe,a,b,c)
extern	void	w_install_argn_pe (PE,  PWord s, int n, PWord v, int t );
#define		w_install_argn(a,b,c,d)	w_install_argn_pe(hpe,a,b,c,d)
extern	void	w_install_unbound_argn_pe (PE,  PWord, int );
#define		w_install_unbound_argn(a,b)	w_install_unbound_argn_pe(hpe,a,b)
extern	void	w_get_argn_pe (PE,  PWord *, int *, PWord, int );
#define		w_get_argn(a,b,c,d)	w_get_argn_pe(hpe,a,b,c,d)
extern	void	w_mk_term_pe (PE,  PWord *, int *, PWord, int );
#define		w_mk_term(a,b,c,d)	w_mk_term_pe(hpe,a,b,c,d)
extern	void	w_get_functor_pe (PE,  PWord *, PWord );
#define		w_get_functor(a,b)	w_get_functor_pe(hpe,a,b)
extern	void	w_get_arity_pe (PE,  int *, PWord );
#define		w_get_arity(a,b)	w_get_arity_pe(hpe,a,b)
extern	void	w_mk_list_pe (PE,  PWord *, int * );
#define		w_mk_list(a,b)	w_mk_list_pe(hpe,a,b)
extern	void	w_mk_unbound_pe (PE,  PWord *, int * );
#define		w_mk_unbound(a,b)	w_mk_unbound_pe(hpe,a,b)
extern	void	w_get_An_pe (PE,  PWord *, int *, int );
#define 	w_get_An(a,b,c)		w_get_An_pe(hpe,a,b,c)
extern	UCHAR *	w_get_uianame_pe (PE,  UCHAR *, PWord, int );
#define		w_get_uianame(a,b,c)	w_get_uianame_pe(hpe,a,b,c)
extern	void	w_mk_uia_pe (PE,  PWord *, int *, UCHAR * );
#define		w_mk_uia(a,b,c)	w_mk_uia_pe(hpe,a,b,c)
extern	void	w_mk_len_uia_pe (PE,  PWord *, int *, UCHAR * , size_t);
#define		w_mk_len_uia(a,b,c,d)	w_mk_len_uia_pe(hpe,a,b,c,d)
extern	void	w_mk_uia_in_place_pe (PE,  PWord *, int *, UCHAR * );
#define		w_mk_uia_in_place(a,b,c)	w_mk_uia_in_place_pe(hpe,a,b,c)
extern	void	w_uia_alloc_pe (PE,  PWord *, int *, size_t );
#define		w_uia_alloc(a,b,c)	w_uia_alloc_pe(hpe,a,b,c)
extern	int	w_uia_clip_pe (PE,  PWord, int);
#define		w_uia_clip(a,b)	w_uia_clip_pe(hpe,a,b)
extern	int	w_uia_peek_pe (PE,  PWord, int, UCHAR *, int );
#define		w_uia_peek(a,b,c,d)	w_uia_peek_pe(hpe,a,b,c,d)
extern	int	w_uia_poke_pe (PE,  PWord, int, UCHAR *, int );
#define		w_uia_poke(a,b,c,d)	w_uia_poke_pe(hpe,a,b,c,d)
extern	int	w_uia_peeks_pe (PE,  PWord, int, UCHAR *, int );
#define		w_uia_peeks(a,b,c,d)	w_uia_peeks_pe(hpe,a,b,c,d)
extern	int	w_uia_pokes_pe (PE,  PWord, int, UCHAR * );
#define		w_uia_pokes(a,b,c)	w_uia_pokes_pe(hpe,a,b,c)
extern	void	w_mk_double_pe (PE, PWord *, int *, double );
#define		w_mk_double(a,b,c)	w_mk_double_pe(hpe,a,b,c)
extern	void	w_get_double_pe (PE, double *, PWord );
#define		w_get_double(a,b)	w_get_double_pe(hpe,a,b)
extern	int	w_unify_pe (PE, PWord, int, PWord, int );
#define 	w_unify(a,b,c,d)	w_unify_pe(hpe,a,b,c,d)
extern	int	w_rungoal_pe (PE, PWord, PWord, int );
#define		w_rungoal(a,b,c)	w_rungoal_pe(hpe,a,b,c)

/*
 * Declarations and prototypes for other related functions.
 */

	/* from arith.c */
extern	void	make_number		 (PE, PWord *, int *, double);
extern	void	make_numberx	 (PE, PWord *, int *, double, int);
	/* from gv.c */
extern	PWord	gv_alloc	 (PE);
extern	int	gv_alloc_gvnum	 (PE, int);
extern	int	gv_isfree	 (PE, int);
extern	void	gv_free		 (PE, PWord);
extern	void	gv_get		 (PE, PWord *, int *, PWord);
extern	void	gv_set		 (PE, PWord, int, PWord);

/* from assembly language (perhaps) */
extern	int	_w_unify	 (prolog_engine *, PWord, PWord);


/*
 * Generated Symbol Starting Character:
 *
 *	The following value is used to begin all generated symbols.
 */

#define Generated_Symbol_Starting_Character 0xfe

/*
 * Heap and stack allocation function (see mem.c)
 */

PWord *allocate_prolog_heap_and_stack ( size_t );

#ifdef __cplusplus
}
#endif

/*
 * The following file contains the als signal numbers
 */

#include "alssig.h"

#endif /* _WINTER_H_INCLUDED_ */
