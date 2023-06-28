
/*===========================================================================*
 |		wam.c                
 | Copyright (c) 1992-96, Applied Logic Systems, Inc.
 |
 | 		-- interpreter for wam instructions
 |
 | Author: Prabhakaran Raman
 | Creation: 5/26/92
 |
 | Macroized 'unwind trail' code
 | Freeze additions - KAB
 *===========================================================================*/

#include "defs.h"
#include "coerce.h"
#include "mtypes.h"
#include "newtokens.h"
#include "module.h"
#include "labels.h"
#include "icode.h"
#include "wintcode.h"
#include "built.h"
#include "machinst.h"
#include "icodegen.h"
#ifdef FREEZE
#include "freeze.h"
#include "hztypes.h"
#endif

#ifdef MacOS
#include "alspi.h"
#endif

/*#include "dump_wam.h" */

#ifdef TRACEBWAM
extern	int tracewam	( Code * );
#endif

	int	run_wam		( Code * );
static	int	wam_unify	( PWord *, PWord * );

#ifdef	Threaded
Code  wam_instrs[W_NUM_OPS];	/* map from instruction number to label */
#endif	/* Threaded */

extern int trailed_mangle0 (PWord,PWord,int,PWord,int);

#ifdef	IProfile

static long iprofile_counts[W_NUM_OPS];
#define DO_PROFILE(op)	: iprofile_counts[op]++; lp##op

init_iprofile()
{
    int i;
    for (i=0; i<W_NUM_OPS; i++)
	iprofile_counts[i] = 0;
}

typedef struct {
    long count;
    int  idx;
} iprofstruct;

static int iprofcmp(a,b)
    iprofstruct *a;
    iprofstruct *b;
{
    return a->count < b->count;
}

dump_iprofile()
{
    int i;
    iprofstruct s[W_NUM_OPS];
#define ABMOP(op,p1,p2,p3,p4) #op,
    static char *inames[] = {
#include "wamops0.h"
    };
#undef ABMOP

    for (i=0; i<W_NUM_OPS; i++) {
	s[i].idx = i;
	s[i].count = iprofile_counts[i];
    }
    qsort(s,W_NUM_OPS,sizeof (iprofstruct), iprofcmp);
    for (i=0; i<W_NUM_OPS; i++) {
	PI_printf("%-20s%d\n",inames[s[i].idx],s[i].count);
    }
}

#else	/* IProfile */
#define DO_PROFILE(op)	/* do nothing */
#endif	/* IProfile */

/*--- Global WAM register locations for use by unify() ---*/

PWord *gr_SPB, *gr_HB, *gr_TR;

/*-----------------------------------*
 * Special purpose WAM code patches
 *-----------------------------------*/

/*//Code *wm_fail;*/
/*//Code *wm_trust_fail;*/
/*//Code *wm_return_success;*/
/*//Code *wm_special;*/
/*//Code *wm_rungoal_code;*/
/*//Code *wm_panic;*/
/*//PWord *rungoal_modpatch, *rungoal_goalpatch;*/

/*-----------------------------------*
 *  type macros and utilities
 *-----------------------------------*/

#define PWORD(v) ((PWord) (v))
#define PWPTR(v) ((PWord *) (v))
#define SHPTR(v) ((short *)(v))

/*---------------------------------------
 |	BIND(r,f): Binds r --> f
 |	Installs pointer to f in r
 |
 |	VVBIND(r,f,...): Special cases the
 |	variable-variable bind case, for
 |	the delay mechanism.  If FREEZE is
 |	undefined, VVBIND is identical to BIND;
 |	Otherwise, it adds a test & action
 |	to the action of BIND
 *--------------------------------------*/

	/*-------------------------------------------------*
	 |  PLAINTRAIL is simple trailing with no delay var
	 |  checking;  arg #2 simply corresponds to TRAIL
	 *-------------------------------------------------*/

    /*-------------------------------------------------*
	 |  RETRAIL is for moving a trail pair of words
	 |  during compaction of the trail during a cut;
	 *-------------------------------------------------*/

#ifdef TRAILVALS

#define PLAINTRAIL(r)                             \
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) \
		*--mr_TR = PWORD(r);                        \
		*--mr_TR = *((PWord *)r); }

#define RETRAIL(r,r1) \
	{ if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) {  \
		*--mr_TR = PWORD(r);               \
		*--mr_TR = PWORD(r1);              \
	} }
#else /* No-TRAILVALS */

#define PLAINTRAIL(r)                             	\
	{ if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) \
		*--mr_TR = PWORD(r);  }

#define RETRAIL(r,r1) 								\
	{ if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB)	\
		*--mr_TR = PWORD(r);  }

#endif /* TRAILVALS */

    /*-------------------------------------------------*
	 |  Debug messages from inside Trailing &
	 |  Binding (with DEBUGFREEZE...)
	 *-------------------------------------------------*/

#define DDVVB(WW,lnn,r,f) \
			printf("%s--@@@ [ln=%d] r=%x[_%lu]  mr_TR=%x f=%x[_%lu]\n",		\
				WW,lnn, 													\
				(int)r,(long)(((PWord *) r) - wm_heapbase),(int)mr_TR-1,	\
				(int)f,(long)(((PWord *) f) - wm_heapbase)); 

#define	MMTCH(f) printf("   match is delay [f=%x[_%lu]]\n", 				\
						  (int)f,(long)(((PWord *) f) - wm_heapbase));	   

int ck_intvl_punch   (PWord, PWord, int);

    /*-------------------------------------------------*
	 |	ck_intvl_punch - used during variable binding,
	 |	once we know that a variable is a delay var,
	 |	then we check whether the delayed term is an
	 |	interval term, and if so, we punch the value
	 | 	we would have bound to the variable into the
	 |	5th slot of the intvl/5 term which will wake
	 |	up and run during the binding interrupt; the
	 |	code for intvl/5 handles the "simulation" of
	 |	the binding by creating appropriate constraints
	 |	which run instead...
	 *-------------------------------------------------*/
int
ck_intvl_punch(r, fv, ft)
	PWord r,fv;
	int ft;
{
	PWord r3, mf;
	int r3t, mfa;

	w_get_argn(&r3, &r3t, r, 4);
	if (r3t == WTP_SYMBOL || r3t == WTP_UIA) return 0;
	w_get_functor(&mf, r3);
	w_get_arity(&mfa, r3);

#ifdef DEBUGSYS	/*..................................................*/
	if (debug_system[INTVBIND]) {
		printf("ck_intvl_punch:mf=%ld TKINTVL=%d mfa = %d\n", mf, TK_INTVL, mfa);
		printf("fv=%lx ft=%d\n",fv, ft);
	}
#endif /* .............................................. DEBUGSYS ..*/

	if ((mf == TK_INTVL) && (mfa == 5)) {
		w_install_argn(r3, 5, fv, ft);
		return 1;
	}
		return 0;
}

#ifdef FREEZE

#ifdef DEBUGFREEZE

	/*-------------------------------------------------*
	 |	Below: TRAIL is the basic trailing macro;
	 |
	 |	VVBIND is a macro for variable-to-variable
	 |	The _SHDW_ arg controls whether VVBIND makes
	 |	calls to SHADOW_REGS/UNSHADOW_REGS (REGS)
	 |	or SHADOW_REGS_S/UNSHADOW_REGS_S (REGS_S)
	 |
	 |  Arg 2 of TRAIL is a line number -- used for
	 |  informational purposes only; eliminated from
	 |  code in non-DEBUGFREEZE case.
	 *-------------------------------------------------*/

	/*-------------------------------------------------*
         | Code (in wam_unify)  where VVBIND is invoked:
	 |   if (f1 < f2) {	;;;; swap f1 and f2
	 |	temp = f1;
	 |	f1 = f2;
	 |	f2 = temp;
	 |   }
	 |	;;;   Guarantee: f1 >= f2 
 	 |
 	 |   if (f2 >= wm_heapbase) {
 	 |	VVBIND(f1, f2, REGS_S, 555);     ;;; f1 >= f2 >= wm_heapbase
 	 |   }
 	 |  else {
 	 |	VVBIND(f2, f1, REGS_S, 666);	;;; f1 >= wm_heapbase > f2
 	 |							;;;	because can't bind two stack
 	 |							;;;	locations together
	 |  }
	 *-------------------------------------------------*/
#ifdef TRAILVALS

/*
#define BIND(r,f)     { TRAIL(r,0); *(r) = PWORD(f); }

#define BIND3(r,f,w)  { TRAIL(r,w); *(r) = PWORD(f); }
*/

#define BIND(r,f)     { BIND3(r,f,0) }

#define BIND3(r,f,w)  { 								\
  if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 		\
	if ( CHK_DELAY(r) ) { 								\
		PWord fv; int ft;								\
		printf("!!@@@ [ln=%d] r=%x  mr_TR=%x\n",			\
						w,(int)r,(int)mr_TR-1);			\
	  	*(PWord *)--mr_TR = PWORD(r); 					\
	  	*(PWord *)--mr_TR = *PWPTR(r);					\
  		*((PWord *)r +1) = (PWord)((PWord *)r + 1);		\
		w_get(&fv, &ft, (PWord)f); 							\
/*		ck_intvl_punch((PWord)(((PWord *)r)-1), fv, ft);		\   */
		if (!ck_intvl_punch((PWord)(((PWord *)r)-1), fv, ft)) {		\
      		*(r) = PWORD(f);							\
		}												\
		printf("setting interrupt\n");					\
	    wm_safety = -2; wm_interrupt_caught = 3; 		\
	} else {											\
	  *(PWord *)--mr_TR = PWORD(r); 					\
	  *(PWord *)--mr_TR = *PWPTR(r);					\
      *(r) = PWORD(f);									\
	}													\
  } else {												\
      *(r) = PWORD(f);									\
  }														\
}

#define TRAIL(r,l) 														  \
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 					  \
	  *(PWord *)--mr_TR = PWORD(r); 									  \
	  *(PWord *)--mr_TR = *PWPTR(r);									  \
	  if ( CHK_DELAY(r) ) { 											  \
			printf(">>@@@ [ln=%d] r=%x  mr_TR=%x\n",l,(int)r,(int)mr_TR-1); \
  			*((PWord *)r +1) = (PWord)((PWord *)r + 1); 				  \
	    	wm_safety = -2; wm_interrupt_caught = 3; } } } 

#define VVBIND(r,f,_SHDW_,lnn)   { 				  				\
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 			\
	  *(PWord *)--mr_TR = PWORD(r); 							\
	  *(PWord *)--mr_TR = *PWPTR(r);							\
	  if ( CHK_DELAY(r) ) { 									\
			DDVVB("vvbx",lnn,r,f)                               \
  			*((PWord *)r +1) = (PWord)((PWord *)r +1); 			\
	  		if ( CHK_DELAY(f) && r != f ) { 					\
				 MMTCH(f)                                       \
  				*((PWord *)f + 1) = (PWord)((PWord *)f + 1); 	\
			    UNSHADOW_##_SHDW_;								\
				combin_dels((PWord)r,(PWord)f); 				\
				SHADOW_##_SHDW_; 								\
  				*(r) = PWORD(f);								\
			} else { /* r >= wm_heapbase; f not delay */		\
				*(f) = PWORD(r);								\
	  			*(PWord *)--mr_TR = PWORD(f); 					\
	  			*(PWord *)--mr_TR = *PWPTR(f);					\
			}													\
		} else {												\
  			*(r) = PWORD(f);									\
		}														\
	} else {													\
  		*(r) = PWORD(f);										\
  }																\
  };            												\
}

#else /* NO TRAILVALS */

#define BIND(r,f)     { BIND3(r,f,0) }

#define BIND3(r,f,w)  { 								\
  if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 		\
	if ( CHK_DELAY(r) ) { 								\
		PWord fv; int ft;								\
		printf("++@@@ [ln=%d] r=%x  mr_TR=%x\n",			\
						w,(int)r,(int)mr_TR-1);			\
	  	*(PWord *)--mr_TR = PWORD(r); 					\
	  	*(PWord *)--mr_TR = *PWPTR(r);					\
  		*((PWord *)r +1) = (PWord)((PWord *)r + 1);		\
		w_get(&fv, &ft, (PWord)f); 							\
/*		ck_intvl_punch((PWord)(((PWord *)r)-1), fv, ft);		\   */
		if (!ck_intvl_punch((PWord)(((PWord *)r)-1), fv, ft)) {		\
      		*(r) = PWORD(f);							\
		}												\
		printf("setting interrupt\n");					\
	    wm_safety = -2; wm_interrupt_caught = 3; 		\
	} else {											\
	  *(PWord *)--mr_TR = PWORD(r); 					\
	  *(PWord *)--mr_TR = *PWPTR(r);					\
      *(r) = PWORD(f);									\
	}													\
  } else {												\
      *(r) = PWORD(f);									\
  }														\
}

#define TRAIL(r,l) 															\
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 						\
	  *--mr_TR = PWORD(r);												 	\
	  if ( CHK_DELAY(r) ) { 												\
			printf("##@@@ [ln=%d] r=%x  mr_TR=%x\n",l,(int)r,(int)mr_TR-1);	\
			*((PWord *)r +1) = (PWord)((PWord *)r + 1);						\
	    	wm_safety = -2; wm_interrupt_caught = 3; } } } 

#define VVBIND(r,f,_SHDW_,lnn)   { 				  				\
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 			\
	  *(PWord *)--mr_TR = PWORD(r); 							\
	  if ( CHK_DELAY(r) ) { 									\
			DDVVB("vvbx",lnn,r,f)                               \
  			*((PWord *)r +1) = (PWord)((PWord *)r +1); 			\
	  		if ( CHK_DELAY(f) && r != f ) { 					\
				 MMTCH(f)                                       \
  				*((PWord *)f + 1) = (PWord)((PWord *)f + 1); 	\
			    UNSHADOW_##_SHDW_;								\
				combin_dels((PWord)r,(PWord)f); 				\
				SHADOW_##_SHDW_; 								\
  				*(r) = PWORD(f);								\
			} else { /* r >= wm_heapbase; f not delay */		\
				*(f) = PWORD(r);								\
	  			*(PWord *)--mr_TR = PWORD(f); 					\
	  			*(PWord *)--mr_TR = *PWPTR(f);					\
			}													\
		} else {												\
  			*(r) = PWORD(f);									\
		}														\
  *(r) = PWORD(f);									\
	} else {													\
  		*(r) = PWORD(f);										\
  }																\
  };            												\
}
#endif /* TRAILVALS */

#else /*=== no-DEBUGFREEZE ===*/

#ifdef TRAILVALS

#define BIND(r,f)     { BIND3(r,f,0) }

#define BIND3(r,f,w)  { 								\
  if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 		\
	if ( CHK_DELAY(r) ) { 								\
		PWord fv; int ft;								\
	  	*(PWord *)--mr_TR = PWORD(r); 					\
	  	*(PWord *)--mr_TR = *PWPTR(r);					\
  		*((PWord *)r +1) = (PWord)((PWord *)r + 1);		\
		w_get(&fv, &ft, (PWord)f); 							\
/*		ck_intvl_punch((PWord)(((PWord *)r)-1), fv, ft);		   */ \
		if (!ck_intvl_punch((PWord)(((PWord *)r)-1), fv, ft)) {		\
      		*(r) = PWORD(f);							\
		}												\
	    wm_safety = -2; wm_interrupt_caught = 3; 		\
	} else {											\
	  *(PWord *)--mr_TR = PWORD(r); 					\
	  *(PWord *)--mr_TR = *PWPTR(r);					\
      *(r) = PWORD(f);									\
	}													\
  } else {												\
      *(r) = PWORD(f);									\
  }														\
}

#define TRAIL(r,l) 											\
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 		\
	  *(mr_TR-2) = *r; 										\
	  *--mr_TR = PWORD(r); 									\
	  mr_TR -= 1;											\
	  if ( CHK_DELAY(r) ) { 								\
  			*((PWord *)r +1) = (PWord)((PWord *)r + 1); 	\
			wm_safety = -2; wm_interrupt_caught = 3; } } }

#define VVBIND(r,f,_SHDW_,lnn)   { 				  				\
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 			\
	  *(PWord *)--mr_TR = PWORD(r); 							\
	  *(PWord *)--mr_TR = *PWPTR(r);							\
	  if ( CHK_DELAY(r) ) { 									\
  			*((PWord *)r +1) = (PWord)((PWord *)r +1); 			\
	  		if ( CHK_DELAY(f) && r != f ) { 					\
  				*((PWord *)f + 1) = (PWord)((PWord *)f + 1); 	\
			    UNSHADOW_##_SHDW_;								\
				combin_dels((PWord)r,(PWord)f); 				\
				SHADOW_##_SHDW_; 								\
  				*(r) = PWORD(f);								\
			} else { /* r >= wm_heapbase; f not delay */		\
				*(f) = PWORD(r);								\
	  			*(PWord *)--mr_TR = PWORD(f); 					\
	  			*(PWord *)--mr_TR = *PWPTR(f);					\
			}													\
		} else {												\
  			*(r) = PWORD(f);									\
		}														\
	} else {													\
  		*(r) = PWORD(f);										\
  }																\
  };            												\
}

#else /* =========== NO TRAILVALS =========== */

#define BIND(r,f)     { BIND3(r,f,0) }

#define BIND3(r,f,w)  { 								\
  if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 		\
	if ( CHK_DELAY(r) ) { 								\
		PWord fv; int ft;								\
/*		printf("^^@@@ [ln=%d] r=%x  mr_TR=%x\n",		\
						w,(int)r,(int)mr_TR-1);		*/ 	\
	  	*(PWord *)--mr_TR = PWORD(r); 					\
  		*((PWord *)r +1) = (PWord)((PWord *)r + 1);		\
		w_get(&fv, &ft, (PWord)f); 						\
		if (!ck_intvl_punch((PWord)(((PWord *)r)-1), fv, ft)) {		\
      		*(r) = PWORD(f);							\
		}												\
/*		printf("setting interrupt\n");		*/			\
	    wm_safety = -2; wm_interrupt_caught = 3; 		\
	} else {											\
	  *(PWord *)--mr_TR = PWORD(r); 					\
      *(r) = PWORD(f);									\
	}													\
  } else {												\
      *(r) = PWORD(f);									\
  }														\
}
#define TRAIL(r,l) 											\
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) {			\
	  *--mr_TR = PWORD(r);									\
	  if ( CHK_DELAY(r) ) {									\
			*((PWord *)r +1) = MMK_VAR((PWord *)r +1);		\
			wm_safety = -2; wm_interrupt_caught = 3; } } }

#define VVBIND(r,f,_SHDW_,lnn)   { 				  				\
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 			\
	  *(PWord *)--mr_TR = PWORD(r); 							\
	  if ( CHK_DELAY(r) ) { 									\
  			*((PWord *)r +1) = (PWord)((PWord *)r +1); 			\
	  		if ( CHK_DELAY(f) && r != f ) { 					\
  				*((PWord *)f + 1) = (PWord)((PWord *)f + 1); 	\
			    UNSHADOW_##_SHDW_;								\
				combin_dels((PWord)r,(PWord)f); 				\
				SHADOW_##_SHDW_; 								\
  				*(r) = PWORD(f);								\
			} else { /* r >= wm_heapbase; f not delay */		\
				*(f) = PWORD(r);								\
	  			*(PWord *)--mr_TR = PWORD(f); 					\
			}													\
		} else {												\
  			*(r) = PWORD(f);									\
		}														\
	} else {													\
  		*(r) = PWORD(f);										\
  }																\
  };            												\
}

#endif /* =========== TRAILVALS =========== */

#endif /*=== DEBUGFREEZE ===*/

#else /*===== no-FREEZE =====*/

#define TRAIL(r,l) \
  { if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) \
	  *--mr_TR = PWORD(r); }

#define VVBIND(r,f,_IGNORE_,lln)   { TRAIL(r,0); *(r) = PWORD(f); }

#define BIND(r,f)     { BIND3(r,f,0) }

#define BIND3(r,f,w)  { 								\
  if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) { 		\
	  *(PWord *)--mr_TR = PWORD(r); 					\
  } \
  *(r) = PWORD(f);										\
}
#endif /*===== FREEZE =====*/

#define DEREF(v)  										\
  { while( M_ISVAR(PWORD(v)) && (v) != *(PWord **)(v))	\
	  (v) = *(PWord **)(v); }

/*----------------
    register PWord *reg1 = NULL;
 *----------------*/

#ifdef TRAILVALS

#define UNWINDTRAIL 									\
			for (reg1 = mr_TR+1; reg1 < mr_B; reg1+=2)	\
				{ *PWPTR(*reg1) = *(((PWord *)reg1)-1);}

#else

#define UNWINDTRAIL 									\
			for (reg1 = mr_TR; reg1 < mr_B; reg1++)		\
				{ *PWPTR(*reg1) = MMK_VAR(*reg1);}
#endif

/*---------------------------------------------------------------*
 * WAM Instruction and Data fields
 * Note : The instruction pointer (P) is not updated
 * until completion of the instruction. Therefore, arguments
 * are extracted as offsets from the start of the instruction.
 *---------------------------------------------------------------*/

#define OPSIZE          1
#define DATASIZE        (sizeof(PWord)/sizeof(Code))
#define REGSIZE         (1+DATASIZE)
#define GCMAGICSIZE     (1+DATASIZE)

#define getpwrd(d)   *PWPTR(P+(d))
#define getsym(d)    *PWPTR(P+(d))
#define getaddr(d)   *(Code **)(P+(d))
#define getreg(d)    ((*(P+(d))? mr_E: mr_SP)+ getpwrd((d)+OPSIZE))

/*---------------------------------------------------------------*
 * Macros for accessing Arg/Env Stack
 *---------------------------------------------------------------*/

#define arg(e,n)    *(PWord *)((e)+(n)+1)
#define env_E(e) 	*(PWord **)(e)
#define env_CP(e) 	*(Code **)((e) + 1)

/*---------------------------------------------------------------*
 | Macros for accessing choicepoints on CP/Trail stack:
 |
 | cp_B has the pointer to PREVIOUS choice point
 | cp_SPB has the CURRENT stack backtrack point
 | cp_HB has the CURRENT heap backtrack point
 | cp_NC has the NEXT CLAUSE address
 *---------------------------------------------------------------*/

#define cp_B(b) 	*(PWord **)((b) + 3)
#define cp_SPB(b) 	*(PWord **)((b) + 2)
#define cp_HB(b) 	*(PWord **)((b) + 1)
#define cp_NC(b) 	*(Code **)(b)
#define cp_SPBm(b)  PWPTR(*((b)+2) & ~3L)

/*-----------------------
 | Macro-instructions
 *----------------------*/

#define DOFAIL	{P = cp_NC(mr_B) ; DISPATCH;}

#define UNIMPLEMENTED 								\
  fprintf(stderr,"Instruction not implemented.\n"); \
  return;

#define UNIFY(f1,f2)                 \
  DEREF(f1);                         \
  DEREF(f2);                         \
  if( f1 == f2 ) DISPATCH;           \
  if(M_ISVAR(f1)){                   \
	if( M_ISVAR(f2)){                \
      if( f1 < f2 ){                 \
	    if( f1 >= wm_heapbase )      \
	       VVBIND(f2,f1,REGS,111)	 \
	    else                         \
	       VVBIND(f1,f2,REGS,222)	 \
      }else{                         \
	    if( f2 >= wm_heapbase )      \
	       VVBIND(f1,f2,REGS,333)	 \
	    else                         \
	       VVBIND(f2,f1,REGS,444)	 \
      }                              \
      DISPATCH;                      \
    }                                \
    BIND(f1,f2)                      \
    DISPATCH;                        \
  }                                  \
  if( M_ISVAR(f2) ){                 \
    BIND(f2,f1);                     \
    DISPATCH;                        \
  }                                  \
  gr_TR = mr_TR; gr_HB = mr_HB;      \
  gr_SPB = mr_SPB;                   \
  if( wam_unify(f1,f2) ){            \
    mr_TR = gr_TR;                   \
    DISPATCH;                        \
  }                                  \
  mr_TR = gr_TR;                     \
  DOFAIL;

/*--------------------------------*
 | Offsets into name table
 *--------------------------------*/

#define MOD_CLOSURE_BDISP NTBL_ENTRYSIZE
#define RESOLVE_REF_BDISP NTBL_ENTRYSIZE
#define OVFLOW_CHECK_BDISP (NTBL_ENTRYSIZE-NTBL_EXECENTRYSIZE)

#define SHADOW_REGS   									\
      mr_E = wm_E; mr_SP = wm_SP; mr_SPB = wm_SPB; 		\
      mr_B = wm_B; mr_TR = wm_TR; mr_H = wm_H;  mr_HB = wm_HB;

#define UNSHADOW_REGS  									\
      wm_E = mr_E; wm_SP = mr_SP; wm_SPB = mr_SPB; 		\
      wm_B = mr_B; wm_TR = mr_TR; wm_H = mr_H; wm_HB = mr_HB;

#define SHADOW_REGS_S   								\
      mr_SPB = wm_SPB; mr_TR = wm_TR; mr_HB = wm_HB;

#define UNSHADOW_REGS_S  								\
      wm_SPB = mr_SPB; wm_TR = mr_TR; wm_HB = mr_HB;

/*-----------------------------------------------------*
 | Initialize wam data areas (called from main)
 *-----------------------------------------------------*/

void
wam_init()
{
    struct {
	CodePtr patchaddr;
	long  argenvsize;
	long  argmask;
    } magic_info[8];

    int   magic_idx = 0;
    register int i;
    PWord *try_patchaddr;

#define PUT_MAGIC(mask,nargs,envsize) 							\
    magic_info[magic_idx].patchaddr = ic_ptr; 					\
	magic_info[magic_idx].argenvsize = (envsize << 16) | nargs;	\
    magic_info[magic_idx].argmask = mask;   					\
	magic_idx++; 												\
    ic_puti( W_GCMAGIC ); 										\
    ic_putl( 0 );

    run_wam((Code *) 0);	/* initialize wam_instrs */

    ic_ptr = (Code *) (w_alloccode(64) + WCI_CLAUSECODE);

    wm_fail = ic_ptr;
    ic_puti(W_FAIL);

    wm_trust_fail = ic_ptr;
    ic_put_align(W_TRUST_ME);
    ic_putl((PWord)wm_fail);

    wm_return_success = ic_ptr;
    PUT_MAGIC(0, 0, 0);
    ic_puti(W_RETURN);
    ic_putl((PWord)1);

    wm_special = ic_ptr;
    PUT_MAGIC(0xf, 3, 0);
    ic_puti(W_SPECIAL);

    wm_rungoal_code = ic_ptr;
    ic_puti(W_WAM_START1);
    ic_putl((PWord)wm_return_success);
    ic_put_align(W_TRY_ME);
    try_patchaddr = (PWord *) ic_ptr;
    ic_putl(0);
    ic_puti(W_ADDTOSP);		/* make 2 arguments */
    ic_putl(-2);
    ic_puti(W_P_SYM);		/* arg1=modid ; w_p_sym SP(0),modid */
    ic_put_reg(1, 0);
    rungoal_modpatch = (PWord *) ic_ptr;
    ic_putl(0);				/* modid to be filled in later */
    ic_puti(W_P_SYM);		/* arg2=goal ; w_p_sym SP(1),goal */
    ic_put_reg(1, 1);
    rungoal_goalpatch = (PWord *) ic_ptr;
    ic_putl(0);				/* goal struct to be filled in later */
    ic_puti(W_WAM_START1);
    ic_putl((PWord)(ic_ptr + DATASIZE + OPSIZE));	
    						/* this should point after w_colon */
    ic_puti(W_COLON);
    PUT_MAGIC(0, 0, 0);
    ic_puti(W_PROCEED);
    *try_patchaddr = PWORD(ic_ptr);	/* try should point to trust */
    ic_put_align(W_TRUST_ME);
    ic_putl((PWord)(ic_ptr + DATASIZE));
    ic_puti(W_RETURN);
    ic_putl((PWord)0);

    /* code to execute on panic */

    wm_panic = ic_ptr;
    ic_puti(W_PANIC);

    ic_putl(-1); /* mark end of code, begin of gc info */

    /* lay down the magic values */

    for (i = 0; i < magic_idx; i++) {
	*((long *) (magic_info[i].patchaddr + 1)) =
	    ic_ptr - magic_info[i].patchaddr;
	ic_putl(magic_info[i].argenvsize);
	ic_putl(magic_info[i].argmask);
    }
    ic_putl(-1);
    ic_putl(ic_ptr - wm_fail);
}

/*---------------------------------------------------------*
 | function invoked by rungoal (called from winter.c)
 *---------------------------------------------------------*/

int
wm_rungoal(a1, a2)		/* module, goal */
    PWord a1, a2;
{
    *rungoal_modpatch = a1;
    *rungoal_goalpatch = a2;

    return (run_wam(wm_rungoal_code));
}

/*-----------------------------------------------------------------*
 | run the wam from a given start address (called from wintcode.c)
 *-----------------------------------------------------------------*/

void
wm_exec(startaddr)
    Code *startaddr;
{
    (void) run_wam(startaddr);
}

/*---------------------------------------------------------------*
 | run_wam is the wam interpreter. It is called by
 | wm_rungoal to run a Prolog goal from foreign interface
 | or by wm_exec to run a regular Prolog query.
 *---------------------------------------------------------------*/

int
run_wam(startaddr)
    Code *startaddr;		/* pointer to clause to execute wam on */
{
    register Code *P = startaddr;
    /* register */ PWord *S = NULL;
    /* register */ PWord *reg1 = NULL;
    register PWord n;		/* used for both counter and deref temp */

	/*-----------------------------------------------------*
 	 | wamreg.h has register declarations that try to
	 | maximize register usage based on directives given
	 | in config.h. A directive is one of the following C
	 | defined constant ( LargeRegModel, SmallRegModel,
	 | DataRegModel, NoRegModel)
	 *-----------------------------------------------------*/

#include "wamreg.h"

    register PWord *reg2;
    PWord *E, *oldB;
    PWord *c22dat = NULL;
    Code *CP;
    int   skip_ovflow, retval;

int special_overflow = 0;

    if (startaddr == (Code *) 0) {
#ifdef Threaded
//#define RelativeThreaded 1
#ifdef RelativeThreaded
#define ABMOP(op,p1,p2,p3,p4) wam_instrs[op] = (Code) (&&l##op - &&lW_MOVE_ES_m2_m4);
#else
#define ABMOP(op,p1,p2,p3,p4) wam_instrs[op] = (Code) &&l##op;
#endif
#include "wamops0.h"
#undef ABMOP
#endif /* Threaded */
	return 1;
    }

    mr_E = mr_SP = wm_SP;
    mr_SPB = wm_SPB;
    mr_TR = wm_TR;
    mr_B = wm_B;
    mr_HB = mr_H = wm_H;

#if 0
//    if (++wm_regidx >= 100) {
//	fprintf(stderr, "Too many nested levels\n");
//	als_exit(1);
//    }
#endif

	push_register_stack(&current_engine);
		
#ifdef Threaded
#define CASE(op) l##op DO_PROFILE(op)
#if RelativeThreaded
#define DISPATCH goto *(&&lW_MOVE_ES_m2_m4 + *P)
#else
#define DISPATCH goto **(void **)P
#endif

    DISPATCH;
#else
#define CASE(op) case op DO_PROFILE(op)
#define DISPATCH goto dispatch

dispatch:
#ifdef TRACEBWAM
	tracewam(P);   
#endif
    switch (*P) {
#endif

	    /*--- Environment control instructions ---*/

CASE(W_MOVE_ES_m2_m4): mr_SP[-4] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_m3): mr_SP[-3] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_m2): mr_SP[-2] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_m1): mr_SP[-1] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_p0): mr_SP[ 0] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_p1): mr_SP[ 1] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_p2): mr_SP[ 2] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_p3): mr_SP[ 3] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_p4): mr_SP[ 4] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m2_p5): mr_SP[ 5] = mr_E[-2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_m4): mr_SP[-4] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_m3): mr_SP[-3] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_m2): mr_SP[-2] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_m1): mr_SP[-1] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_p0): mr_SP[ 0] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_p1): mr_SP[ 1] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_p2): mr_SP[ 2] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_p3): mr_SP[ 3] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_p4): mr_SP[ 4] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_m1_p5): mr_SP[ 5] = mr_E[-1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_m4): mr_SP[-4] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_m3): mr_SP[-3] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_m2): mr_SP[-2] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_m1): mr_SP[-1] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_p0): mr_SP[ 0] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_p1): mr_SP[ 1] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_p2): mr_SP[ 2] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_p3): mr_SP[ 3] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_p4): mr_SP[ 4] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p0_p5): mr_SP[ 5] = mr_E[0]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_m4): mr_SP[-4] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_m3): mr_SP[-3] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_m2): mr_SP[-2] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_m1): mr_SP[-1] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_p0): mr_SP[ 0] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_p1): mr_SP[ 1] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_p2): mr_SP[ 2] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_p3): mr_SP[ 3] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_p4): mr_SP[ 4] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p1_p5): mr_SP[ 5] = mr_E[1]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_m4): mr_SP[-4] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_m3): mr_SP[-3] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_m2): mr_SP[-2] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_m1): mr_SP[-1] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_p0): mr_SP[ 0] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_p1): mr_SP[ 1] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_p2): mr_SP[ 2] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_p3): mr_SP[ 3] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_p4): mr_SP[ 4] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p2_p5): mr_SP[ 5] = mr_E[2]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_m4): mr_SP[-4] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_m3): mr_SP[-3] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_m2): mr_SP[-2] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_m1): mr_SP[-1] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_p0): mr_SP[ 0] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_p1): mr_SP[ 1] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_p2): mr_SP[ 2] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_p3): mr_SP[ 3] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_p4): mr_SP[ 4] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p3_p5): mr_SP[ 5] = mr_E[3]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_m4): mr_SP[-4] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_m3): mr_SP[-3] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_m2): mr_SP[-2] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_m1): mr_SP[-1] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_p0): mr_SP[ 0] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_p1): mr_SP[ 1] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_p2): mr_SP[ 2] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_p3): mr_SP[ 3] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_p4): mr_SP[ 4] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p4_p5): mr_SP[ 5] = mr_E[4]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_m4): mr_SP[-4] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_m3): mr_SP[-3] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_m2): mr_SP[-2] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_m1): mr_SP[-1] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_p0): mr_SP[ 0] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_p1): mr_SP[ 1] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_p2): mr_SP[ 2] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_p3): mr_SP[ 3] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_p4): mr_SP[ 4] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p5_p5): mr_SP[ 5] = mr_E[5]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_m4): mr_SP[-4] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_m3): mr_SP[-3] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_m2): mr_SP[-2] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_m1): mr_SP[-1] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_p0): mr_SP[ 0] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_p1): mr_SP[ 1] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_p2): mr_SP[ 2] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_p3): mr_SP[ 3] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_p4): mr_SP[ 4] = mr_E[6]; P+=OPSIZE; DISPATCH;
CASE(W_MOVE_ES_p6_p5): mr_SP[ 5] = mr_E[6]; P+=OPSIZE; DISPATCH;

CASE(W_ADDTOSP):		/* addtoSP n  */
	    mr_SP += getpwrd(OPSIZE);
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_MOVE_ES):		/* move Esrc, Sdst      */
	    *(mr_SP + getpwrd(OPSIZE + DATASIZE)) = *(mr_E + getpwrd(OPSIZE));
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_MOVE_SS):		/* move Ssrc, Sdst      */
	    *(mr_SP + getpwrd(OPSIZE + DATASIZE)) = *(mr_SP + getpwrd(OPSIZE));
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_MOVE_EE):
	    *(mr_E + getpwrd(OPSIZE + DATASIZE)) = *(mr_E + getpwrd(OPSIZE));
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_MOVE_SE):
	    *(mr_E + getpwrd(OPSIZE + DATASIZE)) = *(mr_SP + getpwrd(OPSIZE));
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_ADDTOSP_EXECUTE):
	    mr_SP += getpwrd(OPSIZE);
	    P = (Code *) getpwrd(OPSIZE+DATASIZE);
	    mr_E = mr_SP;
	    DISPATCH;
	    
CASE(W_EXECUTE):		/* execute ntbl_entryptr     */
	    P = (Code *) getpwrd(OPSIZE);
	    mr_E = mr_SP;
	    DISPATCH;

CASE(W_ADDTOSP_CALL):
	    mr_SP += getpwrd(OPSIZE);
	    env_CP(mr_SP) = P + OPSIZE + DATASIZE + DATASIZE;
	    env_E(mr_SP) = mr_E;
	    P = (Code *) getpwrd(OPSIZE+DATASIZE);
	    mr_E = mr_SP;
	    DISPATCH;

CASE(W_CALL):			/* call ntbl_entryptr     */
	    env_CP(mr_SP) = P + OPSIZE + DATASIZE;
	    env_E(mr_SP) = mr_E;
	    P = (Code *) getpwrd(OPSIZE);
	    mr_E = mr_SP;
	    DISPATCH;

CASE(W_DEALLOCATE2):		/* deallocate2 size1,size2,offset */
	    if (mr_SPB > mr_E) {	/* determinate */
		mr_SP = mr_E - getpwrd(OPSIZE + DATASIZE);	/* e - size2 */
		P += OPSIZE + 2 * DATASIZE;	/* skip the non-det code */
		P += *PWPTR(P);
		DISPATCH;
	    }
	    /* non-determinate ? allocate size1 */
	    mr_SP = mr_SPB - getpwrd(OPSIZE);
	    P += OPSIZE + 3 * DATASIZE;
	    DISPATCH;

CASE(W_DEALLOCATE4):		/* deallocate4 size2  */
	    mr_SP = mr_E - getpwrd(OPSIZE);
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_PROCEED):		/* proceed            */
	    P = env_CP(mr_E) + GCMAGICSIZE;
	    mr_E = env_E(mr_E);
	    DISPATCH;

CASE(W_INIT_YVAR1):		/* init_yvar1 Esrc   */
	    reg1 = mr_E + getpwrd(OPSIZE);
	    *reg1 = MMK_VAR(reg1);
	    reg1++;
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_INIT_YVAR2):		/* init_yvar2 incr  */
	    reg1 += getpwrd(OPSIZE);
	    *reg1 = MMK_VAR(reg1);
	    reg1++;
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_TRIM1):			/* trim1 size1+size2  */
	    mr_SP = mr_E - getpwrd(OPSIZE);
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_TRIM2):			/* trim2 size1, size2  */
	    mr_SP = (mr_SPB >= mr_E 
		     ? mr_E - (getpwrd(OPSIZE) + getpwrd(OPSIZE + DATASIZE))
		     : mr_SPB - getpwrd(OPSIZE + DATASIZE));
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

	    /* Garbage collection check, spy check et al. */

CASE(W_LIBBREAK):		/* libbreak interrupt_num */
	    wm_safety = wm_trigger;
	    wm_interrupt_caught = *(P + OPSIZE);
	    P += 2 * OPSIZE;
	    DISPATCH;

CASE(W_DECR_ICOUNT):
	    decr_icount(P + OPSIZE);
	    goto overflow_check;

CASE(W_SPY):			/* spy branchaddr */
	    if (wm_spying)
		wm_safety = wm_trigger;		/* yes, drop into overflow_check */

CASE(W_OVFLOW_CHECK):				/* overflow branchaddr */
overflow_check:
	    reg1 = (PWord *) P;
	    P += OPSIZE;
overflow_check0:
#ifdef MacOS
#if 0
		if ((unsigned long) mr_SP < (unsigned long) wm_stackbot_safety)
			stack_overflow();
#endif	/* DEBUG */
		yield_counter -= 1;
	    if (yield_counter == 0) {
	    	PI_yield_time();
	    	yield_counter = yield_interval;
	    }
#endif	/* MacOS */
	    if (((unsigned long) mr_TR - (unsigned long) mr_H) 
				>= (unsigned long) wm_safety) {
				if (special_overflow) goto special_finish;
			DISPATCH;
	    }


#if 0
		ghit = (ghit+1) % 50;
	    if (ghit == 0 || ((unsigned long) wm_safety <= (unsigned long) wm_normal))
#else
	    if ((unsigned long) wm_safety <= (unsigned long) wm_normal)
#endif
	    {
				/* --------- ORDINARY GC INTERRUPT --------- */

#ifdef DEBUGSYS	/*..................................................*/
								if (debug_system[GCINTR]) 
									printf("-<<Start GC interrupt\n");
#endif /* .............................................. DEBUGSYS ..*/

			UNSHADOW_REGS;
			gc();
			SHADOW_REGS;

#ifdef DEBUGSYS	/*..................................................*/
								if (debug_system[GCINTR]) 
									printf("-<<Return from GC interrupt\n");
#endif /* .............................................. DEBUGSYS ..*/
                if (special_overflow) goto special_finish;
			DISPATCH;
	    } 		/* ----end-- ORDINARY GC INTERRUPT --------- */

	    else {
				/* --------- PROLOG SYSTEM INTERRUPT --------- */
			ntbl_entry *entfrom = 
				(ntbl_entry *) ((Code *)reg1 - OVFLOW_CHECK_BDISP);
			PWord arg3;

#ifdef MacOS
	{
	extern long coop_interupt;
	    if (coop_interupt) {
	    	PI_yield_time();
		wm_safety = wm_normal;	/* reset interrupt */
		if (special_overflow) goto special_finish;
	    	DISPATCH;
	    }
	}
#endif
                special_overflow = 0;
			n = entfrom->nargs;

			wm_safety = wm_normal;	/* reset interrupt */
			CP = env_CP(mr_E);		/* save oldCP and oldE in temporaries */
			E = env_E(mr_E);

				/* set up $interrupt/3 call */
			mr_SP -= 3 - n;
			if (n == 0) {
		    	arg(mr_SP, 3) = entfrom->tokid_arity;
			}
			else {		/* n > 0 */
		    	reg1 = mr_E + 2;	/* copy arguments to make a structure 
					 				 * to represent the inteurrupted call... */
		    	S = mr_H;
		    	*S++ = MMK_FUNCTOR(MFUNCTOR_TOKID(entfrom->tokid_arity), n);
		    	while (n--)
					*S++ = *reg1++;

		    	arg3 = MMK_STRUCTURE(mr_H);		/* ...and push it as third
							 					 * arg of interrupt/3 call */
		    	mr_H = S;

		    		/* take care of unsafe variables in the structure */
		    	n = entfrom->nargs;
		    	while (n--) {
					reg1 = PWPTR(*--S);
					DEREF(reg1);
					if (M_ISVAR(reg1) && reg1 < wm_heapbase) {
						PLAINTRAIL(reg1);   
			    		*reg1 = *S = *mr_H = MMK_VAR(mr_H);
			    		mr_H++;
					}
					else {
			    		*S = PWORD(reg1);
					}
		    	}
		    		arg(mr_SP, 3) = arg3;
			}		/* end if n== 0 */

					/* module as second arg of interrupt/3 call */
			arg(mr_SP, 2) = MMK_SYM(PWORD(entfrom->modid) & 0xffff);
					/* interrupt caught as first arg of interrupt/3 call */
			arg(mr_SP, 1) = MMK_INT(wm_interrupt_caught);
										/* reset int_caught to default: */
			wm_interrupt_caught = 0;	/* default is a source interrupt */

			env_CP(mr_SP) = CP;
			env_E(mr_SP) = E;
			mr_E = mr_SP;
				/* call $interrupt/3 */
			P = wm_overcode;
			DISPATCH;
	    	}	/* ----end-- PROLOG SYSTEM INTERRUPT --------- */
	    goto get_out;

	    /*-------------------------------------------------------------------*
		 | Get Instructions  g_value, g_list, g_struct, g_int, g_sym, g_uia
	     *-------------------------------------------------------------------*/

CASE(W_G_VALUE):		/* get_value arg1, arg2 */
	    reg1 = PWPTR(*getreg(OPSIZE));
	    S = PWPTR(*getreg(OPSIZE + REGSIZE));
	    P += OPSIZE + 2 * REGSIZE;
	    UNIFY(reg1, S);

CASE(W_G_LIST_SP):		/* get_list Sdisp */
	    reg1 = PWPTR(mr_SP[getpwrd(OPSIZE)]);
	    P += OPSIZE + DATASIZE;
	    goto g_list_common;
CASE(W_G_LIST_E):		/* get_list Edisp  */
	    reg1 = PWPTR(mr_E[getpwrd(OPSIZE)]);
	    P += OPSIZE + DATASIZE;
	    goto g_list_common;

CASE(W_G_LIST_E_p5):
	    reg1 = PWPTR(mr_E[5]);
	    P += OPSIZE;
	    goto g_list_common;
CASE(W_G_LIST_E_p4):
	    reg1 = PWPTR(mr_E[4]);
	    P += OPSIZE;
	    goto g_list_common;
CASE(W_G_LIST_E_p3):
	    reg1 = PWPTR(mr_E[3]);
	    P += OPSIZE;
	    goto g_list_common;
CASE(W_G_LIST_E_p2):
	    reg1 = PWPTR(mr_E[2]);
	    P += OPSIZE;
	    goto g_list_common;

g_list_deref:
	    reg1 = PWPTR(n);
g_list_common:
	    if ( (n=MTP_TAG(reg1)) )
			goto g_list_ground;
	    n = *reg1;
	    if (n != PWORD(reg1))
			goto g_list_deref;
	    /* write mode case */
	    BIND3(reg1, MMK_LIST(mr_H),1000001);
	    S = PWPTR(0);	/* set write mode */
	    DISPATCH;
g_list_ground:
	    /* read mode case */
	    if (n == MTP_LIST) {
			S = MLISTADDR(reg1);
			DISPATCH;
	    }
	    DOFAIL;

CASE(W_G_STRUCT_SP):		/* get_list Sdisp */
	    reg1 = PWPTR(mr_SP[getpwrd(OPSIZE)]);
	    goto g_struct_common;
CASE(W_G_STRUCT_E):		/* get_list Edisp  */
	    reg1 = PWPTR(mr_E[getpwrd(OPSIZE)]);
g_struct_common:
	    DEREF(reg1);
	    if (M_ISVAR(reg1)) {
		BIND3(reg1, MMK_STRUCTURE(mr_H),1000002);
		S = PWPTR(0);
		*mr_H++ = getsym(OPSIZE + DATASIZE);
		P += OPSIZE + 2*DATASIZE;
		DISPATCH;
	    }

	    if (!M_ISSTRUCT(reg1))
		DOFAIL;
	    reg1 = MSTRUCTADDR(reg1);
	    if (MFUNCTOR(reg1) != getsym(OPSIZE + DATASIZE))
		DOFAIL;
	    S = reg1 + 1;
	    P += OPSIZE + 2*DATASIZE;
	    DISPATCH;

CASE(W_G_INT):			/* get_int Src,IntSym */
	    reg1 = PWPTR(*getreg(OPSIZE));
	    DEREF(reg1);
	    if (M_ISVAR(reg1)) {
		BIND3(reg1, getsym(OPSIZE + REGSIZE),1000003);
		P += OPSIZE + REGSIZE + DATASIZE;
		DISPATCH;
	    }

	    if (PWORD(reg1) != getsym(OPSIZE + REGSIZE))
		DOFAIL;
	    P += OPSIZE + REGSIZE + DATASIZE;
	    DISPATCH;

CASE(W_G_SYM):			/* get_sym Src,Sym  */
	    reg1 = PWPTR(*getreg(OPSIZE));
	    DEREF(reg1);
	    if (M_ISVAR(reg1)) {
		BIND3(reg1, getsym(OPSIZE + REGSIZE),1000004);
		P += OPSIZE + REGSIZE + DATASIZE;
		DISPATCH;
	    }
	    if (PWORD(reg1) == getsym(OPSIZE + REGSIZE)) {
		P += OPSIZE + REGSIZE + DATASIZE;
		DISPATCH;
	    }
	    if (M_ISUIA(reg1) &&
		!strcmp((char *) TOKNAME(MSYMBOL(getsym(OPSIZE + REGSIZE))),
			(char *) M_FIRSTUIAWORD(MUIA(reg1)))) {
		P += OPSIZE + REGSIZE + DATASIZE;
		DISPATCH;
	    }
	    DOFAIL;

CASE(W_G_UIA):			/* get_uia src, num, fenceduia    */
	    reg1 = PWPTR(*getreg(OPSIZE));
	    n = getpwrd(OPSIZE + REGSIZE);
	    S = PWPTR(P + OPSIZE + REGSIZE + DATASIZE);
	    P += OPSIZE + REGSIZE + (n + 1) * DATASIZE;
	    DEREF(reg1);
	    if (M_ISVAR(reg1)) {
		BIND(reg1, MMK_UIA((char *) mr_H - (char *) wm_heapbase));
		reg1 = mr_H;
		while (n--)
		    *reg1++ = *S++;	/* copy fenceduia to heap */
		mr_H = reg1;
		DISPATCH;
	    }
	    S++;		/* skip the fence */
	    if (M_ISSYM(reg1) && !strcmp((char *) TOKNAME(MSYMBOL(reg1)), (char *) S))
		DISPATCH;
	    if (M_ISUIA(reg1) && !strcmp((char *) M_FIRSTUIAWORD(MUIA(reg1)), (char *) S))
		DISPATCH;
	    DOFAIL;

	    /* Unify instructions - u_void, u_val, u_var, u_int, u_sym,
	     * u_lval
	     */

CASE(W_U_VOID):		/* unify_void    */
	    P += OPSIZE;
	    if (S) {		/* read_mode */
		S++;
		DISPATCH;
	    }
	    *mr_H = MMK_VAR(mr_H);
	    mr_H++;
	    DISPATCH;

CASE(W_U_VAL_E):		/* unify_value Edisp */
	    reg1 = PWPTR(mr_E[getpwrd(OPSIZE)]);
	    P += OPSIZE + DATASIZE;
	    goto u_val_common;

CASE(W_U_VAL_SP):		/* unify_value Sdisp */
	    reg1 = PWPTR(mr_SP[getpwrd(OPSIZE)]);
	    P += OPSIZE + DATASIZE;
	    goto u_val_common;

CASE(W_U_VAL_SP_m4):
	    reg1 = PWPTR(mr_SP[-4]);
	    goto u_val_common0;
CASE(W_U_VAL_SP_m3):
	    reg1 = PWPTR(mr_SP[-3]);
	    goto u_val_common0;
CASE(W_U_VAL_SP_m2):
	    reg1 = PWPTR(mr_SP[-2]);
	    goto u_val_common0;
CASE(W_U_VAL_SP_m1):
	    reg1 = PWPTR(mr_SP[-1]);
	    goto u_val_common0;
CASE(W_U_VAL_SP_p0):
	    reg1 = PWPTR(*mr_SP);
	    goto u_val_common0;
CASE(W_U_VAL_SP_p1):
	    reg1 = PWPTR(mr_SP[1]);
	    goto u_val_common0;
CASE(W_U_VAL_SP_p2):
	    reg1 = PWPTR(mr_SP[2]);
	    goto u_val_common0;
CASE(W_U_VAL_SP_p3):
	    reg1 = PWPTR(mr_SP[3]);
	    goto u_val_common0;
CASE(W_U_VAL_SP_p4):
	    reg1 = PWPTR(mr_SP[4]);
	    /* goto u_val_common0; */

u_val_common0:
	    P += OPSIZE;
u_val_common:
	    if (S) {		/* read-mode */
		reg2 = S++;
		UNIFY(reg1, reg2);
	    }
	    /* write-mode */
	    *mr_H++ = PWORD(reg1);
	    DISPATCH;

CASE(W_U_VAR_E):		/* unify_variable Edisp */
	    reg1 = mr_E + getpwrd(OPSIZE);
	    P += OPSIZE + DATASIZE;
	    goto u_var_common;

CASE(W_U_VAR_SP):
	    reg1 = mr_SP + getpwrd(OPSIZE);
	    P += OPSIZE + DATASIZE;
	    goto u_var_common;

CASE(W_U_VAR_SP_m4):
	    reg1 = mr_SP-4;
	    goto u_var_common0;
CASE(W_U_VAR_SP_m3):
	    reg1 = mr_SP-3;
	    goto u_var_common0;
CASE(W_U_VAR_SP_m2):
	    reg1 = mr_SP-2;
	    goto u_var_common0;
CASE(W_U_VAR_SP_m1):
	    reg1 = mr_SP-1;
	    goto u_var_common0;
CASE(W_U_VAR_SP_p0):
	    reg1 = mr_SP;
	    goto u_var_common0;
CASE(W_U_VAR_SP_p1):
	    reg1 = mr_SP+1;
	    goto u_var_common0;
CASE(W_U_VAR_SP_p2):
	    reg1 = mr_SP+2;
	    goto u_var_common0;
CASE(W_U_VAR_SP_p3):
	    reg1 = mr_SP+3;
	    goto u_var_common0;
CASE(W_U_VAR_SP_p4):
	    reg1 = mr_SP+4;
	    	/* goto u_var_common0; */

u_var_common0:
	    P += OPSIZE;
u_var_common:
	    if (S) {		/* read-mode */
		*reg1 = *S++;
		DISPATCH;
	    }
		TRAIL(reg1,1003);
	    *reg1 = *mr_H = MMK_VAR(mr_H);
	    mr_H++;
	    DISPATCH;

CASE(W_U_VAR_SP_m1_p2):		/* This is the pair that appears in
				   				append/3.  Shameful, isn't it.  */
	    P += OPSIZE;
	    if (S) {
		mr_SP[-1] = *S++;
		mr_SP[2] = *S++;
		DISPATCH;
	    }
	    reg1 = mr_SP-1;
	    TRAIL(reg1,1021);
	    *reg1 = *mr_H = MMK_VAR(mr_H);
	    mr_H++;
	    reg1 = mr_SP+2;
	    TRAIL(reg1,1029);
	    *reg1 = *mr_H = MMK_VAR(mr_H);
	    mr_H++;
	    DISPATCH;


CASE(W_U_INT):			/* unify_int intsym  */
	    if (S) {		/* read-mode */
		reg1 = PWPTR(*S++);
		DEREF(reg1);
		if (M_ISVAR(reg1)) {
		    BIND(reg1, getsym(OPSIZE));
		    P += OPSIZE + DATASIZE;
		    DISPATCH;
		}
		if (PWORD(reg1) == getsym(OPSIZE)) {
		    P += OPSIZE + DATASIZE;
		    DISPATCH;
		}
		DOFAIL;
	    }
	    /* write-mode */
	    *mr_H++ = getsym(OPSIZE);
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_U_SYM):			/* unify_sym sym */
	    if (S) {
		reg1 = PWPTR(*S++);
		DEREF(reg1);
		if (M_ISVAR(reg1)) {
		    BIND(reg1, getsym(OPSIZE));
		    P += OPSIZE + DATASIZE;
		    DISPATCH;
		}
		if (PWORD(reg1) == getsym(OPSIZE)) {
		    P += OPSIZE + DATASIZE;
		    DISPATCH;
		}
		if (M_ISUIA(reg1) &&
		    !strcmp((char *) TOKNAME(MSYMBOL(getsym(OPSIZE))),
			    (char *) M_FIRSTUIAWORD(MUIA(reg1)))) {
		    P += OPSIZE + DATASIZE;
		    DISPATCH;
		}
		DOFAIL;
	    }

	    *mr_H++ = getsym(OPSIZE);
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_U_LVAL):		/* unify_local_value src */
	    reg1 = PWPTR(*getreg(OPSIZE));
	    if (S) {
		reg2 = PWPTR(*S++);
		P += OPSIZE + REGSIZE;
		UNIFY(reg1, reg2);	/* unify does not return */
	    }
	    P += OPSIZE + REGSIZE;
	    DEREF(reg1);
	    if (M_ISVAR(reg1) && reg1 < wm_heapbase) {
		TRAIL(reg1,1095);
		*mr_H = *reg1 = MMK_VAR(mr_H);
		mr_H++;
		DISPATCH;
	    }

	    *mr_H++ = PWORD(reg1);
	    DISPATCH;

	    /* Put Instructions - p_unsafe, p_sym, p_uia, p_yvar, p_xvar,
	     * p_list, p_struct
	     */

CASE(W_P_UNSAFE):		/* put_unsafe_value src, dst  */
	    reg1 = PWPTR(*getreg(OPSIZE));
	    DEREF(reg1);
	    if (M_ISVAR(reg1) && reg1 < mr_SPB) {
		TRAIL(reg1,1116);
#if CW_INTEL_REGISTER_COLOR_BUG
{
	PWord *temp;
	
	temp = getreg(OPSIZE + REGSIZE);
	*mr_H = MMK_VAR(mr_H);
	*reg1 = *mr_H;
	*temp = *reg1;
}
#else
		*getreg(OPSIZE + REGSIZE) = *reg1 = *mr_H = MMK_VAR(mr_H);
#endif
		mr_H++;
	    }
	    else
		*getreg(OPSIZE + REGSIZE) = PWORD(reg1);
	    P += OPSIZE + 2 * REGSIZE;
	    DISPATCH;

CASE(W_P_SYM):			/* put_sym dst,Sym   */
	    *getreg(OPSIZE) = getsym(OPSIZE + REGSIZE);
	    P += OPSIZE + REGSIZE + DATASIZE;
	    DISPATCH;

CASE(W_P_UIA):			/* put_uia dst, num, fenceduia */
	    *getreg(OPSIZE) = MMK_UIA((char *) mr_H - (char *) wm_heapbase);

	    S = mr_H;
	    n = getpwrd(OPSIZE + REGSIZE);
	    reg1 = PWPTR(P + OPSIZE + REGSIZE + DATASIZE);
	    P += OPSIZE + REGSIZE + DATASIZE + n * DATASIZE;
	    while (n--)
		*S++ = *reg1++;	/* copy fenced uia to heap */
	    mr_H = S;
	    DISPATCH;

CASE(W_P_YVAR_ES):
	    reg1 = mr_E+getpwrd(OPSIZE);
	    *(mr_SP + getpwrd(OPSIZE+DATASIZE)) = *reg1 = MMK_VAR(reg1);
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_P_YVAR_EE):
	    reg1 = mr_E+getpwrd(OPSIZE);
	    *(mr_E + getpwrd(OPSIZE+DATASIZE)) = *reg1 = MMK_VAR(reg1);
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_P_YVAR_SS):
	    reg1 = mr_SP+getpwrd(OPSIZE);
	    *(mr_SP + getpwrd(OPSIZE+DATASIZE)) = *reg1 = MMK_VAR(reg1);
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_P_YVAR_SE):
	    reg1 = mr_SP+getpwrd(OPSIZE);
	    *(mr_E + getpwrd(OPSIZE+DATASIZE)) = *reg1 = MMK_VAR(reg1);
	    P += OPSIZE + 2 * DATASIZE;
	    DISPATCH;

CASE(W_P_XVAR):		/* put_xvar dst      */
#if CW_INTEL_REGISTER_COLOR_BUG
{
	PWord *temp;
	
	temp = getreg(OPSIZE);
	*mr_H = MMK_VAR(mr_H);
	*temp = *mr_H;
}
#else
	    *getreg(OPSIZE) = *mr_H = MMK_VAR(mr_H);
#endif
	    mr_H++;
	    P += OPSIZE + REGSIZE;
	    DISPATCH;

CASE(W_P_LIST):		/* put_list dst      */
	    *getreg(OPSIZE) = MMK_LIST(mr_H);
	    S = PWPTR(0);	/* set write mode    */
	    P += OPSIZE + REGSIZE;
	    DISPATCH;

CASE(W_P_STRUCTURE):		/* put_struct dst,f/a    */
	    *getreg(OPSIZE) = MMK_STRUCTURE(mr_H);
	    *mr_H++ = getsym(OPSIZE + REGSIZE);
	    S = PWPTR(0);
	    P += OPSIZE + REGSIZE + DATASIZE;
	    DISPATCH;

	    /* Branching and indexing instructions     */

CASE(W_JUMP):			/* jump dst */
	    P = getaddr(OPSIZE);
	    DISPATCH;

CASE(W_OVJUMP):
	    reg1 = P;
	    P = getaddr(OPSIZE+OPSIZE);
	    goto overflow_check0;

CASE(W_FOREIGN_JUMP):		/* foreign_jump dst */
	    UNSHADOW_REGS;
	    if (!((*(int (*)(void)) *PWPTR(P + OPSIZE)) ())) {
		SHADOW_REGS;
		DOFAIL;
	    }
	    SHADOW_REGS;
	    P = env_CP(mr_E) + GCMAGICSIZE;
	    mr_E = env_E(mr_E);
	    DISPATCH;

CASE(W_SW_TERM):		/* sw_term varaddr,straddr,lisaddr,conaddr */
	    S = PWPTR(arg(mr_E, 1));
	    DEREF(S);
	    P = getaddr(OPSIZE + MTP_TAG(S) * DATASIZE);
	    S = MLISTADDR(S);	/* needed for when we skip get_list */
	    DISPATCH;

CASE(W_OVSW_TERM):		/* ovsw_term sw_term varaddr,straddr,lisaddr,conaddr */
		special_overflow = 1;
	    reg1 = (PWord *) P;
		goto overflow_check0;
special_finish:
		special_overflow = 0;
	    S = PWPTR(arg(mr_E, 1));
	    DEREF(S);
	    reg1 = (PWord *) P;
	    P = getaddr(OPSIZE + OPSIZE + MTP_TAG(S) * DATASIZE);
	    S = MLISTADDR(S);	/* needed for when we skip get_list */
/*	    goto overflow_check0;	*/
		DISPATCH;

CASE(W_SW_STRUCT):		/* sw_struct [align],nentries,table */
	    reg1 = PWPTR(arg(mr_E, 1));
	    DEREF(reg1);
	    reg1 = PWPTR(MFUNCTOR(MSTRUCTADDR(reg1)));
	    goto sw_common;

CASE(W_SW_CONST):		/* sw_const [align],nentries,table(key,addr) */
	    reg1 = PWPTR(arg(mr_E, 1));
	    DEREF(reg1);
	    if (M_ISUIA(reg1)) {
		if ((reg1 = PWPTR(probe_token((UCHAR *)M_FIRSTUIAWORD(MUIA(reg1))))) == PWPTR(0))
		    DOFAIL;
		reg1 = PWPTR(MMK_SYM(reg1));
	    }

sw_common:

	    n = getpwrd(DATASIZE);	/* no. of entries */
	    {
		register long *low = (long *) (P + 2 * DATASIZE);
		register long *high = (long *) (P + 2 * DATASIZE) + 2 * (n - 1);
		register long *mid;

		while (low <= high) {
		    mid = low + (long) (high - low) / 2;
		    if (((high - low) / 2) % 2)
			mid++;

		    if (*mid == (long) reg1) {
			P = (Code *) * (mid + 1);
			DISPATCH;
		    }
		    if ((unsigned long) *mid < (unsigned long) reg1) {
			low = mid + 2;
		    }
		    else {
			high = mid - 2;
		    }
		}
		DOFAIL;
	    }

CASE(W_TRY_ME):					/* try_me_else [align],addr    */
	    mr_TR -= 4;				/* create a choice point */
	    cp_B(mr_TR) = mr_B;
	    cp_SPB(mr_TR) = mr_SPB = mr_SP;
	    cp_HB(mr_TR) = mr_HB = mr_H;
	    cp_NC(mr_TR) = getaddr(DATASIZE);	/* save addr as next clause addr */

	    mr_B = mr_TR;
	    P += 2 * DATASIZE;
	    DISPATCH;

CASE(W_TRY_ME_JUMP):
	    mr_TR -= 4;				/* create a choice point */
	    cp_B(mr_TR) = mr_B;
	    cp_SPB(mr_TR) = mr_SPB = mr_SP;
	    cp_HB(mr_TR) = mr_HB = mr_H;
	    cp_NC(mr_TR) = getaddr(DATASIZE);	/* save addr as next clause addr */

	    P = getaddr(2*DATASIZE);
	    mr_B = mr_TR;
	    DISPATCH;

CASE(W_RETRY_ME):				/* retry_me_else  [align],L */
		/* unwind trail */
	    UNWINDTRAIL;
	    mr_TR = mr_B;			/* reset trail */
	    mr_H = mr_HB;
	    mr_SP = mr_E = mr_SPB;
	    cp_NC(mr_B) = getaddr(DATASIZE);	/* modify nextclause addr in choice pt */
	    P += 2 * DATASIZE;
	    DISPATCH;

CASE(W_TRUST_ME):		/* trust_me_else  [align],fail  */
		/* unwind trail */
	    UNWINDTRAIL;
	    mr_H = mr_HB;
	    mr_E = mr_SP = mr_SPB;
	    mr_TR = mr_B + 4;	/* remove chioce pt  */
	    mr_B = cp_B(mr_B);
	    mr_SPB = cp_SPBm(mr_B);
	    mr_HB = cp_HB(mr_B);
	    P = (Code *) getpwrd(DATASIZE);
	    DISPATCH;

CASE(W_TRY):			/* try  [align],addr      */
	    mr_TR -= 4;		/* create a choice point */
	    cp_B(mr_TR) = mr_B;
	    cp_SPB(mr_TR) = mr_SPB = mr_SP;
	    cp_HB(mr_TR) = mr_HB = mr_H;
	    cp_NC(mr_TR) = P + 2 * DATASIZE;

	    mr_B = mr_TR;
	    P = getaddr(DATASIZE);
	    DISPATCH;

CASE(W_RETRY):			/* retry [align],addr        */
		/* unwind trail */
	    UNWINDTRAIL;
	    mr_TR = mr_B;
	    mr_H = mr_HB;
	    mr_SP = mr_E = mr_SPB;
	    cp_NC(mr_B) = P + 2 * DATASIZE;
	    P = getaddr(DATASIZE);	/* branch to addr */
	    DISPATCH;

CASE(W_TRUST):			/* trust   [align],addr */
		/* unwind trail */
	    UNWINDTRAIL;
	    mr_H = mr_HB;
	    mr_E = mr_SP = mr_SPB;
	    mr_TR = mr_B + 4;	/* remove choice pt */
	    mr_B = cp_B(mr_B);
	    mr_SPB = cp_SPBm(mr_B);
	    mr_HB = cp_HB(mr_B);
	    P = getaddr(DATASIZE);
	    DISPATCH;

CASE(W_NCIADC):		/* next_choice_in_a_deleted_clause */
#ifdef CodeGC
	    P = (Code *) next_choice_in_a_deleted_clause((long *) P);
	    DISPATCH;
#else
	    printf("wam.c: NCIADC not expected without CodeGC\nexiting...\n");
	    exit(1);
#endif /* CodeGC */

CASE(W_FAIL):
	    DOFAIL;

		/* ----------------------------------------------------------*/
	    /* Cut instructions - macro_cutproceed, cut_proceed,  docut  */
		/* ----------------------------------------------------------*/

CASE(W_MACRO_CUTPROCEED):	/* macro_cutproceed cutpt is arg1 */
	    reg1 = PWPTR(wm_heapbase - MINTEGER(arg(mr_E, 1)));
	    P = env_CP(mr_E);
	    mr_E = env_E(mr_E);
	    goto cut_no_ovflow_check;

CASE(W_CUT_PROCEED):		/* [deallocate]cut_proceed (eg. foo ):- !.  */
	    reg1 = mr_E;
	    P = env_CP(mr_E);
	    mr_E = env_E(mr_E);
	    goto cut;

CASE(W_DOCUT):			/* do_cut        */
	    reg1 = mr_E;
	    P += OPSIZE;

cut:
				/* Check if this is a Prolog interrupt: */
	    if (wm_safety < 0) {	/* Prolog interrupt */
		wm_safety = wm_normal;	/* reset interrupt */

			/* set the $interrupt/3 call */
		mr_SP -= 7;
		arg(mr_SP, 5) = PWORD(P);
		arg(mr_SP, 4) = PWORD(mr_E);		/* fake call */
		arg(mr_SP, 3) = MMK_STRUCTURE(mr_H);
		*mr_H++ = MMK_FUNCTOR(PWORD(find_token((UCHAR *)"!")), 1);
		*mr_H++ = MMK_INT(wm_heapbase - reg1);
		arg(mr_SP, 2) = MMK_SYM(MODULE_BUILTINS);
		arg(mr_SP, 1) = MMK_INT(wm_interrupt_caught);
		wm_interrupt_caught = 0;			/* default is source interrupt */
		env_CP(mr_SP) = wm_special;
		env_E(mr_SP) = mr_SP + 5;
		mr_E = mr_SP;
		P = wm_overcode;
		DISPATCH;
	    }

cut_no_ovflow_check:
						/* cutpt is in arg1 */
	    P += GCMAGICSIZE;

			/* If SPB (Stack pointer backtrack point) is
			   already above the cut point (reg1), nothing
			   to do
			 */
	    if (mr_SPB > reg1)
			DISPATCH;

			/* If SPB is =< cut point, sweep thru the
			   choice point stack, resetting the mr_B
			   and mr_SPB values from the choice point
			   frames until mr_SPB > reg1 
			 */
#ifdef DEBUGSYS
	if (debug_system[CUT_RSB])
	{	printf("cut:reset mr_B init=%p \n", mr_B); 
		fflush(stderr);	}
#endif

	    while (mr_SPB <= reg1) {
			mr_B = cp_B(mr_B);
			mr_SPB = cp_SPBm(mr_B);
	    }

			/* Reset mr_HB from new choice point: */
	    mr_HB = cp_HB(mr_B);

#ifdef DEBUGSYS
	if (debug_system[CUT_CPCTR])
	{	fprintf(stderr,"cut:compact trail mr_B=%p mr_TR=%p mr_H=%p mr_HB=%p\n", mr_B,mr_TR,mr_H,mr_HB); 
		fflush(stderr);	}
#endif

			/* Now have to compact the trail from the
			   old top of trail to the new choice point;
			   Save current (old) top of trail in reg2:
			 */
	    reg2  = mr_TR;
			/* Reset mr_TR to its new value: */
	    mr_TR = mr_B;
			/* Use reg1 to sweep from point just above the
			   new top of trail down to old top of trail:
			 */
	    reg1  = mr_B - 1; 		/* Work from most recent (current) CP 
								 | downward to (old) top of trail */

	    for (;;) {
			if (reg1 < reg2)	/* Finished sweep; continue execution */
		    	DISPATCH;
				/* Sweeping down, the first word of a CP we hit is PrevB,
				   which points back upward; trail entries all have to 
				   point down into the heap. So this is a test for PrevB;
				   if we hit it, skip over the CP:
			 */
			if (*reg1 > PWORD(reg1)) {
		    	reg1 -= 4;
			}
			else {
		    	RETRAIL(PWPTR(*reg1),PWPTR(*(((PWord *)reg1)-1)));	/* trail if necessary */
#ifdef TRAILVALS
		    	reg1 = reg1 - 2;
#else
		    	reg1--;
#endif
			}
	    }	/* for (;;) */
#ifdef TRAILVALS
		/*  We have been skipping down pairs of trail entries, stepping
			on the UPPER of each pair; so we need to drop down one so
	     */
		reg1--;
#endif

	    goto get_out;

	    /* Meta call instructions - cut_macro, mod_closure, ocall, colon,
	     */
	    /* weird_jump, resolve_reference                */

CASE(W_CUTMACRO):		/* cutmacro srcreg, dstreg */
			/* #define OPSIZE          1											*/
			/* #define getpwrd(d)   *PWPTR(P+(d))									*/
			/* #define getreg(d)    ((*(P+(d))? mr_E: mr_SP)+ getpwrd((d)+OPSIZE))	*/

	    reg1 = getreg(OPSIZE);
	    *getreg(OPSIZE + REGSIZE) = MMK_INT(wm_heapbase - reg1);
	    P += OPSIZE + 2 * REGSIZE;
	    DISPATCH;

CASE(W_MOD_CLOSURE):		/* mod_closure addr */
	    --mr_SP;		/* make space for extra arg */
	    env_E(mr_SP) = env_E(mr_E);
	    env_CP(mr_SP) = env_CP(mr_E);
	    arg(mr_SP, 1) =
		MMK_SYM(PWORD(((ntbl_entry *) (P - MOD_CLOSURE_BDISP))->modid) & 0xffff);
	    mr_E = mr_SP;
	    P = getaddr(OPSIZE);
	    DISPATCH;

CASE(W_DBG_CALL):		/* implements dbg_call */
	    skip_ovflow = 1;	/* dont trigger an interrupt for following
				 			 * call
				 			 */
	    goto callpart;

CASE(W_OCALL):			/* implements callWithDelayedInterrupt */
	    skip_ovflow = 1;
	    wm_safety = wm_trigger;	/* trigger an interrupt for the
					 * second call
					 */
	    goto callpart;

CASE(W_COLON):
	    skip_ovflow = 0;

callpart:
	    CP = env_CP(mr_E);
	    E = env_E(mr_E);

	    reg1 = PWPTR(arg(mr_E, 1));		/* validate module id */
	    DEREF(reg1);
	    if (!(M_ISSYM(reg1) || M_ISUIA(reg1)))
		DOFAIL;
	    reg2 = reg1;	/* save reg1 */

	    reg1 = PWPTR(arg(mr_E, 2));		/* extract the called
						 * structure
						 */
	    DEREF(reg1);
	    if (MTP_TAG(reg1) == MTP_STRUCT) {
		S = MSTRUCTADDR(reg1);
		reg1 = PWPTR(MFUNCTOR(S));
		n = MFUNCTOR_ARITY(reg1);
		S += n + 1;	/* make S point to last arg + 1 */
	    }
	    else if (M_ISSYM(reg1) || M_ISUIA(reg1)) {
		n = 0;
	    }
	    else
		DOFAIL;

	    if ((P = call_resolve_reference(PWORD(reg2), PWORD(reg1), n, skip_ovflow))
		== wm_fail)
		DOFAIL;

/*      if( cutmacro((int)reg1) ) reg1 = mr_SP+4;
 * else reg1 = mr_SP + 5;
 */
	    reg1 = mr_SP + 4;
	    while (n--)
		*--reg1 = *--S;
	    mr_E = mr_SP = reg1 - 2;
	    env_CP(mr_SP) = CP;
	    env_E(mr_SP) = E;
	    DISPATCH;

CASE(W_WEIRD_JUMP):		/* jump(DBRef,callstruct) */
	    reg1 = PWPTR(arg(mr_E, 2));		/* get callstruct */
	    DEREF(reg1);
	    if (M_ISVAR(reg1))
		DOFAIL;

	    S = PWPTR(arg(mr_E, 1));	/* get dbref */
	    DEREF(S);

	    if ((P = jump_validate_dbref((PWord) S, (PWord) reg1)) == (Code *) 0)
		DOFAIL;

	    /* construct the call */
	    if (M_ISSTRUCT(reg1)) {
		register int arity;

		CP = env_CP(mr_E);	/* save oldCP and oldE in temporaries
					 */
		E = env_E(mr_E);
		/* copy the structure args into stack */
		/* trashing the previous frame. Its   */
		/* convenient to copy in reverse order */
		reg1 = MSTRUCTADDR(reg1);
		arity = MFUNCTOR_ARITY(*reg1);
		reg1 += arity + 1;
		S = mr_SP + 4;
		while (arity--)
		    *--S = *--reg1;

		mr_E = mr_SP = S - 2;
		env_CP(mr_E) = CP;
		env_E(mr_E) = E;
	    }
	    else {
		mr_SP += 2;
		env_CP(mr_SP) = env_CP(mr_E);
		env_E(mr_SP) = env_E(mr_E);
		mr_E = mr_SP;
	    }
	    DISPATCH;

CASE(W_RESOLVE_REF):
	    P = resolve_reference((ntbl_entry *) (P - RESOLVE_REF_BDISP));
	    DISPATCH;

	    /* Special instructions - wam_start1, wam_start2, special,
	     * cath22, throw, return, abort
	     */

CASE(W_WAM_START1):		/* wam_start1 returnaddr */
	    mr_SP -= 2;
	    env_CP(mr_SP) = getaddr(OPSIZE);
	    env_E(mr_SP) = mr_E;
	    mr_E = mr_SP;
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_WAM_START2):
	    *(mr_B + 2) |= 0x1;	/* mark top choicept as compacted */
	    mr_SP -= 2;
	    env_CP(mr_SP) = env_CP(mr_E);
	    env_E(mr_SP) = env_E(mr_E);
	    mr_E = mr_SP;
	    P += OPSIZE;
	    DISPATCH;

CASE(W_SPECIAL):
	    P = env_CP(mr_E) + GCMAGICSIZE;
	    mr_SP = mr_E + 2;
	    mr_E = env_E(mr_E);
	    DISPATCH;

CASE(W_CATCH22):
	    c22dat = cp_NC(mr_B);
	    P = env_CP(mr_E) + GCMAGICSIZE;
	    mr_E = env_E(mr_E);
	    DISPATCH;

CASE(W_THROW):
#if 0
	    //oldB = wm_regs[wm_regidx-1][wm_B_idx];
#endif
	    oldB = ((PWord *)((current_engine.reg_stack_top-1)->B.ptr));
	    
	    for (;;) {
		if (c22dat == cp_NC(mr_B))
		    DOFAIL;
		reg1 = cp_B(mr_B);
		if (oldB == cp_B(reg1)) {
		    wm_aborted = 1;
		    DOFAIL;
		}
/***** FIX ME !!!!!!!! TRAILVALS   *****
		for (reg1=mr_TR; reg1<mr_B; reg1++) * untrail *
		    ** (PWord **) reg1 = *reg1;
*/
	    UNWINDTRAIL;
		
		mr_TR = mr_B + 4;
		mr_B = cp_B(mr_B);
		mr_HB = cp_HB(mr_B);
		mr_SPB = cp_SPBm(mr_B);
	    }


CASE(W_RETURN):		/* return val */
	    retval = getpwrd(OPSIZE);
	    goto get_out;

CASE(W_GCMAGIC):		/* gcmagic long */
	    fprintf(stderr, "Oops ! Shouldn't have executed GCMAGIC\n");
	    P += OPSIZE + DATASIZE;
	    DISPATCH;

CASE(W_PANIC):			/* panic  */
	    fprintf(stderr, "alspro: panic_fail\n");
	    als_exit(1);

CASE(W_NOP):			/* nop -- should not need this */
	    fprintf(stderr, "Executed nop\n");
	    P += OPSIZE;
	    DISPATCH;

#ifndef Threaded
	default:
	    fprintf(stderr, "Illegal instruction : opcode = %d\n", (unsigned) *P);
	    retval = 0;
	    goto get_out;

    }
#endif
get_out:

    /* Prologue */
#if 0
    //wm_regidx--;
#endif
    pop_register_stack(&current_engine);

#if 0 /* ----------old code ----------*/
    if (untrail) {
	for (;;) {		/* untrail */
	    for (reg1 = mr_TR; reg1 < mr_B; reg1++)
		*PWPTR(*reg1) = *reg1;
	    if (cp_B(mr_B) == wm_B)
		break;
	    mr_TR = mr_B + 4;
	    mr_B = cp_B(mr_B);
	}
	wm_H = cp_HB(mr_B);	/* adjust heap pointer (for handling global
				 * vars)
				 */
    }
    else {
	wm_TR = mr_TR;
	wm_H = mr_H;
	wm_B = mr_B;
    }
#endif /* ----------old code ----------*/

    	/* Cut away top choicepoints prior to returning;
		   put cutpt in reg1: 
		 */
    reg1 = mr_E-1;

			/* If SPB is =< cut point, sweep thru the
			   choice point stack, resetting the mr_B
			   and mr_SPB values from the choice point
			   frames until mr_SPB > reg1 
			 */
    while (mr_SPB <= reg1) {
		mr_B   = cp_B(mr_B);
		mr_SPB = cp_SPBm(mr_B);
    }

			/* Reset mr_HB from new choice point: */
    mr_HB = cp_HB(mr_B);

			/* Now have to compact the trail from the
			   old top of trail to the new choice point;
			   Save current (old) top of trail in reg2:
			 */
    reg2 = mr_TR;

			/* Reset mr_TR to its new value: */
    mr_TR = mr_B;

			/* Use reg1 to sweep from point just above the
			   new top of trail down to old top of trail:
			 */
/*
UNWINDTRAIL for (reg1 = mr_TR+1; reg1 < mr_B; reg1+=2){ *PWPTR(*reg1) = *((PWord *)reg1-1);}
*/
    reg1 = mr_B - 1;

    for (;;) {
		if (reg1 < reg2)
	    	break;
				/* Sweeping down, the first word of a CP we hit is PrevB,
			   	which points back upward; trail entries all have to 
			   	point down into the heap. So this is a test for PrevB;
			   	if we hit it, skip over the CP:
			 	*/
		if (*reg1 > PWORD(reg1)) {
	    	reg1 -= 4;
		}
		else {
	    	RETRAIL(PWPTR(*reg1),PWPTR(*(reg1-1)));	/* trail if necessary */
#ifdef TRAILVALS
		   	reg1 = reg1 - 2;
#else
	    	reg1--;
#endif
		}
    }
/* printf("get_out-end: mr_B=%x, mr_H=%x mr_HB=%x\n",mr_B,mr_H,mr_HB); */
    wm_HB = mr_HB;
    wm_TR = mr_TR;
    wm_H = mr_H;
    wm_B = mr_B;
    return (retval);
}		/* run_wam */

/*---------------------------------------------------------------------*
 * Unification:
 *
 * The following functions unify two formulas f1 and f2.  The unify
 * function returns 0 on failure and something else (probably 1) on
 * success.
 *
 * No special assumptions are made about whether the formulas are
 * dereferenced or not.
 *---------------------------------------------------------------------*/
#undef mr_TR
#undef mr_HB
#undef mr_SPB

#define mr_TR  gr_TR
#define mr_HB  gr_HB
#define mr_SPB gr_SPB

static int
wam_unify(f1, f2)
    register PWord *f1, *f2;
{
    register int t1, t2;	/* types of the formulae */
    PWord *temp;
    register int n, i;

    DEREF(f1);
    DEREF(f2);

    if (f1 == f2)
	return (1);		/* success, formulas are identical */

    if (M_ISVAR(f1)) {
	if (M_ISVAR(f2)) {

	    /*
	     * If both formulas are variables then we must determine which
	     * occurred earlier.  This strategy prevents dangling references.
	     */

	    if (f1 < f2) {	/* swap f1 and f2 */
		temp = f1;
		f1 = f2;
		f2 = temp;
	    }
		/*   f2 <= f1 */

	    if (f2 >= wm_heapbase) {
		VVBIND(f1, f2, REGS_S, 555);    
	    }
	    else {
		VVBIND(f2, f1, REGS_S, 666);	
	    }
	}
	else {
	    BIND3(f1, f2, 202001);
	}
	return (1);
    }

    if (M_ISVAR(f2)) {
	BIND(f2, f1);
	return (1);
    }

    t1 = MTP_TAG(f1);
    t2 = MTP_TAG(f2);

    if (t1 != t2)
	return (0);

    switch (t1) {
	case MTP_STRUCT:
	    f1 = MSTRUCTADDR(f1);
	    f2 = MSTRUCTADDR(f2);

	    if (MFUNCTOR(f1) != MFUNCTOR(f2))
		return 0;

	    /* we need arity to match rest of the structure */
	    n = MFUNCTOR_ARITY(*f1);
#ifdef BIG_STRUCT
	    if (n == ESCAPE_ARITY) {
		f1++;
		f2++;
		n = *f1;
		if (n != *f2)
		    return (0);
	    }
#endif

	    for (i = 1; i <= n; i++)
		if (wam_unify(PWPTR(MSUBTERMN(f1, i)),
			      PWPTR(MSUBTERMN(f2, i))) == 0)
		    return 0;
	    return 1;

	case MTP_LIST:
	    f1 = MLISTADDR(f1);
	    f2 = MLISTADDR(f2);

	    if (wam_unify(PWPTR(MLIST_CAR(f1)), PWPTR(MLIST_CAR(f2))))
		return (wam_unify(PWPTR(MLIST_CDR(f1)), PWPTR(MLIST_CDR(f2))));
	    return (0);

	case MTP_CONST:
	    t1 = MTP_CONSTTAG(f1);
	    t2 = MTP_CONSTTAG(f2);

	    switch (t1) {
		case MTP_SYM:
		    if (t2 != MTP_UIA)
			return (0);
		    return (!strcmp((char *) TOKNAME(MSYMBOL(f1)),
				    (char *) M_FIRSTUIAWORD(MUIA(f2))));

		case MTP_UIA:
		    switch (t2) {
			case MTP_UIA:
			    return (!strcmp((char *) M_FIRSTUIAWORD(MUIA(f1)),
					(char *) M_FIRSTUIAWORD(MUIA(f2))));

			case MTP_SYM:
			    return (!strcmp((char *) M_FIRSTUIAWORD(MUIA(f1)),
					    (char *) TOKNAME(MSYMBOL(f2))));

			default:
			    /* Honest failure here; e.g., 'AB' = 2 */
			    /*  fprintf(stderr, "wam.c: huh1\n");  */
			    return (0);
		    }
		default:
		    return (0);
	    }

	default:
	    fprintf(stderr, "wam.c: Unrecognized (left) type in wam_unify\n");
	    return (0);
    }
}

    /*---------------------------------------------*
     * _w_unify is called by C defined builtins.
     *---------------------------------------------*/

int
_w_unify(f1, f2)
    PWord f1, f2;

{
    int   retval;

    gr_TR = wm_TR;
    gr_HB = wm_HB;
    gr_SPB = wm_SPB;

    retval = wam_unify(PWPTR(f1), PWPTR(f2));

    wm_TR = gr_TR;

    return (retval);
}

#ifdef FREEZE

/*---------------------------------------------------------------*
 |  Function call to bind vars without invoking any
 |  interrupt or vvbind constraint mechanisms; used in
 |  implementing the binding of two interval constraint
 |  delay vars:
 |
 |		'$bind_vars'(R, G)
 |
 |		installs a reference to G into R (R --> G)
 *---------------------------------------------------------------*/
int     pbi_bind_vars           ( void );

int
pbi_bind_vars()
{
	PWord r,  g;
	int   rt, gt;

	w_get_An(&r, &rt, 1);
	w_get_An(&g, &gt, 2);

	if ( rt != WTP_UNBOUND )
		SUCCEED;

#ifdef DEBUGSYS
	if (debug_system[FREZBV])
		printf("++bind_vars (wm_H=%p wm_HB=%p)left:%x[_%lu] right:%x[_%lu]\n", 
				wm_H, wm_HB,(int)r, (long)(((PWord *) r) - wm_heapbase),
				(int)g, (long)(((PWord *) g) - wm_heapbase));
#endif

	*PWPTR(r) = *(wm_SP + 3);   
	PLAINTRAIL(r);
	SUCCEED;
}

#if defined(INTCONSTR)
#include "intrv.h"

void plain_bind	(PWord, PWord);

void
plain_bind(r, v)
	PWord r,  v;
{
	*PWPTR(r) = v;
	PLAINTRAIL(r);
}



void bind_point_unfreeze	(PWord *,int *,double,int);
void
bind_point_unfreeze(r,t,pv,k)
	PWord *r;
	int *t,k;
	double pv;
{
	PWord *vl;
	int vlt;

#ifdef DEBUGSYS
	if (debug_system[CSTRBPUF])
		printf("bind_point_unfreeze(r=%p,t=%p,pv=%e,k=%d)\n",r,t,pv,k); 
#endif
		/* make the number: */
	vl = wm_H++;
	if ((k == INTEGERKIND) || (k == BOOLEANKIND))
		make_numberx(vl, &vlt, pv, WTP_INTEGER);
	else
		make_numberx(vl, &vlt, pv, WTP_DOUBLE);

	/*  -------------------------------------------------------- *
		#define BIND(r,f)     { TRAIL(r,0); *(r) = PWORD(f); }  
		#define PLAINTRAIL(r)                             \
  			{ if( PWPTR(r) < mr_HB  &&  PWPTR(r) >= mr_SPB) \
				*--mr_TR = PWORD(r);                        \
				*--mr_TR = *((PWord *)r); }
	 *  -------------------------------------------------------- */

/*
	if( PWPTR(r) < wm_HB  &&  PWPTR(r) >= wm_SPB) {
	  *(PWord *)--wm_TR = PWORD(r);
	  *(PWord *)--wm_TR = *PWPTR(r);
	}
*/
	  *(PWord *)--wm_TR = PWORD(r);
	  *(PWord *)--wm_TR = *PWPTR(r);
	  *((PWord *)r +1) = (PWord)((PWord *)r + 1);


	  ck_intvl_punch((PWord)(((PWord *)r)-1), *vl, vlt);		
/*----------------?????????????
	  if (!ck_intvl_punch((PWord)(((PWord *)r)-1), vl, vlt)) {		
     	*(r) = PWORD(f);							
	  }												
 *----------????????????-------------*/
	/*  ck_intvl_punch((((PWord *)r)-1), vl);   */
	
	

	wm_safety = -2; 
	wm_interrupt_caught = 3; 
} 
	
#endif /* defined(INTCONSTR) */
#endif /* FREEZE */

