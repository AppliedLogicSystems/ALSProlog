/*=========================================================*
 |			icodegen.h:
 |		Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |			-- Stuff used by most of the ic*.c files.
 |
 | 10/26/94 - C. Houpt -- Redefined the Instruction Pointer as a union so that
 |	    	it can be used as both a short and long pointer.  This
 |	    	avoid the need to casting l-values, which is not ANSI.
 *=========================================================*/

#ifndef	_ICODEGEN_H_INCLUDED_
#define	_ICODEGEN_H_INCLUDED_ 1

#include "icom.h"

#if defined(arch_i386) || defined(arch_sparc) || defined(arch_m68k) || defined(arch_hppa)
extern Code *icode_buf;
#else 
extern Code icode_buf[];
#endif

/*--------------------------------------------------------------*
   Define the Instruction Pointer (ic_uptr) as a union so that 
   it can be used as both a short and long pointer.
 *--------------------------------------------------------------*/

typedef union {
	Code *code_ptr;
	long *long_ptr;
} ic_uptr_type;

extern ic_uptr_type ic_uptr;

/* Define ic_ptr as a code pointer to handle the common case of putting op codes. */

#define ic_ptr	ic_uptr.code_ptr

/* icode1.c */
extern 	void	icode		PARAMS(( int, long, long, long, long ));

/* icode2.c */
extern	void	ic_install_overflow_call PARAMS(( ntbl_entry * ));
extern	void	ic_install_call_entry	PARAMS(( ntbl_entry * ));
extern	void	ic_install_normal_exec_entry PARAMS(( ntbl_entry * ));
extern	void	ic_install_spy		PARAMS(( ntbl_entry * ));
extern	void	ic_install_libbreak	PARAMS(( ntbl_entry *, int ));
extern	void	ic_install_decr_icount	PARAMS(( ntbl_entry * ));
extern	void	ic_install_resolve_ref	PARAMS(( ntbl_entry * ));
extern	void	ic_install_jmp		PARAMS(( ntbl_entry *, Code *, int ));
extern	void	ic_install_try_me_jmp	PARAMS(( ntbl_entry *, Code *, PWord ));
extern	void	ic_install_switch_on_term
		    PARAMS((ntbl_entry *, Code *, Code *, Code *, Code *, int ));
extern	void	ic_install_builtin	PARAMS(( ntbl_entry *, int (*) PARAMS(( void )) ));
extern	void	ic_install_true		PARAMS(( ntbl_entry * ));
extern	void	ic_install_fail		PARAMS(( ntbl_entry * ));
extern	void	ic_install_equal	PARAMS(( ntbl_entry * ));
extern	void	ic_install_call		PARAMS(( ntbl_entry *, long * ));
extern	void	ic_install_module_closure PARAMS(( ntbl_entry *, Code * ));
extern	void	ic_install_next_choice_in_a_deleted_clause
					PARAMS(( Code * ));
extern	void	ic_install_try_me	PARAMS(( Code *, PWord, int ));
extern	void	ic_install_retry_me	PARAMS(( Code *, PWord, int, int ));
extern	void	ic_install_trust_me	PARAMS(( Code *, PWord, int, int ));
extern	long *	ic_install_try		PARAMS(( long *, Code *, int ));
extern	long *	ic_install_retry	PARAMS(( long *, Code *, int, int ));
extern	long *	ic_install_trust	PARAMS(( long *, Code *, int, int ));
extern	Code *	ic_install_tree_overhead PARAMS(( long *, int, Code * ));
extern	Code *	ic_install_no		PARAMS(( Code *, Code *, char * ));
extern	void	ic_install_reference	PARAMS(( Code *, PWord ));

#if Portable
/* icode1.c */
extern	void	ic_punch		PARAMS(( Code *, Code ));
extern	void	ic_putl			PARAMS(( PWord ));
extern	void	ic_put_align		PARAMS(( Code ));
extern	void	ic_put_reg		PARAMS(( Code, long ));
/* icode2.c */
extern	void	ic_install_bref		PARAMS(( ntbl_entry *, PWord, PWord ));
extern	void	ic_install_instr	PARAMS(( ntbl_entry *, PWord, PWord ));
extern	void	ic_install_tmjmp	PARAMS(( ntbl_entry *, long *, long ));
#define ic_install_try_me_jmp ic_install_tmjmp
#endif /* Portable */

#endif	/* _ICODEGEN_H_INCLUDED_ */
