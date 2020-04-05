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
extern 	void	icode		( int, long, long, long, long );

/* icode2.c */
extern	void	ic_install_overflow_call ( ntbl_entry * );
extern	void	ic_install_call_entry	( ntbl_entry * );
extern	void	ic_install_normal_exec_entry ( ntbl_entry * );
extern	void	ic_install_spy		( ntbl_entry * );
extern	void	ic_install_libbreak	( ntbl_entry *, int );
extern	void	ic_install_decr_icount	( ntbl_entry * );
extern	void	ic_install_resolve_ref	( ntbl_entry * );
extern	void	ic_install_jmp		( ntbl_entry *, Code *, int );
extern	void	ic_install_try_me_jmp	( ntbl_entry *, Code *, PWord );
extern	void	ic_install_switch_on_term
		    (ntbl_entry *, Code *, Code *, Code *, Code *, int );
extern	void	ic_install_builtin	( ntbl_entry *, int (*) ( void ) );
extern	void	ic_install_true		( ntbl_entry * );
extern	void	ic_install_fail		( ntbl_entry * );
extern	void	ic_install_equal	( ntbl_entry * );
extern	void	ic_install_call		( ntbl_entry *, long * );
extern	void	ic_install_module_closure ( ntbl_entry *, Code * );
extern	void	ic_install_next_choice_in_a_deleted_clause
					( Code * );
extern	void	ic_install_try_me	( Code *, PWord, int );
extern	void	ic_install_retry_me	( Code *, PWord, int, int );
extern	void	ic_install_trust_me	( Code *, PWord, int, int );
extern	long *	ic_install_try		( long *, Code *, int );
extern	long *	ic_install_retry	( long *, Code *, int, int );
extern	long *	ic_install_trust	( long *, Code *, int, int );
extern	Code *	ic_install_tree_overhead ( long *, int, Code * );
extern	Code *	ic_install_no		( Code *, Code *, const char * );
extern	void	ic_install_reference	( Code *, PWord );

#if Portable
/* icode1.c */
extern	void	ic_punch		( Code *, Code );
extern	void	ic_putl			( PWord );
extern	void	ic_put_align		( Code );
extern	void	ic_put_reg		( Code, long );
/* icode2.c */
extern	void	ic_install_bref		( ntbl_entry *, PWord, PWord );
extern	void	ic_install_instr	( ntbl_entry *, PWord, PWord );
extern	void	ic_install_tmjmp	( ntbl_entry *, long *, PWord );
#define ic_install_try_me_jmp ic_install_tmjmp
#endif /* Portable */

#endif	/* _ICODEGEN_H_INCLUDED_ */
