/*
 * rinfo.h 		-- Include file for relocation information related files
 *
 * Copyright (c) 1991-1993 by Applied Logic Systems, Inc.
 *
 * Author: Ilyas Cicekli
 * Date  : 01/03/91
 *
 * Packaging Notes :
 *	Packaging creates object(.o) files. External variables
 *	referenced from the packaged code need to be identified
 *	and marked for relocation. This is done by keeping track
 *	of the address where a external variable is referenced
 *	together with the name of the external variable. A table
 *	consisting of all such information is placed in the packaged
 *	in the proper format. The linker-loader reads this table and
 *	updates the addresses.
 *	For External variables referenced in "clause" code, a
 *	clause specific relocation table is built during code
 *	generation and stored in the tail end of the clause code.
 *	When packaging a clause this clause specific table is loaded
 *	into the package relocation table. For externals referenced
 *	in "name table entries", we do not build any tables during
 *	code generation. Instead the relocation information is loaded
 *	into the package relocation table during packaging.
 */


#ifdef PACKAGE


/*
 * Definition of the relocatable objects buffer
 */
extern unsigned long rinfo_buf[];

extern unsigned long *rinfo_ptr;

extern int rinfo_flag;

extern int package_rinfo_flag;


#define RELOC_TYPE(ri)		(((*(unsigned long *)(ri)) >> 26) & 0x3f)
#define RELOC_VAL(ri)		(((*(unsigned long *)(ri)) >> 18) & 0xff)
#define RELOC_LOC(ri)		((*(unsigned long *)(ri)) & 0x3ffff)

#define MAX_RINFO_ENTRIES 	1024

/*
 * RELOC_INFO(reloc_type,reloc_loc,reloc_val)
 *
 * where
 * 	reloc_type 	: Type of the relocatable object
 * 	reloc_loc  	: Location of the reloactable object (in icode_buffer)
 *	reloc_val 	: Value of the relocatable object.
 *
 * This macro puts the given relocatable object into the relocatable
 * objects of the icode buffer. There is an overflow check in this macro.
 * If we can guarantee that the relocatable objects buffer is big enough
 * to hold all reloactable objects in a clause, we don't this overflow
 * check. (Note: we don't have an overflow check for icode buffer).
 */
#define RELOC_INFO(rtype,rloc,rval)					\
    if (rinfo_flag) {							\
	if ((rinfo_ptr-rinfo_buf) >= MAX_RINFO_ENTRIES) 		\
	    fatal_error(FE_PCKG_RINFO,0);				\
	*rinfo_ptr = ( ((((unsigned long)(rtype)) & 0x3f) << 26) | 	\
			((((unsigned long)(rval))  & 0xff) << 18) | 	\
			(((unsigned long)(rloc)-(unsigned long)icode_buf)&0x3ffff));\
	rinfo_ptr++;							\
    }


/*
 * Initialize the relocatable objects buffer for a clause.
 */
#define RELOC_IC_INIT 							\
    if (rinfo_flag) reloc_init();


/* 
 * Update locations of shifted relocatable objects in the relocatable
 * objects buffer since the code in the icode buffer is shifted before
 * this macro used. 
 */
#define RELOC_UPDATE(shiftloc,shiftval)					\
    if (rinfo_flag) {							\
	unsigned long *ri;						\
	unsigned long riloc;						\
	unsigned long patchloc;						\
	patchloc=((unsigned long)(shiftloc)-(unsigned long)icode_buf);	\
	for (ri=rinfo_ptr - 1;						\
	     (ri >= rinfo_buf) && (RELOC_LOC(ri) >= patchloc); ri--)	\
	    *ri = ((*ri & 0xfffc0000) | 				\
			   (RELOC_LOC(ri)+(unsigned long)(shiftval)));	\
    }


#define RELOC_UPDATE_FROM_TO(fromshiftloc,toshiftloc,shiftval)		\
    if (rinfo_flag) {							\
	unsigned long *ri;						\
	unsigned long patchloc;						\
	patchloc=((unsigned long)(fromshiftloc)-(unsigned long)icode_buf); \
	for (ri=rinfo_ptr - 1;						\
	     (ri >= rinfo_buf) && (RELOC_LOC(ri) > patchloc); ri--) ;	\
	patchloc=((unsigned long)(toshiftloc)-(unsigned long)icode_buf);   \
	for (;(ri >= rinfo_buf) && (RELOC_LOC(ri) >= patchloc); ri--)	\
	    *ri = ((*ri & 0xfffc0000) | 				\
		   (RELOC_LOC(ri)+(unsigned long)(shiftval)));		\
    }



extern long coff_symidx_array[];

#ifdef arch_m88k
extern long ovtab_symidx[];
extern long ebtab_symidx[];
extern long rrtab_symidx[];
extern long trytab_symidx[];
extern long retry_tab_symidx[];
extern long retry_u_tab_symidx[];
extern long trust_tab_symidx[];
extern long trust_u_tab_symidx[];
#endif 	/* arch_m88k */

#ifdef arch_sparc
extern long ovtab_symidx[];
extern long ebtab_symidx[];
extern long trytab_symidx[];
extern long retry_tab_symidx[];
extern long retry_u_tab_symidx[];
extern long trust_tab_symidx[];
extern long trust_u_tab_symidx[];
#endif 	/* arch_sparc */


#else 	/* PACKAGE */

/*
 * If the packaging system is not in use define some macros
 * used during code generation as an empty macro.
 */
#define RELOC_INFO(rtype,rloc,rval) 	{}
#define RELOC_IC_INIT 	{}
#define RELOC_UPDATE(shiftloc,shiftval) 	{}
#define RELOC_UPDATE_FROM_TO(fromshiftloc,toshiftloc,shiftval) 	{}

#endif 	/* PACKAGE */



/*
 * Relocatable object types
 * First argument of RELOC_INFO macro above
 * is one of the following constants.
 */

#define RELOC_GVAR 			 1
#define RELOC_PROC_CALL 		 2
#define RELOC_PROC_EXEC 		 3

#ifdef arch_m88k
#define RELOC_GVAR_LO16			 4
#define RELOC_GVAR_HI16			 5
#define RELOC_GVAR_OFF26		 6
#define RELOC_PROC_CALL_OFF26 		 7
#define RELOC_PROC_EXEC_OFF26 		 8
#endif 	/* arch_m88k */

#ifdef arch_sparc
#define RELOC_GVAR_HI22			 9
#define RELOC_GVAR_LO10			10
#define RELOC_GVAR_WDISP30		11	
#define RELOC_GVAR_WDISP22		12	
#define RELOC_PROC_CALL_WDISP30		13
#define RELOC_PROC_EXEC_WDISP30 	14
#endif 	/* arch_sparc */




/*
 * The array coff_symidx_array holds indices of symbols (which can occur
 * in prolog code) in coff symbol table. During the initialization
 * of a coff file indices of these symbols are calculated and put into
 * the array coff_symidx_array. 
 * Note : When the first argument of RELOC_INFO is RELOC_GVAR??
 *		  the last argument is set to a constant from the following
 *		  that corresponds to the extern symbol for which relocation
 *		  is needed.
 */

#ifdef arch_i386 

#define symidx_wm_safety 		 0
#define symidx_wm_b_reg 		 1
#define symidx_wm_heapbase 		 2
#define symidx_wm_docut 		 3
#define symidx_OverflowPtr 		 4
#define symidx_UnifyPtr 		 5
#define symidx_wm_try_me 		 6
#define symidx_wm_retry_me 		 7
#define symidx_wm_trust_me 		 8
#define symidx_wm_g_sym 		 9
#define symidx_wm_g_uia 		10
#define symidx_wm_p_uia 		11
#define symidx_wm_u_sym 		12
#define symidx_wm_resolve_ref 		13
#define symidx_call_mod_closure 	14
#define symidx_wm_execute_builtin 	15
#define symidx_wm_interrupt_caught	16
#define NUMOF_COFF_SYMIDX 		17

#endif 	/* arch_i386 */


#ifdef arch_m68k 

#define symidx_wm_unify 		 0
#define symidx_wm_overflow 		 1
#define symidx_wm_heapbase 		 2
#define symidx_wm_docut 		 3
#define symidx_wm_g_sym 		 4
#define symidx_wm_g_uia 		 5
#define symidx_wm_g_int 		 6
#define symidx_wm_p_uia 		 7
#define symidx_cmp_sym_uia 		 8
#define symidx_wm_resolve_ref 		 9
#define symidx_wm_execute_builtin 	10
#define NUMOF_COFF_SYMIDX 		11

#endif 	/* arch_m68k */


#ifdef arch_m88k

#define symidx_wm_unify 		 0
#define symidx_wm_docut 		 1
#define symidx_wm_g_sym 		 2
#define symidx_wm_g_uia 		 3
#define symidx_wm_g_dbl 		 4
#define symidx_wm_p_uia 		 5
#define symidx_wm_p_unsafe		 6

#define symidx_wm_u_sym   		 7
#define symidx_wm_u_int   		 8
#define symidx_wm_u_lval  		 9

#define symidx_mth_base 		10
#define symidx_mth_stk  		11
#define symidx_mth_bot  		12
#define symidx_mth_aftercall		13
#define symidx_mth_pushdbl0 		14

#define symidx_wm_overflow0		15
#define symidx_wm_overflow1		16
#define symidx_wm_overflow2		17
#define symidx_wm_overflow3		18

#define symidx_wm_exec_builtin0		19
#define symidx_wm_exec_builtin1		20
#define symidx_wm_exec_builtin2		21
#define symidx_wm_exec_builtin3		22

#define symidx_wm_resolve_ref0		23
#define symidx_wm_resolve_ref1		24
#define symidx_wm_resolve_ref2		25
#define symidx_wm_resolve_ref3		26

#define symidx_wm_try0			27
#define symidx_wm_try1			28
#define symidx_wm_try2			29
#define symidx_wm_try3			30

#define symidx_wm_retry0		31
#define symidx_wm_retry1		32
#define symidx_wm_retry2		33
#define symidx_wm_retry3		34

#define symidx_wm_retry_u0		35
#define symidx_wm_retry_u1		36
#define symidx_wm_retry_u2		37
#define symidx_wm_retry_u3		38

#define symidx_wm_trust0		39
#define symidx_wm_trust1		40
#define symidx_wm_trust2		41
#define symidx_wm_trust3		42

#define symidx_wm_trust_u0		43
#define symidx_wm_trust_u1		44
#define symidx_wm_trust_u2		45
#define symidx_wm_trust_u3		46

#define NUMOF_COFF_SYMIDX 		47

#endif 	/* arch_m88k */



#ifdef arch_sparc

#define symidx_wm_unify 		 0
#define symidx_wm_docut 		 1
#define symidx_wm_g_int 		 2
#define symidx_wm_g_sym 		 3
#define symidx_wm_g_uia 		 4
#define symidx_wm_p_uia 		 5
#define symidx_wm_p_unsafe		 6
#define symidx_wm_u_sym   		 7
#define symidx_wm_u_int   		 8
#define symidx_wm_u_lval  		 9
#define symidx_wm_heapbase 		10 /* unused */

#define symidx_mth_base 		11
#define symidx_mth_stk  		12
#define symidx_mth_bot  		13
#define symidx_mth_aftercall		14
#define symidx_mth_pushdbl0 		15

#define symidx_wm_resolve_ref		16
#define symidx_wm_stack_overflow	17 /* unused */

#define symidx_wm_overflow0		18
#define symidx_wm_overflow1		19
#define symidx_wm_overflow2		20
#define symidx_wm_overflow3		21

#define symidx_wm_exec_builtin0		22
#define symidx_wm_exec_builtin1		23
#define symidx_wm_exec_builtin2		24
#define symidx_wm_exec_builtin3		25

#define symidx_wm_try0			26
#define symidx_wm_try1			27
#define symidx_wm_try2			28
#define symidx_wm_try3			29

#define symidx_wm_retry0		30
#define symidx_wm_retry1		31
#define symidx_wm_retry2		32
#define symidx_wm_retry3		33

#define symidx_wm_retry_u0		34
#define symidx_wm_retry_u1		35
#define symidx_wm_retry_u2		36
#define symidx_wm_retry_u3		37

#define symidx_wm_trust0		38
#define symidx_wm_trust1		39
#define symidx_wm_trust2		40
#define symidx_wm_trust3		41

#define symidx_wm_trust_u0		42
#define symidx_wm_trust_u1		43
#define symidx_wm_trust_u2		44
#define symidx_wm_trust_u3		45

#define symidx_wm_interrupt_caught	46

#define NUMOF_COFF_SYMIDX 		47

#define MASK30 	0x3FFFFFFF
#define MASK22 	0x003FFFFF
#define MASK10 	0x000003FF

#endif 	/* arch_sparc */



/*
 * Symbol definition types for COFF files.
 */
#define SYMDEF_DECLARE 		1
#define SYMDEF_DECLARE_GLOBAL 	2
#define SYMDEF_REFERENCE 	3


#define COFF_SYMIDX(idx) 	(coff_symidx_array[(idx)])

#define INSERT_SYM(idx,sym) 	\
	COFF_SYMIDX(idx) = coff_insert_symbol(sym,SYMDEF_REFERENCE);

