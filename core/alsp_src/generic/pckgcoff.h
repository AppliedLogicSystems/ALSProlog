/*
 * pckgcoff.h  --  include file for packaging system 
 *
 * Copyright (c) 1990-1993 by Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 6/7/1990 
 */

#ifdef PACKAGE 
#ifndef _pckgcoff_included
#define _pckgcoff_included 	1

/*
 * Routines in "pckgcoff.c" use one of the following
 * error macros in a return on error. This sets
 * the global pckg_error with appropriate error
 * code that can be retrived from prolog by calling
 * $pckg_error/1. All error values are negative
 * so that outer layers of C can unwind on error
 * by testing for negative return and themselves
 * return a  negative value (-1).
 */

extern int pckg_error;

#define PCKG_MALLOC_FAIL   pckg_error = -1
#define PCKG_RAWDAT_FULL   pckg_error = -2
#define PCKG_SYMTBL_FULL   pckg_error = -3
#define PCKG_STRTBL_FULL   pckg_error = -4
#define PCKG_RELTBL_FULL   pckg_error = -5
#define PCKG_EXTS_FULL     pckg_error = -6
#define PCKG_FOPEN_FAIL    pckg_error = -7

/*
 * COFF Opcodes for the predicate "$coff_operation" defined
 * in "bpckg.c"
 */
#define COP_INIT_PACKAGE   		1
#define COP_ALIGN4			2
#define COP_PUT_CHAR		 	3
#define COP_PUT_SHORT  			4
#define COP_PUT_LONG   			5
#define COP_PUT_STRING 			6
#define COP_PUT_SYMBOL 			7
#define COP_PUT_PROCNAME 		8
#define COP_DECLARE_SYMBOL		9
#define COP_DECLARE_GLOBAL_SYMBOL	10
#define COP_DECLARE_C_GLOBAL_SYMBOL	11
#define COP_CREATE_OBJ_FILE 		12
#define COP_OPEN_RAWFILE		13
#define COP_PUT_UIA			14
#define COP_ALIGN8			15

#define NUMOF_COFF_OPERATIONS		15

/*
 * Package Relocation Entry
 */
typedef struct pckg_reloc {
	unsigned long	pr_vaddr;	/* (virtual) address of reference */
	unsigned long	pr_symndx;	/* index into symbol table 	*/
	unsigned short	pr_type;	/* relocation type 		*/
} PRELOC;

/* 
 * Package Relocation Entry Types
 * constants defining possible values for pr_type
 */

#define P_R_VIR32		1

#ifdef arch_m88k
#define P_R_REL26		2
#define P_R_RELH16		3
#define P_R_RELL16		4
#endif /* arch_m88k */

#ifdef arch_sparc
#define P_R_WDISP30		5
#define P_R_WDISP22		6
#define P_R_HI22 		7
#define P_R_LO10		8
#endif /* arch_sparc */

#define P_R_DELETED		0xffff


/*
 * Package Symbol Table Entry
 */

typedef struct pckg_syment
{
	unsigned long	pn_offset;	/* offset to string table	*/
	unsigned long	pn_value;	/* value of symbol 		*/
	unsigned char  	pn_len;		/* length of symbol 		*/
	unsigned char	pn_type;	/* symbol type	 		*/
} PSYMENT;

#define P_S_MAXSYMLEN 	255

/*
 * Package Symbol Entry Types
 * constants defining possible values for pn_type
 */
#define P_S_UNDEFINED 		1
#define P_S_LOCAL 		2
#define P_S_GLOBAL 		3



/********************************************************************
 * 		Macros to create COFF files			    *
 ********************************************************************/

#define COFF_LONG_RAWDATA(item)  \
  if (coff_insert_long_rawdata((unsigned long)(item)) < 0) return(-1);

#define COFF_SHORT_RAWDATA(item) \
  if (coff_insert_short_rawdata((unsigned short)(item)) < 0) return(-1);

#define COFF_BYTE_RAWDATA(item)  \
  if (coff_insert_char_rawdata((unsigned char)(item)) < 0) return(-1);

#define COFF_STRING_RAWDATA(item) \
  if (coff_insert_string_rawdata((char *)(item)) < 0) return(-1);

#define COFF_RAWDATA(from,size)	 \
  if (coff_insert_rawdata((char *)(from),(int)(size)) < 0) return(-1);

#define COFF_ALIGN4	 \
if (coff_align4_rawdata() < 0) return(-1);

#define COFF_DECLARE_GLOBAL_SYMBOL(sym)	\
  if (coff_insert_symbol(sym,SYMDEF_DECLARE_GLOBAL) < 0)  return(-1);

#define COFF_DECLARE_SYMBOL(sym) \
  if (coff_insert_symbol(sym,SYMDEF_DECLARE) < 0)  return(-1);

#define COFF_REFERENCE_SYMBOL(sym)	\
  if (coff_insert_reloc(sym,P_R_VIR32,0) < 0 || \
	  coff_insert_long_rawdata(0) < 0 ) return(-1);

#define COFF_REFERENCE_SYMBOL_IDX(symidx)	\
  if (coff_insert_reloc_symidx(symidx,P_R_VIR32,0) < 0 || \
	  coff_insert_long_rawdata(0) < 0) return(-1);

#define COFF_RELOC_SYMBOL(sym,rtype,offset)  \
  if (coff_insert_reloc(sym,rtype,offset) < 0)	 return(-1);

#define COFF_RELOC_SYMBOL_IDX(symidx,rtype,offset)	\
  if (coff_insert_reloc_symidx(symidx,rtype,offset) < 0) return(-1);


#endif	/* _pckgcoff_included */
#endif 	/* PACKAGE */
