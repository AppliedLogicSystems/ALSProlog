/*=================================================================*
 |			wintcode.h                     
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 |
 |			-- code space management include file
 | Author:  Kevin A. Buettner
 | Creation: 6/21/85
 | Revision History:
 | 01/15/86 - K. Buettner -- IBM PC port
 | 08/14/86 - K. Buettner -- Sun Port
 | 10/26/94 - C. Houpt -- Added emaskCodeAddr() for use in index.c.
 *=================================================================*/
#ifndef _WINTCODE_H_INCLUDED_
#define _WINTCODE_H_INCLUDED_ 1

#include "wntbl.h"

/*-----------------------------------------------------------------*
 | Size of Name Table Entry Pointers
 *-----------------------------------------------------------------*/

#ifdef KERNAL
#define NTBL_SIZE 1024 		/* total number of name table entries	*/
#else
#define NTBL_SIZE 16384		/* total number of name table entries	*/
#endif /* KERNAL */

#define NTBL_ENTRYSIZE	(NTBL_HEADERSIZE+NTBL_OVERFLOWSIZE+ 	\
			 			NTBL_CALLENTRYSIZE+NTBL_EXECENTRYSIZE)

#define OVERFLOWSIZE_BYTES (NTBL_OVERFLOWSIZE * sizeof(Code))
#define CALLENTRYSIZE_BYTES (NTBL_CALLENTRYSIZE * sizeof(Code))
#define EXECENTRYSIZE_BYTES (NTBL_EXECENTRYSIZE * sizeof(Code))
#define CODESIZE_BYTES (NTBL_CODESIZE * sizeof(Code))

/*-----------------------------------------------------------------*
 | Procedure table entry structure
 |
 | If this structure changes, wntbl.m4 for the 386 system must be changed.
 | FIXME!
 |
 | Note from kev (10-5-93):   The assembly language files for the other
 | systems do not require a knowledge of the layout of this structure.
 | The 386 system should not require it either.
 *-----------------------------------------------------------------*/

typedef struct {
						/* entry header */
	PWord tokid_arity;		/* 2 words */
	long *first_clause;		/* 2 words */
	long *last_clause;		/* 2 words */
	long *index_block;		/* 2 words */
	unsigned long timestamp;	/* 2 words */
	long lo_id;				/* 2 words */
	long hi_id;				/* 2 words */
	short flags;			/* 1 word */
	short modid;			/* 1 word */
	short icount;			/* 1 word */
	short nargs;			/* 1 word */
						/* total of 18 words in the header */
	Code overflow[NTBL_OVERFLOWSIZE];
	Code call_entry[NTBL_CALLENTRYSIZE];
	Code exec_entry[NTBL_EXECENTRYSIZE];
	Code code[NTBL_CODESIZE];	/* Actual entry code */
} ntbl_entry;

extern ntbl_entry **w_nametable;

struct codeblock {
    long *addr;
    long size;
    struct codeblock *next;
};

/*-----------------------------------------------------------------*
 | Name Entry Flag Masks
 *-----------------------------------------------------------------*/

#define NMSK_USAGE 		0x7
#define NFLG_UNUSED 	0
#define NFLG_LIBBREAK 	1   /* used to be NFLG_HIDDEN -raman  6/4/93 */
#define NFLG_BUILTIN 	2
#define NFLG_UNDEFINED 	3
#define NFLG_IMPORTED 	4
#define NFLG_SINGLE 	5
#define NFLG_MULTIPLE 	6
#define NFLG_SWITCH 	7

#define NMSK_SPYSET 	0x08
#define NMSK_EXPORT 	0x10
#define NMSK_PERMANENT 	0x20
#define NMSK_DYNAMIC	0x40

#define NMSK_BLT_TYPE  		0x0F00

#define NFLG_BLT_BUILTIN 	0x0000
#define NFLG_BLT_MODCLOSURE 0x0100
#define NFLG_BLT_JMP   		0x0200
#define NFLG_BLT_CALL  		0x0300
#define NFLG_BLT_EQUAL 		0x0400
#define NFLG_BLT_TRUE  		0x0500
#define NFLG_BLT_UNKNOWN	0x0F00

/*-----------------------------------------------------------------*
 | Name table flag indicates that the procedure will be
 | packaged in the package being created.
 *-----------------------------------------------------------------*/
#define NMSK_PCKG_MARK 	0x8000

/*-----------------------------------------------------------------*
 | Flag mask for a packaged name table entry
 | (We don't save NMSK_SPYSET and upper four flags in 
 | a packaged name table entry)
 *-----------------------------------------------------------------*/
#define NMSK_SAVE 		0x0FF7

/*-----------------------------------------------------------------*
 |	Free Block Format:
 |
 |	+-----------------------------------+
 |	|       Block Size                  |
 |	+-----------------------------------+
 |	|       Back Link                   |
 |	+-----------------------------------+
 |	|       Forward Link                |
 |	+-----------------------------------+
 |	|                                   |
 |	+---                             ---+
 |	|                                   |
 |	+---                             ---+
 |	|                                   |
 |	+---                             ---+
 |	|                                   |
 |	+---                             ---+
 |	|                                   |
 |	+-----------------------------------+
 |	|       Block Size                  |
 |	+-----------------------------------+
 |
 |
 |      Used Block Format:
 |
 |	+-----------------------------------+
 |	|       -Block Size                 |
 |	+-----------------------------------+
 |	|       Proc idx/1st arg offset     |
 |	+-----------------------------------+
 |	|       Clause Id                   |
 |	+-----------------------------------+
 |	|       First Argument              |
 |	+-----------------------------------+
 |	|       Offset to Det Entry         |
 |	+-----------------------------------+
 |	|       Next Clause Address         |
 |	+-----------------------------------+
 |	|       Choice Code                 |
 |	+---                             ---+
 |	|                                   |
 |	+-----------------------------------+
 |	|       Clause Code ...             |
 |	+---                             ---+
 |	|                                   |
 |	+-----------------------------------+
 |	|      -Block Size                  |
 |	+-----------------------------------+
 *-----------------------------------------------------------------*/

/*-----------------------------------------------------------------*
 | The following offsets are indices into the blocks
 *-----------------------------------------------------------------*/

#define WCI_SIZE   0

/*-----------------------------------------------------------------*
 | Fields found in a free block (size value is >0) 
 *-----------------------------------------------------------------*/
#define WCI_BLINK  1
#define WCI_FLINK  2

/*-----------------------------------------------------------------*
 | Fields found in a used block (size value is <0) 
 *-----------------------------------------------------------------*/
#define WCI_PROCIDX			1
#define WCI_CLAUSEID		2
#define WCI_FIRSTARGKEY		3
#define WCI_FSTART			4
#define WCI_MASK			4
#define WCI_DSTART			5
#define WCI_EMASK			6
#define WCI_SIZECODE		7
#define WCI_RINFO			8
#define WCI_NEXTCLAUSEADDR	9
#define WCI_CHOICECODE		10

/*-----------------------------------------------------------------*
 | WCI_CHOICEENTRY and WCI_CLAUSECODE are found in wci.h
 *-----------------------------------------------------------------*/

#include "wci.h"

/*-----------------------------------------------------------------*
 | Size of a clause block with no code in it 
 *-----------------------------------------------------------------*/

#define WC_OVERHEAD   WCI_CLAUSECODE+1
#define WC_EPSILON    WC_OVERHEAD+3		/* longwords */
#ifdef KERNAL
#define WC_AREASIZE   1024			/* longwords */
#else
#define WC_AREASIZE   131072			/* longwords */
#endif /* KERNAL */

#define sizeFreeBlock(item)		(*((long *)(item)+WCI_SIZE))
#define sizeUsedBlock(item)		(-(*((long *)(item)+WCI_SIZE)))
#define nextClauseAddr(item)	(*(long **)((long *)(item)+WCI_NEXTCLAUSEADDR))
#define sizeCode(item)			((int)(*((long *)(item) + WCI_SIZECODE)))
#define dstartCode(item)		((int)(*((long *)(item) + WCI_DSTART)))
#define fstartCode(item)		((int)(*((long *)(item) + WCI_FSTART)))
#define emaskCode(item)			((int)(*((long *)(item) + WCI_EMASK)))
#define emaskCodeAddr(item)		((int*)(((long *)(item) + WCI_EMASK)))
#define choiceEntry(item)		((Code *)((long *)(item) + WCI_CHOICEENTRY))
#define choiceCode(item)		((Code *)((long *)(item) + WCI_CHOICECODE))
#define clauseCode(item)		((Code *)((long *)(item) + WCI_CLAUSECODE))
#define procIdx(item)       	(*((long *)(item) + WCI_PROCIDX))
#define clauseId(item)      	(*((long *)(item) + WCI_CLAUSEID))

#define clsRInfo(item) 	 		(*((long *)(item) + WCI_RINFO))
#define clsRInfoBuf(item) 		((long *)((long *)(item) + clsRInfo(item)))

#define backChoiceCode(item)    (long *)((long *)(item) - WCI_CHOICECODE)

/*-----------------------------------------------------------------*
 | Clause group id info
 |
 | A clause group is a set of clauses in a given procedure which are
 | grouped together for purposes of consulting.  Usually a clause group
 | will be all those clauses appearing in a file.  When a file is reconsulted,
 | those clauses in the clause group associated with the file will be removed.
 |
 | There are two additional groups, the asserta group and the assertz group.
 |
 | The clause group id is part of the clause id (see above).  The clause
 | group id is found at the high part of the longword which forms the clause
 | id.
 |
 | CGI_WIDTH is the width of the clause group id field.
 | CGI_OFFSET is the amount to shift to get at this field.
 | CGI_MASK is the mask with which the clause id should be and'd with after
 | 	the field has been shifted right by CGI_OFFSET
 | CGI_VMASK is the mask with which the clause id should be and'd to obtain
 |	the value in the lower part of the long.
 |
 | CGI_ASSERTA and CGI_ASSERTZ are the (unshifted) clause group id's for
 | the asserta group and the assertz group.
 |
 | cgId(clause) is given a clause.  It returns the clause group id.
 *-----------------------------------------------------------------*/

#define CGI_WIDTH	9
#define CGI_OFFSET	(32-CGI_WIDTH)
#define CGI_MASK	((1<<CGI_WIDTH)-1)
#define CGI_VMASK	((1<<CGI_OFFSET)-1)
#define CGI_ASSERTA	((-1<<(CGI_WIDTH-1))+1)
#define CGI_ASSERTZ	((1<<(CGI_WIDTH-1))-1)

#define cgId(item)	(((long)clauseId(item)) >> CGI_OFFSET)

/*-----------------------------------------------------------------*
 | Amount of new space permitted to be on the freelist before
 | attempting to collect it.
 *-----------------------------------------------------------------*/

#define WC_FREELIMIT 32768

/*-----------------------------------------------------------------*
 | Masks 
 *-----------------------------------------------------------------*/

#define WCMSK_TOBEFREED	0x010000	/* block is on the tofree list */
#define WCMSK_MARKED	0x020000	/* block is marked as used. */

/*-----------------------------------------------------------------*
 | Environment masks
 *-----------------------------------------------------------------*/

#define EMSK_OLDE	0x01
#define EMSK_CP		0x02
#define EMSK_A1		0x04
#define EMSK_A2		0x08
#define EMSK_A3		0x10

/*-----------------------------------------------------------------*
 | INITIAL_ICOUNT is the number of times an unindexed procedure must
 | be executed (without modification) for indexing to be generated.
 |
 |	11/9/96: Changed from 6 to 1 to avoid the bug that appeared in 
 |	the nrev example when indexing installed on the fly.
 *-----------------------------------------------------------------*/

#define INITIAL_ICOUNT	1

/*-----------------------------------------------------------------*
 | dbrs_t is a type indicating whether the database is 
 | runable or writable.
 *-----------------------------------------------------------------*/

typedef enum {
    DBRS_WRITABLE,
    DBRS_RUNABLE
} dbprot_t;

extern	unsigned long w_timestamp;
extern	unsigned long w_reconstamp;
extern	PWord	wm_aborted;

extern	dbprot_t w_dbprotect_pe	(PE, dbprot_t );
#define		w_dbprotect(a)	w_dbprotect_pe(hpe,a)
extern	int	w_namelookup_pe	(PE, PWord, PWord, int );
#define		w_namelookup(a,b,c)	w_namelookup_pe(hpe,a,b,c)
extern	ntbl_entry *w_nameprobe_pe (PE, PWord, PWord, int );
#define		w_nameprobe(a,b,c)	w_nameprobe_pe(hpe,a,b,c)
extern	ntbl_entry *w_nameentry_pe	(PE, PWord, PWord, int );
#define		w_nameentry(a,b,c)		w_nameentry_pe(hpe,a,b,c)
extern	void	w_initcode_pe	(PE);
#define		w_initcode()		w_initcode_pe(hpe)
extern	void	w_freecount_pe	(PE, long *, long * );
#define		w_freecount(a,b)		w_freecount_pe(hpe,a,b)
extern	long *	w_alloccode_pe	(PE, int );
#define		w_alloccode(a)	w_alloccode_pe(hpe,a)
extern	void	w_freecode_pe	(PE, long * );
#define		w_freecode(a)	w_freecode_pe(hpe,a)
extern	long *	w_installcode_pe	(PE, Code *, int, int, int* );
#define		w_installcode(a,b,c,d)	w_installcode_pe(hpe,a,b,c,d)
extern	void	copy_code	(long *, long *, int );
extern	void	w_nukeindexing_pe	(PE, ntbl_entry * );
#define		w_nukeindexing(a)	w_nukeindexing_pe(hpe,a)
extern	void	w_fixchoicepoints_pe (PE, long * );
#define		w_fixchoicepoints(a)	w_fixchoicepoints_pe(hpe,a)
extern	void	w_abolish_pe	(PE, ntbl_entry * );
#define		w_abolish(a)	w_abolish_pe(hpe,a)
extern	int	w_erase_pe		(PE, int, long * );
#define		w_erase(a,b)		w_erase_pe(hpe,a,b)
extern	void	w_freeclause_pe	(PE, long * );
#define		w_freeclause(a)	w_freeclause_pe(hpe,a)
extern	void	w_assertz_pe	(PE, PWord, int, Code *, int,
                                         long, int, int, long );
#define		w_assertz(a,b,c,d,e,f,g,h)		w_assertz_pe(hpe,a,b,c,d,e,f,g,h)
extern	void	w_asserta_pe	(PE, PWord, int, Code *, int,
					 long, int, int, long );
#define		w_asserta(a,b,c,d,e,f,g,h)		w_asserta_pe(hpe,a,b,c,d,e,f,g,h)
extern	void	w_addclause_pe	(PE, PWord, int, int, Code *,
					 int, long, int, int,
					 long );
#define		w_addclause(a,b,c,d,e,f,g,h,i)		w_addclause_pe(hpe,a,b,c,d,e,f,g,h,i)
extern	void	w_abolish_cg_pe	(PE, ntbl_entry *, int, int );
#define		w_abolish_cg(a,b,c)		w_abolish_cg_pe(hpe,a,b,c)
extern	void	w_execquery_pe	(PE, Code *, int );
#define		w_execquery(a,b)		w_execquery_pe(hpe,a,b)
extern	void	w_execcommand_pe	(PE, Code *, int );
#define		w_execcommand(a,b)		w_execcommand_pe(hpe,a,b)
extern	void	w_exec_pe		(PE, Code *, int, const char* );
#define		w_exec(a,b,c)		w_exec_pe(hpe,a,b,c)
extern	void	w_assert_builtin_pe (PE, const char *, int, int (*) ( PE ) );
#define		w_assert_builtin(a,b,c)		w_assert_builtin_pe(hpe,a,b,c)
extern	void	w_assert_built2_pe	(PE, const char *, int, void (*) (PE, ntbl_entry *, PWord, PWord ), PWord, PWord );
#define		w_assert_built2(a,b,c,d,e)		w_assert_built2_pe(hpe,a,b,c,d,e)
extern	void	w_assert_foreign_pe (PE, PWord, const char *, int, int (*) ( PE ) );
#define		w_assert_foreign(a,b,c,d)		w_assert_foreign_pe(hpe,a,b,c,d)
extern	void	w_dynamic_pe	(PE, PWord, PWord, int );
#define		w_dynamic(a,b,c)		w_dynamic_pe(hpe,a,b,c)
extern	int	w_spy_pe		(PE, PWord, PWord, int );
#define		w_spy(a,b,c)		w_spy_pe(hpe,a,b,c)
extern	int	w_nospy_pe		(PE, PWord, PWord, int );
#define		w_nospy(a,b,c)		w_nospy_pe(hpe,a,b,c)
extern	void	w_libbreak_pe	(PE, PWord, PWord, int ,int );
#define		w_libbreak(a,b,c,d)		w_libbreak_pe(hpe,a,b,c,d)
extern	PWord	nextproc_pe	(PE, PWord, int );
#define		nextproc(a,b)		nextproc_pe(hpe,a,b)
extern	long *	first_clause_pe	(PE, int );
#define		first_clause(a)	first_clause_pe(hpe,a)
struct prolog_database_;
extern	long *	next_clause_db	(struct prolog_database_ *db, long * );
#define		next_clause(a)	next_clause_db(hpe->db,a)
extern	void	make_dbref_pe	(PE, long *, PWord *, int * );
#define		make_dbref(a,b,c)		make_dbref_pe(hpe,a,b,c)
extern	long *	w_validate_dbref_pe (PE, long *, int nid, long cid );
#define		w_validate_dbref(a,b,c)		w_validate_dbref_pe(hpe,a,b,c)
#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif
extern	long *	validate_dbref_pe	(PE, PWord, int, PWord * );
#define		validate_dbref(a,b,c)		validate_dbref_pe(hpe,a,b,c)
extern	Code *	jump_validate_dbref_pe (PE, PWord ref, PWord term );
#define		jump_validate_dbref(a,b)		jump_validate_dbref_pe(hpe,a,b)
#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif
extern	void	gen_indexing_pe	( PE );
#define		gen_indexing()		gen_indexing_pe(hpe)
extern	void	decr_icount_pe	(PE, Code * );
#define		decr_icount(a)	decr_icount_pe(hpe,a)
extern	void	seticount_pe	(PE, ntbl_entry * );
#define		seticount(a)	seticount_pe(hpe,a)
#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif
extern	long *	next_choice_in_a_deleted_clause_pe (PE, long * );
#define		next_choice_in_a_deleted_clause(a)	next_choice_in_a_deleted_clause_pe(hpe,a)
#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif
extern	void	w_collect_pe	(PE);
#define		w_collect()	w_collect_pe(hpe)
extern	PWord *	w_frame_info_pe	(PE, PWord *, long **, long * );
#define		w_frame_info(a,b,c)		w_frame_info_pe(hpe,a,b,c)
extern	void	w_relink_pe	(PE, ntbl_entry * );
#define		w_relink(a)	w_relink_pe(hpe,a)
extern	void	w_relinkall_pe	(PE);
#define		w_relinkall()	w_relinkall_pe(hpe)
extern	char *	w_getnamestring_pe	(PE, Code *, char * );
#define		w_getnamestring(a,b)		w_getnamestring_pe(hpe,a,b)
extern	int	nameprobe_pe	(PE, PWord, PWord, int );
#define		nameprobe(a,b,c)		nameprobe_pe(hpe,a,b,c)

/*-----------------------------------------------------------------*
 | from index.c 
 *-----------------------------------------------------------------*/
extern	void	indexproc_pe	(PE, PWord, PWord, int );
#define		indexproc(a,b,c)		indexproc_pe(hpe,a,b,c)
extern	void	do_indexing_pe	(PE, ntbl_entry * );
#define		do_indexing(a)	do_indexing_pe(hpe,a)

/*-----------------------------------------------------------------*
 | from gc.c 
 *-----------------------------------------------------------------*/
//extern	int	gc		( void );

#endif /* _WINTCODE_H_INCLUDED_ */
