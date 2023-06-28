/*
 * pckg.h  --  include file for packaging system  
 *
 * Copyright (c) 1989-1993 by Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   :  3/8/1989
 */


extern long *system_pckg; 		/* currently loaded package */
extern	void	pckg_run_init_goal	( void );

#ifdef PACKAGE

extern long *pckg_start_pred; 	/* start predicate of package */

extern char  *pckg_toktbl;		/* package token table */
extern long  pckg_toktbl_size; 	/* number of tokens in package token table */

extern char  *pckg_modtbl; 		/* package module table */

extern char  *pckg_gvars; 		/* package global variables */

extern char loaded_pckg_name[]; /* list of package names */


#define MAX_PCKGS 32 			/* maximum package files in a package */

#define PCKG_PREVIOUS_PCKG(pckg)	(*(long *)pckg)
#define PCKG_CODE_START(pckg)		(*(long *)(((char *)pckg)+4))
#define PCKG_CODE_END(pckg)			(*(long *)(((char *)pckg)+8))
#define PCKG_NAMETBL(pckg)			(*(long *)(((char *)pckg)+12))
#define PCKG_NAME(pckg)				(((char *)pckg)+16)

#define ENDOF_PCKGLIST_MARK		-1
#define ENDOF_PCKGLIST(pckg) 	(pckg == (long *)ENDOF_PCKGLIST_MARK)
#define IN_PCKG(addr,pckg) 		(addr >= PCKG_CODE_START(pckg) && \
								 addr <= PCKG_CODE_END(pckg))

#endif 	/* PACKAGE */

