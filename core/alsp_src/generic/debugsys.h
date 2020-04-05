/*=================================================================*
 |			debugsys.h                
 |      Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |		Header file for debugsys.c
 |
 | Authors(s):	Everyone
 | Editor:		Ken Bowen
 | Date:		6/26/96
 *=================================================================*/

#ifndef DEBUGSYS_I
#define DEBUGSYS_I 1

#ifdef DEBUGSYS

	/* -------------------------------------------------------------*
	 |	The debugging features enumeration type: 
	 |	Warning:  When entries are added/deleted in this enum list,
	 |  you must make corresponding changes in the the debug_sys_features/2
	 |	facts (originally) in blt_sys.pro
	 * -------------------------------------------------------------*/
typedef enum {
	GCBEEP,			/* gcbeep 		(0)		*/
	GCINFO,			/* gcinfo		(1)		*/
	GCFREEZEINFO,	/* gcfreezeinfo	(2)		*/
	CSTRPRIM,		/* cstrprim		(3)		*/
	CSTRCHNG,		/* cstrchng		(4)		*/
	CSTRUPDT,		/* cstrupdt		(5)		*/
	CSTRUPTM,		/* cstruptm		(6)		*/
	CSTRUPAD,		/* cstrupad		(7)		*/
	CSTRUPXT,		/* cstrupxt		(8)		*/
	GCINTR,			/* gcintr 		(9)		*/
	CSTRBPUF,		/* cstrbpuf 	(10)	*/
	FREZBV,			/* frezbv 		(11)	*/
	CUT_RSB,		/* cut_rsb		(12)	*/
	CUT_CPCTR,		/* cut_cpctr	(13)	*/
	FRZDELAY,		/* frzdelay		(14) 	*/
	INTVBIND,		/* intvbind		(15) 	*/
	MAX_DEBUG_FEATS
	} debug_feats;

extern int debug_system[];

extern	int pbi_toggle_debug_system	( void );


#endif /* DEBUGSYS */

#endif /* DEBUGSYS_I */
