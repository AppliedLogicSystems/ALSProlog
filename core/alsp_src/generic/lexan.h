/*=======================================================================*
 |		lexan.h              
 |	Copyright (c) 1985 by Kevin A. Buettner
 |	Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 |
 |		-- lexical analyzer include file
 |
 | Author:  Kevin A. Buettner
 | Creation: 10/25/84
 | 06/15/85 -  K. Buettner -- compiler mods, better storage management
 | 08/08/85 -  K. Buettner -- split lexical analyzer and parser
 |                            into two files; rewrote parts of it for both 
 |							  versatility and efficiency
 *=======================================================================*/

/* prevent multiple copies from being included */
#ifndef _LEXAN_H_INCLUDED_
#define _LEXAN_H_INCLUDED_ 1

/*
 * Character Types
 */

#define LX_WSP  	0
#define LX_SPEC 	1
#define LX_LCAL 	2
#define LX_UCAL 	3
#define LX_NUM  	4
#define LX_CMNT 	5
#define LX_SNGL 	6
#define LX_EOF  	7
#define LX_SQT  	8
#define LX_DQT  	9
#define LX_NL   	10
#define LX_CHRQ    	11

/* 
 * Token Classes
 */

#define TKTP_VAR		0
#define TKTP_FUNCTOR	1
#define TKTP_OP			2
#define TKTP_INT		3      /* integer      */
#define TKTP_OTHER		4
#define TKTP_FULLSTOP	5
#define TKTP_EOF		6
#define TKTP_STRING		7
#define TKTP_CONST		8     /* must be a constant (when user quotes things) */
#define TKTP_OBJECT		9     /* pointer to constructed object (doubles and
				 				 uninterned atoms)			*/

typedef struct _lex_buf {
            char *bufptr;	/* pointer to beginning of buffer */
            char *curpos;	/* pointer to next unprocessed character
                                   in the buffer                        */
            char *tokpos;	/* pointer to start of current token    */
            void (*nextbuf) ( struct _lex_buf * );
	    					/* fnc responsible for getting next buffer */
            int (*err_rec) ( struct _lex_buf *, const char * );
	    					/* fnc responsible for error message/recovery */
            int see_idx;	/* index into see table (if applicable) */
         } lxi_but;

extern lxi_but *lexbdp; /* lex buffer descriptor pointer */
extern char lx_chtb[];

/* prevent lexinit.c from complaining */
extern	void	next_token	( void );

#endif /* LEXAN_H_INCLUDED */
