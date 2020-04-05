/*=================================================================*
 |			parser.h     
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 |
 |			-- include file for parser.c and company
 |
 | Author:  Kevin A. Buettner
 | Creation: 11/15/84, MV8000
 | Revision History:
 | 06/15/85 - K.Buettner -- Modifications for WAM compiler parsing
 | 11/14/90 - K.Buettner -- separation of unary and binary ops
 *=================================================================*/
#ifndef __parser_h_
#define __parser_h_

typedef struct tkentry_ {
	unsigned short length;
	unsigned char *tkname;	/* pointer to string which is name of token */
	unsigned short unop;	/* unary operator precedence and assoc */
	unsigned short binop;	/* binary op precedence and assoc */
} tkentry;

extern tkentry *toktable;	/* note: although I am exporting the toktable,
				 *       it should not be accessed by modules
				 *       other than parser.c except through
				 *       the macros provided here in parser.h
				 */

extern int errcount;

#define TOKNAME(t)  (toktable[(t)].tkname)
#define TOKNAMELEN(t) (toktable[(t)].length)
#define TOKUNOP(t)  (toktable[(t)].unop)
#define TOKBINOP(t) (toktable[(t)].binop)

#define N_TOK_CHARS 256
extern long *char_to_tok_map;
#define char_to_tok(ch) (char_to_tok_map[(ch)])
#define tok_to_char(tok) (*TOKNAME(tok))
#define is_char_tok(tok) ((tok) == (char_to_tok(tok_to_char(tok))))
#define is_char_code(ch) (0 <= (ch) && (ch) < N_TOK_CHARS)

/*
 * Associativity/Precedence information is bundled into a single short.
 * The Precedence is shifted by left by four bits and the Associativity
 * information is or'd into the lower four bits.
 *
 * Bit 0, the least significant bit, indicates a right x or y.  0 represents
 * x and 1 represents y.  Prefix and infix operators will use this bit.
 *
 * Bit 1 indicates a left x or y with the x/y designations the same as for
 * bit 0.  Postfix and infix operators will use this bit.
 *
 * Bit 2 will be set if the operator is a prefix operator.  In this manner
 * it is possible to distinguish the two types of unary operators (prefix
 * and postfix).  When reducing the operator stack it will also be possible
 * to determine if the operator is unary or binary since postfix operators
 * are reduced immediately.  Thus there will only be infix and prefix operators
 * on the stack and bit 2 will be 0 for all infix operators.  This is important
 * for error checking.
 *
 * Bit 3 is unused at this time.   It may eventually be used to represent
 * either cut macros or symbols which prolog will wish to expand. (Or both
 * since we have two bit 3's to work with.)
 */

#define ASSC_RIGHT	1
#define ASSC_LEFT	2
#define ASSC_PREFIX	4


#define OP_FX(prec)	(((prec)<<4) | ASSC_PREFIX)
#define OP_FY(prec)	(((prec)<<4) | (ASSC_RIGHT | ASSC_PREFIX))
#define OP_XF(prec)	((prec)<<4)
#define OP_YF(prec)	(((prec)<<4) | ASSC_LEFT)
#define OP_XFX(prec)	((prec)<<4)
#define OP_XFY(prec)	(((prec)<<4) | ASSC_RIGHT)
#define OP_YFX(prec)	(((prec)<<4) | ASSC_LEFT)

/*
 * PREC_ONLY, PREC_LEFT, and PREC_RIGHT all return the precedence in a variety
 * of forms. PREC_ONLY returns just the precedence (but still shifted)
 * while PREC_LEFT and PREC_RIGHT will return the precedence plus the
 * appropriate left or right bit.
 */

#define PREC_ONLY(ap) ((ap)&0xfff0)
#define PREC_RIGHT(ap) ((ap)&0xfff1)
#define PREC_LEFT(ap) ((ap)&0xfff2)


extern char tokstr[];		/* defined in lexan.c */


/* symtab.c */
extern	void	symtab_init	( void );
extern	long	find_token	( const UCHAR * );
extern	long	probe_token	( UCHAR * );
extern	int	tok_table_size	(void);

/* parser.c */
extern	void	parser_init	( void );
extern	void	parser_reset	( void );
extern	int	find_var	( char * );
extern	void	push_rator	( long, long );
extern	void	bld_clause	( void );
extern	void	nt_query	( void );
extern	void	parser_error	( const char * );
extern	void	read_loop	( void (*) (void), int );
extern	pword	bld_strl	( char * );
extern	pword	bld_vlst	( void );
extern	int	qtok		( int );
extern	void	bld_showanswers	( void );
extern	int	consult		( int );
extern	pword	prim_read	( void );
extern	int	exec_query_from_buf ( char * );
extern	UCHAR *	token_name	( int );

/* mapsym.c */
extern	void	push_symmap	( void );
extern	void	pop_symmap	( void );
extern	long	symmap		( long );
extern	long *	sym_order	( long * );

/* expand.c */
extern	void	parser_action	( int, pword );
extern	pword	cvt_term_to_rule ( PWord, int );

#endif /* __parser_h_ */
