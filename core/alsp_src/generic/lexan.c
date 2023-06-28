/*===================================================================*
 |			lexan.c      
 |		Copyright (c) 1985-1995 by Kevin A. Buettner
 |
 |			-- lexical analyzer for prolog system
 |
 | Author:  Kevin A. Buettner
 | Creation: 10/25/84
 | Revision History:
 | 06/15/85 - K. Buettner -- compiler mods, better storage management
 | 08/08/85 - K. Buettner -- split lexical analyzer and parser into two 
 |							 files; rewrote parts of it for both
 |                      	 versatility and efficiency.
 | 09/12/85 - K. Buettner -- parsing of floating point numbers
 | 10/07/86 - K. Buettner -- uninterned atoms
 | 10/26/94 -  C. Houpt -- Various UCHAR* casts.
 *===================================================================*/
#include "defs.h"

#ifdef CW_PLUGIN
#include <DropInCompilerLinker.h>
#endif

#ifdef OLDLEXAN
#include "lexan.h"

/*
 * Lexical Analysis Global Variables
 */

#include "newlexinit.h"		/* Get initial lex table */

lxi_but *lexbdp = &seetbl[USRSEEI].lb;	/* pointer to buffer descriptor */

#define nxtbuf() ((void) (*(lexbdp->nextbuf))(lexbdp), bp = lexbdp->curpos)

#define CType(c) (lx_chtb[(unsigned char) (c)])

char  tokstr[1024];

static	int	escape_char	( char ** );

/*----------------------------------------------------------------------*
 | next_token gets the next token from the input stream, classifies it and
 | returns. curtok is set with an integer value representing the token.
 | curtokttype represents the class of the token.  The TKTP_ defines are
 | used to identify the class (see lexan.h).
 *----------------------------------------------------------------------*/

void
next_token()
{
    register char *bp = lexbdp->curpos;		/* buffer pointer */
    int   ctype;		/* character type */
    register char *s = tokstr;	/* char pointer into token */

    /*
     * Step 1:  Make sure that we have really have a buffer; get one if
     *  we don't.
     */

#ifdef CW_PLUGIN
{
	extern CompilerLinkerParameterBlockPtr gCPB;
	if (linenum % 50 == 0) CWDisplayLines(gCPB, linenum);
}
#endif

    if (bp == (char *) 0)
	nxtbuf();

    /*
     * Step 2:  Skip white space and comments.  Note that C style comments
     *  are handled as a special case.
     */

    for (;;) {

	/* get character type of current char */
	ctype = CType((*bp));

	if (ctype == LX_WSP)
	    while (CType((*++bp)) == LX_WSP) ;	/* skip white space */
	else if (ctype == LX_CMNT || ctype == LX_NL)
	    nxtbuf();
	else if (*bp == '/' && *(bp + 1) == '*') {
	    int   starting_line;
	    int   nesting_level;


	    bp += 2;		/* skip first "slash star" */
	    starting_line = linenum;

	    for (nesting_level = 1; nesting_level > 0;)
		if (*bp == '*' && *(bp + 1) == '/') {
		    nesting_level--;
		    bp += 2;
		}
		else if (*bp == '/' && *(bp + 1) == '*') {
		    nesting_level++;
		    bp += 2;
		}
		else {
		    ctype = CType((*bp++));	/* increment is here */

		    if (ctype == LX_NL)		/* check for newline */
			nxtbuf();
		    else if (ctype == LX_EOF) {		/* check for EOF */
			static char buf[70];

			lexbdp->curpos = bp;
			sprintf(buf,
				"Unclosed C-Style comment beginning at line %d.", starting_line);
			parser_error(buf);
		    }
		}
	}
	else
	    break;
    }

    lexbdp->tokpos = bp;	/* mark start of token (for error recovery) */
    curtkty = TKTP_OTHER;	/* assume that the token type is OTHER */

    switch (CType((*bp))) {
	case LX_WSP:		/* can't happen */
	    break;
	case LX_CMNT:		/* can't happen */
	    break;
	case LX_EOF:
	    curtkty = TKTP_EOF;
	    break;
	case LX_SPEC:
	    while (CType((*bp)) == LX_SPEC)
		*s++ = *bp++;
	    *s = '\0';
	    break;
	case LX_LCAL:
	case LX_UCAL:
	    do {
		*s++ = *bp++;
		ctype = CType((*bp));
	    } while (ctype == LX_LCAL ||
		     ctype == LX_UCAL ||
		     ctype == LX_NUM);
	    *s = '\0';
	    break;
	case LX_NUM:{
		long  iv;
		double frac;
		double d = 0;		/* placate -Wall with initialization */

		if (*bp == '0' && *(bp + 1) == 'x' &&
		    (CType((*(bp + 2))) == LX_NUM ||
		     (*(bp + 2) >= 'a' && *(bp + 2) <= 'f') ||
		     (*(bp + 2) >= 'A' && *(bp + 2) <= 'F'))) {
		    bp += 2;
		    for (iv = 0;;) {
			if (CType((*bp)) == LX_NUM)
			    iv = (iv << 4) + (*bp++ - '0');
			else if ('a' <= *bp && *bp <= 'f')
			    iv = (iv << 4) + (*bp++ - 'a' + 10);
			else if ('A' <= *bp && *bp <= 'F')
			    iv = (iv << 4) + (*bp++ - 'A' + 10);
			else
			    break;
		    }
		    curtkty = TKTP_OBJECT;
		    curtok = (long) MK_DOUBLE((double) iv);
		    break;	/* out of the switch */
		}

		if (*bp == '0' && *(bp + 1) == '\'') {
		    curtkty = TKTP_INT;
		    bp += 2;
		    if (*bp == '\\') {
			char *buf_ptr;

			buf_ptr = bp + 1;
			curtok = escape_char(&buf_ptr);
			bp = buf_ptr;
		    }
		    else {
			curtok = *bp;
			bp++;
		    }
#ifdef notdef
		    if (*bp != '\'')
			parser_error("Single quote character expected in character constant");
		    bp++;
#endif
		    break;	/* out of the switch */
		}



		iv = 0;
		curtkty = TKTP_INT;
		while (CType((*bp)) == LX_NUM && iv < 3276) {
		    iv = 10 * iv + (*bp - '0');
		    bp++;
		}

		if (CType((*bp)) == LX_NUM) {
		    curtkty = TKTP_OBJECT;
		    d = (double) iv;
		    frac = 0;
		    while (CType((*bp)) == LX_NUM)
			d = 10.0 * d + (*bp++) - '0';
		}

		if (*bp == '.' && CType((*(bp + 1))) == LX_NUM) {
		    char *t;

		    if (curtkty == TKTP_INT) {
			d = iv;
			curtkty = TKTP_OBJECT;
		    }

		    frac = 0;
		    bp++;
		    while (CType((*bp)) == LX_NUM)
			bp++;
		    t = bp - 1;

		    while (*t != '.')
			frac = frac / 10.0 + *t-- - '0';

		    d += frac / 10;
		}

		if ((*bp == 'e' || *bp == 'E') &&
		    (((*(bp + 1) == '-' || *(bp + 1) == '+')
		      && CType((*(bp + 2))) == LX_NUM)
		     || CType((*(bp + 1))) == LX_NUM)) {
		    int   exp = 0;
		    int   neg = 0;
		    double m, z;

		    if (*++bp == '-') {
			neg = 1;
			bp++;
		    }
		    else if (*bp == '+')
			bp++;

		    while (CType((*bp)) == LX_NUM)
			exp = 10 * exp + (*bp++) - '0';

		    if (curtkty == TKTP_INT) {
			d = (double) iv;
			curtkty = TKTP_OBJECT;
		    }

		    m = 1.0;
		    z = 10.0;
		    while (exp != 0) {
			if (exp & 1) {
			    m *= z;
			    exp -= 1;
			}
			else {
			    z *= z;
			    exp >>= 1;
			}
		    }
		    if (neg)
			d /= m;
		    else
			d *= m;
		}

		if (curtkty == TKTP_OBJECT)
		    curtok = (long) MK_DOUBLE(d);
		else
		    curtok = iv;
	    }
	    break;

	case LX_SNGL:
	    *s++ = *bp++;
	    *s = '\0';
	    break;

	case LX_SQT:
	    /*
	     * note: need to add code to take care of case where
	     * string is too long
	     */
	    *s++ = *bp++;	/* I am putting the ' in on purpose */
	    while (1) {
		if (CType((*bp)) == LX_NL) {
		    lexbdp->curpos = bp;
		    parser_error("Unterminated (single) quoted string.");
		}
		else if (CType((*bp)) == LX_SQT) {
		    if (CType((*++bp)) == LX_SQT)
			*s++ = *bp++;
		    else
			break;
		}
		else if (*bp == '\\') {
		    char *buf_ptr;

		    buf_ptr = bp + 1;
		    *s++ = escape_char(&buf_ptr);
		    bp = buf_ptr;
		}
		else
		    *s++ = *bp++;
	    }
	    *s = '\0';
	    break;

	case LX_DQT:
	    curtkty = TKTP_STRING;
	    bp++;

	    while (1) {
		if (CType((*bp)) == LX_NL) {
		    lexbdp->curpos = bp;
		    parser_error("Unterminated (double) quoted string.");
		}
		else if (CType((*bp)) == LX_DQT) {
		    if (CType((*++bp)) == LX_DQT)
			*s++ = *bp++;
		    else
			break;
		}
		else if (*bp == '\\') {
		    char *buf_ptr;

		    buf_ptr = bp + 1;
		    *s++ = escape_char(&buf_ptr);
		    bp = buf_ptr;
		}
		else
		    *s++ = *bp++;
	    }
	    *s = '\0';
	    break;

	case LX_CHRQ:
	    curtkty = TKTP_INT;
	    bp++;
	    if (*bp == '^') {
		bp++;
		switch (*bp) {
		    case '^':
			curtok = '^';
			break;
		    case '?':	/* delete character */
			curtok = 0177;
			break;
		    default:
			curtok = (*bp) & 037;
			break;
		}
	    }
	    else
		curtok = *bp;

	    if (CType((*bp)) != LX_NL)
		bp++;

	    break;
    }

    if (curtkty == TKTP_OTHER) {	/* i.e, we haven't defined it yet */
	if (CType(tokstr[0]) == LX_UCAL) {	/* then we have a variable */
	    curtkty = TKTP_VAR;
	    curtok = find_var(tokstr);	/* look in the variable table */
	}
	else {
	    int   firstc = tokstr[0];

	    if (CType(firstc) == LX_SQT) {
		if (*bp == '(') {
		    curtkty = TKTP_FUNCTOR;
		    /* look for token without quote */
		    curtok = find_token((UCHAR *)tokstr + 1);
		}
		else if ( (curtok = probe_token((UCHAR *)tokstr + 1)) )
		    curtkty = TKTP_CONST;
		else {
		    curtkty = TKTP_OBJECT;
		    curtok = (long) MK_UIA(tokstr + 1);
		}
	    }
	    else {
		if (*bp == '(' && CType(firstc) != LX_SNGL) {
		    curtok = find_token((UCHAR *)tokstr);
		    curtkty = TKTP_FUNCTOR;
		}
#ifdef AllUIAConsts
		else if ((curtok = probe_token(tokstr))) {
		    if (TOKUNOP(curtok) || TOKBINOP(curtok))
			curtkty = TKTP_OP;

		    if (curtok == TK_DOT && (CType((*bp)) == LX_WSP ||
					     CType((*bp)) == LX_NL ||
					     CType((*bp)) == LX_EOF))
			curtkty = TKTP_FULLSTOP;
		}
		else {
		    curtkty = TKTP_OBJECT;
		    curtok = (long) MK_UIA(tokstr);
		}
#else
		else {
		    curtok = find_token((UCHAR *)tokstr);
		    if (TOKUNOP(curtok) || TOKBINOP(curtok))
				curtkty = TKTP_OP;

		    if (curtok == TK_DOT && (CType((*bp)) == LX_WSP ||
					     CType((*bp)) == LX_NL ||
					     CType((*bp)) == LX_EOF))
				curtkty = TKTP_FULLSTOP;
		}
#endif
	    }
	}
    }
    lexbdp->curpos = bp;
}

static int
escape_char(bpp)
    register char **bpp;
{
    int   iv;

    switch (**bpp) {
	case 'a':
	    (*bpp)++;
	    return 7;
	case 'b':
	    (*bpp)++;
	    return '\b';
	case 'f':
	    (*bpp)++;
	    return '\f';
	case 'n':
	    (*bpp)++;
	    return LF;
	case 'r':
	    (*bpp)++;
	    return CR;
	case 't':
	    (*bpp)++;
	    return '\t';
	case 'v':
	    (*bpp)++;
	    return '\v';
	case 'x':
	    (*bpp)++;
	    for (iv = 0;;) {
		if (CType((**bpp)) == LX_NUM)
		    iv = (iv << 4) + (*(*bpp)++ - '0');
		else if ('a' <= **bpp && **bpp <= 'f')
		    iv = (iv << 4) + (*(*bpp)++ - 'a' + 10);
		else if ('A' <= **bpp && **bpp <= 'F')
		    iv = (iv << 4) + (*(*bpp)++ - 'A' + 10);
		else
		    return iv;
	    }
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	    for (iv = 0;;) {
		if ('0' <= **bpp && **bpp <= '7')
		    iv = (iv << 3) + (*(*bpp)++ - '0');
		else
		    return iv;
	    }
	default:
	    return *(*bpp)++;
    }
}
#endif /* OLDLEXAN */
