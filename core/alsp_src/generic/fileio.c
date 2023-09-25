/*=======================================================================*
 |			fileio.c     
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 |
 |			-- file input/output for prolog compiler
 |
 | Author:  Kevin A. Buettner
 | Creation:  11/24/84
 | Revision History:
 | 07/12/93 - K.Buettner -- Removed some functions no longer needed for 
 |					 		bootstrapping or debugging
 | 10/26/94 - C. Houpt -- Think C ANSI library does not like the setvbuf() 
 |						  calls.  Also various char* casts.
 *=======================================================================*/
#include "defs.h"

#ifdef OLDFIO

static	void	fio_nxln	( lxi_but * );
static	int	fio_syntax_err	( lxi_but *, const char * );
static	void	file_error	( const char * );

sefldsc seetbl[MAXSEE] =
{ {
    1,				/* inuse */
    TK_USER,		/* tkidx */
    0,				/* fd   */
    0,				/* tok  */
    0,				/* toktype */
    0,				/* linenumber */
    0,				/* errcount */
    {
	(char *) 0,		/* bufptr */
	(char *) 0,		/* curpos */
	(char *) 0,		/* tokpos */
	fio_nxln,		/* nextbuf */
	fio_syntax_err,	/* err_rec */
	USRSEEI			/* see_idx */
    },
    "\0"			/* buffer */
} };

tlfldsc telltbl[MAXTELL] =
{ {1, TK_USER, 0} };

int   cur_si = USRSEEI;
int   cur_ti = USRTELI;

int   linenum = 0;
int   curch = ' ';
long  curtok = 0;
int   curtkty = 0;
FILE *curfd;

const char *curprompt;

#ifdef MacOS
pptinfo prompts[3] =
{
    {"?-\n", "?-_\n"},
    {"$\n", "$_\n"},
    {"\n", "\n"}
};

#else
pptinfo prompts[3] =
{
    {"?- ", "?-_"},
    {"$ ", "$_"},
    {"", ""}
};

#endif

void
fio_init(void)
{
#if !defined(DOS) && !defined(Portable) && !defined(VMS) && !defined(NeXTOS) && !defined(MSWin32)
/* The Think C ANSI library has trouble with the setvbuf calls.  They seem
   to disable stdin. */
#ifndef THINK_C
    static char inbuf[BUFSIZ];
    static char outbuf[BUFSIZ];

    setvbuf(stdin, inbuf, _IONBF, BUFSIZ);
    setvbuf(stdout, outbuf, _IOLBF, BUFSIZ);
#endif /* THINK_C */
#endif /* not (DOS or Portable or VMS or NeXTOS) */
    seetbl[USRSEEI].fd = stdin;
    telltbl[USRTELI].fd = stdout;
    curfd = stdin;
}

static char *univ_fgets(char *s, int n, FILE *stream)
{
    int c;
    char *p = s, *ends = s + n;

    if (s == NULL || n <= 0 || stream == NULL) return NULL;

    while ((c = fgetc(stream)) != EOF && p < ends) {
        *p++ = c; n--;
	if (c == CR || c == LF) break;
    }

    if (p == s) {
        return NULL;
    } else {
        *p = 0;
	return s;
    }
}

/*
 * The following macro is used to define the functions seeidx and tellidx,
 * both of which take a token as an argument and return the index into
 * their respective tables if it is present, a free index if not present,
 * and a -1 if the table is full.
 */

#define GEN_TABS(fncname,tbl,tbmax)				\
static int fncname ( int );				\
static int							\
fncname(int tok)							\
{								\
    int i;							\
    for (i=0;i<tbmax;i++)					\
	if (tbl[i].inuse && tbl[i].tkidx == tok)		\
	    break;						\
    if (i >= tbmax) {						\
	for (i=0; i<tbmax && tbl[i].inuse; i++) {};		\
	if (i>=tbmax)						\
	    i = -1;						\
    }								\
    return(i);							\
}

GEN_TABS(seeidx, seetbl, MAXSEE)
GEN_TABS(tellidx, telltbl, MAXTELL)
static void
fio_nxln(lxi_but *lbp)
{
    register char *b = seetbl[(lbp->see_idx)].buffer;

    if (lbp->see_idx == USRSEEI)
#ifdef MacOS
	PI_oprintf("%s\n", curprompt);
#else
	PI_oprintf("%s", curprompt);
#endif
    lbp->bufptr = b;
    lbp->curpos = b;
readit:
    if (univ_fgets(b, SEEBFSZ, seetbl[(lbp->see_idx)].fd) == NULL || *b == 0) {
#if 0
	//if (wm_interrupt_caught && wm_regidx == 0)
#endif
	if (wm_interrupt_caught && current_engine.reg_stack_top == current_engine.reg_stack_base)
	{
	    /* Ignore control-C's at top level */
	    wm_interrupt_caught = 0;

#ifdef DOS
	    rewind(seetbl[(lbp->see_idx)].fd);
#else
	    clearerr(seetbl[(lbp->see_idx)].fd);
#endif
	    goto readit;
	}
	*b = '\0';		/* end-of-file marker */
    }
    linenum++;

}

static int
fio_syntax_err(lxi_but *lbp, const char *errstring)
{
    char  b[SEEBFSZ];
    char *bp, *sp;
    int   si = lbp->see_idx;

    if (lbp->bufptr)
	PI_oprintf("%s", lbp->bufptr);
    else
	PI_oprintf("<EOF>\n");

    for (bp = b, sp = seetbl[si].buffer;;) {
	if (sp == lbp->tokpos) {
	    sp++;
	    *bp++ = '^';
	    *bp++ = '\0';
	    break;

	}
	else if (*sp == '\t')
	    *bp++ = *sp++;
	else if (*sp == '\0') {
	    *bp++ = *sp++;
	    break;
	}
	else {
	    *bp++ = ' ';
	    sp++;
	}
    }

    PI_oprintf("%s\n", b);

    if (curfd == stdin)
	PI_oprintf("Syntax Error: %s\n", errstring);
    else
	PI_oprintf("File `%s', Line %d, Syntax Error: %s\n\n",
		   TOKNAME(seetbl[cur_si].tkidx), linenum, errstring);

    if (si == USRSEEI) {
	lbp->bufptr = (char *) 0;
	lbp->curpos = (char *) 0;
    }
    else {
	while (curtkty != TKTP_EOF && curtkty != TKTP_FULLSTOP)
	    next_token();
    }

    return (1);
}



int
fio_see(int tok)
{
    char  ebuf[100];
    int   i;

    seetbl[cur_si].tok = curtok;
    seetbl[cur_si].toktype = curtkty;
    seetbl[cur_si].fd = curfd;
    seetbl[cur_si].linenumber = linenum;

    /* Experimental */
    seetbl[cur_si].errcount = errcount;

    i = seeidx(tok);
    if (i < 0) {
	file_error("Open file limit exceeded (see)");
	return (0);		/* failure */
    }
    else if (seetbl[i].inuse) {
	cur_si = i;
	curtok = seetbl[cur_si].tok;
	curtkty = seetbl[cur_si].toktype;
	curfd = seetbl[cur_si].fd;
	linenum = seetbl[cur_si].linenumber;

	/* Experimental */
	errcount = seetbl[cur_si].errcount;
	lexbdp = &seetbl[cur_si].lb;
	return (1);		/* success */
    }
    else {
	FILE *fd;

	if ((fd = fopen((char *)TOKNAME(tok), "r")) == NULL) {
	    sprintf(ebuf, "Error opening '%s' for read access", TOKNAME(tok));
	    file_error(ebuf);

	    return (0);		/* failure */
	}
	seetbl[i].inuse = 1;
	seetbl[i].tkidx = tok;
	seetbl[i].fd = fd;
	seetbl[i].linenumber = 0;	/* no lines read yet */

	/* Experimental */
	seetbl[i].errcount = 0;	/* no errors yet */

	seetbl[i].lb.bufptr = (char *) 0;	/* nothing read yet */
	seetbl[i].lb.curpos = (char *) 0;	/* ditto */
	seetbl[i].lb.nextbuf = fio_nxln;
	seetbl[i].lb.err_rec = fio_syntax_err;
	seetbl[i].lb.see_idx = i;
	cur_si = i;
	lexbdp = &seetbl[cur_si].lb;
	curtok = seetbl[cur_si].tok;
	curtkty = seetbl[cur_si].toktype;
	curfd = seetbl[cur_si].fd;
	linenum = seetbl[cur_si].linenumber;

	/* Experimental */
	errcount = seetbl[cur_si].errcount;

	return (1);		/* success */
    }
}

void
fio_seen(void)
{
    if (cur_si == USRSEEI) {
	clearerr(stdin);
	seetbl[USRSEEI].toktype = 5;	/* hmm.... */
	seetbl[USRSEEI].lb.curpos = (char *) 0;
    }
    else {
	fclose(curfd);
	seetbl[cur_si].inuse = 0;
    }

    cur_si = USRSEEI;
    curtok = seetbl[cur_si].tok;
    curtkty = seetbl[cur_si].toktype;
    curfd = seetbl[cur_si].fd;
    linenum = seetbl[cur_si].linenumber;

    /* Experimental */
    errcount = seetbl[cur_si].errcount;

    lexbdp = &seetbl[cur_si].lb;
}

/* returns the index to the token representing name of file */

int
fio_seeing(void)
{
    return (seetbl[cur_si].tkidx);
}


static void
file_error(const char *s)
{
    PI_oprintf("%s\n", s);
}

/*
 * Since 'getc' returns a negative value for characters whose ASCII
 * code is bigger than 0x80 in DOS environment (Problem in stdio.h file
 * of High C compiler), we have to type cast the returned value as
 * 'unsgined char' when it is needed.
 *                      -- Ilyas 5/29/91
 */

int
fio_get0(void)
{				/* returns next character read, -1 on end of
				 * file.
				 */
    int   ch;

    if ((ch = getc(curfd)) == EOF) {
	if (curfd == stdin)
	    rewind(stdin);
	return (-1);
    }
    else
	return ((int) ((unsigned char) ch));
}


int
fio_get(void)
{				/* returns next non-control character or
				 * blank, -1 on eof
				 */
    int   ch;

    do {
	if ((ch = getc(curfd)) == EOF) {
	    if (curfd == stdin)
		rewind(stdin);
	    return (-1);
	}
    } while ((int) ((unsigned char) ch) <= ' ');

    return ((int) ((unsigned char) ch));
}



void
fio_put(int ch)	/* prints the single character ch to the
				 * current file
				 */
{
    PI_oputchar(ch);
}



void
fio_nl(void)
{				/* writes the newline character to the
				 * current output
				 */
    PI_oputchar('\n');
}


int
fio_tell(int tok)
{
    char  ebuf[100];
    int   i;

    i = tellidx(tok);
    if (i < 0) {
	file_error("Open file limit exceeded (tell)");
	return (0);		/* failure */
    }
    else if (telltbl[i].inuse) {
	cur_ti = i;
	return (1);		/* success */
    }
    else {
	FILE *fd;

	if ((fd = fopen((char *)TOKNAME(tok), "w")) == NULL) {
	    sprintf(ebuf, "error opening '%s' for write access", TOKNAME(tok));
	    file_error(ebuf);
	    return (0);		/* failure */
	}
	telltbl[i].inuse = 1;
	telltbl[i].tkidx = tok;
	telltbl[i].fd = fd;
	cur_ti = i;
	return (1);		/* success */
    }
}

void
fio_told(void)
{
    if (cur_ti == USRTELI) {
	return;			/* do nothing ... don't want to really close
				 * it
				 */
    }
    else {
	fclose(outfd);
	telltbl[cur_ti].inuse = 0;
    }

    cur_ti = USRTELI;
}

int
fio_telling(void)
{
    return (telltbl[cur_ti].tkidx);
}

void
fio_flush(void)
{
    fflush(outfd);
}
#endif /* OLDFIO */
