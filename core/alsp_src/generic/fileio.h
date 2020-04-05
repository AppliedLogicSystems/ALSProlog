/*===================================================================*
 |		fileio.h
 |	Copyright (c) 1985 by Kevin A. Buettner
 |	Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 | 
 |		-- include file for prolog file operations
 | 
 | Author:  Kevin A. Buettner
 | Creation: 11/24/84
 | 07/12/93,  Kev         -- Phasing out this package
 *===================================================================*/
#include <stdio.h>
#include "lexan.h"

#define MAXSEE  4		/* decreased from 16 to 4 */
#define MAXTELL 2		/* decreased from 16 to 2 */
#define USRSEEI 0
#define USRTELI 0

#define SEEBFSZ 256             /* decreased from 1024 to 256 by kev */

typedef struct {
            int inuse;		/* indicates that the slot is in use */
            int tkidx;		/* index into token table of file name */
            FILE *fd;		/* file descriptor              */
            long tok;		/* current token read           */
            int toktype;	/* type of current token        */
            int linenumber;	/* current line number in file  */
	    int errcount;	/* Number of errors seen in this file */
            lxi_but lb;		/* buffer descriptor for lexical analyzer */
            char buffer[SEEBFSZ];	/* line buffer          */
         } sefldsc;

typedef struct {
            int inuse;		/* in use flag                  */
            int tkidx;		/* token table index            */
            FILE *fd;		/* file descriptor              */
         } tlfldsc;

#define outfd           (telltbl[cur_ti].fd)

extern sefldsc  seetbl[];
extern tlfldsc  telltbl[];
extern int      cur_si;
extern int      cur_ti;

extern long curtok;
extern int curtkty;
extern FILE *curfd;
extern int linenum;

typedef struct {
            const char *pprompt;	/* primary prompt string        */
            const char *sprompt;	/* secondary prompt string      */
         } pptinfo;

extern const char *curprompt;
extern pptinfo prompts[];

#define PMPT_QUERY   0
#define PMPT_CONSULT 1
#define PMPT_READ    2

extern	void	fio_init	(void);
extern	int	fio_see		(int);
extern	void	fio_seen	(void);
extern	int	fio_seeing	(void);
extern	int	fio_get0	(void);
extern	int	fio_get		(void);
extern	void	fio_put		(int);
extern	void	fio_nl		(void);
extern	int	fio_tell	(int);
extern	void	fio_told	(void);
extern	int	fio_telling	(void);
extern	void	fio_flush	(void);
