/*=========================================================================*
 |			bos.c   
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems
 |
 |			-- Operating system interface.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | Revision History: (fixes, not addition of new builtins)
 | 06/28/85 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K. Buettner -- IBM PC conversion
 | 10/26/94 - C. Houpt -- Added UCHAR* casts for various calls.
 *=========================================================================*/

#include "defs.h"

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __GO32__
#include <process.h>
#endif

int
pbi_access()
{				/* $access(path,mode) */
    PWord v1, v2;
    int   t1, t2;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t2 == WTP_INTEGER &&
	v2 >= 0 && v2 <= 4 &&
	getstring(&str, v1, t1) &&
	access((char *) str, (int) v2) != -1)
	SUCCEED;
    else
	FAIL;

}

int
pbi_chdir()
{
    PWord v1;
    int   t1;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);

    if (getstring(&str, v1, t1) && chdir((char *) str) == 0)
	SUCCEED;
    else
	FAIL;
}


int
pbi_getenv()
{
    PWord v1, v2;
    PWord slv;
    int   t1, t2;
    int   slt;
    char *es;
    UCHAR *b;


    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (!getstring(&b, v1, t1) ||
	((es = getenv((char *) b)) == (char *) 0))
	FAIL;

    w_mk_uia(&slv, &slt, (UCHAR *)es);

    if (w_unify(v2, t2, slv, slt))
	SUCCEED;
    else
	FAIL;
}

int
pbi_system()
{
    PWord v;
    int   t;
    UCHAR *str;

    w_get_An(&v, &t, 1);
    if (getstring(&str, v, t)
	|| (t == WTP_LIST 
	    && list_to_string((str = (UCHAR *) wm_H + 1), v, wm_normal - 256))) {
#if defined(DOS) || defined(__GO32__)
      char *cp;
      int switching=1, switched=0;
      for (cp=str; *cp; cp++)
      {
	switch (*cp)
	{
	case ' ':
	case '\t':
	case '\\':
	  if (switched)
	    switching = 0;
	  break;
	case '>':
	case '<':
	case '|':
	  switching = 1;
	  switched = 0;
	  break;
	case '/':
	  if (switching)
	    *cp = '\\';
	default:
	  switched = 1;
	  break;
	}
      }
#endif
	system((char *) str);
	SUCCEED;
    }

    else
	FAIL;
}


/*
 * ptmpnam calls either tmpnam or tempnam to obtain the name of a temporary
 * file.  tmpnam is pretty lame, but unfortunately tempnam (note the 'e') is
 * not universally available.  So we will use HAVE_TEMPNAM to control this.
 *
 * Kev's note:  I first put this into fsunix.c since it seemed to be the
 * most appropriate place for it.  But on further reflection, I realized
 * that Unix Section 3 system calls (library functions) are universally
 * available across platforms by virtue of being part of the C Library.
 *
 * Putting it here will prevent needless duplication of code among the
 * fs*.c files.  I think that our philosophy should be to put only
 * *REALLY* system dependent code into the fs*.c files.
 *
 */

int
pbi_tmpnam()
{
    PWord v1, vtn;
    int t1, ttn;
    char *s;

    w_get_An(&v1, &t1, 1);
#ifdef HAVE_TEMPNAM
    s = tempnam(0,"apt");
#else
    s = tmpnam(0);
#endif /* HAVE_TEMPNAM */
    w_mk_uia(&vtn, &ttn, (UCHAR *)s);

#ifdef HAVE_TEMPNAM
    free(s);
#endif /* HAVE_TEMPNAM */

    if (w_unify(v1, t1, vtn, ttn))
	SUCCEED;
    else
	FAIL;
}


int
pbi_protect_bottom_stack_page()
{				/* $protect_bottom_stack_page */
    protect_bottom_stack_page();
    SUCCEED;
}


extern char imagedir[];
extern char imagename[];
int
pbi_get_image_dir_and_name()
{
    PWord v1, v2, vd, vn;
    int t1, t2, td, tn;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_mk_uia(&vd, &td, (UCHAR *)imagedir);
    w_mk_uia(&vn, &tn, (UCHAR *)imagename);

    if (w_unify(v1, t1, vd, td) && w_unify(v2, t2, vn, tn))
	SUCCEED;
    else
	FAIL;
}
