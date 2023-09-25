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
#if defined(UNIX_LINUX) || defined(UNIX_IRIX) || defined(UNIX_CYGWIN32)
/* On Irix, crypt() is not defined in unistd.h */
#include <crypt.h>
#endif
#include <unistd.h>
#endif


#ifdef MacOS
#include <Errors.h>

/* From MoreFiles - from Macintosh Sample Code library. */
#include <FileCopy.h>
#endif

#ifdef MSWin32
#include <windows.h>
#include "fswin32.h"
#endif

#ifdef UNIX
#include <pwd.h>
#include <fcntl.h>
#endif

#ifndef PURE_ANSI
int
pbi_access(void)
{				/* $access(path,mode) */
    PWord v1, v2;
    int   t1, t2;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t2 == WTP_INTEGER &&
	v2 >= 0 && v2 <= 4 &&
	getstring(&str, v1, t1) &&
	*str != 0 &&	/* This check is to work around a bug in
			   SunOS 4.1.3 (and GUSI) which causes access() to
			   succeed on empty strings. */
	access((char *) str, (int) v2) != -1)
	SUCCEED;
    else
	FAIL;

}
#endif /* PURE_ANSI */

#ifdef OSACCESS
#ifndef PURE_ANSI
int
pbi_chdir(void)
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
#endif /* PURE_ANSI */

int
pbi_getenv(void)
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
pbi_get_user_home(void)
{
#ifdef UNIX
    PWord v1, v2, h;
    int   t1, t2, ht;
    const char *name;
    const struct passwd *record;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    if (t1 == PI_SYM) name = PI_getsymname(NULL, v1, 0);
    else if (t1 == PI_UIA) name = PI_getuianame(NULL, v1, 0);
    else PI_FAIL;

    record = getpwnam(name);
    if (!record) PI_FAIL;

    PI_makeuia(&h, &ht, record->pw_dir);
    
    if (PI_unify(v2, t2, h, ht)) PI_SUCCEED;
    else PI_FAIL;
#else
    PI_FAIL;
#endif
}

#endif /* OSACCESS */

int
pbi_system(void)
{
    PWord v;
    int   t;
    UCHAR *str;

    w_get_An(&v, &t, 1);
    if (getstring(&str, v, t)
	|| (t == WTP_LIST 
	    && list_to_string((str = (UCHAR *) wm_H + 1), v, wm_normal - 256))) {
#if defined(DOS) || defined(__GO32__) || defined(OS2)
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
/*
  I think that system should fail or throw an error or something,
  but too much code relies on it always succeeding.
	if (system((char *) str) == 0) SUCCEED;
	else FAIL;
*/
    }

    else
	FAIL;
}


#if 0 // ifdef OSACCESS: tmpnam/mktemp are depricated for security
/*
 * pbi_tmpnam calls either tmpnam or tempnam to obtain the name of a temporary
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
pbi_tmpnam(void)
{
    PWord v1, vtn;
    int t1, ttn;
    char s[PATH_MAX] = "/tmp/tmp.XXXXXXXX";

    w_get_An(&v1, &t1, 1);
    mktemp(s);
    w_mk_uia(&vtn, &ttn, (UCHAR *)s);

    if (w_unify(v1, t1, vtn, ttn))
	SUCCEED;
    else
	FAIL;
}
#endif /* OSACCESS */


int
pbi_protect_bottom_stack_page(void)
{				/* $protect_bottom_stack_page */
#if 0
    protect_bottom_stack_page();
#endif
	protect_stack(&current_engine);
    SUCCEED;
}

int   argcount = 0;	/* global copies of main's argc and argv */
char **argvector = NULL;

int pbi_command_line(void)
{
    PWord v1, list, newlist;
    int   t1, i, listt, newlistt;

    PI_getan(&v1, &t1, 1);

	/* Create the list of arguments from the argv array.
	   The list is generated backwards - this makes list construction
	   easy because arguments are just pushed onto the front of the list.
	   Also "list" always points to a valid list - a useful invariant.
	*/
	for (i = argcount-1, PI_makesym(&list, &listt, "[]");
		      i >= 0; i--, list = newlist, listt = newlistt) {
		PWord head, tail, arg;
		int headt, tailt, argt;
		
		PI_makelist(&newlist, &newlistt);
		PI_gethead(&head, &headt, newlist);
		PI_gettail(&tail, &tailt, newlist);
		
		PI_makeuia(&arg, &argt, argvector[i]);
		if (!PI_unify(head, headt, arg, argt)) PI_FAIL;
		
		if (!PI_unify(tail, tailt, list, listt)) PI_FAIL;
	}
	
	if (!PI_unify(v1, t1, list, listt)) PI_FAIL;
	
	PI_SUCCEED;
}

#if defined(UNIX) /* _XOPEN_CRYPT */
int pbi_crypt(void)
{
    PWord v1, v2, v3, u;
    int   t1, t2, t3, ut;
    UCHAR *b1,*b2;
/*	char ss[36];	*/
	char *ss;	

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (!getstring(&b1, v1, t1) ||
         !getstring(&b2, v2, t2))
	FAIL;

    ss = crypt((char *)b1, (char *)b2);
    w_mk_uia(&u, &ut, (UCHAR *)ss);

    if (w_unify(v3, t3, u, ut))
		SUCCEED;
    else
		FAIL;
}
#endif

int pbi_copy_file(void)
{
    PWord v1, v2;
    int   t1, t2;
    char *from_file, *to_file;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (!getstring((UCHAR **)&from_file, v1, t1)
        || !getstring((UCHAR **)&to_file, v2, t2))
	FAIL;
    if (os_copy_file(from_file, to_file)) SUCCEED;
	else FAIL;
}

int pbi_get_current_image(void)
{
	PWord v1, s;
	int t1, st;
	
	PI_getan(&v1, &t1, 1);
	
	PI_makeuia(&s, &st, executable_path);
	
	return PI_unify(v1, t1, s, st);
}




