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

#ifdef MacOS
#include <Errors.h>

/* From MoreFiles - from Macintosh Sample Code library. */
#include <FileCopy.h>
#endif

#ifdef MSWin32
#include <windows.h>
#include "fswin32.h"
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
	*str != 0 &&	/* This check is to work around a bug in
			   SunOS 4.1.3 (and GUSI) which causes access() to
			   succeed on empty strings. */
	access((char *) str, (int) v2) != -1)
	SUCCEED;
    else
	FAIL;

}

#ifdef OSACCESS
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
#endif /* OSACCESS */

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


#ifdef OSACCESS
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
pbi_tmpnam()
{
    PWord v1, vtn;
    int t1, ttn;
    char *s;

    w_get_An(&v1, &t1, 1);
#ifdef HAVE_TEMPNAM
    s = tempnam(0,"pt");
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
#endif /* OSACCESS */


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

int pbi_crypt(void)
{
#if defined(UNIX) && defined(HAVE_UNISTD_H) && !defined( __GO32__)
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

	ss = crypt(b1, b2);
    w_mk_uia(&u, &ut, (UCHAR *)ss);

    if (w_unify(v3, t3, u, ut))
		SUCCEED;
    else
		FAIL;

#else
	FAIL;
#endif
}

#ifdef MacOS
static unsigned char *c2pstrcpy(unsigned char *ps, const char *cs)
{
	size_t l = strlen(cs);
	if (l > 255) l = 255;
	ps[0] = l;
	memcpy(ps+1, cs, l);
	
	return ps;
}
#endif

static int copy_file(const char *from_file, const char *to_file)
{
#if defined(MacOS)
    Str255 pfrom_file, pto_file;
    FSSpec from_file_spec, to_file_spec, to_dir_spec;
    char cwd[256];
    Str255 pcwd;
    OSErr err;
    CInfoPBRec catInfo; 
    c2pstrcpy(pfrom_file, from_file);
    c2pstrcpy(pto_file, to_file);
    
	getcwd(cwd, 255);
	c2pstrcpy(pcwd, cwd);
	
	catInfo.dirInfo.ioCompletion = NULL;
	catInfo.dirInfo.ioNamePtr = pcwd;
	catInfo.dirInfo.ioVRefNum = 0;
	catInfo.dirInfo.ioFDirIndex = 0;
	catInfo.dirInfo.ioDrDirID = 0;
	err = PBGetCatInfoSync(&catInfo);
	if (err != noErr) return 0;
		
    err = FSMakeFSSpec(catInfo.dirInfo.ioVRefNum, catInfo.dirInfo.ioDrDirID, pfrom_file, &from_file_spec);
    if (err != noErr) return 0;

    err = FSMakeFSSpec(catInfo.dirInfo.ioVRefNum, catInfo.dirInfo.ioDrDirID, pto_file, &to_file_spec);
    if (err != noErr && err != fnfErr) return 0;
    
    err = FSMakeFSSpec(to_file_spec.vRefNum, to_file_spec.parID, "\p", &to_dir_spec);
    if (err != noErr) return 0;
   
    err = FSpFileCopy(&from_file_spec, &to_dir_spec, to_file_spec.name, NULL, 0, 0);
    
    if (err == noErr) return 1;
    else return 0;

#elif defined(MSWin32)

    if (CopyFile(from_file, to_file, TRUE)) return 1;
    else return 0;

#elif defined(unix)
    /* FIX ME: This should really be done directly as on the Mac and Win32.
               If cp is not available, this will fail.
               I wonder if there are any libraries out there that can handle
               all permisions/dates/symbolic paths/etc? */

    char *copy_command = malloc(strlen(from_file) + strlen(to_file) + 5);
    int result;
    
    sprintf(copy_command, "cp %s %s", from_file, to_file);

    result = system(copy_command);
    
    free(copy_command); 
    
    return (result == 0);
#else
#error
#endif
}


int pbi_copy_file(void)
{
    PWord v1, v2;
    int   t1, t2, success;
    char *from_file, *to_file;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (!getstring(&from_file, v1, t1)
        || !getstring(&to_file, v2, t2))
	FAIL;
    if (copy_file(from_file, to_file)) SUCCEED;
	else FAIL;
}
