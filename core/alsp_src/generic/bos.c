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
#ifdef UNIX_IRIX
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
pbi_access(PE)
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
pbi_chdir(PE)
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
pbi_getenv(PE)
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
pbi_get_user_home(PE)
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
pbi_system(PE)
{
    PWord v;
    int   t;
    UCHAR *str;

    w_get_An(&v, &t, 1);
    if (getstring(&str, v, t)
	|| (t == WTP_LIST 
	    && list_to_string(hpe, (str = (UCHAR *) wm_H + 1), v, wm_normal - 256))) {
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
pbi_tmpnam(PE)
{
    PWord v1, vtn;
    int t1, ttn;
    char *s;

    w_get_An(&v1, &t1, 1);
    s = tmpnam(NULL);
    w_mk_uia(&vtn, &ttn, (UCHAR *)s);

    free(s);

    if (w_unify(v1, t1, vtn, ttn))
	SUCCEED;
    else
	FAIL;
}
#endif /* OSACCESS */


int
pbi_protect_bottom_stack_page(PE)
{				/* $protect_bottom_stack_page */
#if 0
    protect_bottom_stack_page();
#endif
	protect_stack(hpe);
    SUCCEED;
}

// THREADED
int   argcount = 0;	/* global copies of main's argc and argv */
char **argvector = NULL;

int pbi_command_line(PE)
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

#if defined(UNIX) && !defined(UNIX_CYGWIN32) /* _XOPEN_CRYPT */
int pbi_crypt(PE)
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

	ss = crypt(b1, b2);
    w_mk_uia(&u, &ut, (UCHAR *)ss);

    if (w_unify(v3, t3, u, ut))
		SUCCEED;
    else
		FAIL;
}
#endif

int pbi_copy_file(PE)
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

int pbi_get_current_image(PE)
{
	PWord v1, s;
	int t1, st;
	
	PI_getan(&v1, &t1, 1);
	
	PI_makeuia(&s, &st, executable_path);
	
	return PI_unify(v1, t1, s, st);
}

typedef struct {
  prolog_engine *hpe;
  PWord goal; int goalt;
  int globals;
} thread_info;

#include <process.h>

static void *prolog_thread(void *data)
{
  thread_info *info = data;
  PWord builtinsmod, init, usermod;
  int builtinsmod_t, init_t, usermod_t;
	int i;

	for (i = 0; i < info->globals; i++)  gv_alloc(info->hpe);
  PI_makesym_pe(info->hpe, &builtinsmod, &builtinsmod_t, "builtins");
  PI_makesym_pe(info->hpe, &init, &init_t, "thread_init");
//  PI_makesym_pe(info->hpe, &builtinsmod, &builtinsmod_t, "sio");
//  PI_makesym_pe(info->hpe, &init, &init_t, "sio_pckg_init");
  PI_makesym_pe(info->hpe, &usermod, &usermod_t, "user");

  PI_rungoal_pe(info->hpe, builtinsmod, init, PI_SYM);
  PI_rungoal_pe(info->hpe, usermod,  info->goal, info->goalt);

delete_prolog_engine(info->hpe);

  free(info);

	_endthreadex(0);
  return NULL;
}

#ifdef UNIX
#include <pthread.h>
#endif

void simple_copy(prolog_engine *pa, PWord *a, int *at,
		    prolog_engine *pb, PWord b, int bt);


int pbi_new_thread(PE)
{
	PWord v1, v2;
	int t1, t2;
	prolog_engine *hpe2;
	int id = 0;
	thread_info *info;
	 
	PI_getan(&v1, &t1, 1);
	PI_getan(&v2, &t2, 2);
	
	hpe2 = malloc(sizeof(prolog_engine));
	memset(hpe2, 0x00, sizeof(*hpe2));

	hpe2->db = hpe->db;
	init_prolog_engine(hpe2,  DEFAULT_HEAP_SIZE, DEFAULT_STACK_SIZE);

	info = malloc(sizeof(*info));
	info->hpe = hpe2;
	simple_copy(hpe2, &info->goal, &info->goalt, hpe, v1, t1);
	info->globals = wm_gvbase - wm_trailbase;

#ifdef UNIX
	pthread_create((pthread_t *)&id, NULL, prolog_thread, info);
#elif macintosh
	id = CreateThread(hpe, prolog_thread, info);
#elif WIN32
//	CreateThread(NULL, 0, prolog_thread, info, 0, (unsigned long *)&id);
	{ HANDLE h;
	h = (HANDLE)_beginthreadex(NULL, 0, prolog_thread, info, 0, (unsigned int *)&id);
	CloseHandle(h);
	}
#endif

	return PI_unify(v2, t2, id, PI_INT);
}


struct _sync_record {
  int thread;
  prolog_engine *pe;
  PWord term;
  int term_t;
  int who;
#ifdef UNIX
  pthread_cond_t cond;
#elif macintosh
  condition cond;
#elif WIN32
  
#endif 
  struct _sync_record *next;
};

typedef struct _sync_record sync_record;

#ifdef UNIX
pthread_mutex_t sync_mutex;
#elif macintosh
#elif WIN32
CRITICAL_SECTION sync_mutex;
#endif
sync_record *sync_stack = NULL;


void thread_init(void)
{
#ifdef UNIX
  pthread_mutex_init(&sync_mutex, NULL);
#elif macintosh
#elif WIN32
	InitializeCriticalSection(&sync_mutex);
#endif
}


int pbi_receive(PE)
{
  PWord thread, term, copy;
  int thread_t, term_t, copyt;
  sync_record sync;

  PI_getan(&thread, &thread_t, 1);
  PI_getan(&term, &term_t, 2);

#ifdef UNIX	
  sync.thread = pthread_self();
#elif WIN32
	sync.thread = (int)GetCurrentThread();
#endif
  sync.pe = hpe;
  sync.term = term;
  sync.term_t = term_t;
#ifdef UNIX
  pthread_mutex_lock(&sync_mutex);
#elif WIN32
	EnterCriticalSection(&sync_mutex);
#endif
  sync.next = sync_stack;
  sync_stack = &sync;
#ifdef UNIX
  pthread_cond_init(&sync.cond, NULL);
  pthread_cond_wait(&sync.cond, &sync_mutex);
  pthread_mutex_unlock(&sync_mutex);
  pthread_cond_destroy(&sync.cond);
#elif WIN32
	LeaveCriticalSection(&sync_mutex);
	
#endif
  
  simple_copy(hpe, &copy, &copyt, sync.pe, sync.term, sync.term_t);

  return PI_unify(thread, thread_t, sync.who, PI_INT) &&
    PI_unify(term, term_t, copy, copyt);
}

int pbi_send(PE)
{
  PWord thread, term, copy;
  int thread_t, term_t, copyt;
  sync_record *s, *p;
  int r;

  PI_getan(&thread, &thread_t, 1);
  PI_getan(&term, &term_t, 2);

#ifdef UNIX
  pthread_mutex_lock(&sync_mutex);
#elif WIN32
	EnterCriticalSection(&sync_mutex);
#endif
  for(p = NULL, s = sync_stack; s ; p = s, s = s->next)
    if (s->thread == thread) break;
  
  if (s) {
    simple_copy(hpe, &copy, &copyt, s->pe, s->term, s->term_t);
    r = PI_unify(term, term_t, copy, copyt);
    
    if (r) {
      s->pe = hpe;
      s->term = term;
      s->term_t = term_t;
#ifdef UNIX
      s->who= pthread_self();
#elif WIN32
	s->who = (int)GetCurrentThread();
#endif
#ifdef UNIX
      pthread_cond_signal(&s->cond);
#elif WIN32
	//Event
#endif
      if (p) p->next = s->next;
      else sync_stack = s->next;
    } else s = NULL;
  }
#ifdef UNIX  
  pthread_mutex_unlock(&sync_mutex);
#else
	LeaveCriticalSection(&sync_mutex);
#endif
  if (s) return r; else PI_FAIL;
}

/* Copy term b to term a */
/* This must be expanded to do full copying with loops */
void simple_copy(prolog_engine *pa, PWord *a, int *at,
		    prolog_engine *pb, PWord b, int bt)
{
  switch(bt) {
  case PI_VAR:
    *a = (PWord)a; *at = PI_VAR;
  case PI_INT:
    *a = b; *at = bt;
    break;
  case PI_SYM:
    if (pa->db == pb->db) {
      *a = b; *at = bt;
    } else {
      PI_makesym_pe(pa, a, at, PI_getsymname_pe(pb, NULL, b, bt));
    }
    break;
  case PI_UIA:
    PI_makeuia_pe(pa, a, at, PI_getuianame_pe(pb, NULL, b, bt));
    break;
  case PI_LIST: 
    {
      PWord bcar, bcdr, acar, acdr, a2car, a2cdr;
      int bcart, bcdrt, acart, acdrt, a2cart, a2cdrt;

      PI_makelist_pe(pa, a, at);
      PI_gethead_pe(pb, &bcar, &bcart, b);
      PI_gettail_pe(pb, &bcdr, &bcdrt, b);
      simple_copy(pa, &acar, &acart, pb, bcar, bcart);
      simple_copy(pa, &acdr, &acdrt, pb, bcdr, bcdrt);
      PI_gethead_pe(pa, &a2car, &a2cart, *a);
      PI_gettail_pe(pa, &a2cdr, &a2cdrt, *a);
      if (acart != PI_VAR) PI_unify_pe(pa, a2car, a2cdr, acar, acart);
      if (acdrt != PI_VAR) PI_unify_pe(pa, a2cdr, a2cdrt, acdr, acdrt);
    }
    break;
  case PI_STRUCT:
    {
      PWord functor;
      int arity, i;

      PI_getstruct_pe(pb, &functor, &arity, b);
      PI_makestruct_pe(pa, a, at, functor, arity);
      for (i = 1; i <= arity; i++) {
	PWord aarg, barg, copy;
	int aargt, bargt, copyt;

        PI_getargn_pe(pa, &aarg, &aargt, *a, i);
	PI_getargn_pe(pb, &barg, &bargt, b, i);
	simple_copy(pa, &copy, &copyt, pb, barg, bargt);
	if (copyt != PI_VAR) PI_unify_pe(pa, aarg, aargt, copy, copyt);
      }
    }
    break;
  case PI_DOUBLE:
    {
      double d;
      PI_getdouble_pe(pb, &d, b);
      PI_makedouble_pe(pa, a, at, d);
    }
    break;
  }
}

// THREADED - complete all of the above