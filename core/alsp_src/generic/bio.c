/*=========================================================================*
 |			bio.c   
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems
 |
 |			-- I/O Prolog builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | Revision History:
 | 06/28/85 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K. Buettner -- IBM PC conversion
 | 07/12/39 - P. Raman    -- PBI_NAMEMAX removal
 | 07/23/93 - K. Buettner -- Removal of functions defining I/O preds
 |                                which are not needed any more
 | 0/26/94 - C. Houpt -- Added ifdef OBP around pbi_obp_* routines to allow
 |						debugging without OBP.
 *=========================================================================*/
#include "defs.h"
#include "icom.h"
#include "wintcode.h"

int pbi_save_image_with_state_to_file(void)
{
    PWord v1;
    int t1;
    UCHAR *name;

    w_get_An(&v1, &t1, 1);

    if (getstring(&name, v1, t1) && ss_save_image_with_state((char *)name))
	SUCCEED;
    else
	FAIL;
}

int pbi_attach_state_to_file(void)
{
    PWord v1;
    int t1;
    UCHAR *name;

    w_get_An(&v1, &t1, 1);

    if (getstring(&name, v1, t1) && ss_attach_state_to_file((char *)name))
	SUCCEED;
    else
	FAIL;
}

#if !defined(KERNAL) && !defined(PURE_ANSI)
int
pbi_save_state_to_file(void)		/* save_state_to_file */
{
    PWord v1;
    int t1;
    UCHAR *name;
    w_get_An(&v1, &t1, 1);

    if (getstring(&name, v1, t1) && ss_save_state((char *)name, 0))
	SUCCEED;
    else
	FAIL;
}
#endif /* !defined(KERNAL) && !defined(PURE_ANSI) */

static	int	pload_file	( UCHAR *, int );

static int
pload_file(UCHAR *name, int reconbit)
{
    int   retval;

    dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
    retval = load_file((char *) name, reconbit); 
/*    retval = load_obp((char *) name, reconbit);   */
    (void) w_dbprotect(odbrs);
    return retval;
}

int
pbi_load(void)
{				/* $load(File,Flag) */
    PWord v1, v2;
    int   t1, t2;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t2 == WTP_INTEGER && getstring(&str, v1, t1) && pload_file(str, (int) v2))
	SUCCEED;
    else
	FAIL;
}

int
pbi_nl(void)
{
    fio_nl();
    SUCCEED;
}

int
pbi_ttyflush(void)
{
    fio_flush();

    SUCCEED;
}

static void simple_write(PWord v, int t)
{
    switch (t) {
    case PI_VAR:
    	printf("V%lx", v); 
	break;
    case PI_LIST:
	{
	    PWord list, head, listnil;
	    int listt, headt, listnilt;
	    
	    printf("[");
	    
	    list = v; listt = t;
	    while(1) {
	        PI_gethead(&head, &headt, list);
	        simple_write(head, headt);
	        PI_gettail(&list, &listt, list);
	        if (listt == PI_LIST) printf(",");
	        else break;
	    }
	    
	    PI_makesym(&listnil, &listnilt, "[]");
	    
	    if (!(list == listnil && listt == PI_SYM)) {
		printf(",");
	    	simple_write(list, listt);
	    }
	    
	    printf("]");
	}
	break;
    case PI_STRUCT:
    	{
    	    PWord funcname, a;
    	    int i, arity, at;
	    PI_getstruct(&funcname, &arity, v);
	    printf("%s(", PI_getsymname(NULL, funcname, 0));
	    
	    i = 1;
	    while(1) {
	    	PI_getargn(&a, &at, v, i);
	    	simple_write(a, at);
	    	i++;
	    	if (i <= arity) printf(",");
	    	else break;
	    }
	    printf(")");
	}
	break;
    case PI_SYM:
	printf("%s", PI_getsymname(NULL, v, 0));
	break;
    case PI_INT:
    	printf("%ld", v);
	break;
    case PI_UIA:
	printf("%s", PI_getuianame(NULL, v, 0));
	break;
    case PI_DOUBLE:
	{
	    double d;
	    PI_getdouble(&d, v);
	    printf("%f", d);
	}
	break;
    default:
    	printf("<Unknown Object %lx>", v);
    	break;
    }
}

int pbi_debug(void)
{
    PWord v;
    int   t;

    PI_getan(&v, &t, 1);

    simple_write(v, t);
    printf("\n");
    fflush(stdout);
    SUCCEED;
}

#ifndef KERNAL
int
pbi_write(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    prolog_write(v, t);
    SUCCEED;
}
#endif /* KERNAL */

#ifdef OLDCIO


int
pbi_display(void)
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    prolog_display(va1, ta1);

    SUCCEED;
}

int
pbi_get(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (w_unify(v, t, (PWord) fio_get(), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

int
pbi_get0(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (w_unify(v, t, (PWord) fio_get0(), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}



int
pbi_put(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (t == WTP_INTEGER) {
	fio_put((int) v);
	SUCCEED;
    }
    else
	FAIL;
}


int
pbi_read(void)
{
    PWord a1v, rv;
    int   a1t, rt;

    w_get_An(&a1v, &a1t, 1);

    heap_copy(&rv, &rt, LIST_CAR(prim_read()));

    if (w_unify(rv, rt, a1v, a1t))
	SUCCEED;
    else
	FAIL;
}


int
pbi_see(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (force_uia(&v, &t) && fio_see((int) v))
	SUCCEED;
    else
	FAIL;
}


int
pbi_seeing(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (w_unify(v, t, (PWord) fio_seeing(), WTP_SYMBOL))
	SUCCEED;
    else
	FAIL;
}


int
pbi_seen(void)
{
    fio_seen();

    SUCCEED;
}


int
pbi_tell(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (force_uia(&v, &t) && fio_tell((int) v))
	SUCCEED;
    else
	FAIL;
}


int
pbi_telling(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (w_unify(v, t, (PWord) fio_telling(), WTP_SYMBOL))
	SUCCEED;
    else
	FAIL;
}


int
pbi_told(void)
{
    fio_told();

    SUCCEED;
}



int
pbi_writeq(void)
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    prolog_writeq(va1, ta1);

    SUCCEED;
}
#endif /* OLDCIO */


#ifdef DynamicForeign

int
pbi_load_foreign(void)
{				/* $loadforeign(Name, LibStr, InitFunction) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR *name;
    UCHAR *libstr;
    UCHAR *initfunction;
    dbprot_t odbrs;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    odbrs = w_dbprotect(DBRS_WRITABLE);;
    if (getstring(&name, v1, t1) &&
	getstring(&libstr, v2, t2) &&
	getstring(&initfunction, v3, t3) &&
	load_foreign((char *) name, (char *) libstr, (char *) initfunction)) {

	(void) w_dbprotect(odbrs);
	SUCCEED;
    }
    else {
	(void) w_dbprotect(odbrs);
	FAIL;
    }
}

#endif /* DynamicForeign */

#ifdef SYS_OBP
int
pbi_obp_push_stop(void)
{
#ifdef OBP
    obp_push();
    makeobp = 0;
#endif
    SUCCEED;
}

int
pbi_obp_pop(void)
{
#ifdef OBP
    obp_pop();
#endif
    SUCCEED;
}

int
pbi_obp_open(void)
{
#ifdef OBP
    PWord v1;
    int   t1;
    UCHAR *name;

    w_get_An(&v1, &t1, 1);

    if (getstring(&name, v1, t1)) {
	obp_push();
	makeobp = 1;
	if (obp_open((char *) name))
	    SUCCEED;
	else {
	    obp_pop();
	    FAIL;
	}
    }
    else
	FAIL;
#else
    FAIL;
#endif
}

int
pbi_obp_close(void)
{
#ifdef OBP
    obp_close();
    obp_pop();
#endif
    SUCCEED;
}


int
pbi_obp_load(void)
{				/* obp_load(FileName,Stat) */
    PWord v1, v2;
    int   t1, t2, status;
    UCHAR *name;
#ifdef OBP
    int   old_makeobp = makeobp;
#endif

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (getstring(&name, v1, t1)) {
#ifdef OBP
	dbprot_t odbrs;
	makeobp = 0;		/* don't want to make obp out of loading of
				 		 * one
				 		 */
	odbrs = w_dbprotect(DBRS_WRITABLE);
	status = f_load((char *) name);
	(void) w_dbprotect(odbrs);
	makeobp = old_makeobp;
#else
	status = 0;
#endif
	if (w_unify(v2, t2, status, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}

#endif /* SYS_OBP */


#ifdef OLDCONSULT
int
pbi_old_consult(void)
{				/* old_consult(FileName,NErrs) */
    PWord v1, v2;
    int   t1, t2;
    UCHAR *name;
    int   ec;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (getstring(&name, v1, t1)
	   && (ec = consult(find_token(name))) != -1) {	
	if (w_unify(v2, t2, ec, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}
#endif /* OLDCONSULT */

