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

static	int	pload_file	PARAMS(( UCHAR *, int ));

int
pbi_display()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    prolog_display(va1, ta1);

    SUCCEED;
}

int
pbi_get()
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
pbi_get0()
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (w_unify(v, t, (PWord) fio_get0(), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}


static int
pload_file(name, reconbit)
    UCHAR *name;
    int   reconbit;
{
    int   retval;

    dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
    retval = load_file((char *) name, reconbit);
    (void) w_dbprotect(odbrs);
    return retval;
}

int
pbi_load()
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
pbi_nl()
{
    fio_nl();
    SUCCEED;
}


int
pbi_put()
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
pbi_read()
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
pbi_see()
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
pbi_seeing()
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
pbi_seen()
{
    fio_seen();

    SUCCEED;
}


int
pbi_tell()
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
pbi_telling()
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
pbi_told()
{
    fio_told();

    SUCCEED;
}


int
pbi_ttyflush()
{
    fio_flush();

    SUCCEED;
}


int
pbi_write()
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    prolog_write(v, t);

    SUCCEED;
}


int
pbi_writeq()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    prolog_writeq(va1, ta1);

    SUCCEED;
}


#ifdef DynamicForeign

int
pbi_load_foreign()
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

int
pbi_obp_push_stop()
{
#ifdef OBP
    obp_push();
    makeobp = 0;
#endif
    SUCCEED;
}

int
pbi_obp_pop()
{
#ifdef OBP
    obp_pop();
#endif
    SUCCEED;
}

int
pbi_obp_open()
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
pbi_obp_close()
{
#ifdef OBP
    obp_close();
    obp_pop();
#endif
    SUCCEED;
}


int
pbi_obp_load()
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

int
pbi_old_consult()
{				/* old_consult(FileName,NErrs) */
    PWord v1, v2;
    int   t1, t2;
    UCHAR *name;
    int   ec;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (getstring(&name, v1, t1)
/*     && (ec = consult(find_token((char *) name))) != -1) { */
	   && (ec = consult(find_token(name))) != -1) {	
	if (w_unify(v2, t2, ec, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}

int
pbi_save_state_to_file()		/* save_state_to_file */
{
    PWord v1;
    int t1;
    UCHAR *name;

    w_get_An(&v1, &t1, 1);

    if (getstring(&name, v1, t1) && ss_save_state((char *)name))
	SUCCEED;
    else
	FAIL;
}
