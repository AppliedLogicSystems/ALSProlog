/*
 * alsglue.c		-- provides "glue" between dynamically loaded
 *			   shared objects and the rest of ALS-Prolog.
 *
 *	Copyright (c) 1994 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Created: 4/19/94
 */

#include "defs.h"

/*
 * The following function should never be called, but if it is, we want
 * to be sure that nothing bad will happen.
 */

static int canthappen = 0;

void
PI_alsglue()
{
    if (canthappen) {
	(void) PI_forceuia( (PWord *) 0, (int *) 0 );
	(void) PI_getan( (PWord *) 0, (int *) 0, (int) 0 );
	(void) PI_getargn( (PWord *) 0, (int *) 0, (PWord) 0, (int) 0 );
	(void) PI_gethead( (PWord *) 0, (int *) 0, (PWord) 0 );
	(void) PI_gettail( (PWord *) 0, (int *) 0, (PWord) 0 );
	(void) PI_getdouble( (double *) 0, (PWord) 0 );
	(void) PI_getstruct( (PWord *) 0, (int *) 0, (PWord) 0 );
	(void) PI_getsymname( (char *) 0, (PWord) 0, (int) 0 );
	(void) PI_getuianame( (char *) 0, (PWord) 0, (int) 0 );
	(void) PI_getuiasize( (PWord) 0, (int *) 0 );
	(void) PI_makedouble( (PWord *) 0, (int *) 0, (double) 0 );
	(void) PI_makelist( (PWord *) 0, (int *) 0 );
	(void) PI_makestruct( (PWord *) 0, (int *) 0, (PWord) 0, (int) 0 );
	(void) PI_makesym( (PWord *) 0, (int *) 0, (char *) 0 );
	(void) PI_makeuia( (PWord *) 0, (int *) 0, (char *) 0 );
	(void) PI_allocuia( (PWord *) 0, (int *) 0, (int) 0 );
	(void) PI_printf( 0 );
	(void) PI_aprintf( 0 );
	(void) PI_rungoal( (PWord) 0, (PWord) 0, (int) 0 );
	(void) PI_rungoal_with_update( (PWord) 0, (PWord *) 0, (int *) 0 );
	(void) PI_unify( (PWord) 0 , (int) 0, (PWord) 0 , (int) 0 );
	(void) PrologInit( (PSTRUCT *) 0 );
	(void) PI_shutdown();
	(void) PI_toplevel();
	(void) PI_prolog_init( (char *) 0, (int) 0, (char **) 0 );
	(void) PI_app_printf( 0 );
    }
}
