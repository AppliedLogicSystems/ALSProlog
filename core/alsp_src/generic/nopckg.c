
/*
 * nopckg.c      -- Empty package
 *
 * Copyright (c) 1989-1993, Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 7/10/89
 */

#include "defs.h"


long *system_pckg = (long *) -1;

#ifdef PACKAGE

/*
 * Declare global package variables that will be used when no package
 * is linked with ALS library "alspro.a".
 */

long *pckg_start_pred = (long *) -1;

char *pckg_toktbl;		/* package token table */
long  pckg_toktbl_size;		/* number of tokens in package token table */

char *pckg_modtbl;		/* package module table */

char *pckg_gvars;		/* package global variables */

#endif /* PACKAGE */
