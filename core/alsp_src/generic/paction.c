/*
 * paction.c
 *	Copyright (c) 1991-1993 Applied Logic Systems, Inc.
 *
 *| Interface to Highland FLEX license manager,  setting demo expiration dates, etc.
 *
 * Author : Prabhakaran Raman
 * Date   : 11/12/91
 *
 *     FLEX is defined as a compilation flag for machines
 *     that have FLEX license manager.
 *
 *     EXP_DATE is a compilation flag that is set to the expiration time
 *     in seconds. This is used to create a demo tape for machines that
 *     do not have FLEX license manager.
 */

#include <stdio.h>

#include "defs.h"

#ifdef _M88KBCS_TARGET
#define MAXPATHLEN 1024
#endif

#ifdef FLEX
#include <lm_client.h>
#include <lm_code.h>

LM_CODE(code, ENCRYPTION_CODE_1, ENCRYPTION_CODE_2,
	VENDOR_KEY1, VENDOR_KEY2, VENDOR_KEY3);

static char feature[64] = "alspro_";

#endif /* FLEX */

void
paction()
{
#ifdef FLEX
    int   rc;
    float version;

    (void) lm_init(VENDOR_NAME, &code, (LM_HANDLE **) 0);

    strcat(feature, ProcStr);
    strcat(feature, "_");
    strcat(feature, MinorOSStr);

    sscanf(SysVersionNum, "%f", &version);

    /* The version number in license file must be >= second arg.
     * The last arg says to count all license requests from this
     * user on this host as a single license.
     */

    rc = lm_checkout(feature, (double) version, 1, LM_CO_NOWAIT,
		     &code, LM_DUP_USER | LM_DUP_HOST | LM_DUP_DISP);

    if (rc) {
	switch (rc) {
	    case MAXUSERS:
		fprintf(stderr, "Maximum number of users for \"%s\" reached - Try again later\n", feature);
		break;
	    default:
		printf("Checkout of \"%s\" failed\n", feature);
		lm_perror("client");
		break;
	}
	exit(rc);
    }
#else  /* FLEX */

#ifdef EXP_DATE
    if ((unsigned long) time(0) >= EXP_DATE)
	fatal_error(FE_DEMO_LIMIT, 0);
#endif
    return;

#endif /* FLEX */
}
