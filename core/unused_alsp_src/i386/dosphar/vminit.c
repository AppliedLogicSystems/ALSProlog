/*
 *	vminit.c 
 *	Copyright (c) 1989-91 Applied Logic Systems, Inc.
 *
 *| Routines used only in CMS Virtual Memory Environment
 *
 * Author : Ilyas Cicekli
 * Date   :  3/12/1992
 */

#include "config.h"

#include "pckg.h"


#ifdef PACKAGE

/*
 * Unlock packaged Prolog code
 */
void pckg_unlock() 
{
	long *p;
	unsigned long psize;

    for (p=system_pckg; !ENDOF_PCKGLIST(p); p = (long *)PCKG_PREVIOUS_PCKG(p)) {
		psize = (unsigned long)PCKG_CODE_END(p) -
				(unsigned long)PCKG_CODE_START(p);
      	vmunlockr((unsigned long)PCKG_CODE_START(p),psize);
	}
}

PI_vminit() 
{
	pckg_unlock();
	return(0);
}

#else 	/* PACKAGE */

PI_vminit() 
{
	return(0);
}

#endif 	/* PACKAGE */

