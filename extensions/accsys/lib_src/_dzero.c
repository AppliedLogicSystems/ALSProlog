/************
 * _dzero.c *
 ************/

/************************************************************
*                                                           *
* Copyright 1989 - 1991, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                                      *
*                                                           *
*************************************************************
*                                                           *
* Published by                                              *
*        Copia International, Inc.                          *
*        Wheaton, Illinois                                  *
*        U. S. A.                                           *
*                                                           *
*************************************************************/

#include <stdlib.h>
#include "d4def.h"

int _DECLARE _zero4(ptr)
	CHAR_PTR ptr;
{
	if (*ptr++) goto nonzero4;
	if (*ptr++) goto nonzero4;
	if (*ptr++) goto nonzero4;
	if (*ptr)   goto nonzero4;

	return(1);

nonzero4:
	return(0);
}


int _DECLARE _zero2(ptr)
	CHAR_PTR ptr;
{
	if (*ptr++) goto nonzero2;
	if (*ptr)   goto nonzero2;

	return(1);

nonzero2:
	return(0);
}
