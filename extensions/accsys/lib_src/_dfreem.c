/************
* _dfreem.c *
*************/

/*****************************************************
*                                                    *
* Copyright 1989, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                               *
*                                                    *
******************************************************
*                                                    *
* Published by                                       *
*        Copia International, Inc.                   *
*        Wheaton, Illinois                           *
*        U. S. A.                                    *
*                                                    *
******************************************************/
 

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"


void _DECLARE _dfredm(nodeptr,count)  /* d_report arrangement: N/A */
        PTBLNOD  nodeptr;
        int     count;
{
   while (count-- > 0)
   {
        _ACfree(nodeptr->nodebuf);
        nodeptr--;
   }
} /* end of _dfredm() */


void _DECLARE _dfrexm(nodeptr,count)  /* d_report arrangement: N/A */
        PXDXNOD  nodeptr;
        int      count;
{
   while (count-- > 0)
   {
        _ACfree(nodeptr->nodebuf);
        nodeptr--;
   }

} /* end of _dfrexm() */
