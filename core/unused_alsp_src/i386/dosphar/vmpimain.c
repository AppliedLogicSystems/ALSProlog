/*
 *	vmpimain.c		
 *	Copyright (c) 1988-92 Applied Logic Systems, Inc.
 *
 *| Default PI_main to use when the user doesn't want
 *| to build her/his own foreign interface.
 *| (in PharLap Environment by using CMS Virtual Memory Manager)
 *
 * Author:  Ilyas Cicekli 
 * Created: 03/13/1992
 * Revision History:
 *	Revised: mm/dd/yy	Who		Why
 */

#include "config.h"
#include "alspi.h"

PI_main_init()
{
	PI_vminit();
    return(1);
}

PI_main()
{
	PI_toplevel();
}

PI_App_init()
{
  /* 
   * To change the window system to something other than the default
   * set WinsTypeStr to one of MOTIF_WIN_STR, OL_WIN_STR,
   * DEC_WIN_STR, NEXT_WIN_STR, DOS_WIN_STR or NO_WIN_STR
   */

  return(1);
}

