/*======================================================================*
 |		wamops.h
 |	Copyright (c) 1990-1995 Applied Logic Systems, Inc.
 *======================================================================*/

#ifndef _WAMOPS_INCLUDED_
#define _WAMOPS_INCLUDED_ 1

#ifdef Bytecode
#undef Threaded
#endif

#ifdef Portable

#define ABMOP(op,p1,p2,p3,p4) op,

enum AbstractMachineOps {
#include "wamops0.h"
    W_NUM_OPS		/* not an opcode, but a convenient 
			 * way to know how many there are */
};

#undef ABMOP

/*---------------------------------------------
 |	Threaded implies Portable, 
 |	but Portable is not necessarily Threaded
 |	(could be Byte)
 *--------------------------------------------*/
#ifdef	Threaded

extern Code wam_instrs[];
#define abinst(op)	wam_instrs[(op)]

#else  /* !Threaded */

#define abinst(op)	((Code ) (op))

#endif   /* Threaded */

#endif /* Portable */

#endif /* _WAMOPS_INCLUDED_ */
