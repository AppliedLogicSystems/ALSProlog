/*
 * alssig.h		-- special als signal numbers
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *	Copyright (c) 1993 Motorola, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 5/20/93
 * Revision History:
 *
 * Description:
 *	This file contains the signal numbers for internally generated 
 * signals.
 *
 * ALSSIG_REISS_CNTRLC		-- a control-c needs to be reissued
 *				   I don't understand this one.  We should
 *				   get rid of it someday, but until I
 *				   understand what it does, I will leave
 *				   it alone. 		-- Kev
 * ALSSIG_STACK_OVERFLOW	-- a (prolog) stack overflow has occurred
 * ALSSIG_LIBLOAD		-- a library needs to be loaded
 * ALSSIG_HEAP_OVERFLOW		-- the (prolog) heap has overflowed
 * ALSSIG_ERROR			-- builtin should throw error
 * ALSSIG_UNDEFINED_PRED	-- an undefined predicate was called
 */

#ifndef _ALSSIG_H_INCLUDED_
#define _ALSSIG_H_INCLUDED_ 1

/*
 * ALS Signal Numbers
 */

#define	ALSSIG_REISS_CNTRLC	64
#define ALSSIG_STACK_OVERFLOW	65
#define	ALSSIG_LIBLOAD		66
#define ALSSIG_HEAP_OVERFLOW	67
#define ALSSIG_ERROR		68
#define ALSSIG_UNDEFINED_PRED	69

#endif /* _ALSSIG_H_INCLUDED_ */
