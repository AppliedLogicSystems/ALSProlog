/*
 *
 * int.c		-- interrupt helper functions for the SPARC
 *	Copyright (c) 1989-1993 Applied Logic Systems, Inc.
 *
 *
 * Author: Kevin A. Buettner
 * Creation: 5/24/89
 * Revision History:
 *	2/8/91		-- modified for SPARC
 *
 */

#include "defs.h"
#include "wintcode.h"

extern	PWord	int_get_goal_tokid	PARAMS(( Code * ));
extern	PWord	int_get_module		PARAMS(( Code * ));

PWord int_get_goal_tokid(codeaddr)
    Code *codeaddr;
{
    ntbl_entry *ent;

    /*
     * codeaddr will point at the overflow field
     */

    ent = (ntbl_entry *) (((char *) (codeaddr)) - 
			      (int) ((ntbl_entry *) 0)->exec_entry);
    
    return (MMK_FUNCTOR(MFUNCTOR_TOKID(ent->tokid_arity),ent->nargs));
    
}


PWord int_get_module(codeaddr)
    Code *codeaddr;
{
    ntbl_entry *ent;

    ent = (ntbl_entry *) (((char *) (codeaddr)) - 
			      (int) ((ntbl_entry *) 0)->exec_entry);
    
    return (MMK_SYM(ent->modid));
}
