/*
 * int.c		-- interrupt helper functions for the 88k
 *	Copyright (c) 1989 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 12/15/89
 * Revision History:
 *
 */

#include "defs.h"
#include "wintcode.h"

extern	PWord	int_get_goal_tokid	PARAMS(( char * ));
extern	PWord	int_get_module		PARAMS(( char * ));

/*
 * int_get_goal_tokid is used to extract a fixed up version of the token id
 * and arity from the overflow return address passed to it.
 */


PWord int_get_goal_tokid(codeaddr)
    char *codeaddr;
{
    ntbl_entry *ent;
    ent = (ntbl_entry *) 
	(codeaddr - (int) (char *) (((ntbl_entry *) 0)->overflow+1));
    
    return (MMK_FUNCTOR(MFUNCTOR_TOKID(ent->tokid_arity),ent->nargs));
}


PWord int_get_module(codeaddr)
    char *codeaddr;
{
    ntbl_entry *ent;
    ent = (ntbl_entry *) 
	(codeaddr - (int) (char *) (((ntbl_entry *) 0)->overflow+1));
    
    return (MMK_SYM(ent->modid));
}
