/*=================================================================*
 |			debugsys.c                
 |      Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |			-- system debugging machinery
 |
 | Authors(s):	Everyone
 | Editor:		Ken Bowen
 | Date:		6/26/96
 |	
 |	
 |	Plan & philosophy:
 |	A.  Any and all "interior" or system-level debugging machinery
 |		is to be controlled by this overall mechanism;
 |	B.	All functions which provide system-level debugging support
 |		are to be defined in this file;
 |	C.	All code which invokes these functions (in other files) is
 |		to be appropriately wrappered with #ifdef DEBUGSYS
 |	D.	Fine-grained control of the system-level debugging is via
 |		use of the boolean array "debug_system"
 |	E.	There is an enumeration type which provides symbolic names
 |		for the debugging features corresponding to the slots in
 |		the boolean array "debug_system"
 |	F.	Thus, roughly, all access to system-level debugging should
 |		look like this:
 |
 |		#ifdef DEBUGSYS
 |		if (debug_system[...]) { ....... }
 |		#endif * DEBUGSYS *
 |	G.	The file debugsys.h provides the externs for the functions
 |		defined in this file, and should be included in any file
 |		which invokes one of these functions
 *=================================================================*/
#include "defs.h"

#ifdef DEBUGSYS

	/****** The debugging features enumeration type: 
	 ---  defined in debugsys.h
typedef enum {
	GC_BEEP,			* gcbeep (0) *
	MAX_DEBUG_FEATS
	} debug_feats;
	************/

	/* The debugging control boolean array, all init'd to 0: */
int debug_system[MAX_DEBUG_FEATS] = {0};

	/*-------------------------------------------------------
	 |	For access to debug_system[] from prolog:
	 *------------------------------------------------------*/
int pbi_toggle_debug_system	( void );

int
pbi_toggle_debug_system()
{
    PWord feat;
    int   feat_t;

    w_get_An(&feat, &feat_t, 1);

	if (feat_t != WTP_INTEGER)
		FAIL;
	else if ((0 <= feat) && (feat <= MAX_DEBUG_FEATS)) {
		debug_system[feat] = ~debug_system[feat];
		SUCCEED;
	}
	else
		FAIL;

}





	/*-------------------------------------------------------
	 |		GARBAGE COLLECTION ROUTINES
	 *------------------------------------------------------*/

void print_chpts (register long *);

void
print_chpts(register long *b)
/*    register long *b;   */
{
    while (b) {
	printf("%lx:%10lx%10lx%10lx%10lx\n",
	       (long) b,
	       (long) chpt_NextClause(b),
	       (long) chpt_HB(b),
	       (long) chpt_SPB(b),
	       (long) chpt_B(b));
	b = chpt_B(b);
    }
}
#endif /* DEBUGSYS */
