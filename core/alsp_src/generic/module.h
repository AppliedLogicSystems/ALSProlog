/*=========================================================*
 |			module.h                     
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 |
 |			-- module management include file
 |
 | Author:  Kevin A. Buettner
 | Creation: 6/21/85
 | Revision History:
 | 01/15/86 - K.Buettner -- IBM PC port
 | 08/14/86 - K.Buettner -- Sun Port
 | 10/05/88 - K.Hughes -- Changed all references to modules outside 
 |					of module.h to use their atom name token IDs.
 *=========================================================*/
#ifndef _MODULE_H_INCLUDED_
#define _MODULE_H_INCLUDED_ 1

#include "wintcode.h"

#define cur_mod (*top_module)		/* id of current module (used
					   					by parser and code generator) */

/*//extern int *top_module;*/				/* pointer to top of module stack */
/*//extern int *top_clausegroup;*/

#define MODULE_GLOBAL	TK_USER		/* id of the global module      */
#define MODULE_BUILTINS TK_BUILTINS	/* id of the builtins module	*/

		/* Maximum number of modules */
#ifdef KERNAL
#define NMODULES 32
#else
#define NMODULES 256
#endif /* KERNAL */

		/* Total number of use definitions permissible */
#ifdef KERNAL
#define NUSEDEFS 512
#else
#define NUSEDEFS 4096
#endif /* KERNAL */

		/* Maximum number of default use entries */
#define MAXDEFUSES 100

		/* Maximum number of procedures to be automatically entered in a module at
   				module initialization */
#ifdef KERNAL
#define MAXDEFPROCS 100
#else
#define MAXDEFPROCS 400
#endif /* KERNAL */

		/* Maximum module nesting permitted */
#define MAXMODNESTING 30

#define mod_adduse(module,use)	adduse(hpe, mod_id(hpe, module),use)

#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif

extern	Code *	resolve_reference  (PE, ntbl_entry *);

#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif

extern	ntbl_entry * resolve_ref  (PE, PWord, PWord, int);

#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif

extern	Code *	call_resolve_reference  (PE, PWord, PWord, int, int);

#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif

extern	int		next_module	 (PE, int, PWord *, int *, PWord *, int *);
extern	int		mod_id		 (PE, int);
extern	int		modprobe_id	 (PE, PWord);
extern	void	new_mod		 (PE, PWord);
extern	void	end_mod		 (PE);
extern	void	push_clausegroup  (PE, int);
extern	int		pop_clausegroup	 (PE);
extern	void	add_default_use	 (PE, int);
extern	void	add_default_proc  (PE, PWord, int);
extern	void	adduse		 (PE, int, int);
extern	void	export_pred	 (PE, PWord, PWord, int);
extern	void	createModuleClosureProcedure  (PE, PWord, int, PWord);
extern  int		createModCloseProc	 (PE, int, PWord, int, PWord);
extern	void	module_init	 (PE);

#endif /* _MODULE_H_INCLUDED_ */
