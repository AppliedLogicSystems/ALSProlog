/*===================================================================*
 |			fatal.c      
 |      Copyright (c) 1990-95 Applied Logic Systems, Inc.
 |
 |			-- fatal error messages and function
 |
 | Creation: 12/17/90
 | Author: Kevin A. Buettner
 *===================================================================*/
#include "defs.h"

static const char *errors[] =
{
  "symbol table full",									/* FE_SYMSPACE */
  "string area for symbol table exhausted",				/* FE_STRSPACE */
  "system ran out of memory while creating .obp file",	/* FE_MAPSYM */
  "PATH environment variable inaccessible or absent",	/* FE_PATH */
  "image directory not found or unreadable",			/* FE_INFND */
  "heap size too small",	/* FE_SMALLHEAP */
  "stack size too small",	/* FE_SMALLSTACK */
  "error allocating heap",	/* FE_BIGHEAP */
  "error allocating stack",	/* FE_BIGSTACK */
  "allocation of heap/stack area resulted in addresses with invalid tags",
														/* FE_TAGERR */
  "error removing indexing block",						/* FE_REMIDX */
  "error opening /dev/zero while allocating heap and stack",	
														/* FE_DEVZERO */
  "unable to continue from prolog stack overflow",		/* FE_STKOVERFLOW */
  "unable to allocate code generation buffer",			/* FE_ICODEBUFINIT */
  "Compilation of a large clause caused overflow of an internal buffer.",
														/* FE_ICODEBUFOVER */
  "Memory exhausted while attempting to allocate procedure entry.",
														/* FE_XMEM_NTBL */
  "Procedure entry table is full.",						/* FE_FULL_NTBL */
  "Memory exhausted while allocating clause space.", 	/* FE_XMEM_CLAUSE */
  "Internal GCMAGIC error (code space gc), addr=%x.", 	/* FE_GCMAGIC_CGC */
  "Internal GCMAGIC error (regular gc), addr=%x.", 		/* FE_GCMAGIC */
  "Module table is full.",								/* FE_FULL_MODTBL */
  "Too many nested modules (Module stack overflow).",	/* FE_OVER_MODSTK */
  "Too many nested consults (Clause group stack overflow).",
														/* FE_OVER_CGSTK */
  "Default use table is full.",		/* FE_FULL_DEFUSES */
  "Default procs table is full.",	/* FE_FULL_DEFPROC */
  "Module use table is full.",		/* FE_FULL_MODUSE */
  "First token ID in package token table doesn't match.",
														/* FE_SYM_PCKG */
  "Internal compiler error number 1.",					/* FE_IN_COMP1 */
  "Heap/choice point stack overflow.",					/* FE_OVER_HEAP */
  "Internal execution error (panic fail).",				/* FE_PANIC_FAIL */
  "Internal execution error (panic continue).",			/* FE_PANIC_CONTINUE */
  "Unrecognized command line switch (%c)",				/* FE_BAD_SWITCH */
  "Numeric argument expected with -%c flag.",			/* FE_BAD_NSWITCH */
  "Error allocating reference count array for file descriptors",
														/* FE_FDREFCNTS */
  "Demonstration time limit has expired.\n",			/* FE_DEMO_LIMIT */
  "Internal error while initializing builtins table.",	/* FE_IN_BLTINIT */
  "Internal error #1 during indexing",					/* FE_IN_INDEX1 */
  "Internal error #1 in packaging code (Unsupported builtin type).",
														/* FE_IN_PCKG1 */
  "Internal error #2 in packaging code (Unsupported builtin type).",
														/* FE_IN_PCKG2 */
  "Internal error #3 in packaging code (Unsupported builtin type).",
														/* FE_IN_PCKG3 */
  "Packaging error (RInfo buffer full).",				/* FE_PCKG_RINFO */
  "Packaging error (Package hash table full).",			/* FE_PCKG_HTBL */
 "Internal error #4 in packaging code (Illegal package symbol entry type).",
														/* FE_IN_PCKG4 */
  "Internal error #5 in packaging code (Illegal package relocation type).",
														/* FE_IN_PCKG5 */
  "Internal error #6 in packaging code (Illegal package relocation type).",
														/* FE_IN_PCKG6 */
  "Internal error #7 in packaging code (Illegal package relocation type).",
														/* FE_IN_PCKG7 */
 "Internal error #8 in packaging code (Illegal package symbol entry type).",
														/* FE_IN_PCKG8 */
  "Procedure entry table is full during package initialization.",
														/* FE_PCKG_NTBL */
  "Internal error #9 in packaging code (Illegal procedure type).",
														/* FE_IN_PCKG9 */
  "Internal error #10 in packaging code (Illegal relocation information type).",
														/* FE_IN_PCKG10 */
  "Internal error #1 in w_get (Unknown constant %x).",	/* FE_IN_WGET1 */
  "Internal error #2 in w_get (Unrecognized type %x).",	/* FE_IN_WGET2 */
  "Internal error in w_install (Unrecognized type %x).",
														/* FE_IN_WINSTALL */
  "Internal error in heap_copy (Inappropriate object).",
														/* FE_IN_HEAPCOPY */
  "Internal error in termcmp (Illegal type).",			/* FE_IN_TERMCMP */
  "Memory exhausted while allocating icode capture space.",
														/* FE_CAPTURE_INC */
  "Abolish of %s failed.",								/* FE_ABOLISH_FAIL */
  "Assert of sys_searchdir(\"%s\") failed.",			/* FE_ASSERT_SSD */
  "Assert of searchdir(\"%s\") failed.",				/* FE_ASSERT_SD  */
  "Assert of als_system failed.",						/* FE_ASSERT_SYS */
  "Assert of command line failed.",						/* FE_ASSERT_COM */
  "Unable to get current working directory pathname.",	/* FE_GETCWD */
  "Error in initial memory allocation.",				/* FE_ALS_MEM_INIT */
  "Error initializing code space.",						/* FE_CODESPACE_INIT */
  "Error initializing module system.",					/* FE_MODULE_INIT */
  "Error initializing symbol table.",					/* FE_SYMTAB_INIT */
  "mem.c: AM_MAXGLOBALS constant not large enough.", 	/* FE_SS_MAXGLOBALS */
  "Saved code state file does not correspond to image.", /* FE_SS_INTEGRITY */
  "Can not open saved code state file \"%s\".",			/* FE_SS_OPENERR */
  "Error in dynamic foreign interface memory allocation.",
														/* FE_SS_FMALLOC */
  "Error in parsing the ALS_OPTIONS environment variable.",
														/* FE_ALS_OPTIONS */
  "Autoload Error: unable to load \"%s\".",	/* FE_AUTOLOAD */
  "Overflow/Underflow of file descriptor count array",
														/* FE_FDREFOVERFLOW */

};


/*
 * fatal_error is called when there is a fatal error.
 */


void
fatal_error(int errcode, long arg)
{
    if (arg)
        PI_app_printf(PI_app_printf_fatal_error,
	              errors[errcode], arg);
    else
        PI_app_printf(PI_app_printf_fatal_error,
	              errors[errcode]);


    als_exit(errcode);
}
