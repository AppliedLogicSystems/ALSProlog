/*=====================================================================*
 |		pimain.c
 |	Copyright (c) 1988-1996, Applied Logic Systems, Inc.
 |
 |		-- default main() that initializes prolog and starts
 |			the development shell.
 |
 *=====================================================================*/

#ifdef HAVE_CONFIG_H
	/* In ALS-Prolog source tree */

#include "defs.h"

#else /* !HAVE_CONFIG_H */
	/* Not in ALS-Prolog source tree... */

#include <stdio.h>
#include <stdlib.h>

#include "alspi.h"

#endif /* !HAVE_CONFIG_H */

#if defined(KERNAL) && defined(__MWERKS__) && defined(macintosh)
#include <console.h>
#endif

void
main(int argc, char ** argv)
{
    int   exit_status, success;

#if defined(KERNAL) && defined(__MWERKS__) && defined(macintosh)
    argc = ccommand(&argv);
#endif

    if ((exit_status = PI_prolog_init(argc, argv)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
	exit(EXIT_ERROR);
    }

    if ((exit_status = PI_toplevel(&success)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog shell crashed !\n");
	exit(EXIT_ERROR);
    }

    PI_shutdown();
        
    if (success) exit(EXIT_SUCCESS);
    else exit(EXIT_FAILURE);
}




