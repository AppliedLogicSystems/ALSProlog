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

void
main(int argc, char ** argv)
{
    int   exit_status;

    if ((exit_status = PI_prolog_init(argc, argv)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
	exit(EXIT_FAILURE);
    }

    if ((exit_status = PI_toplevel()) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog shell crashed !\n");
	exit(EXIT_FAILURE);
    }

    PI_shutdown();
        
    exit(EXIT_SUCCESS);
}




