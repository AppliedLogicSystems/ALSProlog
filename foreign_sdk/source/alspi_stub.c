#include "alspi.h"

#include <stddef.h>
#include <stdarg.h>


#ifdef macintosh
#pragma export on
#endif

ALSPI_API(char *)	PI_forceuia	( PWord *, int * )
{

}

ALSPI_API(void)	PI_getan	( PWord *, int *, int )
{

}

ALSPI_API(void)	PI_getargn	( PWord *, int *, PWord, int )
{

}


ALSPI_API(void)	PI_gethead	( PWord *, int *, PWord )
{

}


ALSPI_API(void)	PI_gettail	( PWord *, int *, PWord )
{

}


ALSPI_API(void)	PI_getdouble	( double *, PWord )
{

}


ALSPI_API(void)	PI_getstruct	( PWord *, int *, PWord )
{

}


ALSPI_API(char *)	PI_getsymname	( char *, PWord, int )
{

}


ALSPI_API(char *)	PI_getuianame	( char *, PWord, int )
{

}


ALSPI_API(void)	PI_getuiasize	( PWord, int * )
{

}


ALSPI_API(void)	PI_makedouble	( PWord *, int *, double )
{

}


ALSPI_API(void)	PI_makelist	( PWord *, int * )
{

}


ALSPI_API(void)	PI_makestruct	( PWord *, int *, PWord, int )
{

}


ALSPI_API(void)	PI_makesym	( PWord *, int *, const char * )
{

}


ALSPI_API(void)	PI_makeuia	( PWord *, int *, const char * )
{

}


ALSPI_API(void)	PI_allocuia	( PWord *, int *, int )
{

}


ALSPI_API(int)	PI_printf	( const char *, ... )
{

}


ALSPI_API(int)	PI_aprintf	( const char *, const char *, ... )
{

}


ALSPI_API(int)	PI_vprintf	( const char *, va_list )
{
	return 0;
}


extern  ALSPI_API(int)	PI_vaprintf	( const char *, const char *, va_list )
{
	return 0;
}


ALSPI_API(int)	PI_rungoal	( PWord, PWord, int )
{

}


ALSPI_API(int)	PI_rungoal_with_update	( PWord, PWord *, int * )
{

}


ALSPI_API(int)	PI_rungoal_with_update_and_catch	( PWord, PWord *, int *, int * )
{

}


ALSPI_API(int)	PI_unify	( PWord , int, PWord , int )
{

}


ALSPI_API(void)	PrologInit	( PSTRUCT * )
{

}


ALSPI_API(void)	PI_shutdown	( void )
{

}


ALSPI_API(void)	PI_toplevel	( void )
{

}


ALSPI_API(int)	PI_prolog_init	( int, char **)
{

}


ALSPI_API(int)	PI_startup	( const PI_system_setup *)
{

}


ALSPI_API(void)	PI_throw	(PWord obj, int objt)
{

}


ALSPI_API(void)	PI_getball	(PWord *obj, int *objt)
{

}


ALSPI_API(int)	PI_main		(int argc, char *argv[], void (*init)(void))
{

}

#ifdef APP_PRINTF_CALLBACK
ALSPI_API(void)	PI_set_app_printf_callback(void (*callback)(int, va_list))
{

}
#endif

ALSPI_API(void)	PI_app_printf	( int, ... )
{

}


extern  ALSPI_API(void)    PI_vapp_printf  ( int, va_list )
{

}


ALSPI_API(const char *)	PI_get_options	( void )
{

}




ALSPI_API(void)	PI_set_console_functions(console_func read,
				console_func write, console_func error)
{

}




ALSPI_API(int)	sym_insert_2long ( char *, int, long, long )
{

}


ALSPI_API(int)	sym_insert_dbl	( char *, int, double )
{

}


ALSPI_API(int)	CI_get_integer	( PWord *, int )
{

}


ALSPI_API(int)	CI_get_double	( double *, unsigned long, unsigned long )
{

}

ALSPI_API(const char *) find_callback(void *func, void *object)
{

}

#ifdef macintosh
#pragma export off
#endif

