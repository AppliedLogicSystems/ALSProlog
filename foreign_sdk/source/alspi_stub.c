#ifdef WIN32
#include <windows.h>
#endif

#include "alspi.h"

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

#ifdef macintosh
#pragma export on
#endif


EXPORT ALSPI_API(char *)	PI_forceuia	( PWord *, int * )
{

}

EXPORT ALSPI_API(void)	PI_getan	( PWord *, int *, int )
{

}

EXPORT ALSPI_API(void)	PI_getargn	( PWord *, int *, PWord, int )
{

}


EXPORT ALSPI_API(void)	PI_gethead	( PWord *, int *, PWord )
{

}


EXPORT ALSPI_API(void)	PI_gettail	( PWord *, int *, PWord )
{

}


EXPORT ALSPI_API(void)	PI_getdouble	( double *, PWord )
{

}


EXPORT ALSPI_API(void)	PI_getstruct	( PWord *, int *, PWord )
{

}


EXPORT ALSPI_API(char *)	PI_getsymname	( char *, PWord, int )
{

}


EXPORT ALSPI_API(char *)	PI_getuianame	( char *, PWord, int )
{

}


EXPORT ALSPI_API(void)	PI_getuiasize	( PWord, int * )
{

}


EXPORT ALSPI_API(void)	PI_makedouble	( PWord *, int *, double )
{

}


EXPORT ALSPI_API(void)	PI_makelist	( PWord *, int * )
{

}


EXPORT ALSPI_API(void)	PI_makestruct	( PWord *, int *, PWord, int )
{

}


EXPORT ALSPI_API(void)	PI_makesym	( PWord *, int *, const char * )
{

}


EXPORT ALSPI_API(void)	PI_makeuia	( PWord *, int *, const char * )
{

}


EXPORT ALSPI_API(void)	PI_allocuia	( PWord *, int *, int )
{

}


EXPORT ALSPI_API(int)	PI_printf	( const char *, ... )
{

}


EXPORT ALSPI_API(int)	PI_aprintf	( const char *, const char *, ... )
{

}


EXPORT ALSPI_API(int)	PI_vprintf	( const char *, va_list )
{
	return 0;
}


EXPORT ALSPI_API(int)	PI_vaprintf	( const char *, const char *, va_list )
{
	return 0;
}


EXPORT ALSPI_API(int)	PI_rungoal	( PWord, PWord, int )
{

}


EXPORT ALSPI_API(int)	PI_rungoal_with_update	( PWord, PWord *, int * )
{

}


EXPORT ALSPI_API(int)	PI_rungoal_with_update_and_catch	( PWord, PWord *, int *, int * )
{

}


EXPORT ALSPI_API(int)	PI_unify	( PWord , int, PWord , int )
{

}


EXPORT ALSPI_API(void)	PrologInit	( PSTRUCT * )
{

}


EXPORT ALSPI_API(void)	PI_shutdown	( void )
{

}


EXPORT ALSPI_API(void)	PI_toplevel	( void )
{

}


EXPORT ALSPI_API(int)	PI_prolog_init	( int, char **)
{

}


EXPORT ALSPI_API(int)	PI_startup	( const PI_system_setup *)
{

}


EXPORT ALSPI_API(void)	PI_throw	(PWord obj, int objt)
{

}


EXPORT ALSPI_API(void)	PI_getball	(PWord *obj, int *objt)
{

}


EXPORT ALSPI_API(int)	PI_main		(int argc, char *argv[], void (*init)(void))
{

}

#ifdef APP_PRINTF_CALLBACK
EXPORT ALSPI_API(void)	PI_set_app_printf_callback(void (*callback)(int, va_list))
{

}
#endif

EXPORT ALSPI_API(void)	PI_app_printf	( int, ... )
{

}


EXPORT ALSPI_API(void)    PI_vapp_printf  ( int, va_list )
{

}


EXPORT ALSPI_API(const char *)	PI_get_options	( void )
{

}




EXPORT ALSPI_API(void)	PI_set_console_functions(console_func read,
				console_func write, console_func error)
{

}




EXPORT ALSPI_API(int)	sym_insert_2long ( char *, int, long, long )
{

}


EXPORT ALSPI_API(int)	sym_insert_dbl	( char *, int, double )
{

}


EXPORT ALSPI_API(int)	CI_get_integer	( PWord *, int )
{

}


EXPORT ALSPI_API(int)	CI_get_double	( double *, unsigned long, unsigned long )
{

}

EXPORT ALSPI_API(const char *) find_callback(void *func, void *object)
{

}


BOOL WINAPI DllMain ( HINSTANCE hInst, DWORD wDataSeg, LPVOID lpvReserved );
BOOL WINAPI DllMain( HINSTANCE hInst, DWORD wDataSeg, LPVOID lpReserved )
{
}

#ifdef macintosh
#pragma export off
#endif

