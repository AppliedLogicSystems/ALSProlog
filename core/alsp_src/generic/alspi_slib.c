#include "alspi.h"
#include "cinterf.h"
#include "alspi_slib.h"

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif
	
static const alspi_func_ptrs *alspi_funcs;
extern void pi_init(void);

#ifdef __MWERKS__
#pragma export on
#endif
EXPORT int alspi_dlib_init(const alspi_func_ptrs *a_f,
	       library_func_ptrs *lib_funcs)
{
  if (a_f == NULL && lib_funcs == NULL) return ALSPI_DLIB_VERSION;

  alspi_funcs = a_f;
  lib_funcs->pi_init = pi_init;
  lib_funcs->pi_shutdown = NULL;

  return 0;
}
#ifdef __MWERKS__
#pragma export off
#endif

char *PI_forceuia( PWord *a, int *b )
{
    return alspi_funcs->PI_forceuia(a,b);
}

void PI_getan( PWord *a, int *b, int c)
{
    alspi_funcs->PI_getan(a,b,c);
}

void PI_getargn( PWord *a, int *b, PWord c, int d)
{
    alspi_funcs->PI_getargn(a,b,c,d);
}

void PI_gethead( PWord *a, int *b, PWord c)
{
    alspi_funcs->PI_gethead(a,b,c);
}

void PI_gettail( PWord *a, int *b, PWord c)
{
    alspi_funcs->PI_gettail(a,b,c);
}

void PI_getdouble( double *a, PWord b)
{
    alspi_funcs->PI_getdouble(a,b);
}

void PI_getstruct( PWord *a, int *b, PWord c)
{
    alspi_funcs->PI_getstruct(a,b,c);
}

char *PI_getsymname( char *a, PWord b, int c)
{
    return alspi_funcs->PI_getsymname(a,b,c);
}

char *PI_getuianame( char *a, PWord b, int c)
{
    return alspi_funcs->PI_getuianame(a,b,c);
}

void PI_getuiasize(PWord a, int *b)
{
    alspi_funcs->PI_getuiasize(a,b);
}

void PI_makedouble( PWord *a, int *b, double c)
{
    alspi_funcs->PI_makedouble(a,b,c);
}

void PI_makelist( PWord *a, int * b)
{
    alspi_funcs->PI_makelist(a,b);
}

void PI_makestruct( PWord *a, int *b, PWord c, int d)
{
    alspi_funcs->PI_makestruct(a,b,c,d);
}

void PI_makesym( PWord *a, int *b, const char * c)
{
    alspi_funcs->PI_makesym(a,b,c);
}

void PI_makeuia( PWord *a, int *b, const char *c )
{
    alspi_funcs->PI_makeuia(a,b,c);
}

void PI_allocuia( PWord *a, int *b, int c)
{
    alspi_funcs->PI_allocuia(a,b,c);
}

int PI_printf( const char *a, ...)
{
    va_list l;
    int result;

    va_start(l, a);
    result = alspi_funcs->PI_vprintf(a, l);
    va_end(l);

    return result;
}

int PI_aprintf( const char *a, const char *b, ... )
{
     va_list l;
    int result;

    va_start(l, b); 
    result = alspi_funcs->PI_vaprintf(a, b, l);
    va_end(l);

    return result;
}

void PI_app_printf( int a, ... )
{
    va_list l;

    va_start(l, a);
    alspi_funcs->PI_vapp_printf(a, l);
    va_end(l);
}

int PI_vprintf( const char *a, va_list b)
{
    return alspi_funcs->PI_vprintf(a, b);
}

int PI_vaprintf( const char *a, const char *b, va_list c)
{
    return alspi_funcs->PI_vaprintf(a,b,c);
}

void PI_vapp_printf( int a, va_list b)
{
    alspi_funcs->PI_vapp_printf(a,b);
}

int PI_rungoal( PWord a, PWord b, int c)
{
    return alspi_funcs->PI_rungoal(a,b,c);
}

int PI_rungoal_with_update( PWord a, PWord *b, int *c)
{
    return alspi_funcs->PI_rungoal_with_update(a,b,c);
}

int PI_unify( PWord a, int b, PWord c, int d)
{
    return alspi_funcs->PI_unify(a,b,c,d);
}

void PrologInit( PSTRUCT *a)
{
    alspi_funcs->PrologInit(a);
}

int CI_get_integer( PWord *a, int b)
{
    return alspi_funcs->CI_get_integer(a,b);
}

int CI_get_double( double *a, unsigned long b, unsigned long c)
{
    return alspi_funcs->CI_get_double(a,b,c);
}

int sym_insert_2long( char *a, int b, long c, long d)
{
    return alspi_funcs->sym_insert_2long(a,b,c,d);
}

int sym_insert_dbl( char *a, int b, double c)
{
    return alspi_funcs->sym_insert_dbl(a,b,c);
}

const char *find_callback( void *a, void *b )
{
    return alspi_funcs->find_callback(a,b);
}

#ifdef macintosh
void main(void)
{}
#endif
