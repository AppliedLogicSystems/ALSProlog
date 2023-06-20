#include "alspi.h"
#include "alspi_slib.h"
#include <stdio.h>
#include <string.h>

	
static const alspi_func_ptrs *alspi_funcs;
extern void pi_init(void);
char library_dir[PATH_MAX], executable_path[PATH_MAX];
#ifdef __MWERKS__
#pragma export on
#endif
EXPORT int alspi_dlib_init(const alspi_func_ptrs *a_f,
	       library_func_ptrs *lib_funcs)
{
  if (a_f == NULL && lib_funcs == NULL) return ALSPI_DLIB_VERSION;

  alspi_funcs = a_f;
  strcpy(library_dir, a_f->library_dir);
  strcpy(executable_path, a_f->executable_path);
  lib_funcs->pi_init = pi_init;
  lib_funcs->pi_shutdown = (void (*)(void))NULL;

  return 0;
}
#ifdef __MWERKS__
#pragma export off
#endif

ALSPI_API(char *) PI_forceuia( PWord *a, int *b )
{
    return alspi_funcs->PI_forceuia(a,b);
}

ALSPI_API(void) PI_getan( PWord *a, int *b, int c)
{
    alspi_funcs->PI_getan(a,b,c);
}

ALSPI_API(void) PI_getargn( PWord *a, int *b, PWord c, int d)
{
    alspi_funcs->PI_getargn(a,b,c,d);
}

ALSPI_API(void) PI_gethead( PWord *a, int *b, PWord c)
{
    alspi_funcs->PI_gethead(a,b,c);
}

ALSPI_API(void) PI_gettail( PWord *a, int *b, PWord c)
{
    alspi_funcs->PI_gettail(a,b,c);
}

ALSPI_API(void) PI_getdouble( double *a, PWord b)
{
    alspi_funcs->PI_getdouble(a,b);
}

ALSPI_API(void) PI_getstruct( PWord *a, int *b, PWord c)
{
    alspi_funcs->PI_getstruct(a,b,c);
}

ALSPI_API(char *) PI_getsymname( char *a, PWord b, int c)
{
    return alspi_funcs->PI_getsymname(a,b,c);
}

ALSPI_API(char *) PI_getuianame( char *a, PWord b, int c)
{
    return alspi_funcs->PI_getuianame(a,b,c);
}

ALSPI_API(void) PI_getuiasize(PWord a, int *b)
{
    alspi_funcs->PI_getuiasize(a,b);
}

ALSPI_API(void) PI_makedouble( PWord *a, int *b, double c)
{
    alspi_funcs->PI_makedouble(a,b,c);
}

ALSPI_API(void) PI_makelist( PWord *a, int * b)
{
    alspi_funcs->PI_makelist(a,b);
}

ALSPI_API(void) PI_makestruct( PWord *a, int *b, PWord c, int d)
{
    alspi_funcs->PI_makestruct(a,b,c,d);
}

ALSPI_API(void) PI_makesym( PWord *a, int *b, const char * c)
{
    alspi_funcs->PI_makesym(a,b,c);
}

ALSPI_API(void) PI_makeuia( PWord *a, int *b, const char *c )
{
    alspi_funcs->PI_makeuia(a,b,c);
}

ALSPI_API(void) PI_allocuia( PWord *a, int *b, int c)
{
    alspi_funcs->PI_allocuia(a,b,c);
}

ALSPI_API(int) PI_printf( const char *a, ...)
{
    va_list l;
    int result;

    va_start(l, a);
    result = alspi_funcs->PI_vprintf(a, l);
    va_end(l);

    return result;
}

ALSPI_API(int) PI_aprintf( const char *a, const char *b, ... )
{
     va_list l;
    int result;

    va_start(l, b); 
    result = alspi_funcs->PI_vaprintf(a, b, l);
    va_end(l);

    return result;
}

ALSPI_API(void) PI_app_printf( int a, ... )
{
    va_list l;

    va_start(l, a);
    alspi_funcs->PI_vapp_printf(a, l);
    va_end(l);
}

ALSPI_API(int) PI_vprintf( const char *a, va_list b)
{
    return alspi_funcs->PI_vprintf(a, b);
}

ALSPI_API(int) PI_vaprintf( const char *a, const char *b, va_list c)
{
    return alspi_funcs->PI_vaprintf(a,b,c);
}

ALSPI_API(void) PI_vapp_printf( int a, va_list b)
{
    alspi_funcs->PI_vapp_printf(a,b);
}

ALSPI_API(int) PI_rungoal( PWord a, PWord b, int c)
{
    return alspi_funcs->PI_rungoal(a,b,c);
}

ALSPI_API(int) PI_rungoal_with_update( PWord a, PWord *b, int *c)
{
    return alspi_funcs->PI_rungoal_with_update(a,b,c);
}

ALSPI_API(int) PI_rungoal_with_update_and_catch( PWord a, PWord *b, int *c, int *d)
{
    return alspi_funcs->PI_rungoal_with_update_catch(a,b,c,d);
}

ALSPI_API(int) PI_unify( PWord a, int b, PWord c, int d)
{
    return alspi_funcs->PI_unify(a,b,c,d);
}

ALSPI_API(void) PrologInit( PSTRUCT *a)
{
    alspi_funcs->PrologInit(a);
}

ALSPI_API(int) CI_get_integer( PWord *a, int b)
{
    return alspi_funcs->CI_get_integer(a,b);
}

ALSPI_API(int) CI_get_double( double *a, unsigned long b, unsigned long c)
{
    return alspi_funcs->CI_get_double(a,b,c);
}

ALSPI_API(int) sym_insert_2long( char *a, int b, long c, long d)
{
    return alspi_funcs->sym_insert_2long(a,b,c,d);
}

ALSPI_API(int) sym_insert_dbl( char *a, int b, double c)
{
    return alspi_funcs->sym_insert_dbl(a,b,c);
}

ALSPI_API(const char *) find_callback( void *a, void *b )
{
    return alspi_funcs->find_callback(a,b);
}

ALSPI_API(void) PI_throw( PWord a, int b )
{
    alspi_funcs->PI_throw(a,b);
}

ALSPI_API(void) PI_getball( PWord *a, int *b )
{
    alspi_funcs->PI_getball(a,b);
}

ALSPI_API(void) PI_interrupt(void)
{
    alspi_funcs->PI_interrupt();
}

#ifdef macintosh
Boolean SIOUXIsAppWindow(WindowPtr w)
{
	return alspi_funcs->SIOUXIsAppWindow(w);
}

short SIOUXHandleOneEvent(EventRecord *event)
{
	return alspi_funcs->SIOUXHandleOneEvent(event);
}

void SIOUXSetEventVector(short (*handler)(EventRecord *))
{
	alspi_funcs->SIOUXSetEventVector(handler);
}

QDGlobals *GetQD(void)
{
    return alspi_funcs->GetQD();
}
#endif
