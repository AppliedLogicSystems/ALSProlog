#include <stdarg.h>

#define ALSPI_DLIB_VERSION 1

typedef struct {
    ALSPI_API(char *)	(*PI_forceuia)			( PWord *, int * );
    ALSPI_API(void)	(*PI_getan)			( PWord *, int *, int );
    ALSPI_API(void)	(*PI_getargn)			( PWord *, int *, PWord, int );
    ALSPI_API(void)	(*PI_gethead)			( PWord *, int *, PWord );
    ALSPI_API(void)	(*PI_gettail)			( PWord *, int *, PWord );
    ALSPI_API(void)	(*PI_getdouble)			( double *, PWord );
    ALSPI_API(void)	(*PI_getstruct)			( PWord *, int *, PWord );
    ALSPI_API(char *)	(*PI_getsymname)		( char *, PWord, int );
    ALSPI_API(char *)	(*PI_getuianame)		( char *, PWord, int );
    ALSPI_API(void)	(*PI_getuiasize)		( PWord, int * );
    ALSPI_API(void)	(*PI_makedouble)		( PWord *, int *, double );
    ALSPI_API(void)	(*PI_makelist)			( PWord *, int * );
    ALSPI_API(void)	(*PI_makestruct)		( PWord *, int *, PWord, int );
    ALSPI_API(void)	(*PI_makesym)			( PWord *, int *, const char * );
    ALSPI_API(void)	(*PI_makeuia)			( PWord *, int *, const char * );
    ALSPI_API(void)	(*PI_allocuia)			( PWord *, int *, int );
    ALSPI_API(int)	(*PI_vprintf)			( const char *, va_list );
    ALSPI_API(int)	(*PI_vaprintf)			( const char *, const char *, va_list );
    ALSPI_API(void)	(*PI_vapp_printf)		( int, va_list );
    ALSPI_API(int)	(*PI_rungoal)			( PWord, PWord, int );
    ALSPI_API(int)	(*PI_rungoal_with_update)	( PWord, PWord *, int * );
    ALSPI_API(int)	(*PI_rungoal_with_update_catch)	( PWord, PWord *, int *, int * );
    ALSPI_API(int)	(*PI_unify)			( PWord , int, PWord , int );
    ALSPI_API(void)	(*PrologInit)                   ( PSTRUCT * );
    ALSPI_API(int)	(*CI_get_integer)		( PWord *, int );
    ALSPI_API(int)	(*CI_get_double)		( double *, unsigned long, unsigned long );
    ALSPI_API(int)	(*sym_insert_2long)		( char *, int, long, long );
    ALSPI_API(int)	(*sym_insert_dbl)		( char *, int, double );
    ALSPI_API(const char *)(*find_callback)		( void *, void * );
    ALSPI_API(void)	(*PI_throw)			(PWord, int);
    ALSPI_API(void)	(*PI_getball)			(PWord *, int *);
    		
} alspi_func_ptrs;

typedef struct {
    void (*pi_init)(void);
    void (*pi_shutdown)(void);
} library_func_ptrs;

typedef int (*alspi_init_func)(const alspi_func_ptrs *, library_func_ptrs *);

int alspi_dlib_init(const alspi_func_ptrs *, library_func_ptrs *);
