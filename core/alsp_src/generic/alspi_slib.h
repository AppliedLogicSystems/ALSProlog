#include <stdarg.h>

#define ALSPI_DLIB_VERSION 1

typedef struct {
    char *	(*PI_forceuia)			( PWord *, int * );
    void	(*PI_getan)			( PWord *, int *, int );
    void	(*PI_getargn)			( PWord *, int *, PWord, int );
    void	(*PI_gethead)			( PWord *, int *, PWord );
    void	(*PI_gettail)			( PWord *, int *, PWord );
    void	(*PI_getdouble)			( double *, PWord );
    void	(*PI_getstruct)			( PWord *, int *, PWord );
    char *	(*PI_getsymname)		( char *, PWord, int );
    char *	(*PI_getuianame)		( char *, PWord, int );
    void	(*PI_getuiasize)		( PWord, int * );
    void	(*PI_makedouble)		( PWord *, int *, double );
    void	(*PI_makelist)			( PWord *, int * );
    void	(*PI_makestruct)		( PWord *, int *, PWord, int );
    void	(*PI_makesym)			( PWord *, int *, const char * );
    void	(*PI_makeuia)			( PWord *, int *, const char * );
    void	(*PI_allocuia)			( PWord *, int *, int );
    int		(*PI_vprintf)			( const char *, va_list );
    int		(*PI_vaprintf)			( const char *, const char *, va_list );
    void	(*PI_vapp_printf)		( int, va_list );
    int		(*PI_rungoal)			( PWord, PWord, int );
    int		(*PI_rungoal_with_update)	( PWord, PWord *, int * );
    int		(*PI_unify)			( PWord , int, PWord , int );
    void	(*PrologInit)                   ( PSTRUCT * );
    int		(*CI_get_integer)		( PWord *, int );
    int		(*CI_get_double)		( double *, unsigned long, unsigned long );
    int		(*sym_insert_2long)		( char *, int, long, long );
    int		(*sym_insert_dbl)		( char *, int, double );
    const char *(*find_callback)		( void *, void * );
    		
} alspi_func_ptrs;

typedef struct {
    void (*pi_init)(void);
    void (*pi_shutdown)(void);
} library_func_ptrs;

typedef int (*alspi_init_func)(const alspi_func_ptrs *, library_func_ptrs *);

int alspi_dlib_init(const alspi_func_ptrs *, library_func_ptrs *);
