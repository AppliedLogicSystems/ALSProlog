#include <stdarg.h>

#define ALSPI_DLIB_VERSION 3

#ifdef macintosh
#define ALSPI_APIP(X,Y)	pascal X (*Y)
#elif defined(WIN32)
#define ALSPI_API(X)	X __stdcall
#define ALSPI_APIP(X,Y)	X (__stdcall *Y)
#else
#define ALSPI_API(X)	X
#define ALSPI_APIP(X,Y)	X (*Y)
#endif

typedef struct {
    ALSPI_APIP(char *, PI_forceuia)			( PWord *, int * );
    ALSPI_APIP(void, PI_getan)			( PWord *, int *, int );
    ALSPI_APIP(void, PI_getargn)			( PWord *, int *, PWord, int );
    ALSPI_APIP(void, PI_gethead)			( PWord *, int *, PWord );
    ALSPI_APIP(void, PI_gettail)			( PWord *, int *, PWord );
    ALSPI_APIP(void, PI_getdouble)			( double *, PWord );
    ALSPI_APIP(void, PI_getstruct)			( PWord *, int *, PWord );
    ALSPI_APIP(char *, PI_getsymname)		( char *, PWord, int );
    ALSPI_APIP(char *, PI_getuianame)		( char *, PWord, int );
    ALSPI_APIP(void, PI_getuiasize)		( PWord, int * );
    ALSPI_APIP(void, PI_makedouble)		( PWord *, int *, double );
    ALSPI_APIP(void, PI_makelist)			( PWord *, int * );
    ALSPI_APIP(void, PI_makestruct)		( PWord *, int *, PWord, int );
    ALSPI_APIP(void, PI_makesym)			( PWord *, int *, const char * );
    ALSPI_APIP(void, PI_makeuia)			( PWord *, int *, const char * );
    ALSPI_APIP(void, PI_allocuia)			( PWord *, int *, int );
    ALSPI_APIP(int, PI_vprintf)			( const char *, va_list );
    ALSPI_APIP(int, PI_vaprintf)			( const char *, const char *, va_list );
    ALSPI_APIP(void, PI_vapp_printf)		( int, va_list );
    ALSPI_APIP(int, PI_rungoal)			( PWord, PWord, int );
    ALSPI_APIP(int, PI_rungoal_with_update)	( PWord, PWord *, int * );
    ALSPI_APIP(int, PI_rungoal_with_update_catch)	( PWord, PWord *, int *, int * );
    ALSPI_APIP(int, PI_unify)			( PWord , int, PWord , int );
    ALSPI_APIP(void, PrologInit)                   ( PSTRUCT * );
    ALSPI_APIP(int, CI_get_integer)		( PWord *, int );
    ALSPI_APIP(int, CI_get_double)		( double *, unsigned long, unsigned long );
    ALSPI_APIP(int, sym_insert_2long)		( char *, int, long, long );
    ALSPI_APIP(int, sym_insert_dbl)		( char *, int, double );
    ALSPI_APIP(const char *, find_callback)		( void *, void * );
    ALSPI_APIP(void, PI_throw)			(PWord, int);
    ALSPI_APIP(void, PI_getball)			(PWord *, int *);
    ALSPI_APIP(void, PI_interrupt) (void);
    const char *library_dir;
    const char *executable_path;
#ifdef macintosh
	Boolean (*SIOUXIsAppWindow)(WindowPtr w);
	short (*SIOUXHandleOneEvent)(EventRecord *event);
	void (*SIOUXSetEventVector)(short (*handler)(EventRecord *));
	QDGlobals *(*GetQD)(void);
#endif    		
} alspi_func_ptrs;

typedef struct {
    void (*pi_init)(void);
    void (*pi_shutdown)(void);
} library_func_ptrs;

typedef int (*alspi_init_func)(const alspi_func_ptrs *, library_func_ptrs *);

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

EXPORT int alspi_dlib_init(const alspi_func_ptrs *, library_func_ptrs *);
