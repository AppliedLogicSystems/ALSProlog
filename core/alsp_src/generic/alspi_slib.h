#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ALSPI_DLIB_VERSION 3

typedef struct {
    ALSPI_API(char *)	(*PI_forceuia)			(PE, PWord *, int * );
    ALSPI_API(void)	(*PI_getan)			(PE, PWord *, int *, int );
    ALSPI_API(void)	(*PI_getargn)			(PE, PWord *, int *, PWord, int );
    ALSPI_API(void)	(*PI_gethead)			(PE, PWord *, int *, PWord );
    ALSPI_API(void)	(*PI_gettail)			(PE, PWord *, int *, PWord );
    ALSPI_API(void)	(*PI_getdouble)			(PE, double *, PWord );
    ALSPI_API(void)	(*PI_getstruct)			(PE, PWord *, int *, PWord );
    ALSPI_API(char *)	(*PI_getsymname)		(PE, char *, PWord, int );
    ALSPI_API(char *)	(*PI_getuianame)		(PE, char *, PWord, int );
    ALSPI_API(void)	(*PI_getuiasize)		(PE, PWord, int * );
    ALSPI_API(void)	(*PI_makedouble)		(PE, PWord *, int *, double );
    ALSPI_API(void)	(*PI_makelist)			(PE, PWord *, int * );
    ALSPI_API(void)	(*PI_makestruct)		(PE, PWord *, int *, PWord, int );
    ALSPI_API(void)	(*PI_makesym)			(PE, PWord *, int *, const char * );
    ALSPI_API(void)	(*PI_makeuia)			(PE, PWord *, int *, const char * );
    ALSPI_API(void)	(*PI_allocuia)			(PE, PWord *, int *, int );
    ALSPI_API(int)	(*PI_vprintf)			(PE, const char *, va_list );
    ALSPI_API(int)	(*PI_vaprintf)			(PE, const char *, const char *, va_list );
    ALSPI_API(void)	(*PI_vapp_printf)		(PE, int, va_list );
    ALSPI_API(int)	(*PI_rungoal)			(PE, PWord, PWord, int );
    ALSPI_API(int)	(*PI_rungoal_with_update)	(PE, PWord, PWord *, int * );
    ALSPI_API(int)	(*PI_rungoal_with_update_catch)	(PE, PWord, PWord *, int *, int * );
    ALSPI_API(int)	(*PI_unify)			(PE, PWord , int, PWord , int );
    ALSPI_API(void)	(*PrologInit)                   (PE, PSTRUCT * );
    ALSPI_API(int)	(*CI_get_integer)		(PE, PWord *, int );
    ALSPI_API(int)	(*CI_get_double)		(PE, double *, unsigned long, unsigned long );
    ALSPI_API(int)	(*sym_insert_2long)		(PE, char *, int, long, long );
    ALSPI_API(int)	(*sym_insert_dbl)		(PE, char *, int, double );
    ALSPI_API(const char *)(*find_callback)		(PE, void *, void * );
    ALSPI_API(void)	(*PI_throw)			(PE,PWord, int);
    ALSPI_API(void)	(*PI_getball)			(PE,PWord *, int *);
    ALSPI_API(void)     (*PI_interrupt) (PE);
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

#ifdef __cplusplus
}
#endif
