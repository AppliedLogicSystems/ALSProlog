/*=========================================================================*
 |			foreign.c            
 |		Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |			-- foreign interface functions
 |
 | Author: Kevin A. Buettner
 | Creation Date: 6/6/87
 | Revision History:
 | 06/08/88 - Chris -- Changed names for consistancy
 |                     Added max arg to PI_getuianame()
 | 06/13/88 - Chris -- changed "ptypes.h" to "alspi.h"
 | 07/13/88 - Chris -- moved some stuff from vprintf.h
 | 07/12/93 - Kev   -- added PI_printf and PI_putchar to call new stream 
 |						I/O in prolog
 | 10/26/94	- C. Houpt -- Various char* and UCHAR* casts.
 *=========================================================================*/

#include "defs.h"
#include "module.h"

#ifndef DoubleType

static	void	fixTag		( PWord *, int * );
static	void	tagFix		( PWord *, int * );

static void
fixTag(PWord *v, int *t)
{
    if (*t == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_arity(&arity, *v);
	w_get_functor(&functor, *v);
	if (arity == 4 && functor == TK_DDOUBLE)
	    *t = PI_DOUBLE;
    }
}

static void
tagFix(PWord *v, int *t)
{
    if (*t == PI_DOUBLE)
	*t = WTP_STRUCTURE;
}
#endif

static PWord PI_modid = MODULE_GLOBAL;

int
load_foreign(char *filename, char *libstr, char *initfcn)
{
#ifdef DynamicForeign
    void (*fptr)( void );
    if ( (fptr = load_object(filename, libstr, initfcn)) ) {
	(*fptr)();
	return 1;
    }
    else
#endif
	return 0;
}


/*
 * We must pass in pointers here.
 * Returns 0 if can't intern uia
 * Otherwise returns pointer to symbol table entry.
 *
 *                      chris - 6/8/88
 */

#ifdef macintosh
#pragma export on
#endif

EXPORT ALSPI_API(char *)
PI_forceuia(PWord *val_ptr, int  *type_ptr)
{
    if (force_uia(val_ptr, type_ptr))
	return ((char *) TOKNAME(*val_ptr));
    else
	return ((char *) 0);
}


EXPORT ALSPI_API(void)
PI_getan(PWord *val_ptr, int  *type_ptr, int arg_num)
{
    w_get_An(val_ptr, type_ptr, arg_num);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}

EXPORT ALSPI_API(void)
PI_getargn(PWord *val_ptr, int  *type_ptr, PWord struct_val, int arg_num)
{
    w_get_argn(val_ptr, type_ptr, struct_val, arg_num);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}


EXPORT ALSPI_API(void)
PI_gethead(PWord *val_ptr, int  *type_ptr, PWord list_val)
{
    w_get_car(val_ptr, type_ptr, list_val);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}

EXPORT ALSPI_API(void)
PI_gettail(PWord *val_ptr, int  *type_ptr, PWord list_val)
{
    w_get_cdr(val_ptr, type_ptr, list_val);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}

EXPORT ALSPI_API(void)
PI_getdouble(double *num, PWord v)
{
#ifndef DoubleType
    PWord v1;
    int   i, t1;

    for (i = 0; i < 4; i++) {
	w_get_argn(&v1, &t1, v, i + 1);
	*(((short *) num) + i) = (short) v1;
    }
#else
    w_get_double(num, v);
#endif
}


EXPORT ALSPI_API(void)
PI_getstruct(PWord *func_ptr, int  *arity_ptr, PWord struct_val)
{
    w_get_functor(func_ptr, struct_val);
    w_get_arity(arity_ptr, struct_val);
}


/*
 * Changed to give the programmer the option of whether they
 * want a copy of the symbol name, or a pointer to the symbol
 * table entry. Returns pointer to buffer if uia fits in buffer,
 * otherwise it returns 0.
 *                              chris - 6/8/88
 */

EXPORT ALSPI_API(char *)
PI_getsymname(char *buf_ptr, PWord sym_val, int size)
{
    if (buf_ptr == (char *) 0)
	return ((char *) TOKNAME(sym_val));
    else if ((int)strlen((char *)TOKNAME(sym_val)) <= size)
	return (strcpy(buf_ptr, (char *) TOKNAME(sym_val)));
    else
	return ((char *) 0);	/* couldn't fit uia into buffer */
}


/*
 * Changed to allow programmer to pass in size. Returns pointer to
 * buffer if uia fits in buffer, otherwise it returns 0.
 * Returns pointer to the first word of UIA if given buffer pointer
 * is zero.
 */

EXPORT ALSPI_API(char *)
PI_getuianame(char *buf_ptr, PWord uia_val, int size)
{
    if (buf_ptr == NULL)
	return ((char *) M_FIRSTUIAWORD(uia_val));
    if (w_get_uianame((UCHAR *)buf_ptr, uia_val, size) != NULL)
	return (buf_ptr);
    else
	return NULL;
}


EXPORT ALSPI_API(void)
PI_getuiasize(PWord uia_val, int *size_ptr)
{
    *size_ptr = (int) M_UIASIZE(uia_val);
}

/*
 * Changed the value passed in because there is no
 * need for it to be an address.  - chris 7/15/88
 */

EXPORT ALSPI_API(void)
PI_makedouble(PWord *v, int *t, double n)
{
    make_numberx(v, t, n, WTP_DOUBLE);
#ifndef DoubleType
    fixTag(v, t);
#endif
}


/* Creation of lists and structures is fixed.   -- Ilyas & Raman 5/16/91 */

EXPORT ALSPI_API(void)
PI_makelist(PWord *val_ptr, int *val_type)
{
    w_mk_list(val_ptr, val_type);

    w_install_unbound_car(*val_ptr);
    w_install_unbound_cdr(*val_ptr);
}


EXPORT ALSPI_API(void)
PI_makestruct(PWord *val_ptr, int *val_type, PWord functor, int arity)
{
    int   i;

    w_mk_term(val_ptr, val_type, functor, arity);

    for (i = 1; i <= arity; i++) {
	w_install_unbound_argn(*val_ptr, i);
    }
}

EXPORT ALSPI_API(void)
PI_makesym(PWord *val_ptr, int  *val_type,  const char *buf_ptr)
{
    *val_ptr = find_token((UCHAR *)buf_ptr);
    if (val_type)
	*val_type = PI_SYM;
}

EXPORT ALSPI_API(void)
PI_makeuia(PWord *val_ptr, int  *val_type, const char *buf_ptr)
{
    if ((*val_ptr = probe_token((UCHAR *)buf_ptr)) == 0)
	w_mk_uia(val_ptr, val_type, (UCHAR *)buf_ptr);
    else
	*val_type = PI_SYM;
}

EXPORT ALSPI_API(void)
PI_allocuia(PWord *val_ptr, int *val_type, int size)
{
    w_uia_alloc(val_ptr, val_type, (size_t)size);
}

#define MAXPRINTFBUF 4096

/*
 * PI_printf(format,args...)
 * char *format
 */

/*VARARGS0 */
EXPORT ALSPI_API(int)
PI_printf(const char *fmt, ...)
{
    va_list l;
    int result;
    
    va_start(l, fmt);
    result = PI_vprintf(fmt, l);
    va_end(l);
    
    return result;
}

EXPORT ALSPI_API(int)
PI_vprintf(const char *fmt, va_list args)
{
    char *buf;
    PWord vArg, vFunctor, vStruct, vSIO;
    int   tArg, tFunctor, tStruct, tSIO;

    buf = malloc(MAXPRINTFBUF);
    vsprintf(buf, fmt, args);
    PI_makeuia(&vArg, &tArg, buf);
    free(buf);
    PI_makesym(&vFunctor, &tFunctor, "printf");
    PI_makestruct(&vStruct, &tStruct, vFunctor, 1);
    w_install_argn(vStruct, 1, vArg, tArg);
    PI_makesym(&vSIO, &tSIO, "sio");
    return PI_rungoal(vSIO, vStruct, tStruct);
}

/*
 * PI_aprintf   -- alias printf
 *      PI_aprintf(alias,format,args...)
 *      char *alias;
 *      char *format
 */

/*VARARGS0 */
EXPORT ALSPI_API(int)
PI_aprintf(const char *alias, const char *fmt, ...)
{
    va_list l;
    int result;
    
    va_start(l, fmt);
    result = PI_vaprintf(alias, fmt, l);
    va_end(l);
    
    return result;
}

EXPORT ALSPI_API(int)
PI_vaprintf(const char *alias, const char *fmt, va_list args)
{
    char *buf;
    PWord vArg, vFunctor, vStruct, vSIO;
    int   tArg, tFunctor, tStruct, tSIO;

    buf = malloc(MAXPRINTFBUF);
    vsprintf(buf, fmt, args);
    PI_makeuia(&vArg, &tArg, buf);
    free(buf);
    PI_makesym(&vFunctor, &tFunctor, "printf");
    PI_makestruct(&vStruct, &tStruct, vFunctor, 2);
    w_install_argn(vStruct, 2, vArg, tArg);
    PI_makesym(&vArg, &tArg, alias);
    w_install_argn(vStruct, 1, vArg, tArg);
    PI_makesym(&vSIO, &tSIO, "sio");
    return PI_rungoal(vSIO, vStruct, tStruct);
}

#ifdef APP_PRINTF_CALLBACK
void (*PI_app_printf_callback)(int, va_list) = NULL;

EXPORT ALSPI_API(void)
PI_set_app_printf_callback(void (*callback)(int, va_list))
{
    PI_app_printf_callback = callback;
}

EXPORT ALSPI_API(void)
PI_vapp_printf(int messtype, va_list args)
{
    if (PI_app_printf_callback) {
    	PI_app_printf_callback(messtype, args);
    }    	
}

EXPORT ALSPI_API(void)
PI_app_printf(int messtype, ...)
{
    if (PI_app_printf_callback) {
	va_list args;
    	va_start(args, messtype);
    	PI_app_printf_callback(messtype, args);
    	va_end(args);
    }
}
#endif

EXPORT ALSPI_API(int)
PI_rungoal(PWord mod, PWord goal, int goaltype)
{
    return w_rungoal(mod, goal, goaltype);
}

EXPORT ALSPI_API(int)
PI_rungoal_with_update(PWord mod, PWord *gvp, int *gtp)
{
    int status, handle;
    handle = gv_alloc();
    gv_set(*gvp,*gtp,handle);
    status = w_rungoal(mod, *gvp, *gtp);
    gv_get(gvp,gtp,handle);
    gv_free(handle);
    return status;
}

EXPORT ALSPI_API(int)
PI_rungoal_with_update_and_catch(PWord mod, PWord *gvp, int *gtp, int *exception)
{
    int status, handle;
    handle = gv_alloc();
    gv_set(*gvp,*gtp,handle);
    status = w_rungoal(mod, *gvp, *gtp);
    gv_get(gvp,gtp,handle);
    gv_free(handle);
    *exception = wm_interrupt_caught != 0;
    return status;
}

EXPORT ALSPI_API(void)
PI_interrupt(void)
{
	pbi_forceCtlC();
}

EXPORT ALSPI_API(int)
PI_unify(PWord val1, int type1, PWord val2, int type2)
{
#ifndef DoubleType
    tagFix(&val1, &type1);
    tagFix(&val2, &type2);
#endif
    return w_unify(val1, type1, val2, type2);
}



EXPORT ALSPI_API(void)
PrologInit(PSTRUCT *ap)
{

    for (;;) {
	if (ap->name == (char *) -1)
	    break;
	else if (ap->arity == -1) {
	    PWord tok = find_token((UCHAR *)ap->name);

	    new_mod(tok);	/* this pair of calls will create and */
	    end_mod();		/* initialize the module if necessary */
	    PI_modid = tok;
	    ap++;
	}
	else {
	    w_assert_foreign((PWord) PI_modid,
			     ap->name,
			     ap->arity,
			     ap->func);

#ifdef PACKAGE
	    if (ap->funcname != (char *) -1)
#ifdef PCKG_NO_UNDERBAR
		insert_builtin_addr((long) ap->func, ((char *) (ap->funcname)) + 1);
#else  /* PCKG_NO_UNDERBAR */
		insert_builtin_addr((long) ap->func, ap->funcname);
#endif /* PCKG_NO_UNDERBAR */
#endif /* PACKAGE */

	    ap++;
	}
    }

    PI_modid = MODULE_GLOBAL;	/* set default for next call to PrologInit */
}

#ifdef macintosh
#pragma export off
#endif
