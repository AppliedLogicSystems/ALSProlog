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

#if HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif


#ifndef DoubleType

static	void	fixTag		PARAMS(( PWord *, int * ));
static	void	tagFix		PARAMS(( PWord *, int * ));

static void
fixTag(v, t)
    PWord *v;
    int  *t;
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
tagFix(v, t)
    PWord *v;
    int  *t;
{
    if (*t == PI_DOUBLE)
	*t = WTP_STRUCTURE;
}
#endif



/*
 * We must pass in pointers here.
 * Returns 0 if can't intern uia
 * Otherwise returns pointer to symbol table entry.
 *
 *                      chris - 6/8/88
 */

char *
PI_forceuia(val_ptr, type_ptr)
    PWord *val_ptr;
    int  *type_ptr;
{
    if (force_uia(val_ptr, type_ptr))
	return ((char *) TOKNAME(*val_ptr));
    else
	return ((char *) 0);
}


void
PI_getan(val_ptr, type_ptr, arg_num)
    PWord *val_ptr;
    int  *type_ptr;
    int   arg_num;
{
    w_get_An(val_ptr, type_ptr, arg_num);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}

void
PI_getargn(val_ptr, type_ptr, struct_val, arg_num)
    PWord *val_ptr;
    int  *type_ptr;
    PWord struct_val;
    int   arg_num;
{
    w_get_argn(val_ptr, type_ptr, struct_val, arg_num);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}

void
PI_gethead(val_ptr, type_ptr, list_val)
    PWord *val_ptr;
    int  *type_ptr;
    PWord list_val;
{
    w_get_car(val_ptr, type_ptr, list_val);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}

void
PI_gettail(val_ptr, type_ptr, list_val)
    PWord *val_ptr;
    int  *type_ptr;
    PWord list_val;
{
    w_get_cdr(val_ptr, type_ptr, list_val);
#ifndef DoubleType
    fixTag(val_ptr, type_ptr);
#endif
}

void
PI_getdouble(num, v)
    double *num;
    PWord v;
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


void
PI_getstruct(func_ptr, arity_ptr, struct_val)
    PWord *func_ptr;
    int  *arity_ptr;
    PWord struct_val;
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

char *
PI_getsymname(buf_ptr, sym_val, size)
    char *buf_ptr;
    PWord sym_val;
    int   size;
{
    if (buf_ptr == (char *) 0)
	return ((char *) TOKNAME(sym_val));
    else if (strlen((char *)TOKNAME(sym_val)) <= size)
	return (strcpy(buf_ptr, (char *) TOKNAME(sym_val)));
    else
	return ((char *) 0);	/* couldn't fit uia into buffer */
}


/*
 * Changed to allow programmar to pass in size. Returns pointer to
 * buffer if uia fits in buffer, otherwise it returns 0.
 * Returns pointer to the first word of UIA if given buffer pointer
 * is zero.
 */

char *
PI_getuianame(buf_ptr, uia_val, size)
    char *buf_ptr;
    PWord uia_val;
    int   size;
{
    if (buf_ptr == NULL)
	return ((char *) M_FIRSTUIAWORD(uia_val));
    if (w_get_uianame((UCHAR *)buf_ptr, uia_val, size) != NULL)
	return (buf_ptr);
    else
	return NULL;
}


void
PI_getuiasize(uia_val, size_ptr)
    PWord uia_val;
    int  *size_ptr;
{
    *size_ptr = (int) M_UIASIZE(uia_val);
}

/*
 * Changed the value passed in because there is no
 * need for it to be an address.  - chris 7/15/88
 */

void
PI_makedouble(v, t, n)
    PWord *v;
    int  *t;
    double n;
{
    make_numberx(v, t, n, WTP_DOUBLE);
#ifndef DoubleType
    fixTag(v, t);
#endif
}


/* Creation of lists and structures is fixed.   -- Ilyas & Raman 5/16/91 */

void
PI_makelist(val_ptr, val_type)
    PWord *val_ptr;
    int  *val_type;
{
    w_mk_list(val_ptr, val_type);

    w_install_unbound_car(*val_ptr);
    w_install_unbound_cdr(*val_ptr);
}


void
PI_makestruct(val_ptr, val_type, functor, arity)
    PWord *val_ptr;
    int  *val_type;
    PWord functor;
    int   arity;
{
    int   i;

    w_mk_term(val_ptr, val_type, functor, arity);

    for (i = 1; i <= arity; i++) {
	w_install_unbound_argn(*val_ptr, i);
    }
}

void
PI_makesym(val_ptr, val_type, buf_ptr)
    PWord *val_ptr;
    int  *val_type;
    char *buf_ptr;
{
    *val_ptr = find_token((UCHAR *)buf_ptr);
    if (val_type)
	*val_type = PI_SYM;
}

void
PI_makeuia(val_ptr, val_type, buf_ptr)
    PWord *val_ptr;
    int  *val_type;
    char *buf_ptr;
{
    if ((*val_ptr = probe_token((UCHAR *)buf_ptr)) == 0)
	w_mk_uia(val_ptr, val_type, (UCHAR *)buf_ptr);
    else
	*val_type = PI_SYM;
}

void
PI_allocuia(val_ptr, val_type, size)
    PWord *val_ptr;
    int  *val_type;
    int   size;
{
    w_uia_alloc(val_ptr, val_type, (size_t)size);
}

#define MAXPRINTFBUF 4096

/*
 * PI_printf(format,args...)
 * char *format
 */

/*VARARGS0 */
int
#ifdef HAVE_STDARG_H
PI_printf(char *fmt, ...)
#else
PI_printf(fmt, va_alist)
    char *fmt;
    va_dcl
#endif
{
    va_list args;
    char *buf;
    PWord vArg, vFunctor, vStruct, vSIO;
    int   tArg, tFunctor, tStruct, tSIO;

#ifdef HAVE_STDARG_H
    va_start(args, fmt);
#else
    va_start(args);
#endif

    buf = malloc(MAXPRINTFBUF);
    vsprintf(buf, fmt, args);
    PI_makeuia(&vArg, &tArg, buf);
    free(buf);
    PI_makesym(&vFunctor, &tFunctor, "put_atom");
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
int
#ifdef HAVE_STDARG_H
PI_aprintf(char *alias, char *fmt, ...)
#else
PI_aprintf(alias, fmt, va_alist)
    char *alias;
    char *fmt;
    va_dcl
#endif
{
    va_list args;
    char *buf;
    PWord vArg, vFunctor, vStruct, vSIO;
    int   tArg, tFunctor, tStruct, tSIO;

#ifdef HAVE_STDARG_H
    va_start(args, fmt);
#else
    va_start(args);
#endif

    buf = malloc(MAXPRINTFBUF);
    vsprintf(buf, fmt, args);
    PI_makeuia(&vArg, &tArg, buf);
    free(buf);
    PI_makesym(&vFunctor, &tFunctor, "put_atom");
    PI_makestruct(&vStruct, &tStruct, vFunctor, 2);
    w_install_argn(vStruct, 2, vArg, tArg);
    PI_makesym(&vArg, &tArg, alias);
    w_install_argn(vStruct, 1, vArg, tArg);
    PI_makesym(&vSIO, &tSIO, "sio");
    return PI_rungoal(vSIO, vStruct, tStruct);
}

#ifdef APP_PRINTF_CALLBACK
void (*PI_app_printf_callback)(int, va_list) = NULL;

void PI_set_app_printf_callback(void (*callback)(int, va_list))
{
    PI_app_printf_callback = callback;
}

void PI_app_printf(int messtype, ...)
{
    if (PI_app_printf_callback) {
	va_list args;
    	va_start(args, messtype);
    	PI_app_printf_callback(messtype, args);
    }
}
#endif

int
PI_rungoal(mod, goal, goaltype)
    PWord mod;
    PWord goal;
    int   goaltype;
{
    return w_rungoal(mod, goal, goaltype);
}

int
PI_rungoal_with_update(mod, gvp, gtp)
    PWord mod;
    PWord *gvp;
    int *gtp;
{
    int status, handle;
    handle = gv_alloc();
    gv_set(*gvp,*gtp,handle);
    status = w_rungoal(mod, *gvp, *gtp);
    gv_get(gvp,gtp,handle);
    gv_free(handle);
    return status;
}


int
PI_unify(val1, type1, val2, type2)
    PWord val1;
    int   type1;
    PWord val2;
    int   type2;
{
#ifndef DoubleType
    tagFix(&val1, &type1);
    tagFix(&val2, &type2);
#endif
    return w_unify(val1, type1, val2, type2);
}


static PI_modid = MODULE_GLOBAL;

int
load_foreign(filename, libstr, initfcn)
    char *filename;
    char *libstr;
    char *initfcn;
{
#ifdef DynamicForeign
    void (*fptr)PARAMS(( void ));
    if ( (fptr = load_object(filename, libstr, initfcn)) ) {
	(*fptr)();
	return 1;
    }
    else
#endif
	return 0;
}

void
PrologInit(ap)
    PSTRUCT *ap;
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
