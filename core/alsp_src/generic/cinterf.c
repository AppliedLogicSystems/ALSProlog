/*=================================================================*
 |			cinterf.c    
 |		Copyright (c) 1992-95, Applied Logic Systems Inc.
 |
 |			-- support routines for C interface
 |
 | Author : Prabhakaran Raman
 | Creation : 2/1/92
 | Revision History :
 | 08/20/92 - Ron -- Added functions CI_get_cstring and CI_get_integer 
 |					 for use for compact C interface.
 | 08/26/92 - Raman-- added static initialization of hash table, and
 |                    faster check for functor "c" in COMPACT_CI_get_*
 | 10/26/94 - C. Houpt -- Various unsigned long* casts.
 *=================================================================*/
#include "defs.h"

#ifdef BCINTER

#include <stdio.h>
#include "alspi.h"

static	int	lookup_c_sym	( char * );
static	int	c_structinfo	( void );
static	int	c_typeinfo	( void );
static	int	c_constinfo	( void );
static	int	c_rconstinfo	( void );
static	int	c_call		( void );
static	int	c_makeuia	( void );
static	int	c_convertCstrPstr ( void );

/*
 * symbols for "$farptr", "f", "[]" and "c";
 * initialized in cinterf_init()
 */

static PWord farsym;
static int farsymtype;

static PWord fieldsym;
static int fieldsymtype;

static PWord pnil;
static int niltype;

static PWord constsym;
static int constsymtype;

#if 0
/*
 * Convert a DOS far pointer into a prolog structure of
 * the form $farptr(short,short,short).
 */

void
CI_makefar(vp, tp, ptr)
    PWord *vp;
    int  *tp;
    unsigned short *ptr;
{
    PWord arg;
    int   argtype;

    PI_makestruct(vp, tp, farsym, 3);

    PI_getargn(&arg, &argtype, *vp, 1);
    PI_unify(arg, argtype, (PWord) * (ptr), PI_INT);

    PI_getargn(&arg, &argtype, *vp, 2);
    PI_unify(arg, argtype, (PWord) * (ptr + 1), PI_INT);

    PI_getargn(&arg, &argtype, *vp, 3);
    PI_unify(arg, argtype, (PWord) * (ptr + 2), PI_INT);
}
#endif

/*
 * SYMBOL TABLE
 */

#if defined(MacOS) || defined(MSWin32)
/* The MacOS interface is huge, give it more space for symbols. */
#define SYMTBLSZ   8191		/* this number must be prime */
#else
#define SYMTBLSZ   4093		/* this number must be prime */
#endif

typedef struct {
    long  val1, val2;
} twolongs;

typedef union {
    twolongs longvals;
    double doubleval;
} dbl_or_twolongs;

typedef struct {
    char *name;
    int   type;
    dbl_or_twolongs val;
} SymTblEntry;

SymTblEntry symtable[SYMTBLSZ];

#define SYMNAME(i)  symtable[(i)].name
#define SYMTYPE(i)  symtable[(i)].type
#define SYMLONGVAL1(i) symtable[(i)].val.longvals.val1
#define SYMLONGVAL2(i) symtable[(i)].val.longvals.val2
#define SYMDBLVAL(i) symtable[(i)].val.doubleval

/*
 * lookup_c_sym
 *
 */

static int
lookup_c_sym(symbol)
    char *symbol;
{
    register unsigned long sum, acc, slen, i, start;
    register char *s;
    int   inc;


    s = symbol;

    if (*s == '\0') {
	inc = 12;		/* let's be arbitrary about the empty string */
	start = SYMTBLSZ - 1;
    }
    else {
	slen = 0;
	sum = *s;

	while (*s) {
	    acc = 0;
	    for (i = 0; i < sizeof (long) && *s; i++)
		acc = (acc << 8) | *s++;
	    sum += acc;
	    slen += i;
	}

	sum *= slen;

	inc = (sum % (SYMTBLSZ / slen)) + 1;

	start = sum % SYMTBLSZ;
    }
/*printf("lookup_c_sym: sym=%s  slen=%d \n",symbol,slen);*/

    for (i = start; SYMNAME(i) != (char *) 0 && strcmp(symbol, SYMNAME(i)) != 0;) {
{
/*printf("lookup_c_sym: i=%d  s(i)=%s \n",i,SYMNAME(i));*/
	i = (i + inc) % SYMTBLSZ;
}
	if (i == start)
	    return (-1);
    }

    return (i);

}

#ifdef macintosh
#pragma export on
#endif


EXPORT ALSPI_API(int)
sym_insert_dbl(char *symbol, int type,  double doubleval)
{
    int   idx;

    if ((idx = lookup_c_sym(symbol)) < 0) {
	fprintf(stderr, "Error: in cinterf, Constant symbol table full.\n");
	return (0);
    }

    SYMNAME(idx) = symbol;
    SYMTYPE(idx) = type;
    SYMDBLVAL(idx) = doubleval;

    return (1);
}


EXPORT ALSPI_API(int)
sym_insert_2long(char *symbol, int type, long longval1, long longval2)
{
    int   idx;

    if ((idx = lookup_c_sym(symbol)) < 0) {
	fprintf(stderr, "Error: in cinterf, Constant symbol table full.\n");
	return (0);
    }

    SYMNAME(idx) = symbol;
    SYMTYPE(idx) = type;
    SYMLONGVAL1(idx) = longval1;
    SYMLONGVAL2(idx) = longval2;

    return (1);
}



/*---------------------------------------------------------------------------*
 | The CI_get_integer function will take a pointer to an argument
 | (which is a prolog object) and try to change it to a 32-bit integer.
 | If successful, the function returns "1", otherwise it returns "0".
 | Conversion is attempted in the following way:
 |
 | PI_ TYPE          CONVERSION
 | ========          ==========
 | PI_DOUBLE         Double is extracted an coerced into a C "unsigned long"
 | PI_UIA            Pointer to start of UIA body
 | PI_STRUCT         if principal functor is c/1, and sole argument is
 |                      PI_SYM or PI_UIA, do constant lookup on argument.  If
 |                      found, return the constant.  Otherwise, return "0".
 | PI_LIST           Take first four elements of list and use the lowest byte
 |                      of each to construct your 32-bit integer using the low
 |                      byte from the first element to be the highest byte of
 |                      the 32-bit int.
 | PI_SYM            None.  Return 0.
 | PI_VAR            None.  Return 0.
 *---------------------------------------------------------------------------*/

EXPORT ALSPI_API(int)
CI_get_integer(PWord *arg, int type)
{
    double adbl;

    switch (type) {
	case PI_INT:
	    return (1);
	case PI_DOUBLE:
	    PI_getdouble(&adbl, *arg);
	    *arg = (unsigned long) adbl;
	    return (1);

	case PI_UIA:
	    *arg = (unsigned long) PI_getuianame(0, *arg, 0);
	    return (1);

	case PI_STRUCT:
	    {
		PWord func, val;
		int arity, valtype;
		char *name;
		int   i;

		PI_getstruct(&func, &arity, *arg);

		if (func != constsym || (arity != 1))
		    return (0);

		PI_getargn(&val, &valtype, *arg, 1);
		if (valtype == PI_SYM)
		    name = PI_getsymname(0, val, 0);
		else if (valtype == PI_UIA)
		    name = PI_getuianame(0, val, 0);
		else
		    return (0);

		if ((i = lookup_c_sym(name)) == 0)
		    return (0);

		if (SYMNAME(i) == (char *) 0)
		    return (0);

		switch (SYMTYPE(i)) {
		    case CI_INTTYPE:
		    case CI_SHORTTYPE:
		    case CI_CHARTYPE:
		    case CI_LONGTYPE:
		    case CI_PTRTYPE:
			*arg = SYMLONGVAL1(i);
			return (1);

		    case CI_FLOATTYPE:
		    case CI_DOUBLETYPE:
			*arg = (unsigned long) SYMDBLVAL(i);
			return (1);
		}

		return (0);
	    }
	case PI_LIST:
	    {
		unsigned long result;
		PWord head, tail;
		int headtype, tailtype;

		PI_gethead(&head, &headtype, *arg);
		result = (head & 0xff) << 24;
		PI_gettail(&tail, &tailtype, *arg);
		if (tailtype != PI_LIST)
		    return (0);
		PI_gethead(&head, &headtype, tail);
		result |= ((head & 0xff) << 16);
		PI_gettail(&tail, &tailtype, tail);
		if (tailtype != PI_LIST)
		    return (0);
		PI_gethead(&head, &headtype, tail);
		result |= ((head & 0xff) << 8);
		PI_gettail(&tail, &tailtype, tail);
		if (tailtype != PI_LIST)
		    return (0);
		PI_gethead(&head, &headtype, tail);
		result |= (head & 0xff);
		*arg = result;
		return (1);

	    }
    }

    return (0);
}

/*
 * int CI_get_double(double *,unsigned long, unsigned long)
 */

EXPORT ALSPI_API(int)
CI_get_double(double *dbl, unsigned long arg, unsigned long type)
{
    switch (type) {
	case PI_INT:
	    *dbl = (double) arg;
	    return (1);

	case PI_DOUBLE:
	    PI_getdouble(dbl, (PWord)arg);
	    return (1);

	case PI_STRUCT:
	    {
		PWord func, val;
		int arity, valtype;
		char *name;
		int   i;

		PI_getstruct(&func, &arity, (PWord)arg);

		if (func != constsym || arity != 1)
		    return (0);

		PI_getargn(&val, &valtype, (PWord)arg, 1);
		if (valtype == PI_SYM)
		    name = PI_getsymname(0, val, 0);
		else if (valtype == PI_UIA)
		    name = PI_getuianame(0, val, 0);
		else
		    return (0);

		if ((i = lookup_c_sym(name)) == 0)
		    return (0);

		if (SYMNAME(i) == (char *) 0)
		    return (0);

		switch (SYMTYPE(i)) {
		    case CI_INTTYPE:
		    case CI_SHORTTYPE:
		    case CI_CHARTYPE:
		    case CI_LONGTYPE:
		    case CI_PTRTYPE:
			*dbl = (double) SYMLONGVAL1(i);
			return (1);

		    case CI_FLOATTYPE:
		    case CI_DOUBLETYPE:
			*dbl = SYMDBLVAL(i);
			return (1);
		}

		return (0);
	    }
    }

    return (0);
}

#ifdef macintosh
#pragma export off
#endif

/*
 * Interface to structures and other c types
 */

static int
c_structinfo()
{				/* usage :
				 * $c_structinfo(name,size__re,list_ret)
				 */
    PWord V1;
    int   T1;
    PWord V2;
    int   T2;
    PWord V3;
    int   T3;
    PWord v;
    int   t;
    PWord list;
    int   listtype;
    PWord head;
    int   headtype;
    PWord arg;
    int   argtype;
    PWord sval;
    int   stype;
    char *name;
    register FieldEntry *field;
    int   size;
    register int i;
    int   arity;

    PI_getan(&V1, &T1, 1);
    PI_getan(&V2, &T2, 2);
    PI_getan(&V3, &T3, 3);

    if (T1 == PI_UIA)
	name = PI_getuianame(0, V1, 0);
    else if (T1 == PI_SYM)
	name = PI_getsymname(0, V1, 0);
    else
	PI_FAIL;

    if ((i = lookup_c_sym(name)) == 0)
	PI_FAIL;

    if (SYMNAME(i) == (char *) 0 || SYMTYPE(i) != CI_STRUCTTYPE)
	PI_FAIL;

    size = SYMLONGVAL1(i);
    field = (FieldEntry *) SYMLONGVAL2(i);

    /*
     * create a prolog list out of fields
     */

    for (i = 0; field[i].fname; i++) {
	PI_makelist(&list, &listtype);
	if (!PI_unify(V3, T3, list, listtype))
	    PI_FAIL;

	PI_gethead(&head, &headtype, list);

	/* for non-array type construct f(name,offset,typecode/structname)
	 * and for array type construct f(name,offset,typecode/structname,arraysz)
	 * and unify the term with head
	 */

	arity = (field[i].arraysz ? 4 : 3);
	PI_makestruct(&sval, &stype, fieldsym, arity);
	if (!PI_unify(head, headtype, sval, stype))
	    PI_FAIL;

	PI_getargn(&arg, &argtype, sval, 1);
	PI_makeuia(&v, &t, field[i].fname);
	if (!PI_unify(arg, argtype, v, t))
	    PI_FAIL;

	PI_getargn(&arg, &argtype, sval, 2);
	if (!PI_unify(arg, argtype, (PWord) field[i].foffset, PI_INT))
	    PI_FAIL;

	PI_getargn(&arg, &argtype, sval, 3);
	if (field[i].ftype != 0) {
	    if (!PI_unify(arg, argtype, field[i].ftype, PI_INT))
		PI_FAIL;
	}
	else {
	    PI_makeuia(&v, &t, field[i].type_name);
	    if (!PI_unify(arg, argtype, v, t))
		PI_FAIL;
	}

	if (field[i].arraysz) {
	    PI_getargn(&arg, &argtype, sval, 4);
	    if (!PI_unify(arg, argtype, field[i].arraysz, PI_INT))
		PI_FAIL;
	}

	PI_gettail(&V3, &T3, list);
    }

    if (!PI_unify(V3, T3, pnil, niltype) ||
	!PI_unify(V2, T2, size, PI_INT))
	PI_FAIL;
    PI_SUCCEED;
}


static int
c_typeinfo()
{				/* usage :
				 * $c_typeinfo(name,size_reet)ype_ret)
				 */
    PWord V1;
    int   T1;
    PWord V2;
    int   T2;
    PWord V3;
    int   T3;
    PWord v;
    char *name;
    register int i;

    PI_getan(&V1, &T1, 1);
    PI_getan(&V2, &T2, 2);
    PI_getan(&V3, &T3, 3);

    if (T1 == PI_UIA)
	name = PI_getuianame(0, V1, 0);
    else if (T1 == PI_SYM)
	name = PI_getsymname(0, V1, 0);
    else
	PI_FAIL;

/*printf("c_typeinfo: name=%s  \n",name); */

    if ((i = lookup_c_sym(name)) == 0)
	PI_FAIL;

    if (SYMNAME(i) == (char *) 0)
	PI_FAIL;

    switch (SYMTYPE(i)) {
	case CI_CTYPE:
	    v = SYMLONGVAL2(i);
	    break;

	case CI_STRUCTTYPE:
	    v = 0;
	    break;

	default:
	    PI_FAIL;
    }

    if (!PI_unify(V2, T2, SYMLONGVAL1(i), PI_INT) ||
	!PI_unify(V3, T3, v, PI_INT))
	PI_FAIL;

    PI_SUCCEED;
}

/*
 * Interface to #define constants and runtime constants
 */


static int
c_constinfo()
{				/* usage : $c_constinfo(name,val_ret) */
    PWord V1;
    int   T1;
    PWord V2;
    int   T2;
    PWord v;
    int   t;
    char *name;
    register int i;

    PI_getan(&V1, &T1, 1);
    PI_getan(&V2, &T2, 2);

    if (T1 == PI_UIA)
	name = PI_getuianame(0, V1, 0);
    else if (T1 == PI_SYM)
	name = PI_getsymname(0, V1, 0);
    else
	PI_FAIL;

    if ((i = lookup_c_sym(name)) == 0)
	PI_FAIL;

    if (SYMNAME(i) == (char *) 0)
	PI_FAIL;

    switch (SYMTYPE(i)) {
	case CI_INTTYPE:
	case CI_SHORTTYPE:
	case CI_CHARTYPE:
	case CI_LONGTYPE:
	case CI_PTRTYPE:
	    PI_makedouble(&v, &t, (double) SYMLONGVAL1(i));
	    break;

	case CI_STRINGTYPE:
	    PI_makeuia(&v, &t, (char *) SYMLONGVAL1(i));
	    break;

	case CI_FLOATTYPE:
	case CI_DOUBLETYPE:
	    PI_makedouble(&v, &t, SYMDBLVAL(i));
	    break;

	default:
	    PI_FAIL;
    }

    if (!PI_unify(V2, T2, v, t))
	PI_FAIL;

    PI_SUCCEED;
}


static int
c_rconstinfo()
{				/* usage : $c_rconstinfo(name,val_ret) */
    PWord V1;
    int   T1;
    PWord V2;
    int   T2;
    PWord v;
    int   t;
    char *name;
    register int i;

    PI_getan(&V1, &T1, 1);
    PI_getan(&V2, &T2, 2);

    if (T1 == PI_UIA)
	name = PI_getuianame(0, V1, 0);
    else if (T1 == PI_SYM)
	name = PI_getsymname(0, V1, 0);
    else
	PI_FAIL;

    if ((i = lookup_c_sym(name)) == 0)
	PI_FAIL;

    if (SYMNAME(i) == (char *) 0 || SYMTYPE(i) != CI_RCONSTTYPE)
	PI_FAIL;

    PI_makedouble(&v, &t, (double) SYMLONGVAL1(i));
    if (!PI_unify(V2, T2, v, t))
	PI_FAIL;

    PI_SUCCEED;
}


/*
 * usage: $c_call(PrologPtrToFcn, ArgList, VarForFcnRetrnPtr)
 */

#define MAXNARGS 30

static int
c_call()
{
    PWord v1;
    int   t1;			/* function pointer */
    PWord v2;
    int   t2;			/* arglist */
    PWord vret;
    int   tret;			/* function return value */
    int   (*funcptr) ( char *, ...);
    long  retval;
    PWord head;
    int   headtype;
    char *args[MAXNARGS + 1];	/* array for saving arguments */
    int   nargs = 0;		/* #input arguments */

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);
    PI_getan(&vret, &tret, 3);

    if (!CI_get_integer(&v1, t1))
	PI_FAIL;
    funcptr = (int (*)(char *, ...)) v1;

    while (t2 == PI_LIST) {
	PI_gethead(&head, &headtype, v2);
	if (headtype == PI_SYM)
	    args[nargs++] = PI_getsymname(0, head, 0);
	else if (CI_get_integer(&head, headtype))
	    args[nargs++] = (char *) head;
	else
	    PI_FAIL;

	PI_gettail(&v2, &t2, v2);
    }

    switch (nargs) {
	case 0:
	    retval = (*(int (*)(void))funcptr) ();
	    break;
	case 1:
	    retval = (*funcptr) (args[0]);
	    break;
	case 2:
	    retval = (*funcptr) (args[0], args[1]);
	    break;
	case 3:
	    retval = (*funcptr) (args[0], args[1], args[2]);
	    break;
	case 4:
	    retval = (*funcptr) (args[0], args[1], args[2], args[3]);
	    break;
	case 5:
	    retval = (*funcptr) (args[0], args[1], args[2], args[3], args[4]);
	    break;
	case 6:
	    retval = (*funcptr) (args[0], args[1], args[2], args[3],
				 args[4], args[5]);
	    break;
	case 7:
	    retval = (*funcptr) (args[0], args[1], args[2], args[3],
				 args[4], args[5], args[6]);
	    break;
	case 8:
	    retval = (*funcptr) (args[0], args[1], args[2], args[3],
				 args[4], args[5], args[6], args[7]);
	    break;
	case 9:
	    retval = (*funcptr) (args[0], args[1], args[2], args[3],
				 args[4], args[5], args[6], args[7],
				 args[8]);
	    break;
	case 10:
	    retval = (*funcptr) (args[0], args[1], args[2], args[3],
				 args[4], args[5], args[6], args[7],
				 args[8], args[9]);
	    break;

	default:
	    PI_FAIL;
    }

    if (!PI_unify(vret, tret, retval, PI_INT))
	PI_FAIL;
    PI_SUCCEED;
}

/*
 *  Some basic STRING handling functions.  Added by Ron 9/22/92 to
 *  help in the testing of compact interfaces generated for the
 *  Mac Toolbox.  c_makeuia() will take either a C string or a
 *  Pascal string and return a UIA which represents it.
 *  c_convertCstrPstr convert between c and pascal strings.
 */

static int
c_makeuia()
{
    PWord PascalFlag;
    int PascalFlagType;
    PWord Pointer;
    int PointerType;
    PWord ReturnArg;
    int ReturnArgType;
    PWord theUIA;
    int theUIAtype;
    char  buf[256];

    PI_getan(&PascalFlag, &PascalFlagType, 1);
    PI_getan(&Pointer, &PointerType, 2);
    PI_getan(&ReturnArg, &ReturnArgType, 3);

    if (PascalFlagType != PI_INT)
	PI_FAIL;

    if (PointerType != PI_INT)
	if (!CI_get_integer(&Pointer, PointerType))
	    PI_FAIL;

    if (PascalFlag) {
	short len = *(char *) Pointer;

	Pointer++;
	strncpy(buf, (char *) Pointer, (size_t)len);

	Pointer = (PWord) & buf[0];
    }

    PI_makeuia(&theUIA, &theUIAtype, (char *) Pointer);

    if (!PI_unify(theUIA, theUIAtype, ReturnArg, ReturnArgType))
	PI_FAIL;

    PI_SUCCEED;
}


static int
c_convertCstrPstr()
{
    PWord Pointer, PascalFlag;
    int PointerType, PascalFlagType;
    long  i, len;
    char *ptr;

    PI_getan(&PascalFlag, &PascalFlagType, 1);
    PI_getan(&Pointer, &PointerType, 2);

    if (PascalFlagType != PI_INT)
	PI_FAIL;

    if (PointerType != PI_INT)
	if (!CI_get_integer(&Pointer, PointerType))
	    PI_FAIL;

    if (PascalFlag) {
	/* Convert FROM Pascal TO C */
	len = *(char *) Pointer;
	ptr = (char *) (Pointer + 1);

	for (i = 0; i < len; i++)
	    *ptr = *(ptr + 1);

	*ptr = 0;

	PI_SUCCEED;
    }

    /* ELSE convert FROM C to Pascal */

    ptr = (char *) Pointer;
    len = strlen(ptr);

    for (i = len; i > 0; i--)
	ptr[i] = ptr[i - 1];

    ptr[0] = (char)((len >= 255) ? 255 : len);

    PI_SUCCEED;
}

#define CB_TABLE_SIZE 257

struct {
    void *func, *object;
    const char *term;    
} callback_table[CB_TABLE_SIZE];

static void init_callback_table(void)
{
    int i;
    
    i = SYMLONGVAL1(1);
    
    for (i = 0; i < CB_TABLE_SIZE; i++) callback_table[i].func = NULL;
}

static int lookup_callback(void *func, void *object)
{
    int firstkey, key;
    
    firstkey = key = ((unsigned long)func + (unsigned long)object) % CB_TABLE_SIZE;
    
    while (1) {
    	if (callback_table[key].func == NULL
    	    || (callback_table[key].func == func && callback_table[key].object == object)) break;
    	key = (key+1) % CB_TABLE_SIZE;
    	if (key == firstkey) {key = -1; break;}
    }

    return key;
}

static void add_callback(void *func, void *object, const char *term)
{
    int i;
    
    if ((i = lookup_callback(func, object)) >= 0) {
        callback_table[i].func = func;
        callback_table[i].object = object;
        callback_table[i].term = term;
    }
}

static void remove_callback(void *func, void *object, const char *term)
{
    int i;

    if ((i = lookup_callback(func, object)) >= 0) {
        callback_table[i].func = NULL;
    }
}

#ifdef macintosh
#pragma export on
#endif

EXPORT ALSPI_API(const char *)
find_callback(void *func, void *object)
{
    int i;

    if ((i = lookup_callback(func, object)) >= 0) {
        return callback_table[i].term;
    } else return NULL;
}

#ifdef macintosh
#pragma export off
#endif

/* Warning: when binding and unbinding symbols to (callback, object) pairs, I assume
   that the C string pointer to the symbol will never move. Is this true? */
static int c_bind_callback(void)
{
	PWord arg1, arg2, arg3;
	int type1, type2, type3;
	
	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
			
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else PI_FAIL;
	
	add_callback((void *)arg1, (void *)arg2, (const char *)arg3);
	PI_SUCCEED;
}

static int c_unbind_callback(void)
{
	PWord arg1, arg2, arg3;
	int type1, type2, type3;
	
	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else PI_FAIL;
	
	remove_callback((void *)arg1, (void *)arg2, (const char *)arg3);
	PI_SUCCEED;
}

/* *INDENT-OFF* */
PI_BEGIN
    PI_PDEFINE("$c_structinfo", 3, c_structinfo, "_c_structinfo")
    PI_PDEFINE("$c_typeinfo", 3, c_typeinfo, "_c_typeinfo")
    PI_PDEFINE("$c_constinfo", 2, c_constinfo, "_c_constinfo")
    PI_PDEFINE("$c_rconstinfo", 2, c_rconstinfo, "_c_rconstinfo")
    PI_PDEFINE("$c_call", 3, c_call, "_c_call")
    PI_PDEFINE("$c_makeuia", 3, c_makeuia, "_c_makeuia")
    PI_PDEFINE("$c_convertcstrpstr", 2, c_convertCstrPstr, "_c_convertCstrPstr")

    PI_PDEFINE("$c_bind_callback", 3, c_bind_callback, "_c_bind_callback")
    PI_PDEFINE("$c_unbind_callback", 3, c_unbind_callback, "_c_unbind_callback")
PI_END
/* *INDENT-ON* */

void
cinterf_init()
{
    PI_makesym(&fieldsym, &fieldsymtype, "f");
    PI_makesym(&pnil, &niltype, "[]");	/* make a nil symbol */
    PI_makesym(&farsym, &farsymtype, "$farptr");
    PI_makesym(&constsym, &constsymtype, "c");

    init_callback_table();

    PI_INIT;
}
#endif /* BCINTER */
