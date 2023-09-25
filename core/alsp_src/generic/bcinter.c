/*=================================================================*
 |			bcinter.c   
 |		Copyright (c) 1991-1995 Applied Logic Systems, Inc.
 |
 |			-- support routines for C interface
 |
 | Author: Prabhakaran Raman
 | Date  : 7/24/91
 | 10/26/94 - C. Houpt - Added char* casts for standard library string calls.
 *=================================================================*/

#include "defs.h"
#include <stdio.h>
#ifdef BCINTER

/*
 * usage : $c_malloc(size,ptr_return)
 */

int
pbi_c_malloc(void)
{
    PWord v1;
    int   t1;
    PWord v2;
    int   t2;
    PWord v;
    int   t;
    long  return_value;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_INTEGER
	&& v1 > 0
	&& (return_value = (long) malloc((size_t)v1)) != (long) NULL) {
	make_number(&v, &t, (double) return_value);
	if (w_unify(v2, t2, v, t))
	    SUCCEED;
    }
    FAIL;
}

/*
 * usage : $c_free(ptr)
 */

int
pbi_c_free(void)
{
    PWord v;
    int   t;
    double ptrval;

    w_get_An(&v, &t, 1);

    if (get_number(v, t, &ptrval)) {
	free((char *) (long) ptrval);
	SUCCEED;
    }
    FAIL;
}

/*
 * usage : $c_set(UIAorPtr, [f(offset,typeid,data{,length}),...])
 *
 * offset is an integer offset from from the start of UIA or Pointer
 * typeid is an integer identifying a C type as shown below
 *      1  -- int
 *      2  -- unsigned int
 *      3  -- long
 *      4  -- unsigned long
 *      5  -- pointer
 *      6  -- char
 *      7  -- unsigned char
 *      8  -- short
 *      9  -- unsigned short
 *      10 -- string
 *      11 -- string of given length (length is 4th arg)
 *      12 -- float
 *      13 -- double
 *      14 -- far pointer  (DOS only)
 *      15 -- raw data of given length
 */

int
pbi_c_set(void)
{
    PWord v1;
    int   t1;
    PWord v2;
    int   t2;
    PWord head;
    int   headtype;
    int   arity;
    PWord offset;
    int   offsettype;
    PWord type;
    int   typetype;
    PWord data;
    int   datatype;
    PWord length;
    int   lengthtype;
    double doubleval;
    register UCHAR *ptr;
    register UCHAR *src;
    register UCHAR *i;

    /*
     * get the arguments
     */
    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    /*
     * the target object can be a uia or a c pointer
     */
    if (t1 == WTP_UIA)
	ptr = (UCHAR *) M_FIRSTUIAWORD(v1);
    else if (get_number(v1, t1, &doubleval))
	ptr = (UCHAR *) (long) doubleval;
    else
	FAIL;

    if (t2 != WTP_LIST)
	FAIL;

    while (t2 == WTP_LIST) {

	w_get_car(&head, &headtype, v2);

	if (headtype != WTP_STRUCTURE)
	    FAIL;

	w_get_arity(&arity, head);

	if (arity < 3)
	    FAIL;

	w_get_argn(&offset, &offsettype, head, 1);
	w_get_argn(&type, &typetype, head, 2);
	w_get_argn(&data, &datatype, head, 3);

	if (offsettype != WTP_INTEGER ||
	    typetype != WTP_INTEGER ||
	    !(arity == 3 || (arity == 4 && (type == 11 || type == 15)))
	    )
	    FAIL;

	switch (type) {
	    case 1:		/* int type */
		if (!get_number(data, datatype, &doubleval))
		    FAIL;
		*(int *) (ptr + offset) = (int) doubleval;
		break;

	    case 2:		/* unsigned int type */
		if (!get_number(data, datatype, &doubleval))
		    FAIL;
		*(unsigned int *) (ptr + offset) = (unsigned int) doubleval;
		break;

	    case 3:		/* long type */
		if (!get_number(data, datatype, &doubleval))
		    FAIL;
		*(long *) (ptr + offset) = (long) doubleval;
		break;

	    case 4:		/* unsigned long type */
		if (!get_number(data, datatype, &doubleval))
		    FAIL;
		*(unsigned long *) (ptr + offset) = (unsigned long) doubleval;
		break;

	    case 5:		/* ptr type */
		if (!get_number(data, datatype, &doubleval))
		    FAIL;
		*(UCHAR **) (ptr + offset) = (UCHAR *) (long) doubleval;
		break;

	    case 6:		/* char type */
		if (datatype != WTP_INTEGER)
		    FAIL;
		*(char *) (ptr + offset) = (char) data;
		break;

	    case 7:		/* unsigned char type */
		if (datatype != WTP_INTEGER)
		    FAIL;
		*(unsigned char *) (ptr + offset) = (unsigned char) data;
		break;

	    case 8:		/* short type */
		if (datatype != WTP_INTEGER)
		    FAIL;
		*(short *) (ptr + offset) = (short) data;
		break;

	    case 9:		/* unsigned short type */
		if (datatype != WTP_INTEGER)
		    FAIL;
		*(unsigned short *) (ptr + offset) = (unsigned short) data;
		break;

	    case 10:		/* string type */
		if (datatype == WTP_UIA)
		    src = (UCHAR *) M_FIRSTUIAWORD(data);
		else if (datatype == WTP_SYMBOL)
		    src = TOKNAME(data);
		else
		    FAIL;
		strcpy((char *)ptr + offset, (char *)src);
		break;

	    case 11:		/* fixed length string */
		if (datatype == WTP_UIA)
		    src = (UCHAR *) M_FIRSTUIAWORD(data);
		else if (datatype == WTP_SYMBOL)
		    src = TOKNAME(data);
		else
		    FAIL;

		w_get_argn(&length, &lengthtype, head, 4);
		if (lengthtype != WTP_INTEGER || length < 0)
		    FAIL;
		strncpy((char *)ptr + offset, (char *)src, (size_t) length);
		break;

	    case 12:		/* float type */
		if (!get_number(data, datatype, &doubleval))
		    FAIL;
		*(float *) (ptr + offset) = (float) doubleval;
		break;

	    case 13:		/* double type */
		if (!get_number(data, datatype, &doubleval))
		    FAIL;
		*(double *) (ptr + offset) = doubleval;
		break;

#if defined(DOS) && !defined(__GO32__) && !defined(OS2)
	    case 14:		/* far pointer type */
		if (datatype != WTP_STRUCTURE)
		    FAIL;
		w_get_arity(&arity, data);
		if (arity != 3)
		    FAIL;

		w_get_argn(&v, &t, data, 1);
		*(unsigned short *) (ptr + offset) = (unsigned short) v;
		w_get_argn(&v, &t, data, 2);
		*(((unsigned short *) (ptr + offset)) + 1) = (unsigned short) v;
		w_get_argn(&v, &t, data, 3);
		*(((unsigned short *) (ptr + offset)) + 2) = (unsigned short) v;
		break;
#endif

	    case 15:		/* raw data of given length */
		if (datatype == WTP_UIA)
		    src = (UCHAR *) M_FIRSTUIAWORD(data);
		else
		    FAIL;

		w_get_argn(&length, &lengthtype, head, 4);
		if (lengthtype != WTP_INTEGER || length < 0)
		    FAIL;

		i = ptr + offset;
		while (length--)
		    *i++ = *src++;

		break;


	    default:
		FAIL;
	}

	w_get_cdr(&v2, &t2, v2);
    }
    SUCCEED;
}

/*
 * usage : $c_examine(+ptr, [f(+offset,+typeid,-data{,+length}),...])
 *
 * Notes : see comments for pbi_c_set.
 */

int
pbi_c_examine(void)
{
    PWord v1;
    int   t1;
    PWord v2;
    int   t2;
    PWord v;
    int   t;
    PWord head;
    int   headtype;
    int   arity;
    PWord offset;
    int   offsettype;
    PWord type;
    int   typetype;
    PWord data;
    int   datatype;
    PWord length;
    int   lengthtype;
    double doubleval;
    UCHAR *chptr;
    register UCHAR *ptr;
    register UCHAR *i;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_UIA)
	ptr = (UCHAR *) M_FIRSTUIAWORD(v1);
    else if (get_number(v1, t1, &doubleval))
	ptr = (UCHAR *) (long) doubleval;
    else
	FAIL;

    if (t2 != WTP_LIST)
	FAIL;

    while (t2 == WTP_LIST) {

	w_get_car(&head, &headtype, v2);

	if (headtype != WTP_STRUCTURE)
	    FAIL;

	w_get_arity(&arity, head);

	if (arity < 3)
	    FAIL;

	w_get_argn(&offset, &offsettype, head, 1);
	w_get_argn(&type, &typetype, head, 2);
	w_get_argn(&data, &datatype, head, 3);

	if (offsettype != WTP_INTEGER ||
	    typetype != WTP_INTEGER ||
	    !(arity == 3 || (arity == 4 && (type == 11 || type == 15)))
	    )
	    FAIL;

	switch (type) {
	    case 1:		/* int type */
		make_number(&v, &t, (double)( *(int *) (ptr + offset)));
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 2:		/* unsigned int type */
		make_number(&v, &t, (double) *(unsigned int *) (ptr + offset));
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 3:		/* long type */
		make_number(&v, &t, (double) *(long *) (ptr + offset));
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 4:		/* unsigned long type */
		make_number(&v, &t, (double) *(unsigned long *) (ptr + offset));
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 5:		/* ptr type */
		make_number(&v, &t, (double) (long) *(UCHAR **) (ptr + offset));
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 6:		/* char type */
		if (!w_unify(data, datatype,
			     (PWord) * (char *) (ptr + offset), WTP_INTEGER))
		    FAIL;
		break;

	    case 7:		/* unsigned char type */
		if (!w_unify(data, datatype,
		   (PWord) * (unsigned char *) (ptr + offset), WTP_INTEGER))
		    FAIL;
		break;

	    case 8:		/* short type */
		if (!w_unify(data, datatype,
			   (PWord) * (short *) (ptr + offset), WTP_INTEGER))
		    FAIL;
		break;

	    case 9:		/* unsigned short type */
		if (!w_unify(data, datatype,
		  (PWord) * (unsigned short *) (ptr + offset), WTP_INTEGER))
		    FAIL;
		break;

	    case 10:		/* string type */
		w_mk_uia(&v, &t, ptr + offset);
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 11:		/* string of given length */
		w_get_argn(&length, &lengthtype, head, 4);
		if (lengthtype != WTP_INTEGER || length < 0)
		    FAIL;

		w_uia_alloc(&v, &t, (size_t)length);
		chptr = (UCHAR *) M_FIRSTUIAWORD(v);
		strncpy((char *)chptr, (char *)ptr + offset, (size_t)length);

		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 12:		/* float type */
		make_numberx(&v, &t, (double) *(float *) (ptr + offset), WTP_DOUBLE);
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    case 13:		/* double type */
		make_numberx(&v, &t, *(double *) (ptr + offset), WTP_DOUBLE);
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

#ifdef DOS
	    case 14:		/* far pointer type */
		make_farptr(&v, &t, (unsigned short *) (ptr + offset));
		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;
#endif

	    case 15:		/* raw data of given length */
		w_get_argn(&length, &lengthtype, head, 4);
		if (lengthtype != WTP_INTEGER || length < 0)
		    FAIL;

		w_uia_alloc(&v, &t, (size_t)length);
		chptr = (UCHAR *) M_FIRSTUIAWORD(v);

		i = ptr + offset;
		while (length--)
		    *chptr++ = *i++;

		if (!w_unify(data, datatype, v, t))
		    FAIL;
		break;

	    default:
		FAIL;
	}

	w_get_cdr(&v2, &t2, v2);
    }
    SUCCEED;
}


#ifdef DOS

void
make_farptr(PWord *vp, int *tp, unsigned short *ptr)
{
    w_mk_term(vp, tp, (PWord) find_token("$farptr"), 3);
    w_install_argn(*vp, 1, (PWord) * (ptr), WTP_INTEGER);
    w_install_argn(*vp, 2, (PWord) * (ptr + 1), WTP_INTEGER);
    w_install_argn(*vp, 3, (PWord) * (ptr + 2), WTP_INTEGER);
}

/*
 * usage : $c_make_farptr(ptr,seg,farptr)
 */
int
pbi_c_make_farptr(void)
{
    PWord v1;
    int   t1;
    PWord v2;
    int   t2;
    PWord v3;
    int   t3;
    PWord v;
    int   t;
    double ptrval;
    long  longptrval;
    _Far *farptrval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (get_number(v1, t1, &ptrval) && t2 == WTP_INTEGER) {
	longptrval = (long) ptrval;
	if (v2 == 1)		/* assume data segment */
	    farptrval = (_Far *) (char *) longptrval;
	else			/* assume no segment (0) */
	    farptrval = (_Far *) longptrval;
	make_farptr(&v, &t, (unsigned short *) &farptrval);
	if (!w_unify(v3, t3, v, t))
	    FAIL;
	SUCCEED;
    }
    else
	FAIL;
}

#endif /* DOS */

#endif /* BCINTER */
