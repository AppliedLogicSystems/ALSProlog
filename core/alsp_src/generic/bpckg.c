/*
 * bpckg.c   -- Package builtins defined in C.
 *
 * Copyright (c) 1989-1993 by Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 5/5/89
 */

#include "defs.h"

#ifdef PACKAGE

#include <stdio.h>

/*
 * Include this file if PACKAGE is defined.
 */

#include "wintcode.h"
#include "coerce.h"
#include "pckg.h"
#include "rinfo.h"
#include "pckgcoff.h"

int   pckg_error = 0;

pbi_get_pckg_error()
{				/* $get_pckg_error(Error) */
    PWord v1;
    int   t1;

    w_get_An(&v1, &t1, 1);

    if (!w_unify(v1, t1, pckg_error, WTP_INTEGER))
	FAIL;

    SUCCEED;
}


pbi_package_listasm_clause()
{				/* $listasm_clause(DBRef,PckgName) */
    PWord v1, v2;
    int   t1, t2;
    int   na;
    long *ca;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (((ca = validate_dbref(v1, t1, &na)) != 0 && force_uia(&v2, &t2)) &&
	(listasm_clause(ca, TOKNAME(v2)) != -1))
	SUCCEED;
    else
	FAIL;
}


pbi_package_listasm_ntblentry()
{				/* $listasm_ntblentry(M,P,A,PckgName)   */
    PWord m, p, a, v4;
    int   mt, pt, at, t4;
    ntbl_entry *ent;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);
    w_get_An(&v4, &t4, 4);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) &&
	at == WTP_INTEGER && force_uia(&v4, &t4)) {
	if ((ent = w_nameprobe(m, p, (int) a)) &&
	    (listasm_ntblentry(ent, TOKNAME(v4)) != -1))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


pbi_package_mark_proc()
{				/* $package_mark_proc(Module,Pred,Arity)   */
    PWord m, p, a;
    int   mt, pt, at;
    ntbl_entry *ent;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER &&
	(ent = w_nameprobe(m, p, (int) a)) && mark_package_proc(ent))
	SUCCEED;
    else
	FAIL;
}


pbi_package_unmark_proc()
{				/* $package_unmark_proc(Module,Pred,Arity)   */
    PWord m, p, a;
    int   mt, pt, at;
    ntbl_entry *ent;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER &&
	(ent = w_nameprobe(m, p, (int) a)) && unmark_package_proc(ent))
	SUCCEED;
    else
	FAIL;
}


pbi_package_toktbl()
{				/* $package_toktbl(NumofToks) */
    PWord v1, n;
    int   t1;

    w_get_An(&v1, &t1, 1);

    if ((n = save_toktbl()) && (n != -1) && w_unify(v1, t1, n, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}



pbi_get_default_proc()
{				/* $get_default_proc(idx,pred,arity) */
    PWord v1, v2, v3;
    PWord t1, t2, t3;

    PWord p, a;
    PWord pt, at;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_INTEGER && (get_default_proc(v1, &p, &pt, &a, &at) != 0) &&
	w_unify(v2, t2, p, pt) && w_unify(v3, t3, a, at))
	SUCCEED;
    else
	FAIL;
}


pbi_get_default_use()
{				/* $get_default_use(idx,ModName) */
    PWord v1, v2;
    PWord t1, t2;

    PWord m;
    PWord mt;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_INTEGER && (get_default_use(v1, &m, &mt) != 0) &&
	w_unify(v2, t2, m, mt))
	SUCCEED;
    else
	FAIL;
}


pbi_current_package()
{				/* $current_package(Pckg) */
    PWord v1;
    PWord t1;

    w_get_An(&v1, &t1, 1);

    if ((system_pckg != (long *) -1) &&
	w_unify(v1, t1, find_token(PCKG_NAME(system_pckg)), WTP_SYMBOL))
	SUCCEED;
    else if (w_unify(v1, t1, -1, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}


#define MAX_PROC_NAME_LEN 256

pbi_coff_operation()
{				/* $coff_operation(OpCode,Arg1,Arg2,Arg3,Arg4)
				 */
    PWord v1, v2, v3, v4, v5;
    PWord t1, t2, t3, t4, t5;
    char *ptr;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);

    if ((t1 != WTP_INTEGER) || (v1 < 0) || (v1 > NUMOF_COFF_OPERATIONS))
	FAIL;

    switch (v1) {
	case COP_OPEN_RAWFILE:
	    if (getstring(&ptr, v2, t2) == 0 ||
		coff_open_rawdata_file(ptr) < 0)
		FAIL;
	    break;

	case COP_INIT_PACKAGE:
	    if ((t2 != WTP_INTEGER) || (t3 != WTP_INTEGER) ||
		(t4 != WTP_INTEGER) ||
		coff_init_package(v2, v3, v4) < 0)
		FAIL;
	    break;

	case COP_ALIGN4:
	    if (coff_align4_rawdata() < 0)
		FAIL;
	    break;

	case COP_ALIGN8:
	    if (coff_align8_rawdata() < 0)
		FAIL;
	    break;

	case COP_PUT_CHAR:
	    if (t2 != WTP_INTEGER || coff_insert_char_rawdata(CharVal(v2)) < 0)
		FAIL;
	    break;

	case COP_PUT_SHORT:
	    if (t2 != WTP_INTEGER || coff_insert_short_rawdata(ShortVal(v2)) < 0)
		FAIL;
	    break;

	case COP_PUT_LONG:
	    if (t2 != WTP_INTEGER || coff_insert_long_rawdata(LongVal(v2)) < 0)
		FAIL;
	    break;

	case COP_PUT_STRING:
	    if (!getstring(&ptr, v2, t2) ||
		coff_insert_string_rawdata(ptr) < 0)
		FAIL;
	    break;

	case COP_PUT_SYMBOL:
	    if (!getstring(&ptr, v2, t2) ||
		coff_insert_symbol(ptr, SYMDEF_REFERENCE) < 0 ||
		coff_insert_reloc(ptr, P_R_VIR32, 0) < 0 ||
		coff_insert_long_rawdata(0) < 0)
		FAIL;
	    break;

	case COP_PUT_PROCNAME:
	    {
		char  buf[MAX_PROC_NAME_LEN];

		if (getstring(&ptr, v2, t2) &&
		    xform_uia(&v3, &t3) && xform_uia(&v4, &t4) &&
		    t5 == WTP_INTEGER) {
		    ntbl_entry *ent;

		    if (ent = w_nameprobe(v3, v4, (int) v5))
			sprintf(buf, "%s_%x", ptr, ent);
		    else
			FAIL;
		}
		else
		    FAIL;
		if (coff_insert_symbol(buf, SYMDEF_REFERENCE) < 0 ||
		    coff_insert_reloc(buf, P_R_VIR32, 0) < 0 ||
		    coff_insert_long_rawdata(0) < 0)
		    FAIL;
	    }
	    break;

	case COP_DECLARE_SYMBOL:
	    if (!getstring(&ptr, v2, t2) ||
		coff_insert_symbol(ptr, SYMDEF_DECLARE) < 0)
		FAIL;
	    break;

	case COP_DECLARE_GLOBAL_SYMBOL:
	    if (!getstring(&ptr, v2, t2) ||
		coff_insert_symbol(ptr, SYMDEF_DECLARE_GLOBAL) < 0)
		FAIL;
	    break;

	case COP_DECLARE_C_GLOBAL_SYMBOL:
	    if (!getstring(&ptr, v2, t2))
		FAIL;
#ifdef PCKG_NO_UNDERBAR
	    if (coff_insert_symbol(ptr + 1, SYMDEF_DECLARE_GLOBAL) < 0)
		FAIL;
#else  /* PCKG_NO_UNDERBAR */
	    if (coff_insert_symbol(ptr, SYMDEF_DECLARE_GLOBAL) < 0)
		FAIL;
#endif /* PCKG_NO_UNDERBAR */
	    break;

	case COP_CREATE_OBJ_FILE:
	    if (!getstring(&ptr, v2, t2) || coff_create_obj_file(ptr) < 0)
		FAIL;
	    break;

	case COP_PUT_UIA:	/* put down a UIA */
	    if (t2 != WTP_UIA || t3 != WTP_INTEGER)
		FAIL;
	    coff_insert_rawdata(M_FIRSTUIAWORD(v2), v3);
	    break;

	default:
	    FAIL;
	    break;
    }

    SUCCEED;
}


#endif /* PACKAGE */
