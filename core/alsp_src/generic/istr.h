/*
 * istr.h			-- icode command instruction names
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 */

char *negic[] = {
	"IC_INIT",
	"IC_ENDCLAUSE",
	"IC_ASSERTZ",
	"IC_ASSERTA",
	"IC_EXECQUERY",
	"IC_EXECCOMMAND",
	"IC_CHANGEMOD",
	"IC_ADDUSE",
	"IC_ENDMODULE",
	"IC_NEWMODULE",
	"IC_EXPORTPRED",
	"IC_1STARG",
	"IC_BEGINMACRO",
	"IC_ENDMACRO",
	"IC_PUTMACRO",
	"IC_IDONTKNOW",
	"IC_IDONTKNOW",
	"IC_IDONTKNOW",
	"IC_IDONTKNOW",
	"IC_IDONTKNOW",
	"IC_ADDTO_AUTOUSE",
	"IC_IDONTKNOW",
	"IC_CREMODCLOSURE",
	"IC_CREMODCLOSURE2",
	"IC_BEGINALLOC",
	"IC_ENDALLOC"
};

#define ICODE(code,str,a,s)	str,

char *posic[] = {

#include "icodedef.h"

	""
};

#undef ICODE

