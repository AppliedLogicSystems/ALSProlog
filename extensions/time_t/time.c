#include "alspi.h"
#include "new_alspi.h"

#include <time.h>

static long GetLongFromNumber(AP_World *w, AP_Obj number)
{
	switch (AP_ObjType(w, number)) {
	case AP_INTEGER:
		return AP_GetLong(w, number);
	default:
		return (long) AP_GetDouble(w, number);
	}
}


static AP_Result p_mktime(AP_World *w, AP_Obj tm_struct, AP_Obj time)
{
	struct tm tr;
	time_t t;
	
	if (AP_ObjType(w, tm_struct) != AP_STRUCTURE
		|| AP_Unify(w, AP_GetStructureFunctor(w, tm_struct), AP_NewSymbolFromStr(w, "tm")) != AP_SUCCESS
		|| AP_GetStructureArity(w, tm_struct) != 9) {
		return AP_SetStandardError(w, AP_TYPE_ERROR, AP_NewSymbolFromStr(w, "tm_structure"), tm_struct);
	}
	
	tr.tm_sec = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 1));
	tr.tm_min = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 2));
	tr.tm_hour = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 3));
	tr.tm_mday = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 4));
	tr.tm_mon = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 5));
	tr.tm_year = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 6));
	tr.tm_wday = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 7));
	tr.tm_yday = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 8));
	tr.tm_isdst = GetLongFromNumber(w, AP_GetArgument(w, tm_struct, 9));	
	
	t = mktime(&tr);
	
	return AP_Unify(w, AP_NewNumberFromLong(w, t), time);
}


static AP_Result p_gmtime(AP_World *w, AP_Obj time, AP_Obj result)
{
	AP_Obj tm_struct;
	struct tm *tr;

	time_t t = GetLongFromNumber(w, time);
	
	tr = gmtime(&t);
		
	tm_struct = AP_NewInitStructure(w,
		AP_NewSymbolFromStr(w, "tm"), 9, 
		AP_NewNumberFromLong(w, tr->tm_sec),
		AP_NewNumberFromLong(w, tr->tm_min),
		AP_NewNumberFromLong(w, tr->tm_hour),
		AP_NewNumberFromLong(w, tr->tm_mday),
		AP_NewNumberFromLong(w, tr->tm_mon),
		AP_NewNumberFromLong(w, tr->tm_year),
		AP_NewNumberFromLong(w, tr->tm_wday),
		AP_NewNumberFromLong(w, tr->tm_yday),
		AP_NewNumberFromLong(w, tr->tm_isdst)
	);
	
	return AP_Unify(w, tm_struct, result);
}

static AP_Result p_localtime(AP_World *w, AP_Obj time, AP_Obj result)
{
	AP_Obj tm_struct;
	struct tm *tr;

	time_t t = GetLongFromNumber(w, time);
	
	tr = localtime(&t);
		
	tm_struct = AP_NewInitStructure(w,
		AP_NewSymbolFromStr(w, "tm"), 9, 
		AP_NewNumberFromLong(w, tr->tm_sec),
		AP_NewNumberFromLong(w, tr->tm_min),
		AP_NewNumberFromLong(w, tr->tm_hour),
		AP_NewNumberFromLong(w, tr->tm_mday),
		AP_NewNumberFromLong(w, tr->tm_mon),
		AP_NewNumberFromLong(w, tr->tm_year),
		AP_NewNumberFromLong(w, tr->tm_wday),
		AP_NewNumberFromLong(w, tr->tm_yday),
		AP_NewNumberFromLong(w, tr->tm_isdst)
	);
	
	return AP_Unify(w, tm_struct, result);
}

static int glue_mktime(void) {return AP_OldToNewCall(p_mktime, 2);}
static int glue_gmtime(void) {return AP_OldToNewCall(p_gmtime, 2);}
static int glue_localtime(void) {return AP_OldToNewCall(p_localtime, 2);}

PI_BEGIN
	PI_DEFINE("mktime",2,glue_mktime)
	PI_DEFINE("gmtime",2,glue_gmtime)
	PI_DEFINE("localtime",2,glue_localtime)
PI_END

void pi_init(void)
{
    PI_INIT;
}
