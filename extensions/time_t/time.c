#include "alspi.h"
#include "new_alspi.h"

#include <time.h>

static AP_Result p_mktime(AP_World *w, AP_Obj tm_struct, AP_Obj time)
{
	struct tm tr;
	time_t t;
	
	tr.tm_sec = AP_GetLong(w, AP_GetArgument(w, tm_struct, 1));
	tr.tm_min = AP_GetLong(w, AP_GetArgument(w, tm_struct, 2));
	tr.tm_hour = AP_GetLong(w, AP_GetArgument(w, tm_struct, 3));
	tr.tm_mday = AP_GetLong(w, AP_GetArgument(w, tm_struct, 4));
	tr.tm_mon = AP_GetLong(w, AP_GetArgument(w, tm_struct, 5));
	tr.tm_year = AP_GetLong(w, AP_GetArgument(w, tm_struct, 6));
	tr.tm_wday = AP_GetLong(w, AP_GetArgument(w, tm_struct, 7));
	tr.tm_yday = AP_GetLong(w, AP_GetArgument(w, tm_struct, 8));
	tr.tm_isdst = AP_GetLong(w, AP_GetArgument(w, tm_struct, 9));	
	
	t = mktime(&tr);
	
	return AP_Unify(w, AP_NewNumberFromLong(w, t), time);
}

static AP_Result p_gmtime(AP_World *w, AP_Obj time, AP_Obj result)
{
	AP_Obj tm_struct;
	struct tm *tr;

	time_t t = AP_GetLong(w, time);
	
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

	time_t t = AP_GetLong(w, time);
	
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
