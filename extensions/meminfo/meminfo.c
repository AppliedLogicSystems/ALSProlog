#include "alspi.h"
#include "new_alspi.h"

#include "psapi.h"

static AP_Result meminfo(AP_World *w, AP_Obj arg)
{
	PROCESS_MEMORY_COUNTERS counters;
	AP_Obj memstruct;
	
	if (!GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters)))
		return AP_SetStandardError(w, AP_SYSTEM_ERROR);
	
	memstruct = AP_NewInitStructure(w,
		AP_NewSymbolFromStr(w, "meminfo"), 9, 
		AP_NewFloatFromDouble(w, counters.PageFaultCount),
		AP_NewFloatFromDouble(w, counters.PeakWorkingSetSize),
		AP_NewFloatFromDouble(w, counters.WorkingSetSize),
		AP_NewFloatFromDouble(w, counters.QuotaPeakPagedPoolUsage),
		AP_NewFloatFromDouble(w, counters.QuotaPagedPoolUsage),
		AP_NewFloatFromDouble(w, counters.QuotaPeakNonPagedPoolUsage),
		AP_NewFloatFromDouble(w, counters.QuotaNonPagedPoolUsage),
		AP_NewFloatFromDouble(w, counters.PagefileUsage),
		AP_NewFloatFromDouble(w, counters.PeakPagefileUsage)
	);
	
	
	
	return AP_Unify(w, memstruct, arg);
}

static int glue_meminfo(void) {return AP_OldToNewCall(meminfo, 1);}

PI_BEGIN
	PI_DEFINE("meminfo",1,glue_meminfo)
PI_END

void pi_init(void)
{
    PI_INIT;
}
