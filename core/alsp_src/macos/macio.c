#include "defs.h"
#include <GUSI.h>
#include <FileCopy.h>

#include <Processes.h>
#include <Timer.h>


int os_copy_file(const char *from_file, const char *to_file)
{
    Str255 pfrom_file, pto_file;
    FSSpec from_file_spec, to_file_spec, to_dir_spec;
    char cwd[256];
    Str255 pcwd;
    OSErr err;
    CInfoPBRec catInfo; 
    c2pstrcpy(pfrom_file, from_file);
    c2pstrcpy(pto_file, to_file);
    
	getcwd(cwd, 255);
	c2pstrcpy(pcwd, cwd);
	
	catInfo.dirInfo.ioCompletion = NULL;
	catInfo.dirInfo.ioNamePtr = pcwd;
	catInfo.dirInfo.ioVRefNum = 0;
	catInfo.dirInfo.ioFDirIndex = 0;
	catInfo.dirInfo.ioDrDirID = 0;
	err = PBGetCatInfoSync(&catInfo);
	if (err != noErr) return 0;
		
    err = FSMakeFSSpec(catInfo.dirInfo.ioVRefNum, catInfo.dirInfo.ioDrDirID, pfrom_file, &from_file_spec);
    if (err != noErr) return 0;

    err = FSMakeFSSpec(catInfo.dirInfo.ioVRefNum, catInfo.dirInfo.ioDrDirID, pto_file, &to_file_spec);
    if (err != noErr && err != fnfErr) return 0;
    
    err = FSMakeFSSpec(to_file_spec.vRefNum, to_file_spec.parID, "\p", &to_dir_spec);
    if (err != noErr) return 0;
   
    err = FSpFileCopy(&from_file_spec, &to_dir_spec, to_file_spec.name, NULL, 0, 0);
    
    if (err == noErr) return 1;
    else return 0;
}

static double process_start_time;

#define UW2SEC(x) ((double) x.hi * 4294.967296 + (double) x.lo * 0.000001 )

void os_init_time(void)
{
	UnsignedWide time;
	
	Microseconds(&time);
	process_start_time = UW2SEC(time);
}

double os_cputime(void)
{
/* Sometime I'll update this to subtract suspended time. */
	return os_realtime();
/*	
	ProcessSerialNumber PSN;
	ProcessInfoRec InfoRec;

	PSN.highLongOfPSN = 0;
	PSN.lowLongOfPSN = kCurrentProcess;
	InfoRec.processInfoLength = sizeof(ProcessInfoRec);
	InfoRec.processName = NULL;
	InfoRec.processAppSpec = NULL;

	if (GetProcessInformation(&PSN, &InfoRec) == noErr)
		return (double) InfoRec.processActiveTime / 60.0;
	else return 0.0;
*/
}


double os_realtime(void)
{
	UnsignedWide now;
	
	Microseconds(&now);
	return UW2SEC(now) - process_start_time;	
}

int os_set_timer(double initial, double interval)
{
	return 1;
}