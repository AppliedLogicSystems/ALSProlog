// Based on Win32 SDK Service example file simple.c


#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <process.h>
#include <tchar.h>

#include <alspi.h>

#include "service.h"

// this event is signalled when the
// service should end
//
HANDLE  hServerStopEvent = NULL;

// The parameter list.
PWord param_list;
int param_listt;

static DWORD WINAPI PrologServiceThread(LPVOID lpParam)
{
	PWord u, cs, s, p;
	int ut, cst, st, pt;

	PI_makesym(&u, &ut, "user");
	PI_makesym(&cs, &cst, szServiceMainPredicate);
	PI_makestruct(&s, &st, cs, 1);
	PI_getargn(&p, &pt, s, 1);
	PI_unify(p, pt, param_list, param_listt);
	
	PI_rungoal(u, s, st);
	
	return 0;
}

//
//  FUNCTION: ServiceStart
//
//  PURPOSE: Actual code of the service
//           that does the work.
//
//  PARAMETERS:
//    dwArgc   - number of command line arguments
//    lpszArgv - array of command line arguments
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//    The default behavior is to open a
//    named pipe, \\.\pipe\simple, and read
//    from it.  It the modifies the data and
//    writes it back to the pipe.  The service
//    stops when hServerStopEvent is signalled
//
VOID ServiceStart (DWORD dwArgc, LPTSTR *lpszArgv)
{
    PWord newlist;
    int   i, newlistt;
    HANDLE hThread;
    DWORD dwThreadID, dwWait;

    ///////////////////////////////////////////////////
    //
    // Service initialization
    //

    // report the status to the service control manager.
    //
    if (!ReportStatusToSCMgr(
        SERVICE_START_PENDING, // service state
        NO_ERROR,              // exit code
        3000))                 // wait hint
        goto cleanup;

    // create the event object. The control handler function signals
    // this event when it receives the "stop" control code.
    //
    hServerStopEvent = CreateEvent(
        NULL,    // no security attributes
        TRUE,    // manual reset event
        FALSE,   // not-signalled
        NULL);   // no name

    if ( hServerStopEvent == NULL)
        goto cleanup;

    // report the status to the service control manager.
    //
    if (!ReportStatusToSCMgr(
        SERVICE_START_PENDING, // service state
        NO_ERROR,              // exit code
        3000))                 // wait hint
        goto cleanup;

	// Create Parameter list.
	for (i = dwArgc-1, PI_makesym(&param_list, &param_listt, "[]");
		 i >= 0;
		 i--, param_list = newlist, param_listt = newlistt) {
		PWord head, tail, arg;
		int headt, tailt, argt;
		
		PI_makelist(&newlist, &newlistt);
		PI_gethead(&head, &headt, newlist);
		PI_gettail(&tail, &tailt, newlist);
		
		PI_makeuia(&arg, &argt, lpszArgv[i]);
		PI_unify(head, headt, arg, argt);
		PI_unify(tail, tailt, param_list, param_listt);
	}

    // report the status to the service control manager.
    //
    if (!ReportStatusToSCMgr(
        SERVICE_START_PENDING, // service state
        NO_ERROR,              // exit code
        3000))                 // wait hint
        goto cleanup;

	hThread = CreateThread(
				NULL,
				0,
				PrologServiceThread,
				NULL,
				0,
				&dwThreadID);
				
	if (!hThread) goto cleanup;
	
    // report the status to the service control manager.
    //
    if (!ReportStatusToSCMgr(
        SERVICE_RUNNING,       // service state
        NO_ERROR,              // exit code
        0))                    // wait hint
        goto cleanup;

    //
    // End of initialization
    //
    ////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////
    //
    // Service is now running, perform work until shutdown
    //

    // wait indefinitely until hServDoneEvent is signaled.
    //
    dwWait = WaitForSingleObject(
        hServerStopEvent,  // event object
        INFINITE);         // wait indefinitely

  cleanup:


    if (hServerStopEvent)
        CloseHandle(hServerStopEvent);

}


//
//  FUNCTION: ServiceStop
//
//  PURPOSE: Stops the service
//
//  PARAMETERS:
//    none
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//    If a ServiceStop procedure is going to
//    take longer than 3 seconds to execute,
//    it should spawn a thread to execute the
//    stop code, and return.  Otherwise, the
//    ServiceControlManager will believe that
//    the service has stopped responding.
//    
VOID ServiceStop()
{
    if ( hServerStopEvent )
        SetEvent(hServerStopEvent);
}
