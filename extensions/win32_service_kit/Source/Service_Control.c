// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//
// Copyright (C) 1993-1996  Microsoft Corporation.  All Rights Reserved.
//
//  MODULE:   service.c
//
//  PURPOSE:  Implements functions required by all services
//            windows.
//
//  FUNCTIONS:
//    main(int argc, char **argv);
//    service_ctrl(DWORD dwCtrlCode);
//    service_main(DWORD dwArgc, LPTSTR *lpszArgv);
//    CmdInstallService();
//    CmdRemoveService();
//    CmdDebugService(int argc, char **argv);
//    ControlHandler ( DWORD dwCtrlType );
//    GetLastErrorText( LPTSTR lpszBuf, DWORD dwSize );
//
//  COMMENTS:
//
//  AUTHOR: Craig Link - Microsoft Developer Support
//


#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <process.h>
#include <tchar.h>

#include <alspi.h>

#include "service.h"


// internal variables
SERVICE_STATUS          ssStatus;       // current status of the service
SERVICE_STATUS_HANDLE   sshStatusHandle;
DWORD                   dwErr = 0;
BOOL                    bDebug = FALSE;
TCHAR                   szErr[256];

// variables set by Prolog service definition
TCHAR					szAppName[MAX_PATH];
TCHAR					szServiceName[256] = "PrologServiceInit";
TCHAR					szServiceDisplayName[256];
TCHAR					szServiceMainPredicate[256];

// internal function prototypes
VOID WINAPI service_ctrl(DWORD dwCtrlCode);
VOID WINAPI service_main(DWORD dwArgc, LPTSTR *lpszArgv);
VOID CmdInstallService();
VOID CmdRemoveService();
VOID CmdDebugService(int argc, char **argv);
BOOL WINAPI ControlHandler ( DWORD dwCtrlType );
LPTSTR GetLastErrorText( LPTSTR lpszBuf, DWORD dwSize );

static int prolog_consult(PE, const char *file_name)
{
	PWord u, cs, fn, s, a;
	int ut, cst, fnt, st, at;
	
	PI_makesym(&u, &ut, "user");
    PI_makesym(&cs, &cst, "consult");
    PI_makesym(&fn, &fnt, file_name);
    PI_makestruct(&s, &st, cs, 1);
    PI_getargn(&a, &at, s, 1);
    PI_unify(a, at, fn, fnt);
    
    return PI_rungoal(u, s, st);
}

static BOOL get_string(PE, PWord p, int t, char *s, int max_len)
{
	switch (t) {
	case PI_SYM:
		PI_getsymname(s, p, max_len);
		break;
	case PI_UIA:
		PI_getuianame(s, p, max_len);
		break;
	default:
		return FALSE;
	}
	
	return TRUE;
}

static BOOL get_win32_service(PE)
{
	PWord u, cs, s, r1, r2, r3;
	int ut, cst, st, rt1, rt2, rt3;

	PI_makesym(&u, &ut, "user");
	PI_makesym(&cs, &cst, "win32_service");
	PI_makestruct(&s, &st, cs, 3);

	if (!PI_rungoal_with_update(u, &s, &st)) return FALSE;

	PI_getargn(&r1, &rt1, s, 1);
	PI_getargn(&r2, &rt2, s, 2);
	PI_getargn(&r3, &rt3, s, 3);

	if (!get_string(hpe, r1, rt1, szServiceName, 256)) return FALSE;
	if (!get_string(hpe, r2, rt2, szServiceDisplayName, 256)) return FALSE;
	if (!get_string(hpe, r3, rt3, szServiceMainPredicate, 256)) return FALSE;
	
	return TRUE;
}

static int is_win32_service(PE)
{
	PI_SUCCEED;
}

static int report_event(PE)
{
	PWord a1, a2, a3;
	int t1, t2, t3;
	TCHAR source[256], type[256], message[256];
	DWORD event_type;
	HANDLE hEventSource;
	LPTSTR lpszStrings[1];
	
	PI_getan(&a1, &t1, 1);
	PI_getan(&a2, &t2, 2);
	PI_getan(&a3, &t3, 3);

	if (!get_string(hpe, a1, t1, source, 256)) PI_FAIL;
	if (!get_string(hpe, a2, t2, type, 256)) PI_FAIL;
	if (!get_string(hpe, a3, t3, message, 256)) PI_FAIL;
	
	if (!strcmp(type, "information")) event_type = EVENTLOG_INFORMATION_TYPE;
	else if (!strcmp(type, "warning")) event_type = EVENTLOG_WARNING_TYPE;
	else if (!strcmp(type, "error")) event_type = EVENTLOG_ERROR_TYPE;
	else PI_FAIL;
	
    hEventSource = RegisterEventSource(NULL, source);
	lpszStrings[0] = message;

    if (hEventSource != NULL) {
        ReportEvent(hEventSource, // handle of event source
            event_type,           // event type
            0,                    // event category
            0,                    // event ID
            NULL,                 // current user's SID
            1,                    // strings in lpszStrings
            0,                    // no bytes of raw data
            lpszStrings,          // array of error strings
            NULL);                // no raw data

        (VOID) DeregisterEventSource(hEventSource);
    }

	
	PI_SUCCEED;
}

PI_BEGIN
	PI_DEFINE("is_win32_service", 0, is_win32_service)
	PI_DEFINE("report_event", 3, report_event)
PI_END

static BOOL InitPrologService(void)
{
	HMODULE module;
	TCHAR szPrologAppName[MAX_PATH];
	TCHAR *dot;
	PI_system_setup setup;
	PE;
	
	// Get the application name
	module = GetModuleHandle(NULL);
	if (!module) {
		AddToMessageLog(TEXT("GetModuleHandle failed."));
		goto error;
	}
	
	if (GetModuleFileName(module, szAppName, MAX_PATH) == 0) {
		AddToMessageLog(TEXT("GetModuleFileName failed."));
		goto error;
	}
	
	// Initilize ALS Prolog
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = NULL;
    setup.saved_state = NULL;
	setup.load_executable_state = 1;
    setup.argc = 0;
    setup.argv = NULL;
    setup.hInstance = NULL;
    setup.hPrevInstance = NULL;
    setup.lpCmdLine = NULL;
    setup.nCmdShow = 1;

	hpe = PI_new_engine();

	if (PI_startup(&setup) != 0) {
		AddToMessageLog(TEXT("Prolog initilization failed."));
		goto error;
	}
	
	PI_INIT;
	
#ifndef PACKAGE_STUB
	// Calculate the Prolog service name from the App name.
	strcpy(szPrologAppName, szAppName);
	dot = strrchr(szPrologAppName, '.');
	if (!dot) {
		AddToMessageLog(TEXT("App name doesn't have extension."));
		goto error;
	}
	*dot = 0;
	strcat(szPrologAppName, ".pro");
	
	// Consult the Prolog service code.
	if (!prolog_consult(hpe, szPrologAppName)) {
		AddToMessageLog(TEXT("Prolog consult failed."));
		goto error;
	}
#endif

	// Get the service name, display name and start predicate.
	if (!get_win32_service(hpe)) {
		AddToMessageLog(TEXT("win32_service/3 failed."));
		goto error;
	}
	
	return TRUE;
	
error:
	return FALSE;
}

//
//  FUNCTION: main
//
//  PURPOSE: entrypoint for service
//
//  PARAMETERS:
//    argc - number of command line arguments
//    argv - array of command line arguments
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//    main() either performs the command line task, or
//    call StartServiceCtrlDispatcher to register the
//    main service thread.  When the this call returns,
//    the service has stopped, so exit.
//
void _CRTAPI1 main(int argc, char **argv)
{
    SERVICE_TABLE_ENTRY dispatchTable[] =
    {
        { NULL, (LPSERVICE_MAIN_FUNCTION)service_main },
        { NULL, NULL }
    };
        
    if (!InitPrologService()) {
    	AddToMessageLog(TEXT("Prolog Service Initilization failed."));
    	exit(0);
    }
    
    dispatchTable[0].lpServiceName = szServiceName;

    if ( (argc > 1) &&
         ((*argv[1] == '-') || (*argv[1] == '/')) )
    {
        if ( _stricmp( "install", argv[1]+1 ) == 0 )
        {
            CmdInstallService();
        }
        else if ( _stricmp( "remove", argv[1]+1 ) == 0 )
        {
            CmdRemoveService();
        }
        else if ( _stricmp( "debug", argv[1]+1 ) == 0 )
        {
            bDebug = TRUE;
            CmdDebugService(argc, argv);
        }
        else
        {
            goto dispatch;
        }
        exit(0);
    }

    // if it doesn't match any of the above parameters
    // the service control manager may be starting the service
    // so we must call StartServiceCtrlDispatcher
    dispatch:
        // this is just to be friendly
        printf( "%s -install          to install the service\n", szAppName );
        printf( "%s -remove           to remove the service\n", szAppName );
        printf( "%s -debug <params>   to run as a console app for debugging\n", szAppName );
        printf( "\nStartServiceCtrlDispatcher being called.\n" );
        printf( "This may take several seconds.  Please wait.\n" );

        if (!StartServiceCtrlDispatcher(dispatchTable))
            AddToMessageLog(TEXT("StartServiceCtrlDispatcher failed."));
}


//
//  FUNCTION: service_main
//
//  PURPOSE: To perform actual initialization of the service
//
//  PARAMETERS:
//    dwArgc   - number of command line arguments
//    lpszArgv - array of command line arguments
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//    This routine performs the service initialization and then calls
//    the user defined ServiceStart() routine to perform majority
//    of the work.
//
void WINAPI service_main(DWORD dwArgc, LPTSTR *lpszArgv)
{

    // register our service control handler:
    //
    sshStatusHandle = RegisterServiceCtrlHandler( szServiceName, service_ctrl);

    if (!sshStatusHandle)
        goto cleanup;

    // SERVICE_STATUS members that don't change in example
    //
    ssStatus.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
    ssStatus.dwServiceSpecificExitCode = 0;


    // report the status to the service control manager.
    //
    if (!ReportStatusToSCMgr(
        SERVICE_START_PENDING, // service state
        NO_ERROR,              // exit code
        3000))                 // wait hint
        goto cleanup;


    ServiceStart( dwArgc, lpszArgv );

cleanup:

    // try to report the stopped status to the service control manager.
    //
    if (sshStatusHandle)
        (VOID)ReportStatusToSCMgr(
                            SERVICE_STOPPED,
                            dwErr,
                            0);

    return;
}



//
//  FUNCTION: service_ctrl
//
//  PURPOSE: This function is called by the SCM whenever
//           ControlService() is called on this service.
//
//  PARAMETERS:
//    dwCtrlCode - type of control requested
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//
VOID WINAPI service_ctrl(DWORD dwCtrlCode)
{
    // Handle the requested control code.
    //
    switch(dwCtrlCode)
    {
        // Stop the service.
        //
        // SERVICE_STOP_PENDING should be reported before
        // setting the Stop Event - hServerStopEvent - in
        // ServiceStop().  This avoids a race condition
        // which may result in a 1053 - The Service did not respond...
        // error.
        case SERVICE_CONTROL_STOP:
            ReportStatusToSCMgr(SERVICE_STOP_PENDING, NO_ERROR, 0);
            ServiceStop();
            return;

        // Update the service status.
        //
        case SERVICE_CONTROL_INTERROGATE:
            break;

        // invalid control code
        //
        default:
            break;

    }

    ReportStatusToSCMgr(ssStatus.dwCurrentState, NO_ERROR, 0);
}



//
//  FUNCTION: ReportStatusToSCMgr()
//
//  PURPOSE: Sets the current status of the service and
//           reports it to the Service Control Manager
//
//  PARAMETERS:
//    dwCurrentState - the state of the service
//    dwWin32ExitCode - error code to report
//    dwWaitHint - worst case estimate to next checkpoint
//
//  RETURN VALUE:
//    TRUE  - success
//    FALSE - failure
//
//  COMMENTS:
//
BOOL ReportStatusToSCMgr(DWORD dwCurrentState,
                         DWORD dwWin32ExitCode,
                         DWORD dwWaitHint)
{
    static DWORD dwCheckPoint = 1;
    BOOL fResult = TRUE;


    if ( !bDebug ) // when debugging we don't report to the SCM
    {
        if (dwCurrentState == SERVICE_START_PENDING)
            ssStatus.dwControlsAccepted = 0;
        else
            ssStatus.dwControlsAccepted = SERVICE_ACCEPT_STOP;

        ssStatus.dwCurrentState = dwCurrentState;
        ssStatus.dwWin32ExitCode = dwWin32ExitCode;
        ssStatus.dwWaitHint = dwWaitHint;

        if ( ( dwCurrentState == SERVICE_RUNNING ) ||
             ( dwCurrentState == SERVICE_STOPPED ) )
            ssStatus.dwCheckPoint = 0;
        else
            ssStatus.dwCheckPoint = dwCheckPoint++;


        // Report the status of the service to the service control manager.
        //
        if (!(fResult = SetServiceStatus( sshStatusHandle, &ssStatus))) {
            AddToMessageLog(TEXT("SetServiceStatus"));
        }
    }
    return fResult;
}



//
//  FUNCTION: AddToMessageLog(LPTSTR lpszMsg)
//
//  PURPOSE: Allows any thread to log an error message
//
//  PARAMETERS:
//    lpszMsg - text for message
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//
VOID AddToMessageLog(LPTSTR lpszMsg)
{
    TCHAR   szMsg[256];
    HANDLE  hEventSource;
    LPTSTR  lpszStrings[2];


    if ( !bDebug )
    {
        dwErr = GetLastError();

        // Use event logging to log the error.
        //
        hEventSource = RegisterEventSource(NULL, szServiceName);

        _stprintf(szMsg, TEXT("%s error: %d"), szServiceName, dwErr);
        lpszStrings[0] = szMsg;
        lpszStrings[1] = lpszMsg;

        if (hEventSource != NULL) {
            ReportEvent(hEventSource, // handle of event source
                EVENTLOG_ERROR_TYPE,  // event type
                0,                    // event category
                0,                    // event ID
                NULL,                 // current user's SID
                2,                    // strings in lpszStrings
                0,                    // no bytes of raw data
                lpszStrings,          // array of error strings
                NULL);                // no raw data

            (VOID) DeregisterEventSource(hEventSource);
        }
    }
}




///////////////////////////////////////////////////////////////////
//
//  The following code handles service installation and removal
//


//
//  FUNCTION: CmdInstallService()
//
//  PURPOSE: Installs the service
//
//  PARAMETERS:
//    none
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//
void CmdInstallService()
{
    SC_HANDLE   schService;
    SC_HANDLE   schSCManager;

    TCHAR szPath[512];

    if ( GetModuleFileName( NULL, szPath, 512 ) == 0 )
    {
        _tprintf(TEXT("Unable to install %s - %s\n"), TEXT(szServiceDisplayName), GetLastErrorText(szErr, 256));
        return;
    }

    schSCManager = OpenSCManager(
                        NULL,                   // machine (NULL == local)
                        NULL,                   // database (NULL == default)
                        SC_MANAGER_ALL_ACCESS   // access required
                        );
    if ( schSCManager )
    {
        schService = CreateService(
            schSCManager,               // SCManager database
            TEXT(szServiceName),        // name of service
            TEXT(szServiceDisplayName), // name to display
            SERVICE_ALL_ACCESS,         // desired access
            SERVICE_WIN32_OWN_PROCESS,  // service type
            SERVICE_DEMAND_START,       // start type
            SERVICE_ERROR_NORMAL,       // error control type
            szPath,                     // service's binary
            NULL,                       // no load ordering group
            NULL,                       // no tag identifier
            TEXT(""),                   // dependencies
            NULL,                       // LocalSystem account
            NULL);                      // no password

        if ( schService )
        {
            _tprintf(TEXT("%s installed.\n"), TEXT(szServiceDisplayName) );
            CloseServiceHandle(schService);
        }
        else
        {
            _tprintf(TEXT("CreateService failed - %s\n"), GetLastErrorText(szErr, 256));
        }

        CloseServiceHandle(schSCManager);
    }
    else
        _tprintf(TEXT("OpenSCManager failed - %s\n"), GetLastErrorText(szErr,256));
}



//
//  FUNCTION: CmdRemoveService()
//
//  PURPOSE: Stops and removes the service
//
//  PARAMETERS:
//    none
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//
void CmdRemoveService()
{
    SC_HANDLE   schService;
    SC_HANDLE   schSCManager;

    schSCManager = OpenSCManager(
                        NULL,                   // machine (NULL == local)
                        NULL,                   // database (NULL == default)
                        SC_MANAGER_ALL_ACCESS   // access required
                        );
    if ( schSCManager )
    {
        schService = OpenService(schSCManager, TEXT(szServiceName), SERVICE_ALL_ACCESS);

        if (schService)
        {
            // try to stop the service
            if ( ControlService( schService, SERVICE_CONTROL_STOP, &ssStatus ) )
            {
                _tprintf(TEXT("Stopping %s."), TEXT(szServiceDisplayName));
                Sleep( 1000 );

                while( QueryServiceStatus( schService, &ssStatus ) )
                {
                    if ( ssStatus.dwCurrentState == SERVICE_STOP_PENDING )
                    {
                        _tprintf(TEXT("."));
                        Sleep( 1000 );
                    }
                    else
                        break;
                }

                if ( ssStatus.dwCurrentState == SERVICE_STOPPED )
                    _tprintf(TEXT("\n%s stopped.\n"), TEXT(szServiceDisplayName) );
                else
                    _tprintf(TEXT("\n%s failed to stop.\n"), TEXT(szServiceDisplayName) );

            }

            // now remove the service
            if( DeleteService(schService) )
                _tprintf(TEXT("%s removed.\n"), TEXT(szServiceDisplayName) );
            else
                _tprintf(TEXT("DeleteService failed - %s\n"), GetLastErrorText(szErr,256));


            CloseServiceHandle(schService);
        }
        else
            _tprintf(TEXT("OpenService failed - %s\n"), GetLastErrorText(szErr,256));

        CloseServiceHandle(schSCManager);
    }
    else
        _tprintf(TEXT("OpenSCManager failed - %s\n"), GetLastErrorText(szErr,256));
}




///////////////////////////////////////////////////////////////////
//
//  The following code is for running the service as a console app
//


//
//  FUNCTION: CmdDebugService(int argc, char ** argv)
//
//  PURPOSE: Runs the service as a console application
//
//  PARAMETERS:
//    argc - number of command line arguments
//    argv - array of command line arguments
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//
void CmdDebugService(int argc, char ** argv)
{
    DWORD dwArgc;
    LPTSTR *lpszArgv;

#ifdef UNICODE
    lpszArgv = CommandLineToArgvW(GetCommandLineW(), &(dwArgc) );
#else
    dwArgc   = (DWORD) argc;
    lpszArgv = argv;
#endif

    _tprintf(TEXT("Debugging %s.\n"), TEXT(szServiceDisplayName));

    SetConsoleCtrlHandler( ControlHandler, TRUE );

    ServiceStart( dwArgc, lpszArgv );
}


//
//  FUNCTION: ControlHandler ( DWORD dwCtrlType )
//
//  PURPOSE: Handled console control events
//
//  PARAMETERS:
//    dwCtrlType - type of control event
//
//  RETURN VALUE:
//    True - handled
//    False - unhandled
//
//  COMMENTS:
//
BOOL WINAPI ControlHandler ( DWORD dwCtrlType )
{
    switch( dwCtrlType )
    {
        case CTRL_BREAK_EVENT:  // use Ctrl+C or Ctrl+Break to simulate
        case CTRL_C_EVENT:      // SERVICE_CONTROL_STOP in debug mode
            _tprintf(TEXT("Stopping %s.\n"), TEXT(szServiceDisplayName));
            ServiceStop();
            return TRUE;
            break;

    }
    return FALSE;
}

//
//  FUNCTION: GetLastErrorText
//
//  PURPOSE: copies error message text to string
//
//  PARAMETERS:
//    lpszBuf - destination buffer
//    dwSize - size of buffer
//
//  RETURN VALUE:
//    destination buffer
//
//  COMMENTS:
//
LPTSTR GetLastErrorText( LPTSTR lpszBuf, DWORD dwSize )
{
    DWORD dwRet;
    LPTSTR lpszTemp = NULL;

    dwRet = FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |FORMAT_MESSAGE_ARGUMENT_ARRAY,
                           NULL,
                           GetLastError(),
                           LANG_NEUTRAL,
                           (LPTSTR)&lpszTemp,
                           0,
                           NULL );

    // supplied buffer is not long enough
    if ( !dwRet || ( (long)dwSize < (long)dwRet+14 ) )
        lpszBuf[0] = TEXT('\0');
    else
    {
        lpszTemp[lstrlen(lpszTemp)-2] = TEXT('\0');  //remove cr and newline character
        _stprintf( lpszBuf, TEXT("%s (0x%x)"), lpszTemp, GetLastError() );
    }

    if ( lpszTemp )
        LocalFree((HLOCAL) lpszTemp );

    return lpszBuf;
}
