/*=====================================================================*
 |
 |	mswin32aux.c
 |	Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *=====================================================================*/

#include "alspi.h"
#include "cinterf.h"

HINSTANCE WindowInstance;

static int getInstance(void)
{
	PWord arg1, rval;
	int type1, rtype;
		
	PI_getan(&arg1,&type1,1);
	PI_makedouble(&rval,&rtype,(double) (long) WindowInstance);
	if (PI_unify(arg1,type1,rval,rtype))
		PI_SUCCEED;
	else PI_FAIL;
}



PI_BEGIN
	PI_PDEFINE("getInstance", 1, getInstance, "_getInstance")
PI_END


static LRESULT CALLBACK WindowProcCallback(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    const char *term;
	
    if (term = find_callback(WindowProcCallback, (void *)hWnd)) {
	PWord user, functor, struc, arg1, arg2, arg3, arg4, arg5, d1, d2, d3, d4;
	int usertype, functype, structype, type1, type2, type3, type4, type5, dt1, dt2, dt3, dt4;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&functor,&functype,(char *)term);
	PI_makestruct(&struc,&structype,functor,5);
	PI_getargn(&arg1, &type1, struc, 1);
	PI_getargn(&arg2, &type2, struc, 2);
	PI_getargn(&arg3, &type3, struc, 3);
	PI_getargn(&arg4, &type4, struc, 4);
	PI_getargn(&arg5, &type5, struc, 5);
	PI_makedouble(&d1, &dt1, (double) (long)hWnd);
	PI_makedouble(&d2, &dt2, (double) message);
	PI_makedouble(&d3, &dt3, (double) wParam);
	PI_makedouble(&d4, &dt4, (double) lParam);
	PI_unify(arg1, type1, d1, dt1);
	PI_unify(arg2, type2, d2, dt2);
	PI_unify(arg3, type3, d3, dt3);
	PI_unify(arg4, type4, d4, dt4);

	if (PI_rungoal_with_update(user,&struc,&structype)) {
	    
	    PI_getargn(&arg5, &type5, struc, 5);
	    if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
		    PI_FAIL;
	    
	    return arg5;
	}
    }
	
    return (DefWindowProc(hWnd, message, wParam, lParam));
}


static BOOL CALLBACK DialogProcCallback(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    const char *term;
	
    if (term = find_callback(DialogProcCallback, (void *)hWnd)) {
	PWord user, functor, struc, arg1, arg2, arg3, arg4, arg5, d1, d2, d3, d4;
	int usertype, functype, structype, type1, type2, type3, type4, type5, dt1, dt2, dt3, dt4;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&functor,&functype,(char *)term);
	PI_makestruct(&struc,&structype,functor,5);
	PI_getargn(&arg1, &type1, struc, 1);
	PI_getargn(&arg2, &type2, struc, 2);
	PI_getargn(&arg3, &type3, struc, 3);
	PI_getargn(&arg4, &type4, struc, 4);
	PI_getargn(&arg5, &type5, struc, 5);
	PI_makedouble(&d1, &dt1, (double) (long)hWnd);
	PI_makedouble(&d2, &dt2, (double) message);
	PI_makedouble(&d3, &dt3, (double) wParam);
	PI_makedouble(&d4, &dt4, (double) lParam);
	PI_unify(arg1, type1, d1, dt1);
	PI_unify(arg2, type2, d2, dt2);
	PI_unify(arg3, type3, d3, dt3);
	PI_unify(arg4, type4, d4, dt4);

	if (PI_rungoal_with_update(user,&struc,&structype)) {
	    
	    PI_getargn(&arg5, &type5, struc, 5);
	    if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
		    arg5 = FALSE;
	    
	    return arg5;
	}
    }
	
    return FALSE;
}

static VOID TimerProcCallback(HWND hWnd, UINT uMsg, UINT idEvent, DWORD dwTime)
{
    const char *term;
	
    if (term = find_callback(TimerProcCallback, (void *)hWnd)) {
	PWord user, functor, struc, arg1, arg2, arg3, arg4, d1, d2, d3, d4;
	int usertype, functype, structype, type1, type2, type3, type4, dt1, dt2, dt3, dt4;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&functor,&functype,(char *)term);
	PI_makestruct(&struc,&structype,functor,4);
	PI_getargn(&arg1, &type1, struc, 1);
	PI_getargn(&arg2, &type2, struc, 2);
	PI_getargn(&arg3, &type3, struc, 3);
	PI_getargn(&arg4, &type4, struc, 4);
	PI_makedouble(&d1, &dt1, (double) (long)hWnd);
	PI_makedouble(&d2, &dt2, (double) uMsg);
	PI_makedouble(&d3, &dt3, (double) idEvent);
	PI_makedouble(&d4, &dt4, (double) dwTime);
	PI_unify(arg1, type1, d1, dt1);
	PI_unify(arg2, type2, d2, dt2);
	PI_unify(arg3, type3, d3, dt3);
	PI_unify(arg4, type4, d4, dt4);

	PI_rungoal(user,struc,structype);
    }
}

static BOOL CALLBACK EnumWindowsProcCallback(HWND hWnd, LPARAM lParam)
{
    const char *term;
	
    if (term = find_callback(EnumWindowsProcCallback, NULL)) {
	PWord user, functor, struc, arg1, arg2, arg3, d1, d2;
	int usertype, functype, structype, type1, type2, type3, dt1, dt2;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&functor,&functype,(char *)term);
	PI_makestruct(&struc,&structype,functor,3);
	PI_getargn(&arg1, &type1, struc, 1);
	PI_getargn(&arg2, &type2, struc, 2);
	PI_getargn(&arg3, &type3, struc, 3);
	PI_makedouble(&d1, &dt1, (double) (long)hWnd);
	PI_makedouble(&d2, &dt2, (double) lParam);
	PI_unify(arg1, type1, d1, dt1);
	PI_unify(arg2, type2, d2, dt2);

	if (PI_rungoal_with_update(user,&struc,&structype)) {
	    
	    PI_getargn(&arg3, &type3, struc, 3);
	    if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
		    arg3 = FALSE;
	    
	    return arg3;
	}
    }
	
    return FALSE;
}


/* This one is problematic - There is no object to dispatch on,
   we probably need several copies of this function to get around this. */ 
static LRESULT CALLBACK HookProcCallback(int nCode, WPARAM wParam, LPARAM lParam)
{
    const char *term;
	
    if (term = find_callback(HookProcCallback, NULL)) {
	PWord user, functor, struc, arg1, arg2, arg3, arg4, d1, d2, d3;
	int usertype, functype, structype, type1, type2, type3, type4, dt1, dt2, dt3;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&functor,&functype,(char *)term);
	PI_makestruct(&struc,&structype,functor,4);
	PI_getargn(&arg1, &type1, struc, 1);
	PI_getargn(&arg2, &type2, struc, 2);
	PI_getargn(&arg3, &type3, struc, 3);
	PI_getargn(&arg4, &type4, struc, 4);
	PI_makedouble(&d1, &dt1, (double) nCode);
	PI_makedouble(&d2, &dt2, (double) wParam);
	PI_makedouble(&d3, &dt3, (double) lParam);
	PI_unify(arg1, type1, d1, dt1);
	PI_unify(arg2, type2, d2, dt2);
	PI_unify(arg3, type3, d3, dt3);

	if (PI_rungoal_with_update(user,&struc,&structype)) {
	    
	    PI_getargn(&arg4, &type4, struc, 4);
	    if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
		    arg4 = 0;
	    
	    return arg4;
	}
    }
	
    return 0;
}

static BOOL CALLBACK AbortProcCallback(HDC hdc, int iError)
{
    const char *term;
	
    if (term = find_callback(AbortProcCallback, hdc)) {
	PWord user, functor, struc, arg1, arg2, arg3, d1, d2;
	int usertype, functype, structype, type1, type2, type3, dt1, dt2;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&functor,&functype,(char *)term);
	PI_makestruct(&struc,&structype,functor,3);
	PI_getargn(&arg1, &type1, struc, 1);
	PI_getargn(&arg2, &type2, struc, 2);
	PI_getargn(&arg3, &type3, struc, 3);
	PI_makedouble(&d1, &dt1, (double) (long)hdc);
	PI_makedouble(&d2, &dt2, (double) iError);
	PI_unify(arg1, type1, d1, dt1);
	PI_unify(arg2, type2, d2, dt2);

	if (PI_rungoal_with_update(user,&struc,&structype)) {
	    
	    PI_getargn(&arg3, &type3, struc, 3);
	    if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
		    arg3 = FALSE;
	    
	    return arg3;
	}
    }
	
    return FALSE;
}

/* In the Win32 API there seem to be about 172 callback types.  We'll just do a
   subset from: ddeml, winbase, wingdi, winuser, winnls
   
   */

/*
WINUSER.H line 56
   typedef BOOL (CALLBACK* GRAYSTRINGPROC)(HDC, LPARAM, int);
WINUSER.H line 59
   typedef VOID (CALLBACK* SENDASYNCPROC)(HWND, UINT, DWORD, LRESULT);
WINUSER.H line 61
   typedef BOOL (CALLBACK* PROPENUMPROCA)(HWND, LPCSTR, HANDLE);
WINUSER.H line 64
   typedef BOOL (CALLBACK* PROPENUMPROCEXA)(HWND, LPSTR, HANDLE, DWORD);
WINUSER.H line 67
   typedef int (CALLBACK* EDITWORDBREAKPROCA)(LPSTR lpch, int ichCurrent, int cch, int code);
WINUSER.H line 71
   typedef BOOL (CALLBACK* DRAWSTATEPROC)(HDC hdc, LPARAM lData, WPARAM wData, int cx, int cy);
WINUSER.H line 108
   typedef BOOL (CALLBACK* NAMEENUMPROCA)(LPSTR, LPARAM);
WINUSER.H line 5496
   typedef void (CALLBACK *MSGBOXCALLBACK)(LPHELPINFO lpHelpInfo);

DDEML.H line 197
   typedef HDDEDATA CALLBACK FNCALLBACK(UINT wType, UINT wFmt, HCONV hConv,
DDEML.H line 199
   typedef HDDEDATA (CALLBACK *PFNCALLBACK)(UINT wType, UINT wFmt, HCONV hConv,
   
WINBASE.H line 3984
   typedef BOOL (CALLBACK* ENUMRESTYPEPROC)(HMODULE hModule, LPTSTR lpType,
WINBASE.H line 3986
   typedef BOOL (CALLBACK* ENUMRESNAMEPROC)(HMODULE hModule, LPCTSTR lpType,
WINBASE.H line 3988
   typedef BOOL (CALLBACK* ENUMRESLANGPROC)(HMODULE hModule, LPCTSTR lpType,
   
WINGDI.H line 2225
   typedef int (CALLBACK* OLDFONTENUMPROCA)(CONST LOGFONTA *, CONST TEXTMETRICA *, DWORD, LPARAM);
WINGDI.H line 2233
   typedef int (CALLBACK* OLDFONTENUMPROCA)(CONST LOGFONTA * ,CONST VOID *, DWORD, LPARAM);
WINGDI.H line 2250
   typedef int (CALLBACK* GOBJENUMPROC)(LPVOID, LPARAM);
WINGDI.H line 2251
   typedef VOID (CALLBACK* LINEDDAPROC)(int, int, LPARAM);
WINGDI.H line 2366
   typedef UINT   (CALLBACK* LPFNDEVMODE)(HWND, HMODULE, LPDEVMODE, LPSTR, LPSTR, LPDEVMODE, LPSTR, UINT);
WINGDI.H line 2368
   typedef DWORD  (CALLBACK* LPFNDEVCAPS)(LPSTR, LPSTR, UINT, LPSTR, LPDEVMODE);
WINGDI.H line 2741
   typedef int (CALLBACK* MFENUMPROC)(HDC, HANDLETABLE FAR*, METARECORD FAR*, int, LPARAM);
WINGDI.H line 2744
   typedef int (CALLBACK* ENHMFENUMPROC)(HDC, HANDLETABLE FAR*, CONST ENHMETARECORD *, int, LPARAM);
WINGDI.H line 2879
   typedef BOOL (CALLBACK* ABORTPROC)(HDC, int);
WINGDI.H line 3080
   typedef int (CALLBACK* ICMENUMPROCA)(LPSTR, LPARAM);
   
WINNLS.H line 560
   typedef BOOL (CALLBACK* LOCALE_ENUMPROCA)(LPSTR);
WINNLS.H line 561
   typedef BOOL (CALLBACK* CODEPAGE_ENUMPROCA)(LPSTR);
WINNLS.H line 562
   typedef BOOL (CALLBACK* DATEFMT_ENUMPROCA)(LPSTR);
WINNLS.H line 563
   typedef BOOL (CALLBACK* TIMEFMT_ENUMPROCA)(LPSTR);
WINNLS.H line 564
   typedef BOOL (CALLBACK* CALINFO_ENUMPROCA)(LPSTR);
*/

void mswin32aux_init(void);
void mswin32aux_init(void)
{
	CI_RCONST("WindowProcCallback", WindowProcCallback);	
	CI_RCONST("DialogProcCallback", DialogProcCallback);	
	CI_RCONST("TimerProcCallback", TimerProcCallback);	
	CI_RCONST("EnumWindowsProcCallback", EnumWindowsProcCallback);	
	CI_RCONST("HookProcCallback", HookProcCallback);	
	CI_RCONST("AbortProcCallback", AbortProcCallback);	

	PI_INIT;
}
