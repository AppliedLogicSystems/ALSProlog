/* 
 *  MetaWare Windows 3.0 ADK
 *  (c) Copyright 1991, MetaWare Incorporated
 *
 *  com.h -- Communication buffer.
 *
 */

#ifndef _COM_H
#define _COM_H	_COM_H
pragma push_align_members(64);

#ifdef __CPLUSPLUS__
extern "C" {
#endif

struct com_t {
    unsigned short magic;
    unsigned short pad;
#if M16B
    unsigned long val;
    unsigned long static_link;
#endif
#if __HIGHC__
    int full_value(void)/*!*/;	/* Override later */ 
#endif
    unsigned long ds_in16;	/* DS in 16 bits */
    unsigned short mw_A000H;
    unsigned short mw_B000H;
    unsigned short mw_B800H;
    unsigned short mw_C000H;
    unsigned short mw_D000H;
    unsigned short mw_E000H;
    unsigned short mw_F000H;
    unsigned short mw_WinFlags;
    unsigned long link_32_to_16;
    unsigned long hInst;	/* this instance */
    unsigned long WinMain;
#if M16B
    unsigned long func;
    unsigned long offs;
    unsigned short   sel;
#endif
#if __HIGHC__
    void (*func)();
    unsigned long _far (* callback)();
#endif
    unsigned short junksel;
    };

#define ID_WINMAIN		1
#define ID_CALLBACK		2
#define ID_ENUMFONTS		3
#define ID_ENUMWINDOWS		4
#define ID_ENUMCHILDWINDOWS	5
#define ID_ENUMPROPS		6
#define ID_ENUMMETAFILE		7
#define ID_ENUMOBJECTS		8
#define ID_ENUMTASKWINDOWS	9
#define ID_LINEDDA		10
#define ID_ESCAPE		11
#define ID_GLOBAL_NOTIFY	12
#define ID_GRAY_STRING		13
#define ID_SET_RESOURCE_HANDLER	14
#define ID_SET_TIMER		15
#define ID_SET_WINDOWS_HOOK	16
#define ID_GETDEBUGBREAK	17


#define ID_WEP			95
#define ID_LIBMAIN		96
#define ID_DLLEXPORTS		97
#define ID_GETDFC		98
#define ID_GETDLC		99

extern struct com_t _far *com;

#ifdef __CPLUSPLUS__
}
#endif

pragma pop_align_members();
#endif
