#ifdef __powerc
#include <MacHeadersPPC>
#else
#include <MacHeaders68K>
#endif

/************************************************************************/
/*  Project...: C++ and ANSI-C Compiler Environment                     */
/*  Name......: SIOUX.c                                       			*/
/*  Purpose...: Interface functions to SIOUX			                */
/*  Copyright.: ©Copyright 1993 by metrowerks inc. All rights reserved. */
/************************************************************************/

//	Changes
//	BB 01/10/93 removed diskEvt from switch statement since this called
//				DIBadMount which is not glue code and hence required importing
//				MacOS.lib ...
//	BB 21/01/94 removed the direct call to GrowDrawIcon and replaced it
//				with a call to a clipping function which doesn't draw the
//				lines.
//	BB 25/01/94 Added support for command - '.', also changed calls to
//				ExitToShell() to exit() which allows the ANSI libs to close
//				any open file streams.
//	BB 25/01/94 Added support for tab characters.
//	BB 22/07/94	Added support for EOF, through control - z
//	BB 11/09/94 Added support for control - c, also both control-c and command-.
//				call raise SIGABRT rather than calling exit.
//				Added support for mouse down to pause output.
//				Added support for setting tab behaviour in SIOUX.
//				Fixed support for characters >128
//	BB 20/10/94	Fixed behaviour of replacing tabs for spaces ...
//	BB 25/10/94	Added support for '\r' to move cursor to beginning of line
//	BB 26/10/94 Changed command-'.' and control - c to raise SIGINT instead of
//				SIGABRT.
//	BB 01/12/94	Extended EOF support to include control - d
//	JH 01/09/95 Modified to run with new ANSI C library
//	JH 12/10/95 Moved __system7present() back to unix.c
//

#include <SIOUX.h>
/************************************************************************/
/*  Project...: C++ and ANSI-C Compiler Environment                     */
/*  Name......: SIOUXGlobals.h                                       	*/
/*  Purpose...: SIOUX's public globals				                	*/
/*  Copyright.: ©Copyright 1993 by metrowerks inc. All rights reserved. */
/************************************************************************/

#ifndef __SIOUXGLOBALS__
#define __SIOUXGLOBALS__

#pragma options align=mac68k

#include <Controls.h>
#include <TextEdit.h>
#include <Types.h>
#include <Windows.h>

typedef enum tSIOUXState {
	OFF = 0,
	IDLE,
	PRINTFING,
	SCANFING,
	TERMINATED,
	ABORTED
} tSIOUXState;
	
typedef struct tSIOUXWin {
    WindowRecord	window;					//	Documents Window
    TEHandle		edit;					//	TextEdit Handle
    ControlHandle 	vscroll;		     	//	Vertical scrollbar
	short			linesInFolder;			//	Number of lines in the window
	Boolean			dirty;					//	Is the document dirty (applies only to textWindow)
	short			vrefnum;				//	The window's file position on disk ...
	long			dirid;
	Str63			fname;
} tSIOUXWin, *pSIOUXWin;

extern short		SIOUXselstart;			//	The starting point for a read (can't read before this) ...
extern Rect 		SIOUXDragRect;			//	The global drag rect ...
extern Rect			SIOUXBigRect;			//	The global clip rect ...
extern Boolean		SIOUXQuitting;			//	Are we quitting? ...
extern Boolean		SIOUXUseWaitNextEvent;	//	Can we use WaitNextEvent? ...
extern tSIOUXState	SIOUXState;				//	Used to signal that we are trying to get a string ...
extern pSIOUXWin	SIOUXTextWindow;		//	Pointer to the SIOUX text window structure ...

#pragma options align=reset

#endif
/************************************************************************/
/*  Project...: C++ and ANSI-C Compiler Environment                     */
/*  Name......: SIOUXMenus.h                                       		*/
/*  Purpose...: Menu related functions for SIOUX			            */
/*  Copyright.: ©Copyright 1993 by metrowerks inc. All rights reserved. */
/************************************************************************/

#ifndef __SIOUXMENUS__
#define __SIOUXMENUS__

#if __MWERKS__
#pragma options align=mac68k
#endif

// Menu IDs ...
enum {
	APPLEID 		= 32000,
	APPLEABOUT 		= 1
};

enum {
	FILEID			= 32001,
	FILESAVE 		= 4,
	FILEPAGESETUP	= 6,
	FILEPRINT		= 7,
	FILEQUIT		= 9
};

enum {
	EDITID			= 32002,
	EDITCUT			= 3,
	EDITCOPY		= 4,
	EDITPASTE		= 5,
	EDITCLEAR		= 6,
	EDITSELECTALL	= 8
};

#ifdef __cplusplus
extern "C" {
#endif

// Function prototypes ...
extern void		SIOUXSetupMenus(void);
extern void		SIOUXUpdateMenuItems(void);
extern short	SIOUXDoSaveText(void);
extern void		SIOUXDoEditCut(void);
extern void		SIOUXDoEditCopy(void);
extern void		SIOUXDoEditPaste(void);
extern void		SIOUXDoEditClear(void);
extern void		SIOUXDoEditSelectAll(void);

extern void		SIOUXDoMenuChoice(long menuValue);

#ifdef __cplusplus
}
#endif

#if __MWERKS__
#pragma options align=reset
#endif

#endif
/************************************************************************/
/*  Project...: C++ and ANSI-C Compiler Environment                     */
/*  Name......: SIOUXWindows.h                                       	*/
/*  Purpose...: Menu related functions for SIOUX			            */
/*  Copyright.: ©Copyright 1993 by metrowerks inc. All rights reserved. */
/************************************************************************/

#ifndef __SIOUXWINDOWS__
#define __SIOUXWINDOWS__

//	Dialog IDs
enum {
	GENERICWIND = 32000,
	alrt_cantsave = 32001,
	alrt_yesnocancel = 32002,
	alrt_aboutbox = 32003
};

#ifdef __cplusplus
extern "C" {
#endif

// Function prototypes ...
extern Boolean SIOUXIsAppWindow(WindowPtr window);
extern void SIOUXUpdateScrollbar(void);
extern void SIOUXDoContentClick(WindowPtr window, EventRecord *theEvent);
extern void SIOUXDrawGrowBox(WindowPtr theWindow);
extern void SIOUXUpdateWindow(WindowPtr theWindow);
extern void SIOUXUpdateStatusLine(WindowPtr theWindow);
extern void SIOUXMyGrowWindow(WindowPtr theWindow, Point thePoint);
extern Boolean SIOUXSetupTextWindow(void);
extern void SIOUXDoAboutBox(void);
extern void SIOUXCantSaveAlert(Str63 filename);
extern short SIOUXYesNoCancelAlert(Str63 filename);
extern Boolean SIOUXisinrange(short first, TEHandle te);

#ifdef __cplusplus
}
#endif

#endif
#include <console.h>
#include <signal.h>
#include <stdlib.h>
#include <Traps.h>

//	defined in unix.c ...
#ifdef __cplusplus
extern "C" {
#endif

int  __system7present(void);
long __myraise(long signal);


#ifdef __cplusplus
}
#endif

//	Local Defines
#define SIOUX_BUFSIZ 512U					//	SIOUX's internal buffer size ...
#define TICK_DELTA 30UL						//	Max ticks allowed between tests for UserBreak()
#define EOF (-1L)							//	End Of File marker

//	Local Globals
static Boolean toolBoxDone = false;			//	set to true when the TB is setup
static unsigned long LastTick = 0UL;		//	The tickcount of the last UserBreak test ...
static short inputBuffersize = 0;			//	Used by GetCharsFromSIOUX and DoOneEvent ...
static CursHandle iBeamCursorH = NULL;		//	Contains the iBeamCursor ...
static Boolean atEOF = false;				//	Is the stream at EOF?

typedef struct tSIOUXBuffer {
	char		*startpos,	/* pointer to a block of memory which will serve as the buffer */
				*curpos,	/* pointer to insertion point in the buffer */
				*endpos;	/* pointer to end of text in the buffer */
	long		tepos;		/* current offset in TEHandle (0 if at end) */
} tSIOUXBuffer;
static tSIOUXBuffer SIOUXBuffer;

#define ZEROSIOUXBUFFER()		{															\
									SIOUXBuffer.curpos =									\
										SIOUXBuffer.endpos =								\
											SIOUXBuffer.startpos;							\
									SIOUXBuffer.tepos = -1;									\
								}
#define CURRENTBUFSIZE()		(SIOUXBuffer.endpos - SIOUXBuffer.startpos)
#define CHECKFOROVERFLOW(c)		{															\
									if (CURRENTBUFSIZE() + (c) >= SIOUX_BUFSIZ)				\
										InsertSIOUXBuffer();								\
								}
#define DELETEFROMBUFFER(num)	{															\
									if (SIOUXBuffer.curpos != SIOUXBuffer.endpos)			\
										BlockMoveData(SIOUXBuffer.curpos,					\
												SIOUXBuffer.curpos - (num),					\
												SIOUXBuffer.endpos - SIOUXBuffer.curpos);	\
									SIOUXBuffer.endpos -= (num);							\
									SIOUXBuffer.curpos -= (num);							\
								}
#define ROLLBACKBUFFER(num)		{ SIOUXBuffer.curpos = SIOUXBuffer.endpos - (num); }
#define INSERTCHAR(c)			{															\
									if (SIOUXBuffer.tepos == -1) {							\
										*SIOUXBuffer.curpos = (c);							\
										if (SIOUXBuffer.curpos == SIOUXBuffer.endpos)		\
											SIOUXBuffer.endpos++;							\
										SIOUXBuffer.curpos++;								\
									} else {												\
										TEHandle theTEH = SIOUXTextWindow->edit;			\
										TESetSelect(SIOUXBuffer.tepos,						\
													SIOUXBuffer.tepos + 1,					\
													theTEH);								\
										TEKey(c, theTEH);									\
										SIOUXBuffer.tepos++;								\
										if (SIOUXBuffer.tepos == (*theTEH)->teLength - 1)	\
											SIOUXBuffer.tepos = -1;							\
									}														\
								}
#define INSERTLINEFEED()		{															\
									*SIOUXBuffer.endpos = 0x0d;								\
									SIOUXBuffer.endpos++;									\
									SIOUXBuffer.curpos = SIOUXBuffer.endpos;				\
									SIOUXBuffer.tepos = -1;									\
								}

/************************************************************************/
/* Purpose..: Setup the toolbox			  								*/
/* Input....: ---                       								*/
/* Returns..: ---                        								*/
/************************************************************************/
static void DoSetupToolbox(void)
{
	//	Initialize the ToolBox ...
	InitGraf(&qd.thePort);
	InitFonts();
	FlushEvents(everyEvent, 0L);
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(0L);
	InitCursor();
	
	MaxApplZone();
	MoreMasters();

	toolBoxDone = true;
}

/************************************************************************/
/*	Purpose..: 	Determines position on current line	(in chars)			*/
/*	Input....:	TextEdit Handle											*/
/*	Return...:	Position on current line								*/
/************************************************************************/
static long OffsetOnCurrentLine(TEHandle theTEH)
{
	char *ptr, *start;
	long result;

	//	Check for a CR in the buffer ...
	if (SIOUXBuffer.endpos != SIOUXBuffer.startpos) {
		for (start = SIOUXBuffer.startpos, ptr = SIOUXBuffer.endpos; ptr > start; ptr--)
			if (*ptr == 0x0d) return (SIOUXBuffer.endpos - ptr);
	}

	HLock((Handle)theTEH);
	HLock((*theTEH)->hText);

	start = *(*theTEH)->hText;
	ptr = *(*theTEH)->hText + (*theTEH)->selStart;
	while (ptr > start && ptr[-1] != 0x0d)
		ptr--;

	result = *(*theTEH)->hText + (*theTEH)->selStart - ptr + SIOUXBuffer.endpos - SIOUXBuffer.startpos;

	HUnlock((*theTEH)->hText);
	HUnlock((Handle)theTEH);

	return result;
}

/************************************************************************/
/*	Purpose..:	Handle a mouseDown event								*/
/*	Input....:	pointer to an Event										*/
/*	Return...:	true/false												*/
/************************************************************************/
static Boolean HandleMouseDownEvent(EventRecord *theEvent)
{
	WindowPtr window;
	short part;
	Boolean isSIOUXwindow;

	part = FindWindow(theEvent->where, &window);
	isSIOUXwindow = SIOUXIsAppWindow(window);

	switch (part) {
		case inMenuBar:
			if (SIOUXSettings.setupmenus) {
				SIOUXUpdateMenuItems();
				SIOUXDoMenuChoice(MenuSelect(theEvent->where));
				return true;
			}
			break;
		case inSysWindow:
			if (SIOUXSettings.standalone)
				SystemClick(theEvent, window);
			break;
		case inContent:
			if (window != FrontWindow())
				SelectWindow(window);
			else if (isSIOUXwindow) {
				SelectWindow(window);
				if (SIOUXState == PRINTFING) {
					if (StillDown())
						while (WaitMouseUp())
							;	//	Pause output while mouse is down ...
				} else
					SIOUXDoContentClick(window, theEvent);
				return true;
			}
			break;
		case inDrag:
			if (isSIOUXwindow) {
				DragWindow(window, theEvent->where, &SIOUXBigRect);
				return true;
			}
			break;
		case inGrow:
			if (isSIOUXwindow) {
				SIOUXMyGrowWindow(window, theEvent->where);
				return true;
			}
			break;
		}

	return false;
}

/************************************************************************/
/*	Purpose..:	Handle update and activate/deactivate events			*/
/*	Input....:	pointer to an Event										*/
/*	Return...:	true/false												*/
/************************************************************************/
static Boolean HandleUpdateActivateEvent(EventRecord *theEvent)
{
	if (SIOUXIsAppWindow((WindowPtr)theEvent->message)) {
		if (theEvent->what == updateEvt)
			SIOUXUpdateWindow((WindowPtr)theEvent->message);
		else {	/* must be an activate/deactivate event */
			if (theEvent->modifiers & activeFlag) {
				TEActivate(SIOUXTextWindow->edit);
				ShowControl(SIOUXTextWindow->vscroll);
			} else {
				TEDeactivate(SIOUXTextWindow->edit);
				HideControl(SIOUXTextWindow->vscroll);
			}
			SIOUXDrawGrowBox((WindowPtr)SIOUXTextWindow);
			SIOUXUpdateStatusLine((WindowPtr)SIOUXTextWindow);
		}
		return true;
	}

	return false;
}

/************************************************************************/
/*	Purpose..:	Handle update and activate/deactivate events			*/
/*	Input....:	pointer to an Event										*/
/*	Return...:	true/false												*/
/************************************************************************/
static Boolean HandleOSEvents(EventRecord *theEvent)
{
	switch ((theEvent->message >> 24) & 0xff) {
		case resumeFlag:
			if (theEvent->message & suspendResumeMessage) {
				TEActivate(SIOUXTextWindow->edit);
				ShowControl(SIOUXTextWindow->vscroll);
			} else {
				TEDeactivate(SIOUXTextWindow->edit);
				HideControl(SIOUXTextWindow->vscroll);
			}
			SIOUXDrawGrowBox((WindowPtr)SIOUXTextWindow);
			SIOUXUpdateStatusLine((WindowPtr)SIOUXTextWindow);
			return(true);

		default:
			return(false);
	}
}

/************************************************************************/
/*	Purpose..:	Detect a user break (Command-'.')						*/
/*	Input....:	pointers an size of block								*/
/*	Return...:	true/false												*/
/************************************************************************/
static void UserBreak(void)
{
	EventRecord ev;

	if (TickCount() - LastTick < TICK_DELTA)
		return;

	LastTick = TickCount();

	if (SIOUXUseWaitNextEvent)
		WaitNextEvent(everyEvent, &ev, 0x0, NULL);
	else {
		//	Keep the system happy ...
		SystemTask();

		GetNextEvent(everyEvent, &ev);
	}

	switch (ev.what) {
		case nullEvent:		//	ignore it ...
			break;

		case keyDown:
		case autoKey:		//	check for break ...
			if (ev.modifiers & cmdKey) {
				if ((ev.message & charCodeMask) == '.')
					__myraise(SIGINT);
				if ((ev.message & charCodeMask) == 'q' || (ev.message & charCodeMask) == 'Q') {
					SIOUXQuitting = true;
					__myraise(SIGINT);
				}
			}
			if ((ev.message & charCodeMask) == 0x03 && (ev.message & keyCodeMask) >> 8 != 0x4c)
				__myraise(SIGINT);			//	enter and control c have same char code
			break;

		case mouseDown:
			HandleMouseDownEvent(&ev);
			break;

		case activateEvt:
		case updateEvt:
			HandleUpdateActivateEvent(&ev);
			break;

		case osEvt:
			HandleOSEvents(&ev);
			break;

		case kHighLevelEvent:
			if (__system7present()) {
				AEProcessAppleEvent(&ev);
			}
			break;

		case diskEvt:
			if (HiWord(ev.message) != noErr) {
				Point pt = {100, 100};
				DIBadMount(pt, ev.message);
			}
			break;

		case mouseUp:
		case keyUp:
		default:
			break;
	}
}

/************************************************************************/
/*	Purpose..: 	Insert the current SIOUX buffer into the TE Handle		*/
/*	Input....:	---														*/
/*	Return...:	---														*/
/************************************************************************/
static void InsertSIOUXBuffer(void)
{
	TEHandle theTEH = SIOUXTextWindow->edit;
	short teLength;
	
	HLock((Handle)theTEH);
	teLength = (*theTEH)->teLength;

	if ((teLength + CURRENTBUFSIZE()) > 32767) {
		//	Insert will grow TEHandle past 32K so we cut out excess from top ...
		char *ptr;
		short todelete = (short) ((teLength + CURRENTBUFSIZE()) - 32767) + 8*SIOUX_BUFSIZ;
		
		//	Make sure that the text to be cut ends on a CR ...
		HLock((*theTEH)->hText);
		for (ptr = *(*theTEH)->hText + todelete; *ptr != 0x0d; ptr++) ;

		//	We now point at the CR, increment ptr to point after it ...
		todelete += ++ptr - (*(*theTEH)->hText + todelete);
		HUnlock((*theTEH)->hText);
		
		//	We hit the fields directly to keep TE from redrawing twice
		(*theTEH)->selStart = 0;
		(*theTEH)->selEnd   = todelete;
		TEDelete(theTEH);

		//	Now fix things up...
		teLength = (*theTEH)->teLength;
	}

	TESetSelect(teLength, teLength, theTEH);
	
	//	Now insert the new text ...
	TEInsert(SIOUXBuffer.startpos, CURRENTBUFSIZE(), theTEH);
	teLength = (*theTEH)->teLength;
	SIOUXTextWindow->dirty = true;
	ZEROSIOUXBUFFER();

	TESetSelect(teLength, teLength, theTEH);

	HUnlock((Handle)theTEH);

	if (SIOUXSettings.standalone)
		UserBreak();
	else
		SIOUXUpdateScrollbar();
}

/************************************************************************/
/*	Purpose..:	Determine the user's theoretical menuchoice				*/
/*	Input....:	Character typed											*/
/*	Return...:	Menuchoice												*/
/************************************************************************/
static long myMenuKey(char key)
{
	short theMenu = 0;
	short theMenuItem = 0;

	switch (key) {
		//	File menu choices
		case 's': case 'S':
			theMenu = FILEID;
			theMenuItem = FILESAVE;
			break;
		case 'p': case 'P':
			theMenu = FILEID;
			theMenuItem = FILEPRINT;
			break;
		case 'q': case 'Q':
			theMenu = FILEID;
			theMenuItem = FILEQUIT;
			break;
		case 'x': case 'X':
			theMenu = EDITID;
			theMenuItem = EDITCUT;
			break;
		case 'c': case 'C':
			theMenu = EDITID;
			theMenuItem = EDITCOPY;
			break;
		case 'v': case 'V':
			theMenu = EDITID;
			theMenuItem = EDITPASTE;
			break;
		case 'a': case 'A':
			theMenu = EDITID;
			theMenuItem = EDITSELECTALL;
			break;
	}

	return (((long)theMenu << 16) | theMenuItem);
}

/************************************************************************/
/*	Purpose..: 	Check if insertion range is in edit range				*/
/*	Input....:	first character in edit range							*/
/*	Input....:	Handle to textedit										*/
/*	Return...:	true/false												*/
/************************************************************************/
Boolean SIOUXisinrange(short first, TEHandle te)
{
	if (((*te)->selStart < first) || (*te)->selEnd < first) {
		SysBeep(10);
		return false;
	} else
		return true;
}

/************************************************************************/
/* Purpose..: Handles a single event ...  								*/
/* Input....: If non-zero then pointer to an event						*/
/* Returns..: ---           		             						*/
/************************************************************************/
short SIOUXHandleOneEvent(EventRecord *userevent)
{
	EventRecord theEvent;
	WindowPtr	window;
	char		aChar;

	if (SIOUXState == OFF)
		return false;

	if (userevent)						//	External call of the function ...
		theEvent = *userevent;
	else if (SIOUXUseWaitNextEvent)		//	Internal with WNE allowed ...
		WaitNextEvent(everyEvent, &theEvent, 0x0, NULL);
	else {								//	Internal with no WNE allowed ...
		SystemTask();
		GetNextEvent(everyEvent, &theEvent);
	}

	window = FrontWindow();

	switch (theEvent.what) {
		case nullEvent:
			//	Maintain the cursor
        	if (SIOUXIsAppWindow(window))
        	{
        		GrafPtr savePort;

				GetPort(&savePort);
				SetPort(window);
				GlobalToLocal(&theEvent.where);
        		if (PtInRect(theEvent.where, &(*SIOUXTextWindow->edit)->viewRect) &&
        			iBeamCursorH)
        		{
					SetCursor(*iBeamCursorH);
				} else {
					SetCursor(&qd.arrow);
				}
				LocalToGlobal(&theEvent.where);
				TEIdle(SIOUXTextWindow->edit);
				SetPort(savePort);
				return true;
			} else {
				SetCursor(&qd.arrow);
				TEIdle(SIOUXTextWindow->edit);
			}
			break;

		case mouseDown:
			if (HandleMouseDownEvent(&theEvent))
				return true;
			break;

		case keyDown:
		case autoKey:
        	if (SIOUXIsAppWindow(window)) {
				aChar = (theEvent.message & charCodeMask);
				if (theEvent.modifiers & cmdKey) {
					//	Check first for command - '.'
					if (SIOUXState != TERMINATED && aChar == '.')
						__myraise(SIGINT);
					if (SIOUXSettings.setupmenus) {
						SIOUXUpdateMenuItems();
						SIOUXDoMenuChoice(MenuKey(aChar));
					} else
						SIOUXDoMenuChoice(myMenuKey(aChar));
				} else {
					if ((theEvent.message & keyCodeMask) >> 8 == 0x4c)
						aChar = 0x0d;				//	map enter key to return key ...
					if (SIOUXState == SCANFING) {
						//	If there are too many characters on the line already then just return ...
						if (((*SIOUXTextWindow->edit)->teLength - SIOUXselstart + 1) >= inputBuffersize) {
							SysBeep(10);
							return false;
						}
						switch (aChar) {
							case 0x1a:				//	Control - 'z'
							case 0x04:				//	Control - 'd'
								//	Place in the enter key char which will become the EOF
								aChar = 0x03;
							case 0x0d:				//	Carriage Return
								SIOUXState = IDLE;
								break;
							case 0x03:				//	Control - 'c'
								__myraise(SIGINT);
								break;
							case 0x08:				//	Delete
								if (!SIOUXisinrange(SIOUXselstart + 1, SIOUXTextWindow->edit))
									return false;
								break;
							default:
								break;
						}
						//	if the cursor is currently outside the typeable region then move it ...
						if ((aChar >= ' ') && !SIOUXisinrange(SIOUXselstart, SIOUXTextWindow->edit))
							TESetSelect((*SIOUXTextWindow->edit)->teLength,
										(*SIOUXTextWindow->edit)->teLength,
										SIOUXTextWindow->edit);
					}
					TEKey(aChar, SIOUXTextWindow->edit);
					SIOUXUpdateScrollbar();
					if (aChar < 0x1c || aChar > 0x1f)
						SIOUXTextWindow->dirty = true;
					return true;
				}
			}
			break;

		case activateEvt:
		case updateEvt:
			if (HandleUpdateActivateEvent(&theEvent))
				return true;
			break;

		case osEvt:
			if (HandleOSEvents(&theEvent))
				return true;
			break;

		case kHighLevelEvent:
			if (__system7present()) {
				AEProcessAppleEvent(&theEvent);
			}
			break;

		case diskEvt:
			if (HiWord(theEvent.message) != noErr) {
				Point pt = {100, 100};
				DIBadMount(pt, theEvent.message);
			}
			break;

		case mouseUp:
		case keyUp:
		default:
			break;
	}
	return false;
}

/************************************************************************/
/* Purpose..: Cleans up the data for a quit ...							*/
/* Input....: ---                       								*/
/* Returns..: true killed everything/false user cancelled ...			*/
/************************************************************************/
static Boolean SIOUXCleanUp(void)
{
	short item;
	Str255 aString;

	if (SIOUXTextWindow) {
		if (SIOUXTextWindow->dirty && SIOUXSettings.asktosaveonclose) {
			GetWTitle((WindowPtr)SIOUXTextWindow, aString);
			SetCursor(&qd.arrow);
			item = SIOUXYesNoCancelAlert(aString);
	
			switch (item) {
				case 1:		//	Yes
					if (SIOUXDoSaveText() != noErr && SIOUXSettings.standalone) {	//	Save the textWindow ...
						SIOUXQuitting = false;
						return (false);
					}
					break;
				case 3:		//	Cancel
					SIOUXQuitting = false;
					return (false);
				case 2:		//	No
				default:	//	error
					break;
			}
		}

		//	Kill the textWindow ...
	    KillControls((WindowPtr)SIOUXTextWindow);
	    TEDispose(SIOUXTextWindow->edit);
	    CloseWindow((WindowPtr)SIOUXTextWindow);
	
		DisposePtr((Ptr)SIOUXTextWindow);
		SIOUXTextWindow = 0L;
		ZEROSIOUXBUFFER();
	}

	return (true);
}

/************************************************************************/
/* Purpose..: Install the console package								*/
/* Input....: The stream to install (ignored)							*/
/* Returns..: 0 no error / -1 error occurred							*/
/************************************************************************/
short InstallConsole(short fd)
{
#if !__CFM68K__
#pragma unused (fd)
#endif
	if (SIOUXQuitting || SIOUXState != OFF) return 0;

	if (SIOUXSettings.initializeTB && !toolBoxDone)
		DoSetupToolbox();

	//	Initialize Space for the SIOUX buffer ...
	if ((SIOUXBuffer.startpos = (char *)NewPtr(SIOUX_BUFSIZ)) == NULL)
		return -1;
	ZEROSIOUXBUFFER();

	//	Setup the menus ...
	if (SIOUXSettings.setupmenus)
		SIOUXSetupMenus();

	//	Setup the textWindow ...
	if (SIOUXSetupTextWindow()) {
		if (SIOUXSettings.standalone == false) {
			SIOUXSettings.autocloseonquit = true;
		}

		SIOUXState = IDLE;

		//	Test for WaitNextEvent ...
	
		if (GetToolTrapAddress(_WaitNextEvent) != GetToolTrapAddress(_Unimplemented))
			SIOUXUseWaitNextEvent = true;

		iBeamCursorH = GetCursor(iBeamCursor);

		return 0;
	}

	return(-1);
}

/************************************************************************/
/* Purpose..: Remove the console package								*/
/* Input....: ---														*/
/* Returns..: ---														*/
/************************************************************************/
void RemoveConsole(void)
{
	extern int __aborting;

	if (SIOUXState == OFF || !SIOUXTextWindow)
		return;

	if (__aborting)
		SIOUXState = ABORTED;
	else
		SIOUXState = TERMINATED;
	SIOUXUpdateStatusLine((WindowPtr)SIOUXTextWindow);

	SIOUXselstart = 0;
	TEActivate(SIOUXTextWindow->edit);
	SIOUXUpdateScrollbar();

	if (SIOUXSettings.autocloseonquit)
		SIOUXQuitting = true;

	while (!SIOUXQuitting) {
BackInTheLoop:
		SIOUXHandleOneEvent(NULL);
	}

	if (!SIOUXCleanUp())
		goto BackInTheLoop;

	SIOUXState = OFF;
}

/************************************************************************/
/*	Purpose..: 	Write a string to the console							*/
/*	Input....:	pointer to buffer										*/
/*	Input....:	number of chars in buffer								*/
/*	Return...:	0 no error / -1 error occurred							*/
/************************************************************************/
long WriteCharsToConsole(char *buffer, long n)
{
	long counter, i, spacestoinsert;
	char aChar;
	GrafPtr saveport;

	if (SIOUXQuitting)
		return 0;

	GetPort(&saveport);
	SetPort((WindowPtr)SIOUXTextWindow);

	SIOUXState = PRINTFING;
	SIOUXUpdateStatusLine((WindowPtr)SIOUXTextWindow);

	for(counter = n; counter > 0; counter--)
	{
		aChar = *buffer++;
		switch(aChar) {
			case 0x0d:	//	Line Feed (Mac newline)
				INSERTLINEFEED();
				break;
			case 0x0a:	//	Carriage Return (move to start of line)
				i = OffsetOnCurrentLine(SIOUXTextWindow->edit);
				if (i <= CURRENTBUFSIZE()) {
					ROLLBACKBUFFER(i);
				} else {
					SIOUXBuffer.tepos = (*SIOUXTextWindow->edit)->teLength - (i - CURRENTBUFSIZE());
				}
				break;
			case '\t':	//	Tab character
				if (SIOUXSettings.tabspaces) {
					//	insert spaces for tabs
					CHECKFOROVERFLOW(SIOUXSettings.tabspaces);

					i = OffsetOnCurrentLine(SIOUXTextWindow->edit);

					spacestoinsert = SIOUXSettings.tabspaces -
									 (i % SIOUXSettings.tabspaces);

					for (i = 0; i < spacestoinsert; i++) INSERTCHAR(' ');
				} else
					INSERTCHAR('\t');
				break;
			case '\f':	//	Form Feed
				CHECKFOROVERFLOW(SIOUXTextWindow->linesInFolder);
				for (i = SIOUXTextWindow->linesInFolder; i > 0; i--) INSERTLINEFEED();
				break;
			case '\a':	//	Audible Alert
				SysBeep(1);
				break;
			case '\b':	//	Backspace
				if (CURRENTBUFSIZE() != 0) {
					DELETEFROMBUFFER(1);
				} else {				//	Need to delete the last character from the TextEdit Handle
					short teLength = (*SIOUXTextWindow->edit)->teLength;
					if (teLength > 0) {
						TESetSelect(teLength-1, teLength, SIOUXTextWindow->edit);
						TEDelete(SIOUXTextWindow->edit);
					}
				}
				break;
			case '\v':	//	Vertical Tab
				break;
			default:	//	just add it to SIOUX ...
				INSERTCHAR(aChar);
				break;
		}
		CHECKFOROVERFLOW(0);
	}
	InsertSIOUXBuffer();

	SIOUXState = IDLE;
	SIOUXUpdateStatusLine((WindowPtr)SIOUXTextWindow);

	SetPort(saveport);

	return n;
}


/************************************************************************/
/*	Purpose..: 	Read characters into the buffer							*/
/*	Input....:	pointer to buffer										*/
/*	Input....:	max length of buffer									*/
/*	Return...:	number of characters read / -1 error occurred			*/
/************************************************************************/
long ReadCharsFromConsole(char *buffer, long n)
{
	long charsread;
	GrafPtr saveport;

	if (SIOUXQuitting)
		return 0;

	if (atEOF) {
		atEOF = 0;
		buffer[0] = EOF;
		return 0;
	}

	GetPort(&saveport);
	SetPort((WindowPtr)SIOUXTextWindow);

	SIOUXState = SCANFING;
	inputBuffersize = n;
	SIOUXselstart = (*SIOUXTextWindow->edit)->teLength;

	SelectWindow((WindowPtr)SIOUXTextWindow);
	SIOUXUpdateStatusLine((WindowPtr)SIOUXTextWindow);
	TESetSelect(SIOUXselstart, SIOUXselstart, SIOUXTextWindow->edit);
	TEActivate(SIOUXTextWindow->edit);
	SIOUXUpdateScrollbar();

	while (SIOUXState == SCANFING && !SIOUXQuitting) {
BackInTheLoop:
		SIOUXHandleOneEvent(NULL);
	}
		
	if (SIOUXQuitting) {
		if (!SIOUXCleanUp()) {
			SIOUXQuitting = false;
			goto BackInTheLoop;
		}
		SetPort(saveport);
		exit(0);
	}

	//	put the string into the buffer ...
	charsread = (*SIOUXTextWindow->edit)->teLength - SIOUXselstart;
	BlockMove((*(*SIOUXTextWindow->edit)->hText) + SIOUXselstart,
			  buffer,
			  charsread);
	//	if no error occurred continue else return 0 characters read ...
	if (MemError() == noErr) {
		if (buffer[charsread - 1L] == 0x03)	/* The user did a Control - Z or control - D (ie an EOF) */
			{charsread--; if (charsread > 0) atEOF = 1;}
		else
			buffer[charsread - 1L] = 0x0d;
	} else {
		charsread = 0;
	}

	SIOUXUpdateStatusLine((WindowPtr)SIOUXTextWindow);

	SetPort(saveport);

	return charsread;
}

/*
 *	return the name of the current terminal ...
 */
char *__ttyname(long fildes)
{
	//	all streams have the same name ...
	static char *__SIOUXDeviceName = "SIOUX";
	
	if (fildes >= 0 && fildes <= 2)
		return (__SIOUXDeviceName);

	return (NULL);
}

/*
 *	Set SIOUX's window title ...
 */
void SIOUXSetTitle(unsigned char title[256])
{
	if (SIOUXTextWindow != NULL)
		SetWTitle((WindowPtr)SIOUXTextWindow, title);
}
