/*---------------------------------------------------------*
 |			wxwinaux.c
 |		Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *---------------------------------------------------------*/

/* wxWinExtdaux.c */
#include <stdio.h>
#include <wx.h>

// why isn't this defined in headers?
extern "C" int wxEntry(int argc, char *argv[]);

#include "alspi.h"
#include "cinterf.h"
#include "wxextend.h"

#ifdef wx_mac
#include <SIOUX.h>
#include "wxmacpatch.h"
#endif

class ALSProApp : public wxApp {
	public:
    wxFrame *OnInit(void);
#ifdef wx_mac
	virtual void Dispatch(void);
	virtual void doMacInContent(WindowPtr window);
#endif
};

#ifdef wx_mac
void ALSProApp::Dispatch(void)
{
	if (!SIOUXHandleOneEvent(&cCurrentEvent)) wxApp::Dispatch();
}

void ALSProApp::doMacInContent(WindowPtr window)
{
	wxFrame* theMacWxFrame = findMacWxFrame(window);
	if (theMacWxFrame)
	{
		if (window != ::FrontWindow())
		{		
			wxFrame* frontFrame = findMacWxFrame(::FrontWindow());
			if (!frontFrame) wxFatalError("No wxFrame for frontWindow.");
			if (!frontFrame->IsModal())
			{
				 ::SelectWindow(window); //WCH : should I be calling some wxMethod?
			}
			else ::SysBeep(3);
		}
		else
		{
			doMacContentClick(theMacWxFrame);
		}
	}
}
#endif

// Define a new frame type
class MyFrame: public wxFrame
{ public:
    MyFrame(wxFrame *frame, char *title, int x, int y, int w, int h);
    void OnMenuCommand(int id);
};

// ID for the menu quit command
#define MINIMAL_QUIT 1


// `Main program' equivalent, creating windows and returning main app frame
wxFrame *ALSProApp::OnInit(void)
{
	long user; int usertype;
	long hello; int hellotype;
	long struc; int structype;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&hello,&hellotype,"wx_ALSApp_OnInit");
	PI_makestruct(&struc,&structype,hello,1);
	PI_rungoal(user,struc,structype);
	long arg; int argtype;
	PI_getargn(&arg,&argtype,struc,1);
	if (argtype != PI_INT) CI_get_integer((unsigned long int *)&arg,argtype);

	wxFrame *frame;
	
	frame = (wxFrame *)wxGetTypedObject(arg, wxTYPE_FRAME);
	

  // Return the main frame window
  return frame;
}

// My frame constructor
MyFrame::MyFrame(wxFrame *frame, char *title, int x, int y, int w, int h):
  wxFrame(frame, title, x, y, w, h)
{}

// Intercept menu commands
void MyFrame::OnMenuCommand(int id)
{
  switch (id) {
    case MINIMAL_QUIT:
      delete this;
    break;
  }
}

static void wxALSProButtonOnCommand(Bool *handled, WXID item, WXID event)
{
	
	long user; int usertype;
	long hello; int hellotype;
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&hello,&hellotype,"wx_Button_OnCommand");
	PI_rungoal(user,hello,hellotype);
	
	*handled = TRUE;
}


static void wxALSProFrameOnMenuCommand(Bool *handled, WXID frame, int id)
{
	long user; int usertype;
	long hello; int hellotype;
	long struc; int structype;
	long arg1, arg2; int arg1type, arg2type;
	
	PI_makesym(&user,&usertype,"user");
	PI_makesym(&hello,&hellotype,"wx_Frame_OnMenuCommand");
	PI_makestruct(&struc,&structype,hello,2);
	PI_getargn(&arg1, &arg1type, struc, 1);
	PI_getargn(&arg2, &arg2type, struc, 2);
	PI_unify(arg1, arg1type, frame, PI_INT);
	PI_unify(arg2, arg2type, id, PI_INT);
	PI_rungoal(user,struc,structype);
	
	*handled = TRUE;
}

void StartwxWindows(void);
void StartwxWindows(void)
{
	ALSProApp *x = new ALSProApp;
	wxInitExtension();
	wxRegisterExtensionCallback(wxTYPE_BUTTON, "OnCommand", (wxExtensionFunction)wxALSProButtonOnCommand);
	wxRegisterExtensionCallback(wxTYPE_FRAME, "OnMenuCommand", (wxExtensionFunction)wxALSProFrameOnMenuCommand);

	char *argv[1] = {"alspro"};
	wxEntry(1, argv);
	
	wxCleanupExtension();
}

void ExtensionErrorFunction(char *s);
void ExtensionErrorFunction(char *s)
{
	printf("Error: %s\n", s);
}

char *wxExtensionKeyCodeIntToString(int keyCode);
char *wxExtensionKeyCodeIntToString(int keyCode)
{
  static char a[2];
  a[0] = keyCode;
  a[1] = 0;
  return a;
}


#ifdef wx_mac

WXID wxcListBoxCreate(WXID parentId, char *label, Bool multiple,
  int x, int y, int width, int height, long style, char *name)
{
	return wxcListBoxCreate(parentId, label, multiple,
		x, y, width, height, style);
}

WXID wxcDialogBoxCreate(WXID parentId, char *title, Bool modal,
  int x, int y, int width, int height, long style,
  char *name)
{
	return wxcDialogBoxCreate(parentId, title, modal,
		x, y, width, height, style);
}

WXID wxcMultiTextCreate(WXID parentId, char *label, char *value,
  int x, int y, int width, int height, long style, char *name)
{
	return wxcMultiTextCreate(parentId, label, value,
		x, y, width, height, style);
}

WXID wxcTextCreate(WXID parentId, char *label, char *value,
  int x, int y, int width, int height, long style, char *name)
{
	return wxcTextCreate(parentId, label, value,
		x, y, width, height, style);
}

WXID wxcTextWindowCreate(WXID parentId, int x, int y, int width, int height,
  long style, char *name)
{
	return wxcTextWindowCreate(parentId, x, y, width, height, style);
}

WXID wxcCanvasCreate(WXID parentId, int x, int y, int width, int height, long style,
  char *name)
{
	return wxcCanvasCreate(parentId, x, y, width, height, style);
}

WXID wxcPanelCreate(WXID parentId, int x, int y, int width, int height, long style, char *name)
{
	return wxcPanelCreate(parentId, x, y, width, height, style);
}

WXID wxcMessageCreate(WXID parentId, char *label, int x, int y,
  long style, char *name)
{
	return wxcMessageCreate(parentId, label, x, y,
  		style);
}

Bool wxcMenuAppend(WXID id, int item_id, char *menu_string, WXID submenu_id, char *help_string, Bool checkable)
{
	return wxcMenuAppend(id, item_id, menu_string, submenu_id, help_string);
}

WXID wxcMessageCreateFromBitmap(WXID parentId, WXID bitmapId, int x, int y,
  long style, char *name)
{
	return wxcMessageCreateFromBitmap(parentId, bitmapId, x, y,
  			style);
}

WXID wxcFrameCreate(WXID parentId, char *title, int x, int y,
  int width, int height, long style, char *name)
{
	return wxcFrameCreate(parentId, title, x, y,
  		width, height, style);
}

WXID wxcCheckBoxCreate(WXID parentId, char *label,
  int x, int y, int width, int height, long style, char *name)
{
	return wxcCheckBoxCreate(parentId, label,
  		x, y, width, height, style);
}

WXID wxcButtonCreate(WXID parentId, char *label,
  int x, int y, int width, int height, long style, char *name)
{
	return wxcButtonCreate(parentId, label,
		x, y, width, height, style);
}

WXID wxcButtonCreateFromBitmap(WXID parentId, WXID bitmapId,
  int x, int y, int width, int height, long style, char *name)
{
	return wxcButtonCreateFromBitmap(parentId, bitmapId,
		x, y, width, height, style);
}

WXID wxcIconLoadFromFile(char *file, char *bitmapType)
{
	return wxcIconLoadFromFile(file);
}

Bool wxcFrameSetStatusText(WXID id, char *text, int i)
{
	return wxcFrameSetStatusText(id, text);
}

WXID wxcToolbarCreate(long parent, int x, int y, int w, int h, long style,
   int orientation, int rowsOrColumns, Bool createButtons, char *name)
{
	return wxcToolbarCreate(parent, x, y, w, h, style,
   orientation, rowsOrColumns, createButtons);
}

WXID wxcChoiceCreate(WXID parentId, char *label, int x, int y,
  int width, int height, int n, char **choices, long style, char *name)
{
	return wxcChoiceCreate(parentId, label, x, y,
		width, height, n, choices, style);
}

WXID wxcSliderCreate(WXID parentId, char *label,
  int value, int min_value, int max_value, int width, int x, int y,
  long style, char *name)
{
	return wxcSliderCreate(parentId, label,
		value, min_value, max_value, width, x, y,
		style);
}

WXID wxcRadioBoxCreate(WXID parentId, char *label, int x, int y,
                       int width, int height, int n, char **choices,
                       int majorDim, long style, char *name)
{
	return wxcRadioBoxCreate(parentId, label, x, y,
                       width, height, n, choices,
                       majorDim, style);
}

#endif
