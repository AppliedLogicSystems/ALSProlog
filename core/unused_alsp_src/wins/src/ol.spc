/*---------------------------------------------------------*
 |			ol.spc
 |		Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *---------------------------------------------------------*/

#exclude "/usr/include/?"
#exclude "/usr/include/sys/*"

#exclude <X11/Xlib.h>
#exclude <X11/Xutil.h>
#exclude <X11/Xatom.h>
#exclude <X11/cursorfont.h>
#exclude <X11/Xresource.h>
#exclude <X11/keysym.h>

#exclude <X11/ *>/func

#exclude <Xol/OlXlibExt.h>
#exclude <Xol/regexp.h>

#include "ol.h"

extern OlListItem *"OlListItemPointer"(OlListToken);

extern Buffer * AllocateBuffer(int,int);

extern TextBuffer * AllocateTextBuffer(char *,TextUpdateFunction,caddr_t);

extern ScanResult BackwardScanTextBuffer(TextBuffer *, char *, TextLocation *);

extern int "BufferFilled"(Buffer *);

extern int "BufferLeft"(Buffer *);

extern int "BufferEmpty"(Buffer *);

extern Buffer * CopyBuffer(Buffer * buffer);

extern int CopyTextBufferBlock(
     TextBuffer * text,
     char * buffer,
     TextPosition start_position,
     TextPosition end_position);

extern TextLocation EndCurrentTextBufferWord(
     TextBuffer * textBuffer,
     TextLocation current);

extern ScanResult ForwardScanTextBuffer(
     TextBuffer * text,
     TextLocation * location,
     char * exp);

 extern void FreeBuffer(
     Buffer * buffer);

extern void FreeTextBuffer(
     TextBuffer * text,
     TextUpdateFunction f,
     caddr_t d);

extern Cursor GetOlBusyCursor(
     Screen * screen);

extern Cursor GetOlDuplicateCursor(
     Screen * screen);

extern Cursor GetOlMoveCursor(
     Screen * screen);

extern Cursor GetOlPanCursor(
     Screen * screen);

extern Cursor GetOlQuestionCursor(
     Screen * screen);

extern Cursor GetOlStandardCursor(
     Screen * screen);

extern Cursor GetOlTargetCursor(
     Screen * screen);

extern char * GetTextBufferBlock(
     TextBuffer * text,
     TextLocation start_location,
     TextLocation end_location);

 extern Buffer * GetTextBufferBuffer(
     TextBuffer * text,
     TextLine     line);

int GetTextBufferChar(
     TextBuffer * text,
     TextLocation location);

extern char * GetTextBufferLine(
     TextBuffer * text,
     TextLine lineindex);

extern char * GetTextBufferLocation(
     TextBuffer * text,
     TextLine line_number,
     TextLocation * location);

extern void GrowBuffer(
     Buffer * buffer,
     int increment);

extern TextLocation IncrementTextBufferLocation(
     TextBuffer * text,
     TextLocation location,
     TextLine line,
     TextPosition offset);

extern int InsertIntoBuffer(
     Buffer * target,
     Buffer * source,
     int offset);

extern TextLocation LastTextBufferLocation(
     TextBuffer * text);

extern TextPosition LastTextBufferPosition(
     TextBuffer * text);

extern int LineOfPosition(
     TextBuffer * text,
     TextPosition position);

extern TextLocation LocationOfPosition(
     TextBuffer * text,
     TextPosition position);
/*
 OlDynamicColors is not defined in OpenWindows 3

extern void LookupOlColors(
     Widget w,
     OlDynamicColors * colors);
*/
extern OlInputEvent LookupOlInputEvent(
     Widget   w,
     XEvent * event,
     KeySym * keysym,
     char **  buffer,
     int *    length);

extern TextLocation NextLocation(
     TextBuffer * textBuffer,
     TextLocation current);

extern TextLocation NextTextBufferWord(
     TextBuffer * textBuffer,
     TextLocation current);

extern void OlCallDynamicCallbacks(void);

extern char *OlConvertVirtualTranslation(
     char *virtual_translation);

extern ButtonAction OlDetermineMouseAction(
     Widget   w,
     XEvent * event);

 extern void OlDragAndDrop(
     Widget     w,
     Window *   window,
     Position * xPosition,
     Position * yPosition);

extern Pixmap OlGet50PercentGrey(
     Screen * screen);

extern Pixmap OlGet75PercentGrey(
     Screen * screen);

extern void
     OlGetApplicationResources(
     Widget       w,
     caddr_t      base,
     XtResource * resources,
     int          num_resources,
     ArgList      args,
     Cardinal     num_args);

extern void OlGrabDragPointer(
     Widget w,
     Cursor cursor,
     Window window);

extern void OlRegisterDynamicCallback(
     void (*CB)(),
     caddr_t data);

extern void OlReplayBtnEvent(
     Widget   w,
     caddr_t  client_data,
     XEvent * event);

extern Boolean OlTextEditClearBuffer(
     TextEditWidget ctx);

extern Boolean OlTextEditCopySelection(
     TextEditWidget ctx,
     int            delete);

 extern Boolean OlTextEditGetCursorPosition(
     TextEditWidget ctx,
     TextPosition * start,
     TextPosition * end,
     TextPosition * cursorPosition);

 extern Boolean OlTextEditGetLastPosition(
     TextEditWidget ctx,
     TextPosition * position);

extern Boolean OlTextEditInsert(
     TextEditWidget ctx,
     String         buffer,
     int            length);

extern Boolean OlTextEditPaste(
     TextEditWidget ctx);

extern Boolean OlTextEditReadSubString(
     TextEditWidget ctx,
     char ** buffer,
     TextPosition start,
     TextPosition end);

extern Boolean OlTextEditRedraw(
     TextEditWidget ctx);

extern Boolean OlTextEditSetCursorPosition(
     TextEditWidget ctx,
     TextPosition start,
     TextPosition end,
     TextPosition cursorPosition);

extern TextBuffer * OlTextEditTextBuffer(
     TextEditWidget ctx);

extern Boolean OlTextEditUpdate(
     TextEditWidget ctx,
     Boolean state);

extern int OlTextFieldCopyString(
     TextFieldWidget tfw,
     char *          string);

 extern char * OlTextFieldGetString(
     TextFieldWidget tfw,
     int *           size);

extern void OlUngrabDragPointer(
     Widget w);

extern int OlUnregisterDynamicCallback(
     void (*CB)(),
     caddr_t data);

extern TextPosition PositionOfLine(
     TextBuffer * text,
     TextLine lineindex);

extern TextPosition PositionOfLocation(
     TextBuffer * text,
     TextLocation location);

extern TextLocation PreviousLocation(
     TextBuffer * textBuffer,
     TextLocation current);

extern TextLocation PreviousTextBufferWord(
     TextBuffer * textBuffer,
     TextLocation current);

extern int ReadFileIntoBuffer(
     FILE * fp,
     Buffer * buffer);

extern TextBuffer * ReadFileIntoTextBuffer(
     char * filename,
     TextUpdateFunction f,
     caddr_t d);

extern int ReadStringIntoBuffer(
     Buffer * sp,
     Buffer * buffer);

extern TextBuffer * ReadStringIntoTextBuffer(
     char * string,
     TextUpdateFunction f,
     caddr_t d);

extern void RegisterTextBufferScanFunctions(
     char * (*forward)(),
     char * (*backward)());

extern void RegisterTextBufferUpdate(
     TextBuffer * text,
     TextUpdateFunction f,
     caddr_t d);

extern void RegisterTextBufferWordDefinition(
     int (*word_definition)());

extern EditResult ReplaceBlockInTextBuffer(
     TextBuffer * text,
     TextLocation * startloc,
     TextLocation * endloc,
     char * string,
     TextUpdateFunction f,
     caddr_t d);

extern EditResult ReplaceCharInTextBuffer(
     TextBuffer *       text,
     TextLocation *     location,
     int                c,
     TextUpdateFunction f,
    caddr_t            d);

extern SaveResult SaveTextBuffer(
     TextBuffer * text,
     char * filename);

extern TextLocation StartCurrentTextBufferWord(
     TextBuffer * textBuffer,
     TextLocation current);

extern unsigned long "TextBufferUserData"(TextBuffer *,int);

extern char * "TextBufferName"(TextBuffer *);

extern char "TextBufferModified"(TextBuffer *);

extern char "TextBufferEmpty"(TextBuffer *);

extern char "TextBufferNamed"(TextBuffer *);

extern int "LinesInTextBuffer"(TextBuffer *);

extern int "LastTextBufferLine"(TextBuffer *);

extern int "LastCharacterInTextBufferLine"(TextBuffer *,int);

extern int "LengthOfTextBufferLine"(TextBuffer *,int);

extern int "SameTextLocation"(TextLocation,TextLocation);

extern int UnregisterTextBufferUpdate(
     TextBuffer * text,
     TextUpdateFunction f,
     caddr_t d);

extern  Widget OlMoveFocus(
     Widget  w,
     OlDefine  direction,
     Time  *time);

extern  Widget OlMoveFocus(
     Widget  w,
     OlDefine  direction,
     Time  *time);

extern Widget OlCreatePackedWidgetList(
     OlPackedWidget *pw_list,
     Cardinal num_pw);

extern Screen *"OlDefaultScreen";
extern Display *"OlDefaultDisplay";

extern Pixel   "OlMMToPixel"(int, int);
extern Pixel "Ol_MMToPixel"(int, int);
extern Pixel "OlPointToPixel"(int, int);
extern Pixel "Ol_PointToPixel"(int, int);
extern Pixel "OlScreenMMToPixel"(int, int, Screen *);
extern Pixel "Ol_ScreenMMToPixel"(int, int, Screen *);
extern Pixel "OlScreenPointToPixel"(int, int, Screen *);
extern Pixel "Ol_ScreenPointToPixel"(int, int, Screen *);
extern int "OlPixelToMM"(int, int);
extern int "Ol_PixelToMM"(int, int);
 extern long "OlPixelToPoint"(int, int);
extern long "Ol_PixelToPoint"(int, int);
extern long "OlScreenPixelToPoint"(int, int, Screen *);
extern long "Ol_ScreenPixelToPoint"(int, int, Screen *);
extern int "OlScreenPixelToMM"(int, int, Screen *);
extern int "Ol_ScreenPixelToMM"(int, int, Screen *);
extern int "Ol_ScreenPixelToMM"(int, int, Screen *);

extern void strclose(
     Buffer * sp);

extern char * streexp(void);

extern char * strexp(
     char * string,
     char * curp,
     char * expression);

extern int strgetc(
     Buffer * sp);

extern Buffer * stropen(
     char * string);

extern char * strrexp(
     char * string,
     char * curp,
     char * expression);

/* XT function prototypes */

extern void XtGrabKey (
	Widget    widget,
	KeyCode   keycode,
	unsigned int modifiers,
	Boolean   owner_events,
	int       pointer_mode,
	int       keyboard_mode);

extern int XtGrabKeyboard (
	Widget    widget,
	Boolean   owner_events,
	int       pointer_mode,
	int       keyboard_mode,
	Time      time);

extern void XtUngrabKey (
	Widget    widget,
	KeyCode   keycode,
	unsigned int modifiers);

extern void XtUngrabKeyboard (
	Widget    widget,
	Time      time);
/*
extern void XtWidgetCallCallbacks (
	XtCallbackList callbacks,
	Opaque    call_data);
*/

extern void XtAddActions(
	XtActionList actions,
	Cardinal num_actions);

extern void XtAddCallback(
	Widget w,
	String callback_name,
	XtCallbackProc callback,
	caddr_t client_data);

extern void XtAddCallbacks(
	Widget w,
	String callback_name,
	XtCallbackList callbacks);

extern void XtAddConverter(
	String from_type,
	String to_type,
	XtConverter converter,
	XtConvertArgList convert_args,
	Cardinal num_args);

extern void XtAddEventHandler(
	Widget w,
	EventMask event_mask,
	Boolean nonmaskable,
	XtEventHandler proc,
	caddr_t client_data);

extern void XtAddExposureToRegion(
	XEvent *event,
	Region region);

extern void XtAddGrab(
	Widget w,
	Boolean exclusive,
	Boolean spring_loaded);

extern XtInputId XtAddInput(
	int source,
	caddr_t condition,
	XtInputCallbackProc proc,
	caddr_t client_data);

extern void XtAddRawEventHandler(
	Widget w,
	EventMask event_mask,
	Boolean nonmaskable,
	XtEventHandler proc,
	caddr_t client_data);

extern XtIntervalId XtAddTimeOut(
	unsigned long interval,
	XtTimerCallbackProc proc,
	caddr_t client_data);

extern XtWorkProcId XtAddWorkProc(
	XtWorkProc proc,
	caddr_t client_data);

extern void XtAppAddActions(
	XtAppContext app_context,
	XtActionList actions,
	Cardinal num_actions);

extern void XtAppAddConverter(
	XtAppContext app_context,
	String from_type,
	String to_type,
	XtConverter converter,
	XtConvertArgList convert_args,
	Cardinal num_args);

extern XtInputId XtAppAddInput(
	XtAppContext app_context,
	int source,
	caddr_t condition,
	XtInputCallbackProc proc,
	caddr_t client_data);

extern XtIntervalId XtAppAddTimeOut(
	XtAppContext app_context,
	unsigned long interval,
	XtTimerCallbackProc proc,
	caddr_t client_data);

extern XtWorkProcId XtAppAddWorkProc(
	XtAppContext app_context,
	XtWorkProc proc,
	caddr_t client_data);

extern Widget XtAppCreateShell(
	String application_name,
	String application_class,
	WidgetClass widget_class,
	Display *display,
	ArgList args,
	Cardinal num_args);

extern void XtAppError(
	XtAppContext app_context,
	String message);

extern void XtAppErrorMsg(
	XtAppContext app_context,
	String name,
	String type,
	String class,
	String defaultstr,
	String *params,
	Cardinal *num_params);

extern XrmDatabase *XtAppGetErrorDatabase(
	XtAppContext app_context);

extern void XtAppGetErrorDatabaseText(
	XtAppContext app_context,
	char *name, char *type, char *class,
	char *defaultstr,
	char *buffer_return,
	int nbytes,
	XrmDatabase database);

extern unsigned int XtAppGetSelectionTimeout(
	XtAppContext app_context);

extern void XtAppMainLoop(
	XtAppContext app_context);

extern void XtAppNextEvent(
	XtAppContext app_context,
	XEvent *event_return);

extern Boolean XtAppPeekEvent(
	XtAppContext app_context,
	XEvent *event_return);

extern XtInputMask XtAppPending(
	XtAppContext app_context);

extern void XtAppProcessEvent(
	XtAppContext app_context,
	XtInputMask mask);

extern void XtAppSetErrorHandler(
	XtAppContext app_context,
	XtErrorHandler handler);

extern void XtAppSetErrorMsgHandler(
	XtAppContext app_context,
	XtErrorMsgHandler msg_handler);

extern void XtAppSetSelectionTimeout(
	XtAppContext app_context,
	unsigned long timeout);

extern void XtAppSetWarningHandler(
	XtAppContext app_context,
	XtErrorHandler handler);

extern void XtAppSetWarningMsgHandler(
	XtAppContext app_context,
	XtErrorMsgHandler msg_handler);

extern void XtAppWarning(
	XtAppContext app_context,
	String message);

extern void XtAppWarningMsg(
	XtAppContext app_context,
	String name,
	String type,
	String class,
	String defaultstr,
	String *params,
	Cardinal *num_params);

extern void XtAugmentTranslations(
	Widget w,
	XtTranslations translations);

extern EventMask XtBuildEventMask(
	Widget w);

extern Boolean XtCallAcceptFocus(
	Widget w,
	Time *time);

extern void XtCallbackExclusive(
	Widget w,
	caddr_t client_data, 
	caddr_t call_data);

extern void XtCallbackNone(
	Widget w,
	caddr_t client_data,
	caddr_t call_data);

extern void XtCallbackNonexclusive(
	Widget w,
	caddr_t client_data,
	caddr_t call_data);

extern void XtCallbackPopdown(
	Widget w,
	caddr_t client_data,
	caddr_t call_data);

extern void XtCallCallbacks(
	Widget w,
	String callback_name,
	caddr_t call_data);

extern char *XtCalloc(
	unsigned int num,
	unsigned int size);

extern WidgetClass "XtClass"(
	Widget w);

extern void XtCloseDisplay(
	Display *display);

extern void XtConfigureWidget(
	Widget w,
	Position x,
	Position y,
	Dimension width,
	Dimension height,
	Dimension border_width);

extern void XtConvert(
	Widget w,
	String from_type,
	XrmValuePtr from,
	String to_type,
	XrmValuePtr to_return);

extern void XtConvertCase(
	Display *display,
	KeySym keysym,
	KeySym *lower_sreturn,
	KeySym *upper_sreturn);

extern XtAppContext XtCreateApplicationContext();

extern Widget XtCreateApplicationShell(
	String application_name,
	WidgetClass widget_class,
	ArgList args,
	Cardinal num_args);

extern Widget XtCreateManagedWidget(
	String name,
	WidgetClass widget_class,
	Widget parent,
	ArgList args,
	Cardinal num_args);

extern Widget XtCreatePopupShell(
	String name,
	WidgetClass widget_class,
	Widget parent,
	ArgList args,
	Cardinal num_args);

extern Widget XtCreateWidget(
	String name,
	WidgetClass widget_class,
	Widget parent,
	ArgList args,
	Cardinal num_args);

extern void XtCreateWindow(
	Widget w,
	unsigned int window_class,
	Visual *visual,
	XtValueMask value_mask,
	XSetWindowAttributes *attributes);

extern XrmDatabase XtDatabase(
	Display *display);

extern void XtDestroyApplicationContext(
	XtAppContext app_context);

extern void XtDestroyGC(
	GC gc);

extern void XtDestroyWidget(
	Widget w);

extern void XtDirectConvert(
	XtConverter converter,
	XrmValuePtr args,
	Cardinal num_args,
	XrmValuePtr from,
	XrmValuePtr to_return);

extern void XtDisownSelection(
	Widget w,
	Atom selection,
	Time time);

extern void XtDispatchEvent(
	XEvent *event);

extern Display *"XtDisplay"(
	Widget w);

extern void XtDisplayInitialize (
	XtAppContext   app_context,
	Display        * display,
	String         application_name,
	String         application_class,
	XrmOptionDescRec *options,
	Cardinal       num_options,
	Cardinal       * argc,
	String         argv);

extern void XtError(
	String message);

extern void XtErrorMsg(
	String name, 
	String type, 
	String class, 
	String defaultstr,
	String *params,
	Cardinal *num_params);

extern void XtFree(
	char *ptr);

extern void XtGetApplicationResources(
	Widget w,
	caddr_t base,
	XtResourceList resources,
	Cardinal num_resources,
	ArgList args,
	Cardinal num_args);

extern XrmDatabase *XtGetErrorDatabase();

extern void XtGetErrorDatabaseText(
	char *name,
	char *type, 
	char *class,
	char *defaultstr,
	char *buffer_return,
	int nbytes);

extern GC XtGetGC(
	Widget w,
	XtGCMask value_mask,
	XGCValues *values);

extern void XtGetResourceList(
	WidgetClass class,
	XtResourceList *resources_sreturn,
	Cardinal *num_resources_sreturn);

extern unsigned int XtGetSelectionTimeout();

extern void XtGetSelectionValue(
	Widget w,
	Atom selection,
	Atom target,
	XtSelectionCallbackProc callback,
	caddr_t client_data,
	Time time);

extern void XtGetSelectionValues(
	Widget w,
	Atom selection,
	Atom *targets,
	int count,
	XtSelectionCallbackProc callback,
	caddr_t client_data,
	Time time);

extern void XtGetSubresources(
	Widget w,
	caddr_t base,
	String name,
	String class,
	XtResourceList resources,
	Cardinal num_resources,
	ArgList args,
	Cardinal num_args);

extern void XtGetSubvalues(
	caddr_t base,
	XtResourceList resource,
	Cardinal num_resources,
	ArgList args,
	Cardinal num_args);

extern void XtGetValues(
	Widget w,
	ArgList args,
	Cardinal num_args);

extern XtCallbackStatus XtHasCallbacks(
	Widget w,
	String callback_name);

extern Widget XtInitialize(
	char *shell_name, 
	char *application_class,
	XrmOptionDescRec *options,
	Cardinal num_options, 
	Cardinal *argc_in_out,
	char **argv);

extern void XtInstallAccelerators(
	Widget destination,
	Widget source);

extern void XtInstallAllAccelerators(
	Widget destination,
	Widget source);

extern Boolean "XtIsComposite"(
	Widget w);

extern Boolean "XtIsConstraint"(
	Widget w);

extern Boolean "XtIsManaged"(
	Widget w);

extern Boolean "XtIsRealized"(
	Widget w);

extern Boolean "XtIsSensitive"(
	Widget w);

extern Boolean "XtIsShell"(
	Widget w);

extern Boolean "XtIsSubclass"(
	Widget w,
	WidgetClass widget_class);

extern void XtMainLoop();

extern XtGeometryResult XtMakeGeometryRequest(
	Widget w,
	XtWidgetGeometry *request,
	XtWidgetGeometry *reply_return);

extern XtGeometryResult XtMakeResizeRequest(
	Widget w,
	Dimension width, Dimension height,
	Dimension *width_sreturn, 
	Dimension *height_sreturn);

extern char *XtMalloc(
	unsigned int size);

extern void XtManageChild(
	Widget child);

extern void XtManageChildren(
	WidgetList children,
	Cardinal num_children);

extern "XtMapWidget"(
	Widget w);

extern ArgList XtMergeArgLists(
	ArgList args1,
	Cardinal num_args1,
	ArgList args2,
	Cardinal num_args2);

extern void XtMoveWidget(
	Widget w,
	Position x,
	Position y);

extern Widget XtNameToWidget(
	Widget reference,
	String names);

extern String "XtNewString"(
	String string);

extern void XtNextEvent(
	XEvent *event_return);

extern Display *XtOpenDisplay(
	XtAppContext app_context,
	String display_string,
	String application_name,
	String application_class,
	XrmOptionDescRec *options,
	Cardinal num_options,
	Cardinal *argc,
	String *argv);

extern void XtOverrideTranslations(
	Widget w,
	XtTranslations translations);

extern Boolean XtOwnSelection(
	Widget w,
	Atom selection,
	Time time,
	XtConvertSelectionProc convert_proc,
	XtLoseSelectionProc lose_selection,
	XtSelectionDoneProc done_proc);

extern Widget "XtParent"(
	Widget w);

extern XtAccelerators XtParseAcceleratorTable(
	String source);

extern XtTranslations XtParseTranslationTable(
	String table);

extern Boolean XtPeekEvent(
	XEvent *event_return);

extern XtInputMask XtPending();

extern	void XtPopdown(Widget popup_shell);

extern void XtPopup(
	Widget popup_shell,
	XtGrabKind grab_kind);

extern void XtProcessEvent(
	XtInputMask mask);

extern XtGeometryResult XtQueryGeometry(
	Widget w,
	XtWidgetGeometry *intended, 
	XtWidgetGeometry *preferred_return);

extern void XtRealizeWidget(
	Widget w);

extern char *XtRealloc(
	char *ptr,
	Cardinal num);

extern void XtRegisterCaseConverter(
	Display *display,
	XtCaseProc proc,
	KeySym start,
	KeySym stop);

extern void XtReleaseGC(
	Widget w,
	GC gc);

extern void XtRemoveAllCallbacks(
	Widget w,
	String callback_name);

extern void XtRemoveCallback(
	Widget w,
	String callback_name,
	XtCallbackProc callback,
	caddr_t client_data);

extern void XtRemoveCallbacks(
	Widget w,
	String callback_name,
	XtCallbackList callbacks);

extern void XtRemoveEventHandler(
	Widget w,
	EventMask event_mask,
	Boolean nonmaskable,
	XtEventHandler proc,
	caddr_t client_data);

extern void XtRemoveGrab(
	Widget w);

extern void XtRemoveInput(
	XtInputId id);

extern void XtRemoveRawEventHandler(
	Widget w,
	EventMask event_mask,
	Boolean nonmaskable,
	XtEventHandler proc,
	caddr_t client_data);

extern void XtRemoveTimeOut(
	XtIntervalId timer);

extern void XtRemoveWorkProc(
	XtWorkProcId id);

extern void XtResizeWidget(
	Widget w,
	Dimension width,
	Dimension height,
	Dimension border_width);

extern void XtResizeWindow(
	Widget w);

extern Screen *"XtScreen"(
	Widget w);

extern void XtSetErrorHandler(
	XtErrorHandler handler);

extern void XtSetErrorMsgHandler(
	XtErrorMsgHandler msg_handler);

extern XtSetKeyboardFocus(
	Widget subtree, Widget descendant);

extern void XtSetKeyTranslator(
	Display *display,
	XtKeyProc proc);

extern void XtSetMappedWhenManaged(
	Widget w,
	Boolean map_when_managed);

extern void XtSetSelectionTimeout(
	unsigned long timeout);

extern void XtSetSensitive(
	Widget w,
	Boolean sensitive);

extern void XtSetSubvalues(
	caddr_t base,
	XtResourceList resources,
	Cardinal num_resources,
	ArgList args,
	Cardinal num_args);

extern void XtSetValues(
	Widget w,
	ArgList args,
	Cardinal num_args);

extern void XtSetWarningHandler(
	XtErrorHandler handler);

extern void XtSetWarningMsgHandler(
	XtErrorMsgHandler msg_handler);

extern void XtStringConversionWarning(
	String src, String dst_type);

extern WidgetClass "XtSuperclass"(
	Widget w);

extern void XtToolkitInitialize();

extern void XtTranslateCoords(
	Widget w,
	Position x, 
	Position y,
	Position *rootx_sreturn, 
	Position *rooty_sreturn);

extern void XtTranslateKey(
	Display *display,
	KeyCode keycode,
	Modifiers modifiers,
	Modifiers *modifiers_sreturn,
	KeySym *keysym_sreturn);

extern void XtTranslateKeycode(
	Display *display,
	KeyCode keycode,
	Modifiers modifiers,
	Modifiers *modifiers_sreturn,
	KeySym *keysym_sreturn);

extern void XtUninstallTranslations(
	Widget w);

extern void XtUnmanageChild(
	Widget child);

extern void XtUnmanageChildren(
	WidgetList children,
	Cardinal num_children);

extern "XtUnmapWidget"(
	Widget w);

extern void XtUnrealizeWidget(
	Widget w);

extern void XtWarning(
	String message);

extern void XtWarningMsg(
	String name, 
	String type, 
	String class, 
	String defaultstr, 
	String *params,
	Cardinal *num_params);

extern XtAppContext XtWidgetToApplicationContext(
	Widget w);

extern Window "XtWindow"(
	Widget w);

extern Widget XtWindowToWidget(
	Display *display,
	Window window);

extern void "XtSetArg"(
	Arg arg, 
	String resource_name, 
	XtArgVal value);
