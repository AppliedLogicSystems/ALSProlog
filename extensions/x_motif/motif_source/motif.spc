/*---------------------------------------------------------*
 |			motif.spc
 |		Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |
 |		Specification for the broad interface to 
 |		Motif.  Assumes that interface generation
 |		(and compilation) is done with the define
 |			-D__NO_PROTO
 *---------------------------------------------------------*/

/*
#exclude "/jarrett/usr_include/?"
#exclude "/jarrett/usr_include/sys/?"
*/
	/* The machinery needs to be extended
	   so that the excludes (and later includes
	   from "motif.h" can be given in a form
	   something like:
			#exclude "* /include/?"
			#exclude <* /X11/Xutil.h>
			#include <* /Xm/Protocols.h>
	   and the interface generator will fill
	   in the head path ' "* / ' or ' <* / '
	   appropriately from information it is supplied
	   on the command line.  This need is because the
	   locations of X11R6, Motif1.2 on AIX and HP-UX
	   (and maybe elsewhere) are in other places, as
	   are even X11 and Motif1.1 on DV/X
	 */

#exclude "/usr/include/?"
#exclude "/usr/include/sys/?"
#exclude "/usr/include/ansi/?"
#exclude "/usr/include/bsd/?"
#exclude <X11/Xlib.h>
#exclude <X11/Xutil.h>
#exclude <X11/Xatom.h>
#exclude <X11/cursorfont.h>
#exclude <X11/Xresource.h>
#exclude <X11/keysym.h>
#exclude <X11/StringDefs.h>
#exclude <Xm/CutPasteP.h>

#exclude <X11/ *>/func

#include "motif.h"

#if 0
extern Cardinal MrmCloseHierarchy(
	MrmHierarchy hierarchy_id);

extern int MrmFetchColorLiteral(
	MrmHierarchy hierarchy_id,
	String index,
	Display *display,
	Colormap colormap_id,
	Pixel *pixel);

extern int MrmFetchIconLiteral(
	MrmHierarchy hierarchy_id,
	String index,
	Screen *screen,
	Display *display,
	Pixel fgpix,
	Pixel bgpix,
	Pixmap pixmap);

/*
extern Cardinal MrmFetchInterfaceModule(
	MrmHierarchy hierarchy_id,
	char *module_name,
	Widget parent_widget,
	Widget *widget);
*/

extern int MrmFetchLiteral(
	MrmHierarchy hierarchy_id,
	String index,
	Display *display,
	caddr_t *value,
	MrmCode *type);

extern Cardinal MrmFetchSetValues(
	MrmHierarchy hierarchy_id,
	Widget widget,
	ArgList args,
	Cardinal num_args);

extern Cardinal MrmFetchWidget(
	MrmHierarchy hierarchy_id,
	String index,
	Widget parent_widget,
	Widget *widget,
	MrmType *class);

extern Cardinal MrmFetchWidgetOverride(
	MrmHierarchy hierarchy_id,
	String index,
	Widget parent_widget,
	String override_name,
	ArgList override_args,
	Cardinal override_num_args,
	Widget *widget,
	MrmType *class);

extern void MrmInitialize(
	);

extern Cardinal MrmOpenHierarchy(
	MrmCount num_files,
	String file_names_list[],
	MrmOsOpenParamPtr *ancillary_structures_list,
	MrmHierarchy *hierarchy_id);

extern Cardinal MrmRegisterClass(
	MrmType class_code,
	String class_name,
	String create_name,
	Widget (
	* create_proc) (),
	WidgetClass class_record);

extern Cardinal MrmRegisterNames(
	MrmRegisterArglist register_list,
	MrmCount register_count);
#endif

extern void XmActivateProtocol (
	Widget    shell,
	Atom      property,
	Atom      protocol);

extern void XmAddProtocolCallback (
	Widget      shell,
	Atom        property,
	Atom        protocol,
	XtCallbackProc callback,
	caddr_t     closure);

extern void XmAddProtocols (
	Widget    shell,
	Atom      property,
	Atom      * protocols,
	Cardinal  num_protocols);

extern void XmAddTabGroup (
	Widget    tab_group);

extern void XmCascadeButtonHighlight (
	Widget    cascadeButton,
	Boolean   highlight);

extern void XmClipboardCancelCopy (
	Display   * display,
	Window    window,
	long      item_id);

extern int XmClipboardCopy (
	Display    * display,
	Window     window,
	long       item_id,
	char       * format_name,
	char       * buffer,
	unsigned long length,
	int        private_id,
	int        * data_id);

extern int XmClipboardCopyByName (
	Display   * display,
	Window    window,
	int       data_id,
	char      * buffer,
	unsigned long length,
	int       private_id);

extern int XmClipboardEndCopy (
	Display   * display,
	Window    window,
	long      item_id);

extern int XmClipboardEndRetrieve (
	Display   * display,
	Window    window);

extern int XmClipboardInquireCount (
	Display   * display,
	Window    window,
	int       * count,
	int       * max_format_name_length);

extern int XmClipboardInquireFormat (
	Display    * display,
	Window     window,
	int        index,
	char       * format_name_buf,
	unsigned long buffer_len,
	unsigned long* copied_len);

extern int XmClipboardInquireLength (
	Display   * display,
	Window    window,
	char      * format_name,
	unsigned long* length);

extern int XmClipboardInquirePendingItems (
	Display          * display,
	Window           window,
	char             * format_name,
	XmClipboardPendingList* item_list,
	unsigned long    * count);

extern int XmClipboardLock (
	Display   * display,
	Window    window);

extern int XmClipboardRegisterFormat (
	Display     * display,
	char        * format_name,
	unsigned long format_length);

extern int XmClipboardRetrieve (
	Display    * display,
	Window     window,
	char       * format_name,
	char       * buffer,
	unsigned long length,
	unsigned long* num_bytes,
	int        * private_id);

extern int XmClipboardStartCopy (
	Display   * display,
	Window    window,
	XmString  clip_label,
	Time      timestamp,
	Widget    widget,
	VoidProc  callback,
	long      * item_id);

extern int XmClipboardStartRetrieve (
	Display   * display,
	Window    window,
	Time      timestamp);

extern int XmClipboardUndoCopy (
	Display   * display,
	Window    window);

extern int XmClipboardUnlock (
	Display   * display,
	Window    window,
	Boolean   remove_all_locks);

extern int XmClipboardWithdrawFormat (
	Display   * display,
	Window    window,
	int       data_id);

extern void XmCommandAppendValue (
	Widget       widget,
	XmString     command);

extern void XmCommandError (
	Widget       widget,
	XmString     error);

extern Widget XmCommandGetChild (
	Widget       widget,
	unsigned char child);

extern void XmCommandSetValue (
	Widget       widget,
	XmString     command);

extern int XmConvertUnits (
	Widget    widget,
	int       orientation,
	int       from_unit_type,
	int       from_value,
	int       to_unit_type);

extern Widget XmCreateArrowButton (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);


extern Widget XmCreateArrowButtonGadget (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateBulletinBoard (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateBulletinBoardDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateCascadeButton (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateCascadeButtonGadget (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateCommand (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateDialogShell (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateDrawingArea (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateDrawnButton (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateErrorDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateFileSelectionBox (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateFileSelectionDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateForm (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateFormDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateFrame (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateInformationDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateLabel (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateLabelGadget (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateList (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateMainWindow (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateMenuBar (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateMenuShell (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateMessageBox (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);
extern Widget XmCreateMessageDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);
extern Widget XmCreateOptionMenu (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreatePanedWindow (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreatePopupMenu (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreatePromptDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);


extern Widget XmCreatePulldownMenu (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreatePushButton (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);


extern Widget XmCreatePushButtonGadget (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);


extern Widget XmCreateQuestionDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateRadioBox (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateRowColumn (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateScale (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

 extern Widget XmCreateScrollBar (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);


extern Widget XmCreateScrolledList (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateScrolledText (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateScrolledWindow (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateSelectionBox (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateSelectionDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateSeparator (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateSeparatorGadget (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateText (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateToggleButton (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateToggleButtonGadget (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateWarningDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern Widget XmCreateWorkingDialog (
	Widget    parent,
	String    name,
	ArgList   arglist,
	Cardinal  argcount);

extern void XmCvtStringToUnitType (
	XrmValuePtr args,
	Cardinal  * num_args,
	XrmValue  * from_val,
	XrmValue  * to_val);

extern void XmDeactivateProtocol (
	Widget    shell,
	Atom      property,
	Atom      protocol);

extern Boolean XmDestroyPixmap (
	Screen    * screen,
	Pixmap    pixmap);

extern Widget XmFileSelectionBoxGetChild (
	Widget    widget,
	unsigned char child);

extern void XmFileSelectionDoSearch (
	Widget    widget,
	XmString  dirmask);

extern XmFontList XmFontListAdd (
	XmFontList  oldlist,
	XFontStruct * font,
	XmStringCharSet charset);

extern XmFontList XmFontListCreate (
	XFontStruct * font,
	XmStringCharSet charset);

extern void XmFontListFree (
	XmFontList list);

extern Cursor XmGetMenuCursor (
	Display   * display);

extern Pixmap XmGetPixmap (
	Screen    * screen,
	char      * image_name,
	Pixel     foreground,
	Pixel     background);

extern Boolean XmInstallImage (
	XImage    * image,
	char      * image_name);

extern Atom XmInternAtom (
	Display   * display,
	String    name,
	Boolean   only_if_exists);

extern Boolean XmIsMotifWMRunning (
	Widget    shell);

extern void XmListAddItem (
	Widget    widget,
	XmString  item,
	int       position);

extern void XmListAddItemUnselected (
	Widget    widget,
	XmString  item,
	int       position);

extern void XmListDeleteItem (
	Widget    widget,
	XmString  item);

extern void XmListDeletePos (
	Widget    widget,
	int       position);

extern void XmListDeselectAllItems (
	Widget    widget);

extern void XmListDeselectItem (
	Widget    widget,
	XmString  item);

extern void XmListDeselectPos (
	Widget    widget,
	int       position);

extern Boolean XmListItemExists (
	Widget    widget,
	XmString  item);

extern void XmListSelectItem (
	Widget    widget,
	XmString  item,
	Boolean   notify);

extern void XmListSelectPos (
	Widget    widget,
	int       position,
	Boolean   notify);

extern void XmListSetBottomItem (
	Widget    widget,
	XmString  item);

extern void XmListSetBottomPos (
	Widget    widget,
	int       position);

extern void XmListSetHorizPos (
	Widget    widget,
	int       position);

extern void XmListSetItem (
	Widget    widget,
	XmString  item);

extern void XmListSetPos (
	Widget    widget,
	int       position);

extern Widget XmMainWindowSep1 (
	Widget    widget);

extern Widget XmMainWindowSep2 (
	Widget           widget);

extern void XmMainWindowSetAreas (
	Widget    widget,
	Widget    menu_bar,
	Widget    command_window,
	Widget    horizontal_scrollbar,
	Widget    vertical_scrollbar,
	Widget    work_region);

extern void XmMenuPosition (
	Widget         menu,
	XButtonPressedEvent* event);

extern Widget XmMessageBoxGetChild (
	Widget    widget,
	unsigned char child);

extern Widget XmOptionButtonGadget (
	Widget    option_menu);

extern Widget XmOptionLabelGadget (
	Widget    option_menu);

extern void XmRemoveProtocolCallback (
	Widget      shell,
	Atom        property,
	Atom        protocol,
	XtCallbackProc callback,
	caddr_t     closure);

extern void XmRemoveProtocols (
	Widget    shell,
	Atom      property,
	Atom      * protocols,
	Cardinal  num_protocols);

extern void XmRemoveTabGroup (
	Widget    tab_group);

extern void XmResolvePartOffsets (
	WidgetClass widget_class,
	XmOffsetPtr* offset);

extern void XmScaleGetValue (
	Widget    widget,
	int       * value_return);

extern void XmScaleSetValue (
	Widget    widget,
	int       value);

extern void XmScrollBarGetValues (
	Widget      widget,
	int         * value_return,
	int         * slider_size_return,
	int         * increment_return,
	int         * page_increment_return);

extern void XmScrollBarSetValues (
	Widget    widget,
	int       value,
	int       slider_size,
	int       increment,
	int       page_increment,
	Boolean   notify);

extern void XmScrolledWindowSetAreas (
	Widget    widget,
	Widget    horizontal_scrollbar,
	Widget    vertical_scrollbar,
	Widget    work_region);

extern Widget XmSelectionBoxGetChild (
	Widget    widget,
	unsigned char child);

extern void XmSetFontUnit (
	Display   *display,
	int       font_unit_value);

extern void XmSetMenuCursor (
	Display   * display,
	Cursor    cursorId);

extern void XmSetProtocolHooks (
	Widget      shell,
	Atom        property,
	Atom        protocol,
	XtCallbackProc prehook,
	caddr_t     pre_closure,
	XtCallbackProc posthook,
	caddr_t     post_closure);


extern Dimension XmStringBaseline (
	XmFontList fontlist,
	XmString  string);

extern Boolean XmStringByteCompare (
	XmString  s1,
	XmString  s2);

extern Boolean XmStringCompare (
	XmString  s1,
	XmString  s2);

extern XmString XmStringConcat (
	XmString  s1,
	XmString  s2);

extern XmString XmStringCopy (
	XmString  s1);

extern XmString XmStringCreate (
	char        * text,
	XmStringCharSet charset);

extern XmString XmStringCreateSimple (
	char        * text);

extern XmString XmStringCreateLtoR (
	char        * text,
	XmStringCharSet charset);

extern XmString XmStringDirectionCreate (
	XmStringDirection direction);

extern void XmStringDraw (
	Display   * d,
	Window    w,
	XmFontList fontlist,
	XmString  string,
	GC        gc,
	Position  x,
	Position  y,
	Dimension width,
	Byte      alignment,
	Byte      layout_direction,
	XRectangle* clip);

extern void XmStringDrawImage (
	Display   * d,
	Window    w,
	XmFontList fontlist,
	XmString  string,
	GC        gc,
	Position  x,
	Position  y,
	Dimension width,
	Byte      alignment,
	Byte      layout_direction,
	XRectangle* clip);

extern void XmStringDrawUnderline (
	Display   * d,
	Window    w,
	XmFontList fontlist,
	XmString  string,
	GC        gc,
	Position  x,
	Position  y,
	Dimension width,
	Byte      alignment,
	Byte      layout_direction,
	XRectangle* clip,
	XmString  underline);

extern Boolean XmStringEmpty (
	XmString  s1);

extern void XmStringExtent (
	XmFontList fontlist,
	XmString  string,
	Dimension width,
	Dimension height);

extern void XmStringFree (
	XmString  string);

extern void XmStringFreeContext (
	XmStringContext* context);

extern Boolean XmStringGetLtoR (
	XmString       string,
	XmStringCharSet charset,
	char           ** text);

extern XmStringComponentType XmStringGetNextComponent (
	XmStringContext  * context,
	char             ** text,
	XmStringCharSet  * charset,
	XmStringDirection* direction,
	XmStringComponentType*  unknown_tag,
	short            * unknown_length,
	char             ** unknown_value);

extern Boolean XmStringGetNextSegment (
	XmStringContext* context,
	char           ** text,
	XmStringCharSet* charset,
	XmStringDirection* direction,
	Boolean        * separator);

extern Dimension XmStringHeight (
	XmFontList fontlist,
	XmString  string);

extern Boolean XmStringInitContext (
	XmStringContext* context,
	XmString    string);

extern int XmStringLength (
	XmString  s1);

extern int XmStringLineCount (
	XmString  string);

extern XmString XmStringNConcat (
	XmString  s1,
	XmString  s2,
	int       num_bytes);

extern XmString XmStringNCopy (
	XmString  s1,
	int       num_bytes);

extern XmStringComponentType XmStringPeekNextComponent (
	XmStringContext* context);

extern XmString XmStringSegmentCreate (
	char        * text,
	XmStringCharSet charset,
	XmStringDirection direction,
	Boolean     separator);

extern XmString XmStringSeparatorCreate (
	);

extern Dimension XmStringWidth (
	XmFontList fontlist,
	XmString  string);

extern void XmTextClearSelection (
	Widget    widget,
	Time      time);

extern Boolean XmTextGetEditable (
	Widget    widget);

extern int XmTextGetMaxLength (
	Widget    widget);

extern char * XmTextGetSelection (
	Widget    widget);

extern char * XmTextGetString (
	Widget    widget);

extern void XmTextReplace (
	Widget      widget,
	XmTextPosition from_pos,
	XmTextPosition to_pos,
	char        * value);

extern void XmTextSetEditable (
	Widget    widget,
	Boolean   editable);

extern void XmTextSetMaxLength (
	Widget    widget,
	int       max_length);

extern void XmTextSetSelection (
	Widget      widget,
	XmTextPosition first,
	XmTextPosition last,
	Time        time);

extern void XmTextSetString (
	Widget    widget,
	char      * value);

extern Boolean XmToggleButtonGadgetGetState (
	Widget    widget);

extern void XmToggleButtonGadgetSetState (
	Widget    widget,
	Boolean   state,
	Boolean   notify);

extern Boolean XmToggleButtonGetState (
	Widget    widget);

extern void XmToggleButtonSetState (
	Widget    widget,
	Boolean   state,
	Boolean   notify);

extern Boolean XmUninstallImage (
	XImage    * image);

extern void XmUpdateDisplay (
	Widget    widget);


/* XT prototypes */

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

extern XtAppContext XtCreateApplicationContext(void);

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

extern XrmDatabase *XtGetErrorDatabase(void);

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

extern unsigned int XtGetSelectionTimeout(void);

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

extern void XtMainLoop(void);

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

extern XtAccelerators XtParseAcceleratorTable(
	String source);

extern XtTranslations XtParseTranslationTable(
	String table);

extern Boolean XtPeekEvent(
	XEvent *event_return);

extern XtInputMask XtPending(void);

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

extern void XtToolkitInitialize(void);

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

extern Widget XtWindowToWidget(
	Display *display,
	Window window);


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
/*
extern Boolean XtCallAcceptFocus(
	Widget w,
	Time *time);
*/
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

/* the following functions are defined as macros */

extern void "XmActivateWMProtocol"(
	Widget    shell,
	Atom      protocol);

extern void "XmAddWMProtocolCallback"(
	Widget      shell,
	Atom        protocol,
	XtCallbackProc callback,
	caddr_t     closure);

extern void "XmAddWMProtocols"(
	Widget    shell,
	Atom      * protocols,
	Cardinal  num_protocols);

extern void "XmDeactivateWMProtocol"(
	Widget    shell,
	Atom      protocol);

extern void "XmRemoveWMProtocolCallback"(
	Widget      shell,
	Atom        protocol,
	XtCallbackProc callback,
	caddr_t     closure);

extern void "XmRemoveWMProtocols"(
	Widget    shell,
	Atom      * protocols,
	Cardinal  num_protocols);

extern void "XmSetWMProtocolHooks"(
	Widget      shell,
	Atom        protocol,
	XtCallbackProc prehook,
	caddr_t     pre_closure,
	XtCallbackProc posthook,
	caddr_t     post_closure);

extern WidgetClass "XtClass"(
	Widget w);

extern Display *"XtDisplay"(
	Widget w);

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

extern "XtMapWidget"(
	Widget w);

extern String "XtNewString"(
	String string);

extern Widget "XtParent"(
	Widget w);

extern Screen *"XtScreen"(
	Widget w);

extern WidgetClass "XtSuperclass"(
	Widget w);

extern "XtUnmapWidget"(
	Widget w);

extern Window "XtWindow"(
	Widget w);

extern void "XtSetArg"(
	Arg arg, 
	String resource_name, 
	XtArgVal value);

extern Widget "XmCreateSimpleCheckBox"(
	Widget parent,
	String name,
	ArgList args,
	Cardinal arg_count) ;

extern Widget "XmCreateTextField"(
	Widget parent,
	char *name,
	ArgList arglist,
	Cardinal argcount) ;

extern XmTextPosition "XmTextFieldGetLastPosition"(
	Widget w) ;

extern char * "XmTextFieldGetString"(
	Widget w) ;

extern void "XmTextFieldReplace"(
	Widget w,
	XmTextPosition from_pos,
	XmTextPosition to_pos,
	char *value) ;

extern XmTextPosition XmTextGetLastPosition(
	Widget widget) ;

extern XmTextPosition "XmTextGetTopCharacter"(
	Widget widget) ;

extern void "XmTextSetTopCharacter"(
	Widget widget,
	XmTextPosition top_character) ;

extern XmTextPosition "XmTextGetCursorPosition"(
	Widget widget) ;

extern XmTextPosition "XmTextGetInsertionPosition"(
	Widget widget) ;

extern void "XmTextSetInsertionPosition"(
	Widget widget,
	XmTextPosition position) ;

extern void "XmTextSetCursorPosition"(
	Widget widget,
	XmTextPosition position) ;

extern void "XmTextInsert"(
	Widget widget,
	XmTextPosition position,
	char *value) ;

extern void "XmTextScroll"(
	Widget widget,
	int n) ;

extern XmFontList "XmFontListCopy"(
	XmFontList fontlist) ;

extern void "XmListDeleteAllItems"(
	Widget w) ;

