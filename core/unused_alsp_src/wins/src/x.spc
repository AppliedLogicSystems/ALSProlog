/*---------------------------------------------------------*
 |			x.spc
 |		Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |
 *---------------------------------------------------------*/

#exclude "/usr/include/?"
#exclude "/usr/include/sys/*"

#exclude "x.h"/func

#include "x.h"

extern long "BlackPixel"( Display *display, int screen_number);

extern long "WhitePixel"(Display *display, int screen_number);

extern long "ConnectionNumber"(Display *display);

extern long "DefaultColormap"(Display *display, int screen_number);

extern int "DefaultDepth"(Display *display, int screen_number);

extern GC "DefaultGC"(Display *display, int screen_number);

extern long "DefaultRootWindow"(Display *display);

extern Screen *"DefaultScreenOfDisplay"(Display *display);

extern int "DefaultScreen"(Display *display);

extern Visual *"DefaultVisual"(Display *display, int screen_number);

extern long "DisplayCells"(Display *display, int screen_number);

extern long "DisplayPlanes"(Display *display, int screen_number);

extern char *"DisplayString"(Display *display);

extern long "LastKnownRequestProcessed"(Display *display);

extern long "NextRequest"(Display *display);

extern long "ProtocolVersion"(Display *display);

extern long "ProtocolRevision"(Display *display);

extern long "QLength"(Display *display);

extern Window "RootWindow"(Display *display, int screen_number);

extern int "ScreenCount"(Display *display);

extern Screen *"ScreenOfDisplay"(Display *display, int screen_number);

extern char *"ServerVendor"(Display *display);

extern long "VendorRelease"(Display *display);

extern long "BlackPixelOfScreen"(Screen *screen);

extern long "WhitePixelOfScreen"(Screen *screen);

extern int "CellsOfScreen"(Screen *screen);

extern Colormap "DefaultColormapOfScreen"(Screen *screen);

extern int "DefaultDepthOfScreen"(Screen *screen);

extern GC "DefaultGCOfScreen"(Screen *screen);

extern Visual *"DefaultVisualOfScreen"(Screen *screen);

extern int "DoesBackingStore"(Screen *screen);

extern int "DoesSaveUnders"(Screen *screen);

extern Display *"DisplayOfScreen"(Screen *screen);

extern long "EventMaskOfScreen"(Screen *screen);

extern long "HeightOfScreen"(Screen *screen);

extern long "HeightMMOfScreen"(Screen *screen);

extern int "MaxCmapsOfScreen"(Screen *screen);

extern int "MinCmapsOfScreen"(Screen *screen);

extern int "PlanesOfScreen"(Screen *screen);

extern Window "RootWindowOfScreen"(Screen *screen);

extern long "WidthOfScreen"(Screen *screen);

extern long "WidthMMOfScreen"(Screen *screen);

extern int "ImageByteOrder"(Display *display);

extern long "BitmapBitOrder"(Display *display);

extern long "BitmapPad"(Display *display);

extern long "BitmapUnit"(Display *display);

extern long "DisplayHeight"(Display *display, int sreen_number);

extern long "DisplayHeightMM"(Display *display, int sreen_number);

extern long "DisplayWidth"(Display *display, int sreen_number);

extern long "DisplayWidthMM"(Display *display, int sreen_number);

extern int "IsCursorKey"(KeySym keysym);

extern int "IsFunctionKey"(KeySym keysym);

extern int "IsKeypadKey"(KeySym keysym);

extern int "IsMiscFunctionKey"(KeySym keysym);

extern int "IsModifierKey"(KeySym keysym);

extern int "IsPFKey"(KeySym keysym);

extern long "XMaxRequestSize"(Display *display);

extern unsigned long "XDisplayMotionBufferSize"(Display *display);

extern void "XrmStringToNameList"(char *str, XrmQuarkList name);

extern void "XrmStringToClassList"(char *str, XrmQuarkList class);

extern XActivateScreenSaver(Display *display);

extern XAddHost(Display *display,
                XHostAddress *host);

extern XAddHosts(Display *display,
                XHostAddress *hosts,
                int num_hosts);


extern "XAddPixel"(XImage *ximage,
                long value);

extern  XAddToSaveSet(Display *display,
                Window w);

extern  Status XAllocColor(Display *display,
                Colormap colormap,
                XColor *screen_in_out);

extern  Status XAllocColorCells(Display *display,
                Colormap colormap,
                Bool contig,
                unsigned long plane_masks_return[],
                unsigned int nplanes,
                unsigned long pixels_return[],
                unsigned int npixels);

extern  Status XAllocColorPlanes(Display *display,
                Colormap colormap,
                Bool contig,
                unsigned long pixels_return[],
                int ncolors,
                int nreds,int ngreens,int nblues,
	        unsigned long *rmask_sreturn, unsigned long *gmask_sreturn, unsigned long *bmask_sreturn);


extern  Status XAllocNamedColor(Display *display,
                Colormap colormap,
                char *color_name,
                XColor *screen_def_return,XColor *exact_def_return);

extern  XAllowEvents(Display *display,
                int event_mode,
                Time time);

extern  XAutoRepeatOff(Display *display);

extern  XAutoRepeatOn(Display *display);

extern  XBell(Display *display,
                int percent);

extern  XChangeActivePointerGrab(Display *display,
                unsigned int event_mask,
                Cursor cursor,
                Time time);

extern  XChangeGC(Display *display,
                GC gc,
                unsigned long valuemask,
                XGCValues *values);

extern  XChangeKeyboardControl(Display *display,
                unsigned long value_mask,
                XKeyboardControl *values);

extern  XChangeKeyboardMapping(Display *display,
                int first_keycode,
                int keysyms_per_keycode,
                KeySym *keysyms,
                int num_codes);

extern  XChangePointerControl(Display *display,
                Bool do_accel,Bool do_threshold,
                int accel_numerator, int accel_denominator,
                int threshold);

extern  XChangeProperty(Display *display,
                Window w,
                Atom property, Atom type,
                int format,
                int mode,
                unsigned char *data,
                int nelements);


extern  XChangeSaveSet(Display *display,
                Window w,
                int change_mode);

extern  XChangeWindowAttributes(Display *display,
                Window w,
                unsigned long valuemask,
                XSetWindowAttributes *attributes);

extern  Bool XCheckIfEvent(Display *display,
                XEvent *event_return,
                Bool (*predicate)(),
                char *arg);

extern  Bool XCheckMaskEvent(Display *display,
                long event_mask,
                XEvent *event_return);

extern  Bool XCheckTypedEvent(Display *display,
                int event_type,
                XEvent *event_return);

extern  Bool XCheckTypedWindowEvent(Display *display,
                Window w,
                int event_type,
                XEvent *event_return);

extern  Bool XCheckWindowEvent(Display *display,
                Window w,
                long event_mask,
                XEvent *event_return);

extern  XCirculateSubwindows(Display *display,
                Window w,
                int direction);


extern  XCirculateSubwindowsDown(Display *display,
                Window w);

extern  XCirculateSubwindowsUp(Display *display,
                Window w);

extern  XClearArea(Display *display,
                Window w,
                int x,int  y,
                unsigned int width, unsigned int height,
                Bool exposures);

extern  XClearWindow(Display *display,
                Window w);

extern  XClipBox(Region r,
                XRectangle *rect_return);

extern  XCloseDisplay(Display *display);

extern  XConfigureWindow(Display *display,
                Window w,
                unsigned int value_mask,
                XWindowChanges *values);

extern  XConvertSelection(Display *display,
                Atom selection,Atom target,
                Atom property,
                Window requestor,
                Time time);

extern  XCopyArea(Display *display,
                Drawable src,Drawable dest,
                GC gc,
                int src_x, int src_y,
                unsigned int width, unsigned int height,
                int dest_x, int dest_y);

extern  Colormap XCopyColormapAndFree(Display *display,
                Colormap colormap);

extern  XCopyGC(Display *display,
                GC src, GC dest,
                unsigned long valuemask);

extern  XCopyPlane(Display *display,
                Drawable src, Drawable dest,
                GC gc,
                int src_x, int src_y,
                unsigned int width, unsigned int height,
                int dest_x, int dest_y,
                unsigned long plane);

extern  Pixmap XCreateBitmapFromData(Display *display,
                Drawable d,
                char *data,
                unsigned int width, unsigned int height);

extern  Colormap XCreateColormap(Display *display,
                Window w,
                Visual *visual,
                int alloc);

extern  Cursor XCreateFontCursor(Display *display,
                unsigned int shape);

extern  GC XCreateGC(Display *display,
                Drawable d,
                unsigned long valuemask,
                XGCValues *values);

extern  Cursor XCreateGlyphCursor(Display *display,
                Font source_font, Font mask_font,
                unsigned int source_char, unsigned int mask_char,
                XColor *foreground_color,
                XColor *background_color);

extern  XImage *XCreateImage(Display *display,
                Visual *visual,
                unsigned int depth,
                int format,
                int offset,
                char *data,
                unsigned int width,
                unsigned int height,
                int bitmap_pad,
                int bytes_per_line);

extern  Pixmap XCreatePixmap(Display *display,
                Drawable d,
                unsigned int width, unsigned int height,
                unsigned int depth);

extern  Cursor XCreatePixmapCursor(Display *display,
                Pixmap source,
                Pixmap mask,
                XColor *foreground_color,
                XColor *background_color,
                unsigned int x, unsigned int y);

extern  Pixmap XCreatePixmapFromBitmapData(Display *display,
               Drawable d,
               char *data,
               unsigned int width, unsigned int height,
               unsigned long fg, unsigned long bg,
               unsigned int depth);

extern  Region XCreateRegion();

extern Window XCreateSimpleWindow(Display *display,
                Window parent,
                int x, int y,
                unsigned int width, unsigned int height,
                unsigned int border_width,
                unsigned long border,
                unsigned long background);

extern  Window XCreateWindow(Display *display,
                Window parent,
                int x, int y,
                unsigned int width, unsigned int height,
                unsigned int border_width,
                int depth,
                unsigned int class,
                Visual *visual,
                unsigned long valuemask,
                XSetWindowAttributes *attributes);

extern  XDefineCursor(Display *display,
                Window w,
                Cursor cursor);

extern  int XDeleteContext(Display *display,
                Window w,
                XContext context);

extern  XModifierKeymap *XDeleteModifiermapEntry(XModifierKeymap *modmap,
               KeyCode keycode_entry,
               int modifier);

extern  XDeleteProperty(Display *display,
                Window w,
                Atom property);

extern  int "XDestroyImage"(XImage *ximage);

extern  XDestroyRegion(Region r);

extern  XDestroySubwindows(Display *display,
                Window w);

extern  XDestroyWindow(Display *display,
                Window w);

extern  XDisableAccessControl(Display *display);

extern  XDisplayKeycodes(Display *display,
                  int *min_keycodes_sreturn, int *max_keycodes_sreturn);

extern char *XDisplayName(char *string);

extern  XDrawArc(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                unsigned int width, unsigned int height,
                int angle1, int angle2);

extern  XDrawArcs(Display *display,
                Drawable d,
                GC gc,
                XArc *arcs,
                int narcs);

extern  XDrawImageString(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                char *string,
                int length);

extern  XDrawImageString16(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                XChar2b *string,
                int length);

extern  XDrawLine(Display *display,
                Drawable d,
                GC gc,
                int x1,int y1,int x2,int y2);

extern  XDrawLines(Display *display,
                Drawable d,
                GC gc,
                XPoint *points,
                int npoints,
                int mode);

extern  XDrawPoint(Display *display,
                Drawable d,
                GC gc,
                int x, int y);

extern  XDrawPoints(Display *display,
                Drawable d,
                GC gc,
                XPoint *points,
                int npoints,
                int mode);

extern  XDrawRectangle(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                unsigned int width, unsigned int height);

extern  XDrawRectangles(Display *display,
                Drawable d,
                GC gc,
                XRectangle rectangles[],
                int nrectangles);

extern  XDrawSegments(Display *display,
                Drawable d,
                GC gc,
                XSegment *segments,
                int nsegments);

extern  XDrawString(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                char *string,
                int length);

extern  XDrawString16(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                XChar2b *string,
                int length);

extern  XDrawText(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                XTextItem *items,
                int nitems);

extern  XDrawText16(Display *display,
                Drawable d,
                GC gc,
                int x, int y,
                XTextItem16 *items,
		int nitems);

extern Bool XEmptyRegion(Region r);

extern XEnableAccessControl(Display *display);

extern Bool XEqualRegion(Region r1,Region r2);

extern int XEventsQueued(Display *display,
               int mode);

extern char *XFetchBuffer(Display *display,
                int *nbytes_sreturn,
                int buffer);

extern char *XFetchBytes(Display *display,
                int *nbytes_sreturn);

extern Status XFetchName(Display *display,
                Window w,
                char **window_name_sreturn);

extern XFillArc(Display *display,
                Drawable d,
                GC gc,
                int x,int y,
                unsigned int width,unsigned int height,
                int angle1,int angle2);

extern XFillArcs(Display *display,
                Drawable d,
                GC gc,
                XArc *arcs,
                int narcs);

extern XFillPolygon(Display *display,
                Drawable d,
                GC gc,
                XPoint *points,
                int npoints,
                int shape,
                int mode);

extern XFillRectangle(Display *display,
                Drawable d,
                GC gc,
                int x,int y,
                unsigned int width,unsigned int height);

extern XFillRectangles(Display *display,
                Drawable d,
                GC gc,
                XRectangle *rectangles,
                int nrectangles);

extern int XFindContext(Display *display,
                Window w,
                XContext context,
                caddr_t *data_sreturn);

extern XFlush(Display *display);

extern XForceScreenSaver(Display *display,
                int mode);

extern XFree(char *data);

extern XFreeColormap(Display *display,
                Colormap colormap);

extern XFreeColors(Display *display,
                Colormap colormap,
                unsigned long pixels[],
                int npixels,
                unsigned long planes);

extern XFreeCursor(Display *display,
                Cursor cursor);

extern XFreeExtensionList(char **list);

extern XFreeFont(Display *display,
                XFontStruct *font_struct);

extern XFreeFontInfo(char **names,
                XFontStruct *free_info,
                int actual_count);

extern XFreeFontNames(char *list[]);

extern XFreeFontPath(char **list);

extern XFreeGC(Display *display,
                GC gc);

extern XFreeModifiermap(XModifierKeymap *modmap);

extern XFreePixmap(Display *display,
                Pixmap pixmap);

extern GContext XGContextFromGC(GC gc);

extern int XGeometry(Display *display,
                int screen,
                char *position,char  *default_position,
                unsigned int bwidth,
                unsigned int fwidth,unsigned int fheight,
                int xadder,int yadder,
                int *x_sreturn,int *y_sreturn,
                int *width_sreturn, int *height_sreturn);

extern char *XGetAtomName(Display *display,
                Atom atom);

extern Status XGetClassHint(Display *display,
                Window w,
                XClassHint *class_hints_return);

extern char *XGetDefault(Display *display,
                char *program,
                char *option);

extern XGetErrorDatabaseText(Display *display,
                char *name,char  *message,
                char *default_string,
                char *buffer_return,
                int length);

extern XGetErrorText(Display *display,
                int code,
                char *buffer_return,
                int length);

extern char **XGetFontPath(Display *display,
                int *npaths_sreturn);

extern Bool XGetFontProperty(XFontStruct *font_struct,
                Atom atom,
                unsigned long *value_sreturn);

extern Status XGetGeometry(Display *display,
                  Drawable d,
                  Window *root_sreturn,
                  int *x_sreturn,int *y_sreturn,
                  unsigned int *width_sreturn,unsigned int *height_sreturn,
                  unsigned int *border_width_sreturn,
                  unsigned int *depth_sreturn);

extern Status XGetIconName(Display *display,
                Window w,
                char **icon_name_return);

extern Status XGetIconSizes(Display *display,
                Window w,
                XIconSize **size_list_sreturn,
                int *count_sreturn);

extern XImage *XGetImage(Display *display,
                  Drawable d,
                  int x,int y,
                  unsigned int width,int height,
                  long plane_mask,
                  int format);

extern XGetInputFocus(Display *display,
                Window *focus_sreturn,
                int *revert_to_sreturn);

extern XGetKeyboardControl(Display *display,
                XKeyboardState *values_return);

extern KeySym *XGetKeyboardMapping(Display *display,
                KeyCode first_keycode,
                int keycode_count,
                int *keysyms_per_keycode_sreturn);

extern XModifierKeymap *XGetModifierMapping(Display *display);

extern XTimeCoord *XGetMotionEvents(Display *display,
                Window w,
                Time start,Time stop,
                int *nevents_sreturn);

extern Status XGetNormalHints(Display *display,
                Window w,
                XSizeHints *hints_return);

extern unsigned long "XGetPixel"(XImage *ximage,
                int x,
                int y);

extern XGetPointerControl(Display *display,
                int *accel_numerator_sreturn, int *accel_denominator_sreturn,
                int *threshold_sreturn);

extern int XGetPointerMapping(Display *display,
                unsigned char map_return[],
                int nmap);

extern XGetScreenSaver(Display *display,
                int *timeout_sreturn, int *interval_sreturn,
                int *prefer_blanking_sreturn,
                int *allow_exposures_sreturn);

extern Window XGetSelectionOwner(Display *display,
                Atom selection);


extern Status XGetSizeHints(Display *display,
                Window w,
                XSizeHints *hints_return,
                Atom property);

extern Status XGetStandardColormap(Display *display,
                Window w,
                XStandardColormap *colormap_return,
                Atom property);

extern XImage *XGetSubImage(Display *display,
                Drawable d,
                int x,int y,
                unsigned int width,unsigned int height,
                unsigned long plane_mask,
                int format,
                XImage *dest_image,
                int dest_x,int dest_y);

extern Status XGetTransientForHint(Display *display,
                Window w,
                Window *prop_window_sreturn);

extern XVisualInfo *XGetVisualInfo(Display *display,
                long vinfo_mask,
                XVisualInfo *vinfo_template,
                int *nitems_sreturn);

extern Status XGetWindowAttributes(Display *display,
                Window w,
                XWindowAttributes *window_attributes_return);

extern int XGetWindowProperty(Display *display,
                Window w,
                Atom property,
                long long_offset,long long_length,
                Bool delete,
                Atom req_type,
                Atom *actual_type_sreturn,
                int *actual_format_sreturn,
                unsigned long *nitems_sreturn,
                unsigned long *bytes_after_sreturn,
                unsigned char **prop_sreturn);


extern XWMHints *XGetWMHints(Display *display,
                Window w);

extern Status XGetZoomHints(Display *display,
                Window w,
                XSizeHints *zhints_return);

extern XGrabButton(Display *display,
                unsigned int button,
                unsigned int modifiers,
                Window grab_window,
                Bool owner_events,
                unsigned int event_mask,
                int pointer_mode,int keyboard_mode,
                Window confine_to,
                Cursor cursor);

extern XGrabKey(Display *display,
                int keycode,
                unsigned int modifiers,
                Window grab_window,
                Bool owner_events,
                int pointer_mode,int keyboard_mode);

extern int XGrabKeyboard(Display *display,
                Window grab_window,
                Bool owner_events,
                int pointer_mode,int keyboard_mode,
                Time time);

extern int XGrabPointer(Display *display,
                Window grab_window,
                Bool owner_events,
                unsigned int event_mask,
                int pointer_mode,int keyboard_mode,
                Window confine_to,
                Cursor cursor,
                Time time);

extern XGrabServer(Display *display);

extern XIfEvent(Display *display,
                XEvent *event_return,
                Bool (*predicate)(),
		char *arg);

extern XModifierKeymap *XInsertModifiermapEntry(XModifierKeymap *modmap,
               KeyCode keycode_entry,
               int modifier);

extern XInstallColormap(Display *display,
                Colormap colormap);

extern Atom XInternAtom(Display *display,
                char *atom_name,
                Bool only_if_exists);

extern XIntersectRegion(Region sra,Region srb,Region dr_return);

extern KeySym XKeycodeToKeysym(Display *display,
                KeyCode keycode,
                int index);

extern KeyCode XKeysymToKeycode(Display *display,
                KeySym keysym);

extern char *XKeysymToString(KeySym keysym);

extern XKillClient(Display *display,
                XID resource);

extern char **XListExtensions(Display *display,
int *nextensions_sreturn);

extern char **XListFonts(Display *display,
                char *pattern,
                int maxnames,
                int *actual_count_sreturn);

extern char **XListFontsWithInfo(Display *display,
                char *pattern,
                int maxnames,
                int *count_sreturn,
                XFontStruct **info_sreturn);

extern XHostAddress *XListHosts(Display *display,
                int *nhosts_sreturn,
                Bool *state_sreturn);


extern Colormap *XListInstalledColormaps(Display *display,
                Window w,
                int *num_sreturn);

extern Atom *XListProperties(Display *display,
                Window w,
                int *num_prop_sreturn);

extern Font XLoadFont(Display *display,
                char *name);

extern XFontStruct *XLoadQueryFont(Display *display,
                char *name);

extern Status XLookupColor(Display *display,
                Colormap colormap,
                char *color_name,
                XColor *exact_def_return,XColor *screen_def_return);

extern KeySym XLookupKeysym(XKeyEvent *key_event,
                int index);

extern int XLookupString(XKeyEvent *event_struct,
                char *buffer_return,
                int bytes_buffer,
                KeySym *keysym_sreturn,
                XComposeStatus *status_in_out);

extern XLowerWindow(Display *display,
                Window w);


extern XMapRaised(Display *display,
                Window w);

extern XMapSubwindows(Display *display,
                Window w);

extern XMapWindow(Display *display,
                Window w);

extern XMaskEvent(Display *display,
                long event_mask,
                XEvent *event_return);

extern Status XMatchVisualInfo(Display *display,
                int screen,
                int depth,
                int class,
                XVisualInfo *vinfo_return);

extern XMoveResizeWindow(Display *display,
                Window w,
                int x,int y,
                unsigned int width,int height);

extern XMoveWindow(Display *display,
                Window w,
                int x,int y);

extern XModifierKeymap *XNewModifiermap(int max_keys_per_mod);


extern XNextEvent(Display *display,
                XEvent *event_return);

extern XNoOp(Display *display);

extern XOffsetRegion(Region r,
                int dx,int dy);

extern Display *XOpenDisplay(char *display_name);

extern Status XParseColor(Display *display,
                  Colormap colormap,
                  char *spec,
                  XColor *exact_def_return);

extern int XParseGeometry(char *parsestring,
                int *x_sreturn,int *y_sreturn,
                int *width_sreturn, int *height_sreturn);

extern XPeekEvent(Display *display,
                XEvent *event_return);

extern XPeekIfEvent(Display *display,
                XEvent *event_return,
                Bool (*predicate)(),
		char *arg);

extern int XPending(Display *display);

extern char *Xpermalloc(unsigned int size);

extern Bool XPointInRegion(Region r,
                int x,int y);


extern Region XPolygonRegion(XPoint points[],
                int n,
                int fill_rule);


extern XPutBackEvent(Display *display,
                XEvent *event);


extern XPutImage(Display *display,
                  Drawable d,
                  GC gc,
                  XImage *image,
                  int src_x,int src_y,
                  int dest_x, int dest_y,
                  unsigned int width, int height);

extern int "XPutPixel"(XImage *ximage,
                int x,
                int y,
                unsigned long pixel);

extern Status XQueryBestCursor(Display *display,
                Drawable d,
                unsigned int width,int height,
                unsigned int *width_sreturn, int *height_sreturn);

extern Status XQueryBestSize(Display *display,
                int class,
                Drawable which_screen,
                unsigned int width,int height,
                unsigned int *width_sreturn,int *height_sreturn);

extern Status XQueryBestStipple(Display *display,
                Drawable which_screen,
                unsigned int width,int height,
                unsigned int *width_sreturn,int *height_sreturn);

extern Status XQueryBestTile(Display *display,
                Drawable which_screen,
                unsigned int width,int height,
                unsigned int *width_sreturn,int *height_sreturn);

extern XQueryColor(Display *display,
                Colormap colormap,
                XColor *def_in_out);

extern XQueryColors(Display *display,
                Colormap colormap,
                XColor defs_in_out[],
                int ncolors);

extern Bool XQueryExtension(Display *display,
char *name,
int *major_opcode_sreturn,
int *first_event_sreturn,
int *first_error_sreturn);

extern XFontStruct *XQueryFont(Display *display,
                XID font_ID);

extern XQueryKeymap(Display *display,
                char keys_return[32]);

extern Bool XQueryPointer(Display *display,
                Window w,
                Window *root_sreturn,Window *child_sreturn,
                int *root_x_sreturn, int *root_y_sreturn,
                int *win_x_sreturn, int *win_y_sreturn,
                unsigned int *mask_sreturn);

extern XQueryTextExtents(Display *display,
                XID font_ID,
                char *string,
                int nchars,
                int *direction_sreturn,
		int *font_ascent_sreturn,int *font_descent_sreturn,
                XCharStruct *overall_return);

extern XQueryTextExtents16(Display *display,
                XID font_ID,
                XChar2b *string,
                int nchars,
                int *direction_sreturn,
                int *font_ascent_sreturn,int *font_descent_sreturn,
                XCharStruct *overall_return);

extern Status XQueryTree(Display *display,
                Window w,
                Window *root_sreturn,
                Window *parent_sreturn,
                Window **children_return,
                unsigned int *nchildren_sreturn);

extern XRaiseWindow(Display *display,
                Window w);

extern int XReadBitmapFile(Display *display,
                Drawable d,
                char *filename,
                unsigned int *width_sreturn,unsigned int *height_sreturn,
                Pixmap *bitmap_sreturn,
                int *x_hot_sreturn,int *y_hot_sreturn);

extern XRebindKeysym(Display *display,
                KeySym keysym,
                KeySym list[],
                int mod_count,
                unsigned char *string,
                int bytes_string);

extern XRecolorCursor(Display *display,
                Cursor cursor,
                XColor *foreground_color,XColor *background_color);

extern int XRectInRegion(Region r,
                int x,int y,
                unsigned int width,int height);

extern XRefreshKeyboardMapping(XMappingEvent *event_map);

extern XRemoveFromSaveSet(Display *display,
                Window w);

extern XRemoveHost(Display *display,
                XHostAddress *host);

extern XRemoveHosts(Display *display,
                XHostAddress *hosts,
                int num_hosts);

extern XReparentWindow(Display *display,
                Window w,
                Window parent,
                int x,int y);

extern XResetScreenSaver(Display *display);

extern XResizeWindow(Display *display,
                Window w,
                unsigned int width,int height);

extern char *XResourceManagerString(Display *display);

extern XRestackWindows(Display *display,
                Window windows[],
                int nwindows);

extern Bool XrmGetResource(XrmDatabase database,
               char *str_name,
               char *str_class,
               char **str_type_sreturn,
               XrmValue *value_return);

extern Bool XrmQGetResource(XrmDatabase database,
               XrmNameList quark_name,
               XrmClassList quark_class,
               XrmRepresentation *quark_type_return,
               XrmValue *value_return);

extern Bool XrmQGetSearchList(XrmDatabase database,
               XrmNameList names,
               XrmClassList classes,
               XrmSearchList list_return,
               int list_length);

extern Bool XrmQGetSearchResource(XrmSearchList list,
               XrmName name,
               XrmClass class,
               XrmRepresentation *type_sreturn,
               XrmValue *value_return);


extern void XrmInitialize();

extern void XrmParseCommand(XrmDatabase *database,
                XrmOptionDescList table,
                int table_count,
                char *name,
                int *argc_in_out,
                char **argv_in_out);

extern void XrmMergeDatabases(XrmDatabase source_db,XrmDatabase *target_db);

extern XrmDatabase XrmGetFileDatabase(char *filename);

extern void XrmPutFileDatabase(XrmDatabase database,
               char *stored_db);

extern XrmDatabase XrmGetStringDatabase(char *data);

extern void XrmPutResource(XrmDatabase *database,
               char *specifier,
               char *type,
               XrmValue *value);

extern void XrmQPutResource(XrmDatabase *database,
               XrmBindingList bindings,
               XrmQuarkList quarks,
               XrmRepresentation type,
               XrmValue *value);

extern void XrmPutStringResource(XrmDatabase *database,
               char *specifier,
               char *value);

extern void XrmQPutStringResource(XrmDatabase *database,
               XrmBindingList bindings,
               XrmQuarkList quarks,
               char *value);

extern void XrmPutLineResource(XrmDatabase *database,
               char *line);

extern XrmQuark XrmUniqueQuark();

extern XrmQuark "XrmStringToName"(char *string);

extern XrmQuark "XrmStringToClass"(char *string);


extern XrmQuark "XrmStringToRepresentation"(char *string);

extern XrmQuark XrmStringToQuark(char *string);

extern char *"XrmNameToString"(XrmQuark quark);
	  
extern char *"XrmClassToString"(XrmQuark quark);

extern char *"XrmRepresentationToString"(XrmQuark quark);

extern char *XrmQuarkToString(XrmQuark quark);

extern void XrmStringToQuarkList(char *string,
               XrmQuarkList quarks_return);

extern XrmStringToBindingQuarkList(char *string,
               XrmBindingList bindings_return,
               XrmQuarkList quarks_return);

extern XRotateBuffers(Display *display,
                int rotate);

extern XRotateWindowProperties(Display *display,
                Window w,
                Atom properties[],
                int num_prop,
                int npositions);

extern int XSaveContext(Display *display,
                Window w,
                XContext context,
                caddr_t data);

extern XSelectInput(Display *display,
                Window w,
                long event_mask);

extern Status XSendEvent(Display *display,
                Window w,
                Bool propagate,
                long event_mask,
                XEvent *event_send);

extern XSetAccessControl(Display *display,
                int mode);

extern int (*XSetAfterFunction(Display *display, int (*procedure)()))();

extern XSetArcMode(Display *display,
                GC gc,
                int arc_mode);

extern XSetBackground(Display *display,
                GC gc,
                unsigned long background);

extern XSetClassHint(Display *display,
                Window w,
                XClassHint *class_hints);

extern XSetClipMask(Display *display,
                GC gc,
                Pixmap pixmap);

extern XSetClipOrigin(Display *display,
                GC gc,
                int clip_x_origin,int clip_y_origin);


extern XSetClipRectangles(Display *display,
                GC gc,
                int clip_x_origin,int clip_y_origin,
                XRectangle rectangles[],
                int n,
                int ordering);

extern XSetCloseDownMode(Display *display,
                int close_mode);

extern XSetCommand(Display *display,
                Window w,
                char **argv,
                int argc);

extern XSetDashes(Display *display,
                  GC gc,
                  int dash_offset,
                  char dash_list[],
                  int n);


extern XSetErrorHandler(int (*handler)());

extern XSetFillRule(Display *display,
                GC gc,
                int fill_rule);

extern XSetFillStyle(Display *display,
                GC gc,
                int fill_style);

extern XSetFont(Display *display,
                GC gc,
                Font font);

extern XSetFontPath(Display *display,
                char **directories,
                int ndirs);

extern XSetForeground(Display *display,
                GC gc,
                unsigned long foreground);

extern XSetFunction(Display *display,
                GC gc,
                int function);

extern XSetGraphicsExposures(Display *display,
GC gc,
Bool graphics_exposures);

extern XSetIconName(Display *display,
                Window w,
                char *icon_name);

extern XSetIconSizes(Display *display,
                Window w,
                XIconSize *size_list,
                int count);

extern XSetInputFocus(Display *display,
                Window focus,
                int revert_to,
                Time time);


extern XSetIOErrorHandler(int (*handler)());

extern XSetLineAttributes(Display *display,
                GC gc,
                unsigned int line_width,
                int line_style,
                int cap_style,
                int join_style);

extern int XSetModifierMapping(Display *display,
                  XModifierKeymap *modmap);

extern XSetNormalHints(Display *display,
                Window w,
                XSizeHints *hints);

extern XSetPlaneMask(Display *display,
                GC gc,
                unsigned long plane_mask);

extern int XSetPointerMapping(Display *display,
                unsigned char map[],
                int nmap);

extern XSetRegion(Display *display,
                GC gc,
                Region r);

extern XSetScreenSaver(Display *display,
                int timeout,int interval,
                int prefer_blanking,
                int allow_exposures);

extern XSetSelectionOwner(Display *display,
                Atom selection,
                Window owner,
                Time time);

extern XSetSizeHints(Display *display,
                Window w,
                XSizeHints *hints,
                Atom property);

extern XSetStandardColormap(Display *display,
                Window w,
                XStandardColormap *colormap,
                Atom property);

extern XSetStandardProperties(Display *display,
                Window w,
                char *window_name,
                char *icon_name,
                Pixmap icon_pixmap,
                char **argv,
                int argc,
                XSizeHints *hints);

extern XSetState(Display *display,
                GC gc,
                unsigned long foreground,long background,
                int function,
                unsigned long plane_mask);

extern XSetStipple(Display *display,
                GC gc,
                Pixmap stipple);

extern XSetSubwindowMode(Display *display,
GC gc,
int subwindow_mode);


extern XSetTile(Display *display,
                GC gc,
                Pixmap tile);

extern XSetTransientForHint(Display *display,
                Window w,
                Window prop_window);

extern XSetTSOrigin(Display *display,
                GC gc,
                int ts_x_origin,int ts_y_origin);


extern XSetWindowBackground(Display *display,
                Window w,
                unsigned long background_pixel);

extern XSetWindowBackgroundPixmap(Display *display,
                Window w,
                Pixmap background_pixmap);

extern XSetWindowBorder(Display *display,
                Window w,
                unsigned long border_pixel);

extern XSetWindowBorderPixmap(Display *display,
                Window w,
                Pixmap border_pixmap);

extern XSetWindowBorderWidth(Display *display,
                Window w,
                unsigned int width);

extern XSetWindowColormap(Display *display,
                Window w,
                Colormap colormap);

extern XSetWMHints(Display *display,
                Window w,
                XWMHints *wmhints);

extern XSetZoomHints(Display *display,
                Window w,
                XSizeHints *zhints);

extern XShrinkRegion(Region r,
                int dx,int dy);

extern XStoreBuffer(Display *display,
                char *bytes,
                int nbytes,
                int buffer);

extern XStoreBytes(Display *display,
                char *bytes,
                int nbytes);

extern XStoreColor(Display *display,
                Colormap colormap,
                XColor *color);

extern XStoreColors(Display *display,
                Colormap colormap,
                XColor color[],
                int ncolors);

extern XStoreName(Display *display,
                Window w,
                char *window_name);

extern XStoreNamedColor(Display *display,
                Colormap colormap,
                char *color,
                unsigned long pixel,
                int flags);

extern KeySym XStringToKeysym(char *string);

extern XImage *"XSubImage"(XImage *ximage,
                int x,
                int y,
                unsigned int subimage_width,
                unsigned int subimage_height);

extern XSubtractRegion(Region sra,Region srb,Region dr_return);

extern XSync(Display *display,
                Bool discard);

extern int (*XSynchronize(Display *display,
                Bool onoff))();

extern XTextExtents(XFontStruct *font_struct,
                char *string,
                int nchars,
                int *direction_sreturn,
                int *font_ascent_sreturn,int *font_descent_sreturn,
                XCharStruct *overall_return);

extern XTextExtents16(XFontStruct *font_struct,
                XChar2b *string,
                int nchars,
                int *direction_sreturn,
                int *font_ascent_sreturn, int *font_descent_sreturn,
                XCharStruct *overall_return);

extern int XTextWidth(XFontStruct *font_struct,
                char *string,
                int count);

extern int XTextWidth16(XFontStruct *font_struct,
                XChar2b *string,
                int count);

extern Bool XTranslateCoordinates(Display *display,
                Window src_w,Window dest_w,
                int src_x,int src_y,
                int *dest_x_sreturn,int *dest_y_sreturn,
                Window *child_sreturn);


extern XUndefineCursor(Display *display,
                Window w);

extern XUngrabButton(Display *display,
                unsigned int button,
                unsigned int modifiers,
                Window grab_window);

extern XUngrabKey(Display *display,
                int keycode,
                unsigned int modifiers,
                Window grab_window);

extern XUngrabKeyboard(Display *display,
                Time time);

extern XUngrabPointer(Display *display,
                Time time);

extern XUngrabServer(Display *display);

extern XUninstallColormap(Display *display,
                Colormap colormap);


extern XUnionRectWithRegion(XRectangle *rectangle,
               Region src_region,
               Region dest_region_return);

extern XUnionRegion(Region sra,Region srb,Region dr_return);


extern XContext "XUniqueContext"();

extern XUnloadFont(Display *display,
                Font font);

extern XUnmapSubwindows(Display *display,
                Window w);

extern XUnmapWindow(Display *display,
                Window w);

extern VisualID "XVisualIDFromVisual"(Visual *visual);

extern XWarpPointer(Display *display,
                  Window src_w,Window dest_w,
                  int src_x, int src_y,
                  unsigned int src_width,unsigned int src_height,
                  int dest_x,int dest_y);

extern XWindowEvent(Display *display,
                Window w,
                long event_mask,
                XEvent *event_return);

extern int XWriteBitmapFile(Display *display,
                char *filename,
                Pixmap bitmap,
                unsigned int width,int height,
                int x_hot,int y_hot);

extern XXorRegion(Region sra,Region srb,Region dr_return);

