/*
 * c_wxextend.h
 * This is a modified version of wxextend.h (from wxClips 1.46) which
 * only uses ANSI C syntax so that ALS's c2pro can correctly process it.
 *
 * Changes made:
 * - Removed all C++ classes.
 * - Removed extra prototypes for overloaded functions.
 * - Removed default argument values from prototypes.
 * - Added "struct" keywords where needed.
 *
 * Note: the Macintosh version of wxExtend is slightly different - some
 * functions have fewer arguments. To work around this problem, Mac-only
 * wrapper functions are defined in wxwinaux.c.
 */

#ifndef c_wxextendh
#define c_wxextendh

#include "common.h"

// Implementation-specific extension function, to implement
// call-back methods.
typedef void (*wxExtensionFunction) (void);

// An identifier for a wxWindows object.
typedef long WXID;

// Must be defined by user of library
extern void ExtensionErrorFunction(char *s);

// Must be called by specific language binding to implement
// callbacks. Register the most specific event handler LAST,
// i.e. register a wxWindow OnPaint handler first, then
// the wxCanvas OnPaint handler.
void wxRegisterExtensionCallback(WXTYPE objectType, char *eventType,
  wxExtensionFunction cFunction);

// Find a callback based on object type and event. Finds the
// most specific handler that's applicable to this event/object
// (last to be registered for this event)
wxExtensionFunction wxFindExtensionCallback(WXTYPE objectType, char *eventType);

// Initialize Extension library
void wxInitExtension(void);

// Cleanup Extension library
void wxCleanupExtension(void);

// Utility functions
wxObject *wxGetTypedObject(long objectId, WXTYPE expectedType);
void wxStoreThingInTable(long item_id, wxObject *object, WXTYPE type);
void wxDeleteThingInTable(long item_id);
wxObject *wxGetThingFromTable(long item_id, WXTYPE *type);
long wxExtensionParseBitList(char *bitListString);
//void wxCleanWindows(void);
Bool wxAssociateUserData(WXID id, void* dataPtr);
void* wxGetUserData(WXID id);

 /*
  * I made these public so that the names and values can be inserted
  * into the extension language's namespace... (Are there other pertinent
  * itentifiers that are missing?)
  */
struct wxExtensionBitListStruct
{
  char *word;
  long bits;
};

extern struct wxExtensionBitListStruct wxExtensionBitListTable[];
extern struct wxExtensionBitListStruct wxExtensionCursorTable[];
extern struct wxExtensionBitListStruct wxExtensionKeyCodeTable[];
extern int wxExtensionBitListCount;
extern int wxExtensionCursorCount;
extern int wxExtensionKeyCodeCount;

// Key code int<->string conversion
char *wxExtensionKeyCodeIntToString(int keyCode);
int wxExtensionKeyCodeStringToInt(char *keyCode);

/*
 * Command prototypes
 *
 */

char *wxcGetPlatform(void);
#ifndef wx_mac
char *wxcGetOsVersion(void);
#endif
void wxcCleanWindows(void);

// Window
Bool wxcWindowDelete(WXID id);
Bool wxcWindowShow(WXID id, Bool show);
long wxcWindowGetParent(WXID id);
int wxcWindowGetWidth(WXID id);
int wxcWindowGetHeight(WXID id);
int wxcWindowGetClientWidth(WXID id);
int wxcWindowGetClientHeight(WXID id);
int wxcWindowGetX(WXID id);
int wxcWindowGetY(WXID id);
Bool wxcWindowSetSize(WXID id, int x, int y, int w, int h);
Bool wxcWindowSetClientSize(WXID id, int w, int h);
Bool wxcWindowSetFocus(WXID id);
Bool wxcWindowSetCursor(WXID id, WXID cursorId);
Bool wxcWindowFit(WXID id);
Bool wxcWindowCentre(WXID id, int which);
Bool wxcWindowCentre(WXID id, char *which);
Bool wxcWindowMakeModal(WXID id, Bool modal);
long wxcWindowGetSize(WXID id, int *w, int *h);
long wxcWindowGetClientSize(WXID id, int *w, int *h);
long wxcWindowGetPosition(WXID id, int *x, int *y);
long wxcWindowEnable(WXID id, Bool enable);
float wxcWindowGetCharWidth(WXID id);
float wxcWindowGetCharHeight(WXID id);
long wxcWindowGetTextExtent(WXID id, char* text, float* width, float* height);
#ifndef wx_mac
char *wxcWindowGetName(WXID id);
#endif

// Popup Menu Window
Bool wxcWindowPopupMenu(WXID id, WXID menuId, float x, float y);

// Panel item
Bool wxcPanelItemSetDefault(WXID id);
Bool wxcPanelItemSetLabel(WXID id, char *label);
char *wxcPanelItemGetLabel(WXID id);
WXID wxcPanelCreate(WXID parentId, int x, int y, int width, int height, long style, char *name);
Bool wxcPanelSetLabelPosition(WXID id, int position);
Bool wxcPanelSetLabelPosition(WXID id, char *position);
Bool wxcPanelSetLabelFont(WXID id, WXID fontId);
Bool wxcPanelSetButtonFont(WXID id, WXID fontId);
Bool wxcPanelNewLine(WXID id);
Bool wxcPanelOnPaint(WXID id);
Bool wxcPanelOnEvent(WXID id, WXID eventId);
long wxcPanelGetHorizontalSpacing(WXID id);
long wxcPanelGetVerticalSpacing(WXID id);
Bool wxcPanelSetHorizontalSpacing(WXID id, long spacing);
Bool wxcPanelSetVerticalSpacing(WXID id, long spacing);

// Dialog box
WXID wxcDialogBoxCreate(WXID parentId, char *title, Bool modal,
  int x, int y, int width, int height, long style,
  char *name);

// Canvas
WXID wxcCanvasCreate(WXID parentId, int x, int y, int width, int height, long style,
  char *name);
Bool wxcCanvasSetScrollbars(WXID id, int horiz, int vert,
  int x_length, int y_length, int x_page, int y_page, int x_pos, int y_pos);
Bool wxcCanvasScroll(WXID id, int x_pos, int y_pos);
WXID wxcCanvasGetDC(WXID id);
Bool wxcCanvasOnEvent(WXID id, WXID eventId);
Bool wxcCanvasOnChar(WXID id, WXID eventId);

// Text window
WXID wxcTextWindowCreate(WXID parentId, int x, int y, int width, int height,
  long style, char *name);
Bool wxcTextWindowLoadFile(WXID id, char *filename);
Bool wxcTextWindowSaveFile(WXID id, char *filename);
Bool wxcTextWindowWrite(WXID id, long val);
Bool wxcTextWindowWrite(WXID id, char *val);
Bool wxcTextWindowWrite(WXID id, double val);
Bool wxcTextWindowModified(WXID id);
Bool wxcTextWindowClear(WXID id);
Bool wxcTextWindowDiscardEdits(WXID id);
Bool wxcTextWindowCopy(WXID id);
Bool wxcTextWindowCut(WXID id);
Bool wxcTextWindowPaste(WXID id);
char* wxcTextWindowGetContents(WXID id);
Bool wxcTextWindowReplace(WXID id, long from, long to, char *text);
Bool wxcTextWindowRemove(WXID id, long from, long to);
Bool wxcTextWindowSetEditable(WXID id, Bool editable);
Bool wxcTextWindowSetFont(WXID id, WXID fontId);
Bool wxcTextWindowSetSelection(WXID id, long start, long end);
Bool wxcTextWindowOnChar(WXID id, WXID eventId);
long wxcTextWindowGetLastPosition(WXID id);
Bool wxcTextWindowShowPosition(WXID id, long position);
long wxcTextWindowXYToPosition(WXID id, long x, long y);
Bool wxcTextWindowPositionToXY(WXID id, long position, long *x, long *y);

// Generic device context functionality
Bool wxcDCOk(WXID id);
Bool wxcDCStartDoc(WXID id, char *message);
Bool wxcDCEndDoc(WXID id);
Bool wxcDCStartPage(WXID id);
Bool wxcDCEndPage(WXID id);
Bool wxcDCSetPen(WXID id, long penId);
Bool wxcDCSetBackground(WXID id, WXID brushId);
Bool wxcDCSetBrush(WXID id, long brushId);
Bool wxcDCSetMapMode(WXID id, int mode);
Bool wxcDCSetMapMode(WXID id, char *mode);
Bool wxcDCSetFont(WXID id, long fontId);
Bool wxcDCSetTextForeground(WXID id, char *colour);
Bool wxcDCSetTextBackground(WXID id, char *colour);
Bool wxcDCSetLogicalFunction(WXID id, int op);
// String version
Bool wxcDCSetLogicalFunction(WXID id, char *op);
Bool wxcDCSetColourMap(WXID id, WXID cmapId);
Bool wxcDCBlit(WXID id, double xdest, double ydest, double width, double height, WXID sourceId,
  double xsrc, double ysrc, int op);
// String version
Bool wxcDCBlit(WXID id, double xdest, double ydest, double width, double height, long sourceId,
  double xsrc, double ysrc, char *op);
Bool wxcDCDrawIcon(WXID id, WXID icon_id, float x, float y);
Bool wxcDCDrawLine(WXID id, double x1, double y1, double x2, double y2);
Bool wxcDCDrawPoint(WXID id, double x1, double y1);
Bool wxcDCDrawRectangle(WXID id, double x1, double y1, double x2, double y2);
Bool wxcDCDrawRoundedRectangle(WXID id, double x1, double y1, double x2, double y2,
  double radius);
Bool wxcDCDrawEllipse(WXID id, double x, double y, double width, double height);
Bool wxcDCDrawText(WXID id, char *text, double x1, double y1);
Bool wxcDCSetClippingRegion(WXID id, double x1, double y1, double x2, double y2);
Bool wxcDCDestroyClippingRegion(WXID id);
Bool wxcDCClear(WXID id);
double wxcDCGetWidth(WXID id);
double wxcDCGetHeight(WXID id);
double wxcDCGetMinX(WXID id);
double wxcDCGetMinY(WXID id);
double wxcDCGetMaxX(WXID id);
double wxcDCGetMaxY(WXID id);
Bool wxcDCSetUserScale(WXID id, double x, double y);
#ifndef wx_mac
Bool wxcDCSetDeviceOrigin(WXID id, double x, double y);
Bool wxcDCSetLogicalOrigin(WXID id, double x, double y);
#endif
Bool wxcDCDelete(WXID id);
long wxcDCGetSize(WXID id, float *w, float *h);
long wxcDCDrawArc(long id, float x1, float y1, float x2, float y2,
             float xc, float yc);
long wxcDCGetTextExtent(long id, char* text, float* width, float* height);
long wxcDCBeginDrawing(WXID id);
long wxcDCEndDrawing(WXID id);

// PostScript DC
WXID wxcPostScriptDCCreate(char *file, Bool interactive, WXID parentId);

#ifdef wx_msw
// Windows Printer DC
WXID wxcPrinterDCCreate(char *driver, char *device, char *file, Bool interactive);

// Windows metafile DC
WXID wxcMetaFileDCCreate(char *filename);
WXID wxcMetaFileDCClose(WXID id);

// Windows metafile
Bool wxcMetaFileDelete(WXID id);
Bool wxcMetaFileSetClipboard(WXID id, double width, double height);
Bool wxcMakeMetaFilePlaceable(char *filename, int minX, int minY, int maxX, int maxY, double scale);
#endif

// Memory DC
WXID wxcMemoryDCCreate(void);
Bool wxcMemoryDCSelectObject(WXID id, long bitmapId);

#ifndef wx_mac
// Application object: returns a pointer to the current wxApp
WXID wxcAppCreate(void);
#endif

// Pen
WXID wxcPenCreate(char *colour, int width, int style);
Bool wxcPenDelete(WXID id);

// Brush
WXID wxcBrushCreate(char *colour, int style);
Bool wxcBrushDelete(WXID id);

// Font
WXID wxcFontCreate(int pointSize, int family, int style, int weight,
  Bool underlined);
Bool wxcFontDelete(WXID id);

// Bitmap
WXID wxcBitmapCreate(int width, int height, int depth);
Bool wxcBitmapDelete(WXID id);
#ifndef wx_mac
WXID wxcBitmapLoadFromFile(char *file, long bitmapType);
#endif

int wxcBitmapGetWidth(WXID id);
int wxcBitmapGetHeight(WXID id);
#ifndef wx_mac
int wxcBitmapGetDepth(WXID id);
#endif
WXID wxcBitmapGetColourMap(WXID id);

// Cursor
WXID wxcCursorCreate(int cursorType);
#ifndef wx_mac
WXID wxcCursorLoadFromFile(char *file, char *bitmapType, int x, int y);
#endif
Bool wxcCursorDelete(WXID id);

// Icon
WXID wxcIconCreate(char *file);
WXID wxcIconLoadFromFile(char *file, char *bitmapType);
Bool wxcIconDelete(WXID id);
int wxcIconGetWidth(WXID id);
int wxcIconGetHeight(WXID id);

// Event
long wxcEventGetEventType(WXID id);
long wxcEventGetEventClass(WXID id);

// Key event
Bool wxcKeyEventControlDown(WXID id);
Bool wxcKeyEventShiftDown(WXID id);
Bool wxcKeyEventAltDown(WXID id);
long wxcKeyEventKeyCode(WXID id);
#ifndef wx_mac
char *wxcKeyEventKeyCodeString(WXID id);
#endif
Bool wxcKeyEventPosition(WXID id, double *x, double *y);
 
// Command event
char* wxcCommandEventGetClientData(WXID id);
long wxcCommandEventGetSelection(WXID id);
char* wxcCommandEventGetString(WXID id);
Bool wxcCommandEventChecked(WXID id);
Bool wxcCommandEventIsSelection(WXID id);

// Mouse event
Bool wxcMouseEventIsButton(WXID id);
Bool wxcMouseEventButtonDown(WXID id);
Bool wxcMouseEventControlDown(WXID id);
Bool wxcMouseEventShiftDown(WXID id);
Bool wxcMouseEventButton(WXID id, int butt);
Bool wxcMouseEventLeftDown(WXID id);
Bool wxcMouseEventMiddleDown(WXID id);
Bool wxcMouseEventRightDown(WXID id);
Bool wxcMouseEventLeftUp(WXID id);
Bool wxcMouseEventMiddleUp(WXID id);
Bool wxcMouseEventRightUp(WXID id);
Bool wxcMouseEventDragging(WXID id);
double wxcMouseEventPositionX(WXID id);
double wxcMouseEventPositionY(WXID id);
long wxcMouseEventPosition(long id, double *x, double *y);

// Button
WXID wxcButtonCreate(WXID parentId, char *label,
  int x, int y, int width, int height, long style, char *name);
WXID wxcButtonCreateFromBitmap(WXID parentId, WXID bitmapId,
  int x, int y, int width, int height, long style, char *name);
Bool wxcButtonSetLabel(WXID id, char *label);

// Text control
WXID wxcTextCreate(WXID parentId, char *label, char *value,
  int x, int y, int width, int height, long style, char *name);
Bool wxcTextSetValue(WXID id, char *val);
char *wxcTextGetValue(WXID id);

// Multi-line text control
WXID wxcMultiTextCreate(WXID parentId, char *label, char *value,
  int x, int y, int width, int height, long style, char *name);
Bool wxcMultiTextSetValue(WXID id, char *val);
char *wxcMultiTextGetValue(WXID id);

// Checkbox
WXID wxcCheckBoxCreate(WXID parentId, char *label,
  int x, int y, int width, int height, long style, char *name);
Bool wxcCheckBoxSetValue(WXID id, Bool val);
int wxcCheckBoxGetValue(WXID id);

// Slider
WXID wxcSliderCreate(WXID parentId, char *label,
  int value, int min_value, int max_value, int width, int x, int y,
  long style, char *name);
Bool wxcSliderSetValue(WXID id, int val);
int wxcSliderGetValue(WXID id);

// Message (static text)
WXID wxcMessageCreate(WXID parentId, char *label, int x, int y,
  long style, char *name);
WXID wxcMessageCreateFromBitmap(WXID parentId, WXID bitmapId, int x, int y,
  long style, char *name);

// Listbox
WXID wxcListBoxCreate(WXID parentId, char *label, Bool multiple,
  int x, int y, int width, int height, long style, char *name);
Bool wxcListBoxAppend(WXID id, char *item_string, char *client_data);
int wxcListBoxFindString(WXID id, char *item_string);
Bool wxcListBoxClear(WXID id);
Bool wxcListBoxSetSelection(WXID id, int sel);
Bool wxcListBoxDeselect(WXID id, int sel);
int wxcListBoxGetSelection(WXID id);
char *wxcListBoxGetStringSelection(WXID id);
Bool wxcListBoxSetStringSelection(WXID id, char *sel);
int wxcListBoxNumber(WXID id);
Bool wxcListBoxDelete(WXID id, int n);
char *wxcListBoxGetClientData(WXID id, int sel);
char *wxcListBoxGetString(WXID id, int sel);
int wxcListBoxGetFirstSelection(WXID id);
int wxcListBoxGetNextSelection(void);
long wxcListBoxSetFirstItem(WXID id, long sel);

// Choice (non-editable combobox)
// Implements the choice creation independent of ClipsExpr or CLIPS
WXID wxcChoiceCreate(WXID parentId, char *label, int x, int y,
  int width, int height, int n, char **choices, long style, char *name);
Bool wxcChoiceAppend(WXID id, char *item_string);
int wxcChoiceFindString(WXID id, char *item_string);
Bool wxcChoiceClear(WXID id);
Bool wxcChoiceSetSelection(WXID id, int sel);
int wxcChoiceGetSelection(WXID id);
char *wxcChoiceGetStringSelection(WXID id);
Bool wxcChoiceSetStringSelection(WXID id, char *sel);
char *wxcChoiceGetString(WXID id, int sel);

// RadioBox
WXID wxcRadioBoxCreate(WXID parentId, char *label, int x, int y,
                       int width, int height, int n, char **choices,
                       int majorDim, long style, char *name);
long wxcRadioBoxSetSelection(WXID id, long sel);
long wxcRadioBoxGetSelection(WXID id);
 
// Gauge
#if USE_GAUGE
WXID wxcGaugeCreate(WXID parentId, char *label, int range,
  int x, int y, int width, int height, long style, char *name);
Bool wxcGaugeSetValue(WXID parentId, int val);
Bool wxcGaugeSetBezelFace(WXID parentId, int width);
Bool wxcGaugeSetShadowWidth(WXID parentId, int width);
Bool wxcGaugeSetRange(WXID id, int value);
#endif

// Group box
#ifndef wx_mac
WXID wxcGroupBoxCreate(WXID parentId, char *label,
  int x, int y, int width, int height, long styled, char *name);
#endif

// Menu
WXID wxcMenuCreate(char *title, wxFunction func);
Bool wxcMenuAppend(WXID id, int item_id, char *menu_string, WXID submenu_id, char *help_string, Bool checkable);
Bool wxcMenuAppendSeparator(WXID id);
Bool wxcMenuBreak(WXID id);
Bool wxcMenuEnable(WXID id, int item_id, Bool enabled);
Bool wxcMenuCheck(WXID id, int item_id, Bool checked);
long wxcMenuSetLabel(WXID id, long item_id, char *label);

// Menubar
WXID wxcMenuBarCreate(void);
Bool wxcMenuBarAppend(WXID id, WXID menu_id, char *menu_string);
Bool wxcMenuBarEnable(WXID id, int item_id, Bool enabled);
Bool wxcMenuBarCheck(WXID id, int item_id, Bool checked);
Bool wxcMenuBarChecked(WXID id, int item_id);

// Frame
WXID wxcFrameCreate(WXID parentId, char *title, int x, int y,
  int width, int height, long style, char *name);
Bool wxcFrameSetMenuBar(WXID id, WXID menubar_id);
Bool wxcFrameSetToolBar(WXID id, WXID toolbar_id);
Bool wxcFrameCreateStatusLine(WXID id, int number);
Bool wxcFrameSetStatusText(WXID id, char *text, int i);
Bool wxcFrameSetIcon(WXID id, WXID iconId);
Bool wxcFrameIconize(WXID id, Bool iconize);
Bool wxcFrameSetTitle(WXID id, char *text);
char* wxcFrameGetTitle(WXID id);
#ifndef wx_mac
void wxcFrameOnSize(WXID id, int w, int h);
#endif

// Miscellaneous
char *wxcGetTextFromUser(char *message,
                         char *caption,
                         char *default_string,
                         Bool centre, WXID);
double wxcStringToFloat(char *str);
long wxcStringToLong(char *str);
char *wxcFloatToString(double l);
char *wxcLongToString(long l);
char *wxcStringToSymbol(char *s);
char *wxcSymbolToString(char *s);
char *wxcMessageBox(char *message, char *flags, Bool centre, WXID parentId, char *title);
char *wxcFileSelector(char *message,
  char *def_path, char *def_file, char *def_ext,
  char *def_wildcard, long flags, WXID parentId);
Bool wxcYield(void);
#ifndef wx_mac
Bool wxcSleep(long nSecs);

Bool wxcWriteResource(char *section, char *entry, char *value, char *filename);
char *wxcGetResource(char *section, char *entry, char *filename);

Bool wxcDebugMsg(char *msg);
#endif

// Timer functions
// Bool wxcTimerInit(wxClipsFunction func);
WXID wxcTimerCreate(void);
Bool wxcTimerStart(WXID id, long ms, Bool one_shot);
Bool wxcTimerStop(WXID id);
Bool wxcTimerDelete(WXID id);

#ifndef wx_mac
long wxcGetElapsedTime(Bool resetTimer);
void wxcStartTimer(void);
char *wxcNow(void);

void wxcBeginBusyCursor(void);
void wxcEndBusyCursor(void);

// Toolbar functions
WXID wxcToolbarCreate(long parent, int x, int y, int w, int h, long style,
   int orientation, int rowsOrColumns, Bool createButtons, char *name);
Bool wxcToolbarAddTool(WXID id, int index, WXID bitmapId1, WXID bitmapId2, Bool toggle,
  double x, double y, long clientData);
Bool wxcToolbarClearTools(WXID id);
long wxcToolbarGetToolClientData(WXID id, int index);
double wxcToolbarGetMaxWidth(WXID id);
double wxcToolbarGetMaxHeight(WXID id);
Bool wxcToolbarGetToolState(WXID id, int index);
Bool wxcToolbarGetToolEnabled(WXID id, int index);
Bool wxcToolbarLayout(WXID id);
Bool wxcToolbarOnPaint(WXID id);
Bool wxcToolbarSetMargins(WXID id, double x, double y);
Bool wxcToolbarSetDefaultSize(WXID id, int x, int y);
Bool wxcToolbarEnableTool(WXID id, int tool, Bool enable);
Bool wxcToolbarToggleTool(WXID id, int tool, Bool toggle);
#endif

// DDE functions
WXID wxcClientCreate(void);
WXID wxcClientMakeConnection(WXID id, char *host, char *service, char *topic);
WXID wxcServerCreate(char *service);
Bool wxcConnectionAdvise(WXID id, char *item, char *data);
Bool wxcConnectionExecute(WXID id, char *data);
Bool wxcConnectionDisconnect(WXID id);
Bool wxcConnectionPoke(WXID id, char *item, char *data);
char *wxcConnectionRequest(WXID id, char *item);
Bool wxcConnectionStartAdvise(WXID id, char *item);
Bool wxcConnectionStopAdvise(WXID id, char *item);

#ifndef wx_mac
// Help-accessing functions
WXID wxcHelpCreate(Bool native);
Bool wxcHelpDelete(WXID id);
Bool wxcHelpLoadFile(WXID id, char *file);
Bool wxcHelpDisplayContents(WXID id);
Bool wxcHelpDisplaySection(WXID id, long sectionId);
Bool wxcHelpDisplayBlock(WXID id, long blockId);
Bool wxcHelpKeywordSearch(WXID id, char *keyword);
Bool wxcHelpQuit(WXID id);
#endif

// ODBC functions
#if defined(wx_msw) && USE_ODBC && WXODBC
#include "wx_db.h"

WXID wxcDatabaseCreate(void);
Bool wxcDatabaseOpen(WXID id, char *source, Bool exclusive, Bool readOnly,
   char *username, char *password);
Bool wxcDatabaseClose(WXID id);
Bool wxcDatabaseIsOpen(WXID id);
long wxcDatabaseGetErrorCode(WXID id);
char *wxcDatabaseGetErrorCodeString(WXID id);
char *wxcDatabaseGetDatabaseName(WXID id);
Bool wxcDatabaseErrorOccurred(WXID id);
char *wxcDatabaseGetErrorMessage(WXID id);
long wxcDatabaseGetErrorNumber(WXID id);
Bool wxcDatabaseDelete(WXID id);

WXID wxcRecordSetCreate(WXID dbId, int openType, int options);
Bool wxcRecordSetDelete(WXID rsId);
Bool wxcRecordSetQuery(WXID rsId, char *cols, char *table, char *filter);
int wxcRecordSetGetNumberFields(WXID rsId);
int wxcRecordSetGetNumberParams(WXID rsId);
long wxcRecordSetGetNumberRecords(WXID rsId);
long wxcRecordSetGetNumberCols(WXID rsId);
char *wxcRecordSetGetFilter(WXID rsId);
char *wxcRecordSetGetSortString(WXID rsId);
WXID wxcRecordSetGetDatabase(WXID rsId);
long wxcRecordSetGetErrorCode(WXID rsId);
char *wxcRecordSetGetErrorCodeString(WXID rsId);
Bool wxcRecordSetGetResultSet(WXID rsId);
Bool wxcRecordSetExecuteSql(WXID rsId, char *sql);
Bool wxcRecordSetGetTables(WXID rsId);
Bool wxcRecordSetGetColumns(WXID rsId, char *table);
Bool wxcRecordSetGetPrimaryKeys(WXID rsId, char *table);
Bool wxcRecordSetGetForeignKeys(WXID rsId, char *table, char *fName);
char *wxcRecordSetGetTableName(WXID rsId);
Bool wxcRecordSetSetTableName(WXID rsId, char *table);
Bool wxcRecordSetIsOpen(WXID rsId);
Bool wxcRecordSetIsBOF(WXID rsId);
Bool wxcRecordSetIsEOF(WXID rsId);
Bool wxcRecordSetIsDeleted(WXID rsId);
long wxcRecordSetGetIntData(WXID rsId, int colPos);
long wxcRecordSetGetIntData(WXID rsId, char *col);
double wxcRecordSetGetFloatData(WXID rsId, int colPos);
double wxcRecordSetGetFloatData(WXID rsId, char *col);
char *wxcRecordSetGetCharData(WXID rsId, int colPos);
char *wxcRecordSetGetCharData(WXID rsId, char *col);
char *wxcRecordSetGetColName(WXID rsId, int pos);
long wxcRecordSetGetColType(WXID rsId, int pos);
long wxcRecordSetGetColType(WXID rsId, char *col);
char *wxcRecordSetGetColTypeString(WXID rsId, int pos);
char *wxcRecordSetGetColTypeString(WXID rsId, char *col);
Bool wxcRecordSetMove(WXID rsId, long rows);
Bool wxcRecordSetMoveFirst(WXID rsId);
Bool wxcRecordSetMoveLast(WXID rsId);
Bool wxcRecordSetMoveNext(WXID rsId);
Bool wxcRecordSetMovePrev(WXID rsId);
Bool wxcRecordSetGoto(WXID rsId, long row);
Bool wxcRecordSetGetDataSources(WXID rsId);
Bool wxcRecordSetIsFieldDirty(WXID rsId, int col);
Bool wxcRecordSetIsFieldDirty(WXID rsId, char *col);
Bool wxcRecordSetIsFieldNull(WXID rsId, int col);
Bool wxcRecordSetIsFieldNull(WXID rsId, char *col);
Bool wxcRecordSetIsColNullable(WXID rsId, int col);
Bool wxcRecordSetIsColNullable(WXID rsId, char *col);
Bool wxcRecordSetSetFieldDirty(WXID rsId, int col, Bool dirty);
Bool wxcRecordSetSetFieldDirty(WXID rsId, char *col, Bool dirty);
#endif

#ifdef wx_msw
// MCI (multimedia command interface) string function. Returns
// NULL if no error, otherwise a text description of the error.
char *wxcMCISendString(char *command);

WXID wxcHWNDFind(char *title);
Bool wxcHWNDIconize(WXID hWnd, Bool iconize);
Bool wxcHWNDShow(WXID hWnd, Bool show);
Bool wxcHWNDMove(WXID hWnd, int x, int y, int w, int h, Bool repaint);
Bool wxcHWNDQuit(WXID hWnd);
Bool wxcHWNDRefresh(WXID hWnd, Bool eraseBackground);

#endif

#ifndef wx_mac
void wxcBell(void);
Bool wxcChdir(char *s);
Bool wxcFileExists(char *f);
Bool wxcDirExists(char *f);

// Date functions
WXID wxcDateCreateString(char *s);
WXID wxcDateCreateJulian(long j);
WXID wxcDateCreate(void);
WXID wxcDateCreate(const int m, const int d, const int y);
Bool wxcDateDelete(WXID dateId);
WXID wxcDateAddDays(WXID dateId, long days);
WXID wxcDateSubtractDays(WXID dateId, long days);
long wxcDateSubtract(WXID dateId1, WXID dateId2);
Bool wxcDateAddSelf(WXID dateId, long days);
Bool wxcDateSubtractSelf(WXID dateId, long days);
Bool wxcDateLE(WXID dateId1, WXID dateId2);
Bool wxcDateLEQ(WXID dateId1, WXID dateId2);
Bool wxcDateGE(WXID dateId1, WXID dateId2);
Bool wxcDateGEQ(WXID dateId1, WXID dateId2);
Bool wxcDateEQ(WXID dateId1, WXID dateId2);
Bool wxcDateNEQ(WXID dateId1, WXID dateId2);
char *wxcDateFormat(WXID dateId, const int type);
Bool wxcDateSetFormat(WXID dateId, const int type);
Bool wxcDateSetFormat(WXID dateId, const char *type);
Bool wxcDateSetOption(WXID dateId, const int option, Bool enable);
Bool wxcDateSetOption(WXID dateId, const char *option, Bool enable);
long wxcDateGetJulianDate(WXID dateId);
int wxcDateGetDayOfYear(WXID dateId);
Bool wxcDateIsLeapYear(WXID dateId);
Bool wxcDateSetCurrentDate(WXID dateId);
Bool wxcDateSetJulian(WXID dateId, long julian);
Bool wxcDateSetDate(WXID dateId, int m, int d, int y);
Bool wxcDateAddWeeks(WXID dateId, int n);
Bool wxcDateAddMonths(WXID dateId, int n);
Bool wxcDateAddYears(WXID dateId, int n);
int wxcDateGetDay(WXID dateId);
int wxcDateGetDaysInMonth(WXID dateId);
int wxcDateGetFirstDayOfMonth(WXID dateId);
int wxcDateGetDayOfWeek(WXID dateId);
int wxcDateGetWeekOfMonth(WXID dateId);
int wxcDateGetWeekOfYear(WXID dateId);
char *wxcDateGetDayOfWeekName(WXID dateId);
char *wxcDateGetMonthName(WXID dateId);
int wxcDateGetMonth(WXID dateId);
int wxcDateGetYear(WXID dateId);
WXID wxcDateGetMonthStart(WXID dateId);
WXID wxcDateGetMonthEnd(WXID dateId);
WXID wxcDateGetYearStart(WXID dateId);
WXID wxcDateGetYearEnd(WXID dateId);
#endif

// This should be ok for many objects, so in theory we can
// remove several wxcDelete... functions.
// EXCEPT this won't work for classes that HAVEN'T been
// derived and destructor overridden to delete the object
// from the identifier table.
Bool wxcObjectDelete(WXID id);

#endif // wxextendh
