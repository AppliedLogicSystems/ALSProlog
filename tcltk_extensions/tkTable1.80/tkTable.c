/* 
 * tkTable.c --
 *
 *	This module implements table widgets for the Tk
 *	toolkit.  An table displays a 2D array of strings
 *	and allows the strings to be edited.
 *
 * Based on Tk3 table widget written by Roland King
 *
 * Updates 1996 by:
 * Jeffrey Hobbs	jeff.hobbs@acm.org
 * John Ellson		ellson@lucent.com
 * Peter Bruecker	peter@bj-ig.de
 * Tom Moore		tmoore@spatial.ca
 * Sebastian Wangnick	wangnick@orthogon.de
 *
 */

/*
 * FIX - Is TableCmdDeletedProc necessary?
 */

#include "tkTable.h"

#if !defined(WIN32) && !defined(macintosh)
#define X_WINDOWS
#endif

#ifdef IMP

/* Functions necessary for special X input methods (KANJI) */
#include "tkXimp.h"
extern ImpInfo impInfo;

static void
AddXIMeventMask(d, w)
     Display *d;
     Window   w;
{
  XWindowAttributes attr;
  return; /* FIX, what's going on here? */
  if (!XGetWindowAttributes(d, w, &attr)) return;
  XSelectInput(d, w, (impInfo.eventMask | attr.your_event_mask));
}

static void
ImpEvent(Table *tablePtr)
{
  Display *display = Tk_Display (tablePtr->tkwin);
  Window client = Tk_WindowId (tablePtr->tkwin);
  if (!impInfo.init) {
    Tk_XimpStart (tablePtr->interp, tablePtr->tkwin, 0, NULL);
  }
  if (impInfo.ic && !impInfo.window) {
    impInfo.window = Tk_WindowId(Tk_NameToWindow(tablePtr->interp,
						 ".", tablePtr->tkwin));
    XSetICValues (impInfo.ic, XNClientWindow, impInfo.window, NULL);
    AddXIMeventMask(display, impInfo.window);
  }
}

static void
ImpDestroy(Table *tablePtr)
{
  Window client = Tk_WindowId (tablePtr->tkwin);
  if (impInfo.init && (client == impInfo.eventWin)) {
    XSetICValues (impInfo.ic, XNFocusWindow,impInfo.window, NULL);
  }
}

static void
ImpFocusIn(Table *tablePtr,XEvent *eventPtr)
{
  Display *display = Tk_Display (tablePtr->tkwin);
  Window client = Tk_WindowId (tablePtr->tkwin);
  if (impInfo.init) {
    if (eventPtr->xany.window == impInfo.window) {
      XSetInputFocus (display, client, RevertToParent, CurrentTime);
    } else if (eventPtr->xany.window == client && impInfo.ic) {
      XSetICValues (impInfo.ic, XNFocusWindow, client, NULL);
      XSetICFocus (impInfo.ic);
      /* XmbResetIC( impInfo.ic ); */
      impInfo.icFlag = 1;
      impInfo.eventWin = client;
    }
  }
}

static void
ImpFocusOut(Table *tablePtr,XEvent *eventPtr)
{
  Window client = Tk_WindowId (tablePtr->tkwin);
  if (impInfo.init && (eventPtr->xany.window == client && impInfo.ic)) {
    /*XmbResetIC( impInfo.ic );*/
    XUnsetICFocus (impInfo.ic);
    impInfo.icFlag = 0;
  }
}

static void
ImpInit(Table *tablePtr)
{
  if (impInfo.init && (impInfo.ClientWindowFlag == 0 && impInfo.ic)) {
    impInfo.window=Tk_WindowId(Tk_NameToWindow(tablePtr->interp, ".", tkwin));
    XSetICValues(impInfo.ic, XNClientWindow, impInfo.window, NULL);
    impInfo.ClientWindowFlag = 1;
    AddXIMeventMask(tablePtr->display, impInfo.window);
  }
}

#ifdef KANJI
static WCHAR *
ImpKGetStr(Tcl_Interp *interp, char *cp)
{
  WCHAR *n;
  if (impInfo.init && (impInfo.showFlag == 1)) {
    n = Tk_GetWStr(interp, impInfo.buf);
    impInfo.showFlag = 0;
  } else {
    n = Tk_GetWStr(interp, cp);
  } 
  return n;
}
#endif		/* KANJI */

#else		/* IMP */

#define ImpInit(t)
#define ImpEvent(t)
#define ImpFocusIn(t,e)
#define ImpFocusOut(t,e)
#define ImpDestroy(t)
#define ImpKGetStr(interp,cp)  ENCODE_WSTR(interp,cp)

#endif		/* IMP */

/*
 * simple CmdStruct lookup functions
 */

static char *
getCmdName(const CmdStruct* c,int val)
{
  for(;c->name && c->name[0];c++) {
    if (c->value==val) return c->name;
  }
  return NULL;
}

static int
getCmdValue(const CmdStruct* c, const char *arg)
{
  int len=strlen(arg);
  for(;c->name && c->name[0];c++) {
    if (!strncmp(c->name,arg,len)) return c->value;
  }
  return 0;
}

static void
getCmdError(Tcl_Interp *interp, CmdStruct *c, const char *arg)
{
  int i;
  Tcl_AppendResult (interp, "bad option \"", arg,"\" must be ", (char *) 0);
  for(i=0;c->name && c->name[0];c++,i++) {
    Tcl_AppendResult (interp,(i?", ":""),c->name,(char *) 0);
  }
}

/*
 * Functions for handling custom options that use CmdStructs
 */

static int
TableOptionSet(ClientData clientData, Tcl_Interp *interp,
	       Tk_Window tkwin, char *value, char *widgRec, int offset)
{
  CmdStruct *p=(CmdStruct *)clientData;
  int mode=getCmdValue(p,value);
  if (!mode) {
    getCmdError(interp,p,value);
    return TCL_ERROR;
  }
  *((int*)(widgRec+offset))=mode;
  return TCL_OK;
}

static char *
TableOptionGet(ClientData clientData, Tk_Window tkwin,
	       char *widgRec, int offset, Tcl_FreeProc **freeProcPtr)
{
  CmdStruct *p=(CmdStruct *)clientData;
  int mode = *((int*)(widgRec+offset));
  return getCmdName(p,mode);
}

/*
 * Parses a command string passed in an arg comparing it with all the
 * command strings in the command array. If it finds a string which is a
 * unique identifier of one of the commands, returns the index . If none of
 * the commands match, or the abbreviation is not unique, then it sets up
 * the message accordingly and returns 0
 */

static int
parse_command (Tcl_Interp *interp, CmdStruct *cmds, char *arg)
{
  int len = (int)strlen(arg);
  CmdStruct *matched = (CmdStruct *) 0;
  int err = 0;
  CmdStruct *next = cmds;
  while (*(next->name)) {
    if (strncmp (next->name, arg, len) == 0) {
      /* have we already matched this one if so make up an error message */
      if (matched) {
	if (!err) {
	  Tcl_AppendResult (interp, "ambiguous option \"", arg,
			    "\" could be ", matched->name, (char *) 0);
	  matched = next;
	  err = 1;
	}
	Tcl_AppendResult (interp, ", ", next->name, (char *) 0);
      } else {
	matched = next;
	/* return on an exact match */
	if (len == (int)strlen(next->name))
	  return matched->value;
      }
    }
    next++;
  }
  /* did we get multiple possibilities */
  if (err) return 0;
  /* did we match any at all */
  if (matched) {
    return matched->value;
  } else {
    Tcl_AppendResult (interp, "bad option \"", arg,"\" must be ",
		      (char *) NULL);
    next = cmds;
    while (1) {
      Tcl_AppendResult (interp, next->name, (char *) NULL);
      /* the end of them all ? */
      if (!*((++next)->name)) return 0;
      /* or the last one at least */
      if (*((next + 1)->name))
	Tcl_AppendResult (interp, ", ", (char *) NULL);
      else
	Tcl_AppendResult (interp, " or ", (char *) NULL);
    }
  }
}

#define TableInvalidateAll(tablePtr, force)  \
  TableInvalidate((tablePtr), 0, 0, Tk_Width((tablePtr)->tkwin), \
		  Tk_Height((tablePtr)->tkwin), (force))

/*
 * Turn row/col into an index into the table
 */
#define TableMakeArrayIndex(r, c, i)	sprintf((i), "%d,%d", (r), (c))

/*
 * Turn array index back into row/col
 * return the number of args parsed (should be two)
 */
#define TableParseArrayIndex(r, c, i)	sscanf((i), "%d,%d", (r), (c))

/*
 * Returns the x, y, width and height for a given cell
 */
static void
TableCellCoords(Table * tablePtr, int row, int col,
		int *x, int *y, int *width, int *height)
{
  if (tablePtr->rows <= 0 || tablePtr->cols <= 0) {
    *width = *height = *x = *y = 0;
    return;
  }
  /* real coords required */
  row = MIN(tablePtr->rows-1, MAX(0, row));
  col = MIN(tablePtr->cols-1, MAX(0, col));
  *width = (tablePtr->colPixels)[col];
  *height = (tablePtr->rowPixels)[row];
  *x = tablePtr->highlightWidth + (tablePtr->colStarts)[col] - 
    ((col < tablePtr->titleCols) ? 0 : (tablePtr->colStarts)[tablePtr->leftCol]
     - (tablePtr->colStarts)[tablePtr->titleCols]);
  *y = tablePtr->highlightWidth + (tablePtr->rowStarts)[row] -
    ((row < tablePtr->titleRows) ? 0 : (tablePtr->rowStarts)[tablePtr->topRow]
     - (tablePtr->rowStarts)[tablePtr->titleRows]);
}

/*
 * Is the cell visible on the display?
 * Returns the viewable coordinates of a cell
 */
static int
TableCellVCoords(tablePtr, row, col, rx, ry, rw, rh, full)
     Table *tablePtr;		/* Pointer to table record */
     int row, col;		/* row,col index of cell */
     int *rx, *ry, *rw, *rh;	/* pointers to set x,y,w,h of cell */
     int full;			/* whether to require full visibility */
{
  if ((row < tablePtr->topRow && row >= tablePtr->titleRows) ||
      (col < tablePtr->leftCol && col >= tablePtr->titleCols)) {
    /* hiding in "dead" space between title areas and visible cells */
    *rx = 0; *ry = 0; *rw = 0; *rh = 0;
    return 0;
  } else {
    int x, y, w, h, w0, h0, hl = tablePtr->highlightWidth;
    /* Necessary to use separate vars in case dummies are passed in */
    TableCellCoords (tablePtr, row, col, &x, &y, &w, &h);
    *rx = x; *ry = y;
    w0  = w;  h0 = h;
    if (!full) {
      w0 /= 2; h0 /= 2;
    }
    /* Is the cell at least mostly visible? */
    if (x<hl || y<hl || (x+w0)>Tk_Width(tablePtr->tkwin)-hl ||
	(y+h0)>Tk_Height(tablePtr->tkwin)-hl) {
      /* definitely off the screen */
      *rw = 0; *rh = 0;
      return 0;
    } else {
      if (full) {
	*rw = w; *rh = h;
      } else {
	*rw = MIN(w, Tk_Width(tablePtr->tkwin)-x-hl);
	*rh = MIN(h, Tk_Height(tablePtr->tkwin)-y-hl);
      }
      return 1;
    }
  }
}

/*
 * Given an (x,y) coordinate as displayed on the screen,
 * returns the cell that contains that point
 */
static void
TableWhatCell(Table * tablePtr, int x, int y, int *row, int *col)
{
  int i;
  x = MAX(0, x); y = MAX(0, y);
  /* Adjust for table's global highlightthickness border */
  x -= tablePtr->highlightWidth; y -= tablePtr->highlightWidth;
  /* Adjust the x coord if not in the column titles to change display coords
   * into internal coords */
  x += (x < (tablePtr->colStarts)[tablePtr->titleCols]) ? 0 :
    (tablePtr->colStarts)[tablePtr->leftCol] -
    (tablePtr->colStarts)[tablePtr->titleCols];
  y += (y < (tablePtr->rowStarts)[tablePtr->titleRows]) ? 0 :
    (tablePtr->rowStarts)[tablePtr->topRow] -
    (tablePtr->rowStarts)[tablePtr->titleRows];
  x = MIN(x, tablePtr->maxWidth - 1);
  y = MIN(y, tablePtr->maxHeight - 1);
  for (i = 1; x >= (tablePtr->colStarts)[i]; i++);
  *col = i - 1;
  for (i = 1; y >= (tablePtr->rowStarts)[i]; i++);
  *row = i - 1;
}

/*
 * Given an (x,y) coordinate as displayed on the screen,
 * returns the cell that contains that point if it is near the border
 */
static int
TableAtBorder(Table * tablePtr, int x, int y, int *row, int *col)
{
  int i, borders = 2, bd = tablePtr->borderWidth;
  int dbd = 2*bd;
  x = MAX(0, x); y = MAX(0, y);
  x -= tablePtr->highlightWidth; y -= tablePtr->highlightWidth;
  /* Adjust the x coord if not in the column titles to change display coords
   * into internal coords */
  x += (x < (tablePtr->colStarts)[tablePtr->titleCols]) ? 0 :
    (tablePtr->colStarts)[tablePtr->leftCol] -
    (tablePtr->colStarts)[tablePtr->titleCols];
  y += (y < (tablePtr->rowStarts)[tablePtr->titleRows]) ? 0 :
    (tablePtr->rowStarts)[tablePtr->topRow] -
    (tablePtr->rowStarts)[tablePtr->titleRows];
  x = MIN(x, tablePtr->maxWidth - 1);
  y = MIN(y, tablePtr->maxHeight - 1);
  for (i = 1; i <= tablePtr->cols && x+dbd >= (tablePtr->colStarts)[i]; i++);
  if (x > (tablePtr->colStarts)[--i]+bd) {
    borders--;
    *col = -1;
  } else {
    *col = (--i < tablePtr->leftCol && i >= tablePtr->titleCols) ?
      tablePtr->titleCols-1 : i;
  }
  for (i = 1; i <= tablePtr->rows && y+dbd >= (tablePtr->rowStarts)[i]; i++);
  if (y > (tablePtr->rowStarts)[--i]+bd) {
    borders--;
    *row = -1;
  } else {
    *row = (--i < tablePtr->topRow && i >= tablePtr->titleRows) ?
      tablePtr->titleRows-1 : i;
  }
  return borders;
}

/*
 * Get contents of cell r,c
 * results are VOLATILE (do not free)
 * r && c are in user coords
 */
static char *
TableGetCellValue(Table *tablePtr, int r, int c)
{
  register Tcl_Interp *interp = tablePtr->interp;
  char *result = NULL;
  char buf[INDEX_BUFSIZE];
  Tcl_HashEntry *entryPtr = NULL;
  int new = 1;

  TableMakeArrayIndex(r, c, buf);

  if (tablePtr->caching) {
    /* if we are caching, let's see if we have the value cached */
    entryPtr = Tcl_CreateHashEntry(tablePtr->cache, buf, &new);
    if (!new) {
      result = (char *) Tcl_GetHashValue(entryPtr);
      return result?result:"";
    }
  }
  if (tablePtr->command && tablePtr->useCmd) {
    Tcl_DString script;
    Tcl_DStringInit(&script);
    ExpandPercents(tablePtr, tablePtr->command, r, c, "", (char *)NULL,
		   0, &script, CMD_GET);
    if (Tcl_GlobalEval(interp, Tcl_DStringValue(&script)) == TCL_ERROR) {
      tablePtr->useCmd = 0;
      tablePtr->dataSource &= ~DATA_COMMAND;
      Tcl_AddErrorInfo(interp, "\n\t(in command executed by table)");
      Tcl_AddErrorInfo(interp, Tcl_DStringValue(&script));
      Tk_BackgroundError(interp);
      TableInvalidateAll(tablePtr, 0);
    } else {
      result = interp->result;
    }
    Tcl_FreeResult(interp);
    Tcl_DStringFree(&script);
  } else if (tablePtr->arrayVar) {
    result = Tcl_GetVar2(interp, tablePtr->arrayVar, buf, TCL_GLOBAL_ONLY);
  }
  if (result == NULL)
    result = "";
  if (tablePtr->caching && entryPtr != NULL) {
    /* if we are caching, make sure we cache the returned value */
    /* entryPtr will have been set from above, but check to make sure
     * someone didn't change caching during -command evaluation */
    char *val;
    val = (char *)ckalloc(strlen(result)+1);
    strcpy(val, result);
    Tcl_SetHashValue(entryPtr, val);
  }
  return result;
}

/*
 * Set value of cell r,c (it is unset if value == {})
 * returns TCL_ERROR if an error occured during set, otherwise
 * it returns TCL_OK
 * r && c are in user coords
 */
static int
TableSetCellValue(Table *tablePtr, int r, int c, char *value)
{
  register Tcl_Interp *interp = tablePtr->interp;
  char buf[INDEX_BUFSIZE];
  int code = TCL_OK;

  TableMakeArrayIndex(r, c, buf);

  if (tablePtr->state == tkDisabledUid) return code;
  if (tablePtr->command && tablePtr->useCmd) {
    Tcl_DString script;

    Tcl_DStringInit(&script);
    ExpandPercents(tablePtr, tablePtr->command, r, c, value, (char *)NULL,
		   1, &script, CMD_SET);
    if (Tcl_GlobalEval(interp, Tcl_DStringValue(&script)) == TCL_ERROR) {
      /* An error resulted.  Prevent further triggering of the command
       * and set up the error message. */
      tablePtr->useCmd = 0;
      tablePtr->dataSource &= ~DATA_COMMAND;
      Tcl_AddErrorInfo(interp, "\n\t(in command executed by table)");
      Tk_BackgroundError(interp);
      code = TCL_ERROR;
    }
    Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);
    Tcl_DStringFree(&script);
  } else if (tablePtr->arrayVar) {
    if (value == NULL || *value == '\0') {
      Tcl_UnsetVar2(interp, tablePtr->arrayVar, buf, TCL_GLOBAL_ONLY);
    } else if (Tcl_SetVar2(interp, tablePtr->arrayVar, buf, value,
			   TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG) == NULL) {
      code = TCL_ERROR;
    }
  }
  if (tablePtr->caching && code == TCL_OK) {
    Tcl_HashEntry *entryPtr;
    int new;
    char *val;

    val = (char *)ckalloc(strlen(value)+1);
    strcpy(val, value);
    entryPtr = Tcl_CreateHashEntry(tablePtr->cache, buf, &new);
    Tcl_SetHashValue(entryPtr, val);
  }
  return code;
}

/*
 * Flushes the cache
 */
static void
TableFlushCache(Table *tablePtr)
{
  /* Just get rid of it and reinit it */
  Tcl_DeleteHashTable (tablePtr->cache);
  ckfree ((char *) (tablePtr->cache));
  tablePtr->cache = (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->cache, TCL_STRING_KEYS);
}

/*
 *----------------------------------------------------------------------
 *
 * TableSortCompareProc --
 *
 *	This procedure is invoked by qsort to determine the proper
 *	ordering between two elements.
 *
 * Results:
 *	< 0 means first is "smaller" than "second", > 0 means "first"
 *	is larger than "second", and 0 means they should be treated
 *	as equal.
 *
 * Side effects:
 *	None, unless a user-defined comparison command does something
 *	weird.
 *
 *----------------------------------------------------------------------
 */

static int
TableSortCompareProc(first, second)
    CONST VOID *first, *second;		/* Elements to be compared. */
{
    int r1, c1, r2, c2;
    char *firstString = *((char **) first);
    char *secondString = *((char **) second);

    /* This doesn't account for badly formed indices */
    sscanf(firstString, "%d,%d", &r1, &c1);
    sscanf(secondString, "%d,%d", &r2, &c2);
    if (r1 > r2)
      return 1;
    else if (r1 < r2)
      return -1;
    else if (c1 > c2)
      return 1;
    else if (c1 < c2)
      return -1;
    return 0;
}

/*
 * Sort a list of table cell elements (of form row,col)
 * replace with sorted list
 */
static char *
TableCellSort(Table *tablePtr, char *str)
{
  int listArgc;
  char **listArgv;
  char *result;

  if (Tcl_SplitList(tablePtr->interp, str, &listArgc, &listArgv) != TCL_OK)
    return str;
  qsort((VOID *) listArgv, (size_t) listArgc, sizeof (char *),
	TableSortCompareProc);
  result = Tcl_Merge(listArgc, listArgv);
  ckfree((char *) listArgv);
  return result;
}

/*
 * Parse the argument as an index into the active cell string.
 * Recognises 'end', 'insert' or an integer.  Constrains it to the
 * size of the buffer.  This acts like a "SetIcursor" when *posn is NULL.
 *
 * Side Effects:
 *	Will possibly readjust icursor
 */
static int
TableGetIcursor(Table * tablePtr, char *arg, int *posn)
{
  int tmp, len;
#ifdef KANJI
  WCHAR *data = ENCODE_WSTR(tablePtr->interp, tablePtr->activeBuf);
  len = WSTRLEN(data);
  FREE_WSTR(data);
#else
  len = strlen(tablePtr->activeBuf);
#endif	/* KANJI */
  /* ensure icursor didn't get out of sync */
  if (tablePtr->icursor > len) tablePtr->icursor = len;
  /* is this end */
  if (strcmp(arg, "end") == 0) {
    tmp = len;
  } else if (strcmp(arg, "insert") == 0) {
    tmp = tablePtr->icursor;
  } else {
    if (Tcl_GetInt(tablePtr->interp, arg, &tmp) != TCL_OK) {
      return TCL_ERROR;
    }
    tmp = MIN(MAX(0, tmp), len);
  }
  if (posn)
    *posn = tmp;
  else
    tablePtr->icursor = tmp;
  return TCL_OK;
}

/*
 * ckallocs space for a new tag structure and clears the structure
 * returns the pointer to the new structure
 */
static TagStruct *
TableNewTag(Table * tablePtr)
{
  TagStruct *tagPtr;
  tagPtr = (TagStruct *) ckalloc(sizeof (TagStruct));
  tagPtr->anchor	= (Tk_Anchor)-1;
  tagPtr->bgBorder	= NULL;
  tagPtr->foreground	= NULL;
  tagPtr->fontPtr	= NULL;
#ifdef KANJI
  tagPtr->asciiFontPtr	= NULL;
  tagPtr->kanjiFontPtr	= NULL;
#endif
  tagPtr->image		= NULL;
  tagPtr->imageStr	= NULL;
  tagPtr->relief	= 0;
  return tagPtr;
}

/*
 * Sets the "active" index of the associated array to the current
 * value of the active buffer.
 */
static void
TableSetActiveIndex(tablePtr)
     register Table *tablePtr;
{
  if (tablePtr->arrayVar) {
    tablePtr->flags |= SET_ACTIVE;
    Tcl_SetVar2(tablePtr->interp, tablePtr->arrayVar, "active",
		tablePtr->activeBuf, TCL_GLOBAL_ONLY);
    tablePtr->flags &= ~SET_ACTIVE;
  }
}

/*
 * Get the current selection into the buffer and mark it as unedited.
 * Set the position to the end of the string
 */
static void
TableGetActiveBuf(tablePtr)
     register Table *tablePtr;
{
  char *data = "";

  if (tablePtr->flags & HAS_ACTIVE)
    data = TableGetCellValue(tablePtr, tablePtr->activeRow+tablePtr->rowOffset,
			     tablePtr->activeCol+tablePtr->colOffset);

  if (strcmp(tablePtr->activeBuf, data) == 0) {
    /* this forced SetActiveIndex is necessary if we change array vars and
     * they happen to have these cells equal, we won't properly set the
     * active index for the new array var unless we do this here */
    TableSetActiveIndex(tablePtr);
    return;
  }
  /* is the buffer long enough */
  tablePtr->activeBuf = (char *)ckrealloc(tablePtr->activeBuf, strlen(data)+1);
  strcpy(tablePtr->activeBuf, data);
  TableGetIcursor(tablePtr, "end", (int *)0);
  tablePtr->flags &= ~TEXT_CHANGED;
  TableSetActiveIndex(tablePtr);
}

/*
 * This routine merges two tags by adding any fields from the addTag that
 * are unset in the base Tag
 */
static void
TableMergeTag(TagStruct * baseTag, TagStruct * addTag)
{
  if (baseTag->anchor == (Tk_Anchor)-1)
    baseTag->anchor = addTag->anchor;
  if (baseTag->bgBorder == NULL)
    baseTag->bgBorder = addTag->bgBorder;
  if (baseTag->foreground == NULL)
    baseTag->foreground = addTag->foreground;
  if (baseTag->fontPtr == NULL)
    baseTag->fontPtr = addTag->fontPtr;
#ifdef KANJI
  if (baseTag->asciiFontPtr==NULL)
    baseTag->asciiFontPtr = addTag->asciiFontPtr;
  if (baseTag->kanjiFontPtr==NULL)
    baseTag->kanjiFontPtr = addTag->kanjiFontPtr;
#endif
  if (baseTag->image == NULL)
    baseTag->image = addTag->image;
  /* FIX, should this do a ckalloc/copy? */
  if (baseTag->imageStr == NULL)
    baseTag->imageStr = addTag->imageStr;
  if (baseTag->relief == 0)
    baseTag->relief = addTag->relief;
}

static void
CreateTagEntry(Table *tablePtr, char *name, int argc, char **argv)
{
  Tcl_HashEntry *entryPtr;
  TagStruct *tagPtr = TableNewTag(tablePtr);
  int dummy;
  Tk_ConfigureWidget(tablePtr->interp, tablePtr->tkwin,
		     tagConfig, argc, argv, (char *)tagPtr, 0);
  entryPtr = Tcl_CreateHashEntry (tablePtr->tagTable, name, &dummy);
  Tcl_SetHashValue (entryPtr, (ClientData) tagPtr);
}

static void
InitTagTable(Table *tablePtr)
{
  static char *activeArgs[]	= {"-bg", ACTIVE_BG, "-relief", "flat" };
  static char *selArgs[]	= {"-bg", SELECT_BG, "-relief", "sunken" };
  static char *titleArgs[]	= {"-bg", DISABLED,
				   "-relief", "flat", "-fg", "white" };
  static char *flashArgs[]	= {"-bg", "red" };
  CreateTagEntry(tablePtr, "active", ARSIZE(activeArgs), activeArgs);
  CreateTagEntry(tablePtr, "sel", ARSIZE(selArgs), selArgs);
  CreateTagEntry(tablePtr, "title", ARSIZE(titleArgs), titleArgs);
  CreateTagEntry(tablePtr, "flash", ARSIZE(flashArgs), flashArgs);
}

static void
TableImageProc (ClientData clientData, int x, int y, int width, int height,
		int imageWidth, int imageHeight)
{
  /* This could be improved to check if the image is in view */
  TableInvalidateAll((Table *)clientData, 0);
}

/*
 * Gets a GC correponding to the tag structure passed
 */
#ifdef KANJI
static XWSGC
#else
static GC
#endif
TableGetGc(Table * tablePtr, TagStruct * tagPtr)
{
  XGCValues gcValues;
  gcValues.foreground = tagPtr->foreground->pixel;
  gcValues.background = Tk_3DBorderColor(tagPtr->bgBorder)->pixel;
  gcValues.graphics_exposures = False;
#ifdef KANJI
  tagPtr->fontPtr = Tk_GetFontSet(tagPtr->asciiFontPtr, tagPtr->kanjiFontPtr);
  return Tk_GetGCSet(tablePtr->tkwin,
		     GCForeground|GCBackground|GCFont|GCGraphicsExposures,
		     &gcValues,tagPtr->fontPtr);
#else
#if (TK_MAJOR_VERSION == 8)
  gcValues.font = Tk_FontId(tagPtr->fontPtr);
#else
  gcValues.font = tagPtr->fontPtr->fid;
#endif
  return Tk_GetGC(tablePtr->tkwin,
		  GCForeground|GCBackground|GCFont|GCGraphicsExposures,
		  &gcValues);
#endif
}

#ifdef KANJI
#define TableFreeGc(tablePtr, gc) Tk_FreeGCSet((tablePtr)->display,(gc));
#else
#define TableFreeGc(tablePtr, gc) Tk_FreeGC((tablePtr)->display,(gc));
#endif

#ifdef KANJI
#define TableDrawString(dpy, win, gc, x, y, str, len, font) \
	TkWSDrawString((dpy), (win), (gc), (x), (y), (str), (len))
#else
#if (TK_MAJOR_VERSION == 8)
#define TableDrawString(dpy, win, gc, x, y, str, len, font) \
	Tk_DrawChars((dpy), (win), (gc), (font), (str), (len), (x), (y))
#else	/* TK4 */
#define TableDrawString(dpy, win, gc, x, y, str, len, font) \
	XDrawString((dpy), (win), (gc), (x), (y), (str), (len))
#endif	/* TK8 */
#endif	/* KANJI */

/*
 *--------------------------------------------------------------
 *
 * TableDisplay --
 *
 *	This procedure redraws the contents of a table window.
 *	The excessive amount of conditional code in this function
 *	is due to these factors:
 *		o Differences in KANJI and regular Tk
 *		o Differences in Tk4 and Tk8 font handling
 *		o Lack of XSetClipRectangles on Windows
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Information appears on the screen.
 *
 *--------------------------------------------------------------
 */

static void
TableDisplay(ClientData clientdata)
{
  Table *tablePtr = (Table *) clientdata;
  Tk_Window tkwin = tablePtr->tkwin;
  Display *display = tablePtr->display;
  Tcl_Interp *interp = tablePtr->interp;
  Drawable window;
#ifdef X_WINDOWS
  XRectangle clipRect;
#else
  Drawable clipWind;
#endif
#if (TK_MAJOR_VERSION == 8)
  Tk_FontMetrics fm;
#else
  int direction;
#endif
  int rowFrom, rowTo, colFrom, colTo,
    invalidX, invalidY, invalidWidth, invalidHeight,
    x, y, imageW, imageH, imageX, imageY, row, col, urow, ucol,
    width, height, ascent, descent, bd, vallen,
    originX, originY, activeCell, clipRectSet;
  XCharStruct bbox, bbox2;
#ifdef KANJI
  XWSGC tagGc;
#else
  GC tagGc;
#endif
  GC topGc, bottomGc;
  WCHAR *value = NULL;
  char buf[INDEX_BUFSIZE];
  TagStruct *tagPtr, *titlePtr, *selPtr, *activePtr, *flashPtr,
    *rowPtr, *colPtr;
  Tcl_HashEntry *entryPtr, *ePtr;
  static XPoint rect[6] = { {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0} };
  Tcl_HashTable *colTags = NULL;

  tablePtr->flags &= ~REDRAW_PENDING;
  invalidX = tablePtr->invalidX;
  invalidY = tablePtr->invalidY;
  invalidWidth = tablePtr->invalidWidth;
  invalidHeight = tablePtr->invalidHeight;

  /* 
   * if we are using the slow drawing mode with a pixmap 
   * create the pixmap and adjust x && y for offset in pixmap
   */
  if (tablePtr->drawMode == DRAW_MODE_SLOW) {
    window = Tk_GetPixmap(display, Tk_WindowId (tkwin),
			  invalidWidth, invalidHeight, Tk_Depth (tkwin));
  } else {
    window = Tk_WindowId (tkwin);
  }
#ifndef X_WINDOWS
  clipWind = Tk_GetPixmap(display, window,
			  invalidWidth, invalidHeight, Tk_Depth(tkwin));
#endif

  /* get the border width to adjust the calculations */
  bd = tablePtr->borderWidth;

  /* set up the permanent tag styles */
  entryPtr = Tcl_FindHashEntry (tablePtr->tagTable, "title");
  titlePtr = (TagStruct *) Tcl_GetHashValue (entryPtr);
  entryPtr = Tcl_FindHashEntry (tablePtr->tagTable, "sel");
  selPtr   = (TagStruct *) Tcl_GetHashValue (entryPtr);
  entryPtr = Tcl_FindHashEntry(tablePtr->tagTable, "active");
  activePtr= (TagStruct *) Tcl_GetHashValue(entryPtr);
  entryPtr = Tcl_FindHashEntry(tablePtr->tagTable, "flash");
  flashPtr = (TagStruct *) Tcl_GetHashValue(entryPtr);

  /* find out the cells represented by the invalid region */
  TableWhatCell (tablePtr, invalidX, invalidY, &rowFrom, &colFrom);
  TableWhatCell (tablePtr, invalidX + invalidWidth - 1,
		 invalidY + invalidHeight - 1, &rowTo, &colTo);

  /* 
   * If colTagCmd has been specified then initialize
   * hash table to cache evaluations.
   */
  if (tablePtr->colTagCmd) {
    colTags = (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
    Tcl_InitHashTable (colTags, TCL_ONE_WORD_KEYS);
  }

  /* Cycle through the cells and display them */
  for (row = rowFrom; row <= rowTo; row++) {
    /* 
     * are we in the 'dead zone' between the
     * title rows and the first displayed row 
     */
    if (row < tablePtr->topRow && row >= tablePtr->titleRows)
      row = tablePtr->topRow;

    /* Cache the row in user terms */
    urow = row+tablePtr->rowOffset;

    /* 
     * Get the row tag.  First check in the row hash table.
     * If NULL, try evaluating the row tag command (if specified).
     */
    if ((entryPtr = Tcl_FindHashEntry (tablePtr->rowStyles,
				       (char *) urow)) == NULL) {
      if (tablePtr->rowTagCmd) {
	/* Since it does not exist, eval command with row/col appended */
	sprintf (buf, " %d", urow);
	if (Tcl_VarEval(interp, tablePtr->rowTagCmd, buf,
			(char *) NULL) == TCL_OK &&
	    interp->result && *interp->result) {
	  /* If a result was returned, check to see if it is a known tag */
	  entryPtr = Tcl_FindHashEntry (tablePtr->tagTable, interp->result);
	}
	Tcl_ResetResult (interp);
      }
    }
    rowPtr = (TagStruct *) (entryPtr ? Tcl_GetHashValue(entryPtr) : NULL);

    for (col = colFrom; col <= colTo; col++) {
      activeCell = 0;
      /* 
       * are we in the 'dead zone' between the
       * title cols and the first displayed col
       */
      if (col < tablePtr->leftCol && col >= tablePtr->titleCols)
	col = tablePtr->leftCol;

      /* Cache the col in user terms */
      ucol = col+tablePtr->colOffset;

      /* put the use cell ref into a buffer for the hash lookups */
      TableMakeArrayIndex(urow, ucol, buf);

      /*
       * get the combined tag structure for the cell
       * first clear out a new tag structure that we will build in
       * then add tags as we realize they belong.
       * Tags with the highest priority are added first
       */
      tagPtr = TableNewTag (tablePtr);

      /* if flash mode is on, is this cell flashing */
      if (tablePtr->flashMode &&
	  Tcl_FindHashEntry (tablePtr->flashCells, buf) != NULL)
	TableMergeTag (tagPtr, flashPtr);
      /* is this cell active? */
      if ((tablePtr->flags & HAS_ACTIVE) && tablePtr->state == tkNormalUid
	  && row == tablePtr->activeRow && col == tablePtr->activeCol) {
	TableMergeTag(tagPtr, activePtr);
	activeCell = 1;
      }
      /* is this cell selected? */
      if (Tcl_FindHashEntry(tablePtr->selCells, buf) != NULL)
	TableMergeTag(tagPtr, selPtr);
      /* Am I in the titles */
      if (row < tablePtr->topRow || col < tablePtr->leftCol)
	TableMergeTag (tagPtr, titlePtr);
      /* Does this have a cell tag */
      if ((entryPtr = Tcl_FindHashEntry (tablePtr->cellStyles, buf)) != NULL)
	TableMergeTag (tagPtr, (TagStruct *) Tcl_GetHashValue (entryPtr));
      /* 
       * Merge rowPtr if it exists
       */
      if (rowPtr != (TagStruct *) NULL)
	TableMergeTag (tagPtr, rowPtr);
      /*
       * Then try the columns (same as for rows).
       * First check in the col hash table.
       * If NULL, try evaluating the col tag command (if specified).
       */
      if ((entryPtr = Tcl_FindHashEntry (tablePtr->colStyles,
					 (char *) ucol)) == NULL) {
	if (tablePtr->colTagCmd &&
	    (entryPtr = Tcl_FindHashEntry (colTags,(char *) ucol)) == NULL) {
	  /* Add an entry into the cache so we don't
	   * have to eval this row/col again */
	  entryPtr = Tcl_CreateHashEntry (colTags, (char *) ucol, &imageW);
	  /* Since it does not exist, eval command with row/col appended */
	  sprintf (buf, " %d", col);
	  if (Tcl_VarEval(interp, tablePtr->colTagCmd, buf,
			  (char *) NULL) == TCL_OK && 
	      interp->result && *interp->result &&
	      (ePtr = Tcl_FindHashEntry (tablePtr->tagTable,interp->result))) {
	    /* If a result was returned, check to see if it is a known tag */
	    /* A valid tag.  Add it into the cache for safe keeping */
	    Tcl_SetHashValue (entryPtr, Tcl_GetHashValue (ePtr));
	  } else {
	    Tcl_SetHashValue (entryPtr, NULL);
	  }
	  Tcl_ResetResult (interp);
	}
      }
      if (entryPtr && (colPtr = (TagStruct *) Tcl_GetHashValue(entryPtr)))
	TableMergeTag (tagPtr, colPtr);

      /* Finally, merge in the default tag */
      TableMergeTag (tagPtr, &(tablePtr->defaultTag));

      tagGc = TableGetGc (tablePtr, tagPtr);

      /* get the coordinates for the cell */
      TableCellCoords (tablePtr, row, col, &x, &y, &width, &height);
      if (tablePtr->drawMode == DRAW_MODE_SLOW) {
	/* Correctly adjust x && y with the offset */
	x -= invalidX;
	y -= invalidY;
      }

      /*
       * first fill in a blank rectangle. This is left as a Tk call instead
       * of a direct X call for Tk compatibilty. The TK_RELIEF_FLAT ensures
       * that only XFillRectangle is called anyway so the speed is the same
       */
      Tk_Fill3DRectangle (tkwin, window, tagPtr->bgBorder,
			  x, y, width, height, bd, TK_RELIEF_FLAT);

      /*
       * If an image is in the tag, draw it and skip the text
       */
      if (tagPtr->image != NULL) {
	Tk_SizeOfImage(tagPtr->image, &imageW, &imageH);
	/* Handle anchoring of image in cell space */
	switch (tagPtr->anchor) {
	case TK_ANCHOR_NW:
	case TK_ANCHOR_W:
	case TK_ANCHOR_SW:	/* western position */
	  originX = imageX = 0;
	  break;
	case TK_ANCHOR_N:
	case TK_ANCHOR_S:
	case TK_ANCHOR_CENTER:	/* centered position */
	  imageX = MAX(0, (imageW-width)/2-bd);
	  originX = MAX(0, (width-imageW)/2);
	  break;
	default:	/* eastern position */
	  imageX = MAX(0, imageW-width-2*bd);
	  originX = MAX(0, width-imageW);
	}
	switch (tagPtr->anchor) {
	case TK_ANCHOR_N:
	case TK_ANCHOR_NE:
	case TK_ANCHOR_NW:	/* northern position */
	  originY = imageY = 0;
	  break;
	case TK_ANCHOR_W:
	case TK_ANCHOR_E:
	case TK_ANCHOR_CENTER:	/* centered position */
	  imageY = MAX(0, (imageH-height)/2-bd);
	  originY = MAX(0, (height-imageH)/2);
	  break;
	default:	/* southern position */
	  imageY = MAX(0, imageH-height-2*bd);
	  originY = MAX(0, height-imageH);
	}
	Tk_RedrawImage(tagPtr->image, imageX, imageY,
		       MIN(imageW, width-originX-2*bd),
		       MIN(imageH, height-originY-2*bd), window,
		       x+originX+bd, y+originY+bd);
	/* Jump to avoid display of the text value */
	goto ImageUsed;
      }

      /* if this is the active cell, use the buffer */
      if (activeCell) {
	value = ENCODE_WSTR(interp, tablePtr->activeBuf);
      } else {
	/* Is there a value in the cell? If so, draw it  */
	value = ENCODE_WSTR(interp, TableGetCellValue(tablePtr, urow, ucol));
      }

      vallen = (int)WSTRLEN(value);
      /* If there is a value, show it */
      if (activeCell || vallen) {
	/* get the dimensions of the string */
#ifdef KANJI
	/* max of 1 necessary to avoid TkWSTextExtents bug for 0 */
        TkWSTextExtents(tagGc, value, MAX(vallen, 1),
			&ascent, &descent, &bbox);
#else
#if (TK_MAJOR_VERSION == 8)
	Tk_GetFontMetrics(tagPtr->fontPtr, &fm);
	bbox.rbearing = Tk_TextWidth (tagPtr->fontPtr, value, vallen);
	/* FIX not 100% accurate, but how can I get it in Tk8.0? */
	bbox.lbearing = 0;
	ascent = fm.ascent;
	descent = fm.descent;
#else
	XTextExtents (tagPtr->fontPtr, value, vallen,
		      &direction, &ascent, &descent, &bbox);
#endif	/* TK8 */
#endif	/* KANJI */

	/* 
	 * Set the origin coordinates of the string to draw using the anchor.
	 * origin represents the (x,y) coordinate of the lower left corner of
	 * the text box, relative to the internal (inside the border) window
	 */

	/* set the X origin first */
	switch (tagPtr->anchor) {
	case TK_ANCHOR_NE:
	case TK_ANCHOR_E:
	case TK_ANCHOR_SE:
	  originX = width - bbox.rbearing - 2 * bd - tablePtr->padX;
	  break;
	case TK_ANCHOR_N:
	case TK_ANCHOR_S:
	case TK_ANCHOR_CENTER:
	  originX = (width - (bbox.rbearing - bbox.lbearing)) / 2 - bd;
	  break;
	default:
	  originX = -bbox.lbearing + tablePtr->padX;
	}

	/* then set the Y origin */
	switch (tagPtr->anchor) {
	case TK_ANCHOR_N:
	case TK_ANCHOR_NE:
	case TK_ANCHOR_NW:
	  originY = ascent + descent + tablePtr->padY;
	  break;
	case TK_ANCHOR_W:
	case TK_ANCHOR_E:
	case TK_ANCHOR_CENTER:
	  originY = height - (height - (ascent + descent)) / 2 - bd;
	  break;
	default:
	  originY = height - 2 * bd - tablePtr->padY;
	}

	/*
	 * if this is the selected cell and we are editing
	 * ensure that the cursor will be displayed
	 */
	if (activeCell) {
#ifdef KANJI
	  int dummy;
	  /* dummy required due to bug for 0 in TkWSTextExtents
	   * its ok because ascent/descent should never change */
          TkWSTextExtents(tagGc, value, MIN(vallen, tablePtr->icursor),
                          &dummy, &dummy, &bbox2);
#else
#if (TK_MAJOR_VERSION == 8)
	  bbox2.width = Tk_TextWidth(tagPtr->fontPtr, value,
				     MIN(vallen, tablePtr->icursor));
#else
	  XTextExtents (tagPtr->fontPtr, value,
			MIN(vallen, tablePtr->icursor),
			&direction, &ascent, &descent, &bbox2);
#endif		/* VERSION 8 Font Change */
#endif
	  originX = MAX(tablePtr->insertWidth-bbox2.width,
			MIN(originX, width - bbox2.width - 2*bd -
			    tablePtr->insertWidth-2 + tablePtr->padX));
	}
	/*
	 * use a clip rectangle only if necessary as it means
	 * updating the GC in the server which slows everything down.
	 */
	if ((clipRectSet = ((originX + bbox.lbearing < -bd)
			    || (originX + bbox.rbearing > width - bd)
			    || (ascent + descent > height)))) {
#ifdef X_WINDOWS
	  /* set the clipping rectangle */
	  clipRect.x = x;
	  clipRect.y = y;
	  clipRect.width = width;
	  clipRect.height = height;
#ifdef KANJI
          /* For Ascii font */
          XSetClipRectangles(display, tagGc->fe[0].gc, 0, 0,
			     &clipRect, 1, Unsorted);
          /* For Kanji font */
          XSetClipRectangles(display, tagGc->fe[1].gc, 0, 0,
			     &clipRect, 1, Unsorted);
#else
	  XSetClipRectangles (display, tagGc, 0, 0, &clipRect, 1, Unsorted);
#endif	/* KANJI */
#else
	  Tk_Fill3DRectangle (tkwin, clipWind, tagPtr->bgBorder, 0, 0,
			      width, height, bd, TK_RELIEF_FLAT);
	  TableDrawString(display, clipWind, tagGc,
			  originX+bd, originY-descent+bd,
			  value, vallen, tagPtr->fontPtr);
#ifdef KANJI
	  XCopyArea(display, clipWind, window, tagGc->fe[0].gc, 0, 0,
		    width, height, x, y);
	  XCopyArea(display, clipWind, window, tagGc->fe[1].gc, 0, 0,
		    width, height, x, y);
#else	/* KANJI */
	  XCopyArea(display, clipWind, window, tagGc, 0, 0,
		    width, height, x, y);
#endif	/* KANJI */
#endif /* X_WINDOWS */
	}

#ifdef X_WINDOWS	/* cliprect only on X Windows */
	if (!clipRectSet)
#endif	/* X_WINDOWS */
	  TableDrawString(display, window, tagGc,
			  x+originX+bd, y+originY-descent+bd,
			  value, vallen, tagPtr->fontPtr);

#ifdef X_WINDOWS	/* cliprect only on X Windows */
	/* reset the clip mask */
	if (clipRectSet) {
#ifdef KANJI
          /* For Ascii font */
          XSetClipMask(display, tagGc->fe[0].gc, None);
          /* For Kanji font */
          XSetClipMask(display, tagGc->fe[1].gc, None);
#else
	  XSetClipMask(display, tagGc, None);
#endif	/* KANJI */
        }
#endif	/* X_WINDOWS */

	/* if this is the active cell draw the cursor if it's on.
	 * this ignores clip rectangles. */
	if (activeCell && (tablePtr->flags & CURSOR_ON)) {
	  /* make sure it will fit in the box */
	  int range = MAX(originY-descent-ascent+bd, 0);
	  Tk_Fill3DRectangle(tkwin, window, tablePtr->insertBg,
			     x+originX+bd+bbox2.width, y+range,
			     tablePtr->insertWidth,
			     (range?ascent+descent:height),
			     tablePtr->insertBorderWidth, TK_RELIEF_FLAT);
	}
      }
#ifdef KANJI
      /* Must free this data because it's allocated by GetWStr */
      FREE_WSTR(value);
#endif

    ImageUsed:
      /* Draw the 3d border on the pixmap correctly offset */
      switch (tablePtr->drawMode) {
      case DRAW_MODE_SLOW:
      case DRAW_MODE_TK_COMPAT:
	Tk_Draw3DRectangle (tkwin, window, tagPtr->bgBorder,
			    x, y, width, height, bd, tagPtr->relief);
	break;
      case DRAW_MODE_FAST:
        /*
        ** choose the GCs to get the best approximation
        ** to the desired drawing style
        */
        switch(tagPtr->relief) {
        case TK_RELIEF_FLAT:
          topGc = bottomGc = Tk_3DBorderGC(tkwin, tagPtr->bgBorder,
					   TK_3D_FLAT_GC);
          break;
        case TK_RELIEF_RAISED:
        case TK_RELIEF_RIDGE:
          topGc    = Tk_3DBorderGC(tkwin, tagPtr->bgBorder, TK_3D_LIGHT_GC);
          bottomGc = Tk_3DBorderGC(tkwin, tagPtr->bgBorder, TK_3D_DARK_GC);
          break;
        default: /* TK_RELIEF_SUNKEN TK_RELIEF_GROOVE */
          bottomGc = Tk_3DBorderGC(tkwin, tagPtr->bgBorder, TK_3D_LIGHT_GC);
          topGc    = Tk_3DBorderGC(tkwin, tagPtr->bgBorder, TK_3D_DARK_GC);
          break;
        }
	
	/* draw a line with no width */
	rect[0].x = x;
	rect[0].y = y + height - 1;
	rect[1].y = -height + 1;
	rect[2].x = width - 1;
	rect[3].x = x + width - 1;
	rect[3].y = y;
	rect[4].y = height - 1;
	rect[5].x = -width + 1;
	XDrawLines (display, window, bottomGc, rect+3, 3, CoordModePrevious);
	XDrawLines (display, window, topGc, rect, 3, CoordModePrevious);
      }

      /* delete the tag structure */
      ckfree ((char *) (tagPtr));
      TableFreeGc(tablePtr, tagGc);
    }
  }
#ifndef X_WINDOWS
  Tk_FreePixmap (display, clipWind);
#endif

  /* 
   * if we have got to the end of the table, 
   * clear the area after the last row/col
   */
  TableCellCoords (tablePtr, tablePtr->rows, tablePtr->cols,
		   &x, &y, &width, &height);
  if (tablePtr->drawMode == DRAW_MODE_SLOW) {
    /* Correctly adjust x && y with the offset */
    x -= invalidX;
    y -= invalidY;
  }
  if (x + width < invalidX + invalidWidth - 1) {
#ifdef X_WINDOWS
    if (tablePtr->drawMode != DRAW_MODE_SLOW)
      XClearArea (display, window, x + width, invalidY,
		  invalidX + invalidWidth - x - width, invalidHeight, False);
    else
#endif
      /* we are using a pixmap, so use TK to clear it */
      Tk_Fill3DRectangle (tkwin, window,
			  tablePtr->defaultTag.bgBorder, x+width, 0,
			  invalidX+invalidWidth-x-width, invalidHeight,
			  0, TK_RELIEF_FLAT);
  }

  if (y + height < invalidY + invalidHeight - 1) {
#ifdef X_WINDOWS
    if (tablePtr->drawMode != DRAW_MODE_SLOW)
      XClearArea (display, window, invalidX, y + height, invalidWidth,
		  invalidY + invalidHeight - y - height, False);
    else
#endif
      /* we are using a pixmap, so use TK to clear it */
      Tk_Fill3DRectangle (tkwin, window,
			  tablePtr->defaultTag.bgBorder, 0, y+height,
			  invalidWidth, invalidY+invalidHeight-y-height,
			  0, TK_RELIEF_FLAT);
  }

  /* copy over and delete the pixmap if we are in slow mode */
  if (tablePtr->drawMode == DRAW_MODE_SLOW) {
    /* Get a default GC */
    tagGc = TableGetGc (tablePtr, &(tablePtr->defaultTag));
#ifdef KANJI
    XCopyArea(display, window, Tk_WindowId(tkwin), tagGc->fe[0].gc, 0, 0,
              invalidWidth, invalidHeight, invalidX, invalidY);
    XCopyArea(display, window, Tk_WindowId(tkwin), tagGc->fe[1].gc, 0, 0,
              invalidWidth, invalidHeight, invalidX, invalidY);
#else
    XCopyArea(display, window, Tk_WindowId(tkwin), tagGc, 0, 0,
	      invalidWidth, invalidHeight, invalidX, invalidY);
#endif /* KANJI */
    Tk_FreePixmap (display, window);
    TableFreeGc(tablePtr, tagGc);
  }
  if ((tablePtr->flags & REDRAW_BORDER) && tablePtr->highlightWidth > 0) {
    GC gc = Tk_GCForColor ((tablePtr->flags & HAS_FOCUS)
			   ? (tablePtr->highlightColorPtr)
		    : (tablePtr->highlightBgColorPtr), Tk_WindowId(tkwin));
    tablePtr->flags &= ~REDRAW_BORDER;
    Tk_DrawFocusHighlight (tkwin, gc, tablePtr->highlightWidth,
			   Tk_WindowId(tkwin));
  }
  /* 
   * If colTagCmd was specified then free up
   * the hash table that were used to cache evaluations.
   */
  if (tablePtr->colTagCmd) {
    Tcl_DeleteHashTable (colTags);
    ckfree ((char *) (colTags));
  }
}

/* 
 * Invalidates a rectangle and adds it to the total invalid rectangle
 * waiting to be redrawn if the force flag is set, does an update
 * instantly else waits until Tk is idle
 */

static void
TableInvalidate(Table * tablePtr, int x, int y,
		int width, int height, int force)
{
  int hl;

  /* avoid allocating 0 sized pixmaps which would be fatal */
  if (width <= 0 || height <= 0) return;

  /* make sure that the window hasn't been destroyed already */
  if (tablePtr->tkwin == NULL) return;

  /* If not even mapped, ignore */
  if (!Tk_IsMapped(tablePtr->tkwin)) return;

  /* is this rectangle even on the screen */
  if ((x > Tk_Width(tablePtr->tkwin)) || (y > Tk_Height(tablePtr->tkwin)))
    return;

  /* if no pending updates then replace the rectangle, otherwise find the
    bounding rectangle */

  hl=tablePtr->highlightWidth;
  if (x<hl || y<hl || x+width>=Tk_Width(tablePtr->tkwin)-hl ||
     y+height>=Tk_Height(tablePtr->tkwin)-hl) 
    tablePtr->flags |= REDRAW_BORDER;

  if (tablePtr->flags & REDRAW_PENDING) {
    tablePtr->invalidWidth  = MAX(tablePtr->invalidX+tablePtr->invalidWidth,
				  x + width);
    tablePtr->invalidHeight = MAX(tablePtr->invalidY+tablePtr->invalidHeight,
				  y + height);
    if (tablePtr->invalidX > x) tablePtr->invalidX = x;
    if (tablePtr->invalidY > y) tablePtr->invalidY = y;
    tablePtr->invalidWidth  -= tablePtr->invalidX;
    tablePtr->invalidHeight -= tablePtr->invalidY;
    /* are we forcing this update out */
    if (force && !(tablePtr->batchMode)) {
      Tcl_CancelIdleCall (TableDisplay, (ClientData) tablePtr);
      TableDisplay ((ClientData) tablePtr);
    }
  } else {
    tablePtr->invalidX = x;
    tablePtr->invalidY = y;
    tablePtr->invalidWidth = width;
    tablePtr->invalidHeight = height;
    if (force && !(tablePtr->batchMode)) {
      TableDisplay ((ClientData) tablePtr);
    } else {
      tablePtr->flags |= REDRAW_PENDING;
      Tcl_DoWhenIdle (TableDisplay, (ClientData) tablePtr);
    }
  }
}

/*
 * Called when the flash timer goes off decrements all the entries in the
 * hash table and invalidates any cells that expire, deleting them from the
 * table if the table is now empty, stops the timer else reenables it
 */
static void
TableFlashEvent(ClientData clientdata)
{
  Table *tablePtr = (Table *) clientdata;
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  int entries, count, row, col, x, y, width, height;

  entries = 0;
  for (entryPtr = Tcl_FirstHashEntry (tablePtr->flashCells, &search);
       entryPtr != NULL; entryPtr = Tcl_NextHashEntry (&search)) {
    count = (int) Tcl_GetHashValue (entryPtr);
    if (--count <= 0) {
      /* get the cell address and invalidate that region only */
      if (TableParseArrayIndex(&row, &col, Tcl_GetHashKey(tablePtr->flashCells,
							  entryPtr)) != 2)
	return;

      /* delete the entry from the table */
      Tcl_DeleteHashEntry (entryPtr);

      TableCellCoords(tablePtr, row-tablePtr->rowOffset,
		       col-tablePtr->colOffset, &x, &y, &width, &height);
      TableInvalidate(tablePtr, x, y, width, height, 1);
    } else {
      Tcl_SetHashValue(entryPtr, (ClientData) count);
      entries++;
    }
  }

  /* do I need to restart the timer */
  if (entries && tablePtr->flashMode)
    tablePtr->flashTimer = Tcl_CreateTimerHandler (250, TableFlashEvent,
						   (ClientData) tablePtr);
  else
    tablePtr->flashTimer = 0;
}

/*
 * adds a flash to the list with the default timeout.
 * Only adds it if flashing is enabled and flashtime > 0
 * row/col come in in real coords
 */
static void
TableAddFlash(Table *tablePtr, int row, int col)
{
  char buf[INDEX_BUFSIZE];
  int dummy;
  Tcl_HashEntry *entryPtr;

  if (!tablePtr->flashMode || tablePtr->flashTime < 1)
    return;

  /* create the array index in user coords */
  TableMakeArrayIndex(row+tablePtr->rowOffset, col+tablePtr->colOffset, buf);

  /* add the flash to the hash table */
  entryPtr = Tcl_CreateHashEntry (tablePtr->flashCells, buf, &dummy);
  Tcl_SetHashValue (entryPtr, tablePtr->flashTime);

  /* now set the timer if it's not already going and invalidate the area */
  if (tablePtr->flashTimer == NULL)
    tablePtr->flashTimer = Tcl_CreateTimerHandler (250, TableFlashEvent,
						   (ClientData) tablePtr);
}

/*
 * TableVarProc --
 *
 * This is the trace procedure associated with the Tcl array.
 * No validation will occur here because this only triggers when the
 * array value is directly set, and we can't maintain the old value.
 */
static char *
TableVarProc(clientData, interp, name, index, flags)
    ClientData clientData;	/* Information about table. */
    Tcl_Interp *interp;		/* Interpreter containing variable. */
    char *name;			/* Not used. */
    char *index;		/* Not used. */
    int flags;			/* Information about what happened. */
{
  Table *tablePtr = (Table *) clientData;
  int x, y, width, height, row, col;

  /* This is redundant, as the name should always == arrayVar */
  name = tablePtr->arrayVar;

  /* is this the whole var being destroyed or just one cell being deleted */
  if ((flags & TCL_TRACE_UNSETS) && index == NULL) {
    /* if this isn't the interpreter being destroyed reinstate the trace */
    if ((flags & TCL_TRACE_DESTROYED) && !(flags & TCL_INTERP_DESTROYED)) {
      Tcl_SetVar2 (interp, name, TEST_KEY, "", TCL_GLOBAL_ONLY);
      Tcl_UnsetVar2 (interp, name, TEST_KEY, TCL_GLOBAL_ONLY);
      Tcl_ResetResult (interp);

      /* set a trace on the variable */
      Tcl_TraceVar (interp, name,
		    TCL_TRACE_WRITES | TCL_TRACE_UNSETS | TCL_GLOBAL_ONLY,
		    (Tcl_VarTraceProc *)TableVarProc, (ClientData) tablePtr);

      /* only do the following if arrayVar is our data source */
      if (tablePtr->dataSource & DATA_ARRAY) {
	/* clear the selection buffer */
	TableGetActiveBuf (tablePtr);
	/* flush any cache */
	TableFlushCache(tablePtr);
	/* and invalidate the table */
	TableInvalidateAll(tablePtr, 0);
      }
    }
    return (char *) NULL;
  }
  /* only continue if arrayVar is our data source */
  if (!(tablePtr->dataSource & DATA_ARRAY)) {
    return (char *) NULL;
  }
  /* get the cell address and invalidate that region only.
   * Make sure that it is a valid cell address. */
  if (strcmp("active", index) == 0) {
    row = tablePtr->activeRow;
    col = tablePtr->activeCol;
    if (!(tablePtr->flags & SET_ACTIVE)) {
      /* modified TableGetActiveBuf */
      char *data = "";

      if (tablePtr->flags & HAS_ACTIVE)
	data = Tcl_GetVar2(interp, name, index, TCL_GLOBAL_ONLY);
      if (!data) data = "";

      if (strcmp(tablePtr->activeBuf, data) == 0) {
	return (char *) NULL;
      }
      tablePtr->activeBuf = (char *)ckrealloc(tablePtr->activeBuf,
					      strlen(data)+1);
      strcpy(tablePtr->activeBuf, data);
      TableGetIcursor(tablePtr, "end", (int *)0);
      tablePtr->flags |= TEXT_CHANGED;
    }
  } else if (TableParseArrayIndex(&row, &col, index) == 2) {
    char buf[INDEX_BUFSIZE];
    /* Make sure it won't trigger on array(2,3extrastuff) */
    TableMakeArrayIndex(row, col, buf);
    if (strcmp(buf, index)) {
      return (char *) NULL;
    }
    if (tablePtr->caching) {
      Tcl_HashEntry *entryPtr;
      char *val, *data = NULL;

      data = Tcl_GetVar2(interp, name, index, TCL_GLOBAL_ONLY);
      if (!data) data = "";
      val = (char *)ckalloc(strlen(data)+1);
      strcpy(val, data);
      entryPtr = Tcl_CreateHashEntry(tablePtr->cache, buf, &width);
      Tcl_SetHashValue(entryPtr, val);
    }
    /* convert index to real coords */
    row -= tablePtr->rowOffset;
    col -= tablePtr->colOffset;
    /* did the active cell just update */
    if (row == tablePtr->activeRow && col == tablePtr->activeCol)
      TableGetActiveBuf (tablePtr);
    /* Flash the cell */
    TableAddFlash(tablePtr, row, col);
  } else {
    return (char *) NULL;
  }

  if (Tk_IsMapped(tablePtr->tkwin)) {
    TableCellCoords(tablePtr, row, col, &x, &y, &width, &height);
    TableInvalidate(tablePtr, x, y, width, height, 1);
  }
  return (char *) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * TableGeometryRequest --
 *
 *----------------------------------------------------------------------
 */
static void
TableGeometryRequest(tablePtr)
     register Table *tablePtr;
{
  int x, y, hi = 2*tablePtr->highlightWidth;

  /* Do the geometry request
   * If -width #cols was not specified or it is greater than the real
   * number of cols, use maxWidth as a lower bound, with the other lower
   * bound being the upper bound of the window's user-set width and the
   * value of -maxwidth set by the programmer
   * Vice versa for rows/height
   */
  x = MIN(((tablePtr->maxReqCols == 0 ||
	    tablePtr->maxReqCols > tablePtr->cols) ? tablePtr->maxWidth :
	   (tablePtr->colStarts)[tablePtr->maxReqCols]) + hi,
	  MAX(Tk_Width(tablePtr->tkwin), tablePtr->maxReqWidth+hi));
  y = MIN(((tablePtr->maxReqRows == 0 ||
	    tablePtr->maxReqRows > tablePtr->rows) ? tablePtr->maxHeight :
	   (tablePtr->rowStarts)[tablePtr->maxReqRows]) + hi,
	  MAX(Tk_Height(tablePtr->tkwin), tablePtr->maxReqHeight+hi));
  Tk_GeometryRequest(tablePtr->tkwin, x, y);
}

/*
 *----------------------------------------------------------------------
 *
 * TableAdjustParams --
 *
 * Calculate the row and column starts.  Adjusts the topleft corner variable
 * to keep it within the screen range, out of the titles and keep the screen
 * full make sure the selected cell is in the visible area checks to see if
 * the top left cell has changed at all and invalidates the table if it has
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Number of rows can change if -rowstretchmode == fill.
 *	topRow && leftCol can change to fit display.
 *	activeRow/Col can change to ensure it is an editable cell.
 *
 *----------------------------------------------------------------------
 */

static void
TableAdjustParams(tablePtr)
     register Table *tablePtr;		/* Widget record for table */
{
  int topRow, leftCol, total;
  int i, x, y, width, height;
  int diff, colPad, unpresetCols, lastUnpresetCol, lastColPad, rowPad,
    unpresetRows, lastUnpresetRow, lastRowPad, setRowPixels, setColPixels;
  Tcl_HashEntry *entryPtr;
  int bd, recalc = 0;

  /* cache the borderwidth (doubled) for many upcoming calculations */
  bd = 2 * tablePtr->borderWidth;

  /* Set up the arrays to hold the col pixels and starts */
  if (tablePtr->colPixels) ckfree ((char *) (tablePtr->colPixels));
  tablePtr->colPixels = (int *) ckalloc (tablePtr->cols * sizeof (int));
  if (tablePtr->colStarts) ckfree ((char *) (tablePtr->colStarts));
  tablePtr->colStarts = (int *) ckalloc ((tablePtr->cols+1) * sizeof (int));

  /* get all the preset columns and set their widths */
  lastUnpresetCol = 0;
  setColPixels = 0;
  unpresetCols = 0;
  for (i = 0; i < tablePtr->cols; i++) {
    if ((entryPtr = Tcl_FindHashEntry(tablePtr->colWidths,
				      (char *) i)) == NULL) {
      (tablePtr->colPixels)[i] = -1;
      unpresetCols++;
      lastUnpresetCol = i;
    } else {
      (tablePtr->colPixels)[i] = (int) Tcl_GetHashValue (entryPtr) *
	(tablePtr->charWidth) + bd;
      setColPixels += (tablePtr->colPixels)[i];
    }
  }

  /* work out how much to pad each col depending on the mode */
  diff = Tk_Width(tablePtr->tkwin) - setColPixels - 2*tablePtr->highlightWidth
    - (unpresetCols * ((tablePtr->charWidth)*(tablePtr->defColWidth) + bd));
  /* diff lower than 0 means we can't see the entire set of columns */
  if (diff < 0) diff = 0;
  switch(tablePtr->colStretch) {
  case STRETCH_MODE_NONE:
    colPad = 0;
    lastColPad = 0;
    break;
  case STRETCH_MODE_UNSET:
    if (unpresetCols == 0) {
      colPad = 0;
      lastColPad = 0;
    } else {
      colPad = diff / unpresetCols;
      lastColPad = diff - colPad * (unpresetCols - 1);
    }
    break;
  case STRETCH_MODE_LAST:
    colPad = 0;
    lastColPad = diff;
    lastUnpresetCol = tablePtr->cols - 1;
    break;
  default:	/* STRETCH_MODE_ALL, but also FILL for cols */
    colPad = diff / (tablePtr->cols);
    /* force it to be applied to the last column too */
    lastUnpresetCol = tablePtr->cols - 1;
    lastColPad = diff - colPad * lastUnpresetCol;
  }

  /* now do the padding and calculate the column starts */
  total = 0;
  for (i = 0; i < tablePtr->cols; i++) {
    if ((tablePtr->colPixels)[i] == -1)
      (tablePtr->colPixels)[i] = tablePtr->charWidth * tablePtr->defColWidth
	+ bd + ((i==lastUnpresetCol)?lastColPad:colPad);
    else if (tablePtr->colStretch == STRETCH_MODE_ALL)
      (tablePtr->colPixels)[i] += (i==lastUnpresetCol)?lastColPad:colPad;
    total = (((tablePtr->colStarts)[i] = total) + (tablePtr->colPixels)[i]);
  }
  (tablePtr->colStarts)[i] = tablePtr->maxWidth = total;

  /*
   * The 'do' loop is only necessary for rows because cols do not
   * currently support FILL mode
   */
  do {
    /* Set up the arrays to hold the row pixels and starts */
    /* FIX - this can be moved outside 'do' if you check >row size */
    if (tablePtr->rowPixels) ckfree ((char *) (tablePtr->rowPixels));
    tablePtr->rowPixels = (int *) ckalloc (tablePtr->rows * sizeof (int));
    if (tablePtr->rowStarts) ckfree ((char *) (tablePtr->rowStarts));
    tablePtr->rowStarts = (int *) ckalloc ((tablePtr->rows+1)* sizeof (int));

    /* get all the preset rows and set their heights */
    lastUnpresetRow = 0;
    setRowPixels = 0;
    unpresetRows = 0;
    for (i = 0; i < tablePtr->rows; i++) {
      if ((entryPtr = Tcl_FindHashEntry(tablePtr->rowHeights,
					(char *) i)) == NULL) {
	(tablePtr->rowPixels)[i] = -1;
	unpresetRows++;
	lastUnpresetRow = i;
      } else {
	(tablePtr->rowPixels)[i] = (int) Tcl_GetHashValue (entryPtr) + bd;
	setRowPixels += (tablePtr->rowPixels)[i];
      }
    }

    /* work out how much to pad each row depending on the mode */
    diff = Tk_Height(tablePtr->tkwin)-setRowPixels-2*tablePtr->highlightWidth
      - (unpresetRows * (tablePtr->defRowHeight+bd));
    switch(tablePtr->rowStretch) {
    case STRETCH_MODE_NONE:
      rowPad = 0;
      lastRowPad = 0;
      break;
    case STRETCH_MODE_UNSET:
      if (unpresetRows == 0)  {
	rowPad = 0;
	lastRowPad = 0;
      } else {
	rowPad = MAX(0,diff) / unpresetRows;
	lastRowPad = MAX(0,diff) - rowPad * (unpresetRows - 1);
      }
      break;
    case STRETCH_MODE_LAST:
      rowPad = 0;
      lastRowPad = MAX(0,diff);
      /* force it to be applied to the last column too */
      lastUnpresetRow = tablePtr->rows - 1;
      break;
    case STRETCH_MODE_FILL:
      rowPad = 0;
      lastRowPad = diff;
      if (diff && !recalc) {
	tablePtr->rows += (diff/(tablePtr->defRowHeight + bd));
	if (diff < 0 && tablePtr->rows < 0)
	  tablePtr->rows = 0;
	lastUnpresetRow = tablePtr->rows - 1;
	recalc = 1;
	continue;
      } else {
	lastUnpresetRow = tablePtr->rows - 1;
	recalc = 0;
      }
      break;
    default:	/* STRETCH_MODE_ALL */
      rowPad = MAX(0,diff) / (tablePtr->rows);
      /* force it to be applied to the last column too */
      lastUnpresetRow = tablePtr->rows - 1;
      lastRowPad = MAX(0,diff) - rowPad * lastUnpresetRow;
    }

    /* now do the padding and calculate the row starts */
    total = 0;
    for (i = 0; i < tablePtr->rows; i++) {
      if ((tablePtr->rowPixels)[i] == -1)
	(tablePtr->rowPixels)[i] = tablePtr->defRowHeight + bd
	  + ((i==lastUnpresetRow)?lastRowPad:rowPad);
      else if (tablePtr->colStretch == STRETCH_MODE_ALL)
	(tablePtr->colPixels)[i] += (i==lastUnpresetRow)?lastRowPad:rowPad;

      /* calculate the start of each row */
      total = (((tablePtr->rowStarts)[i] = total) + (tablePtr->rowPixels)[i]);
    }
    (tablePtr->rowStarts)[i] = tablePtr->maxHeight = total;
  } while (recalc);

  /* make sure the top row and col have reasonable real indices */
  tablePtr->topRow = topRow =
    MAX(tablePtr->titleRows, MIN(tablePtr->topRow, tablePtr->rows-1));
  tablePtr->leftCol = leftCol =
    MAX(tablePtr->titleCols, MIN(tablePtr->leftCol, tablePtr->cols-1));

  if (tablePtr->flags & HAS_ACTIVE) {
    /* make sure the active cell has a reasonable real index */
    tablePtr->activeRow = MAX(tablePtr->titleRows,
			       MIN(tablePtr->activeRow, tablePtr->rows-1));
    tablePtr->activeCol = MAX(tablePtr->titleCols,
			       MIN(tablePtr->activeCol, tablePtr->cols-1));
  }

  /* If we dont have the info, dont bother to fix up the other parameters */
  if (Tk_WindowId(tablePtr->tkwin) == None) {
    tablePtr->oldTopRow = tablePtr->oldLeftCol = -1;
    return;
  }

  /* 
   * If we use this value of topRow, will we fill the window?
   * if not, decrease it until we will, or until it gets to titleRows 
   * make sure we don't cut off the bottom row
   */
  for (; topRow > tablePtr->titleRows; topRow--)
    if ((tablePtr->maxHeight - ((tablePtr->rowStarts)[topRow-1] -
				(tablePtr->rowStarts)[tablePtr->titleRows])) >
	Tk_Height (tablePtr->tkwin))
      break;
  /* 
   * If we use this value of topCol, will we fill the window?
   * if not, decrease it until we will, or until it gets to titleCols 
   * make sure we don't cut off the left column
   */
  for (; leftCol > tablePtr->titleCols; leftCol--)
    if ((tablePtr->maxWidth - ((tablePtr->colStarts)[leftCol-1] -
			       (tablePtr->colStarts)[tablePtr->titleCols])) >
	Tk_Width(tablePtr->tkwin))
      break;

  tablePtr->topRow = topRow;
  tablePtr->leftCol = leftCol;

  /*
   * Do we have scrollbars, if so, calculate and call the TCL functions In
   * order to get the scrollbar to be completely full when the whole screen
   * is shown and there are titles, we have to arrange for the scrollbar
   * range to be 0 -> rows-titleRows etc.  This leads to the position
   * setting methods, toprow and leftcol, being relative to the titles, not
   * absolute row and column numbers.
   */
  if (tablePtr->yScrollCmd != NULL || tablePtr->xScrollCmd != NULL) {
    int lastCol, lastRow;
    char buf[INDEX_BUFSIZE];
    double first, last;
    /* Now work out where the bottom right for scrollbar update */
    TableWhatCell(tablePtr,
		  Tk_Width(tablePtr->tkwin)-2*tablePtr->highlightWidth,
		  Tk_Height(tablePtr->tkwin)-2*tablePtr->highlightWidth,
		   &lastRow, &lastCol);

    /* Do we have a Y-scrollbar and rows to scroll? */
    if (tablePtr->yScrollCmd != NULL &&
	tablePtr->rows != tablePtr->titleRows) {
      last = (double)(tablePtr->rows-tablePtr->titleRows);
      first = (topRow-tablePtr->titleRows) / last;
      last = (MAX(topRow - tablePtr->titleRows + 1,
		  lastRow - tablePtr->titleRows)+1) / last;
      sprintf (buf, " %g %g", first, last);
      if (Tcl_VarEval(tablePtr->interp, tablePtr->yScrollCmd,
		      buf, (char *) NULL) != TCL_OK) {
	Tcl_AddErrorInfo(tablePtr->interp,
		     "\n(error executing yscroll command in table widget)");
	Tcl_BackgroundError(tablePtr->interp);
      }
    }
    /* Do we have a X-scrollbar and cols to scroll? */
    if (tablePtr->xScrollCmd != NULL &&
	tablePtr->cols != tablePtr->titleCols) {
      last = (double)(tablePtr->cols-tablePtr->titleCols);
      first = (leftCol-tablePtr->titleCols) / last;
      last = (MAX(leftCol - tablePtr->titleCols + 1,
		  lastCol - tablePtr->titleCols)+1) / last;
      sprintf (buf, " %g %g", first, last);
      if (Tcl_VarEval(tablePtr->interp, tablePtr->xScrollCmd,
		      buf, (char *) NULL) != TCL_OK) {
	Tcl_AddErrorInfo(tablePtr->interp,
		     "\n(error executing xscroll command in table widget)");
	Tcl_BackgroundError(tablePtr->interp);
      }
    }
  }

  /*
   * now check the new value of active cell against the original,
   * If it changed, invalidate the area, else leave it alone
   */
  if (tablePtr->oldActRow != tablePtr->activeRow ||
      tablePtr->oldActCol != tablePtr->activeCol) {
    /* put the value back in the cell */
    if (tablePtr->oldActRow >= 0 && tablePtr->oldActCol >= 0) {
      /* 
       * Set the value of the old active cell to the active buffer
       * SetCellValue will check if the value actually changed
       */
      if (tablePtr->flags & TEXT_CHANGED) {
	/* FIX could make sure the value _really_ changed */
	/* FIX an outside trace will be triggered here and if it
	 * calls something that causes TableAdjustParams to be called
	 * again, we are in data consistency trouble */
	/* HACK - we turn TEXT_CHANGED off now to possibly avoid the
	 * above data inconsistency problem.  Needs testing */
	tablePtr->flags &= ~TEXT_CHANGED;
	TableSetCellValue(tablePtr, tablePtr->oldActRow+tablePtr->rowOffset,
			  tablePtr->oldActCol+tablePtr->colOffset,
			  tablePtr->activeBuf);
      }
      TableCellCoords(tablePtr, tablePtr->oldActRow, tablePtr->oldActCol,
		       &x, &y, &width, &height);
      /* invalidate the old active cell */
      TableInvalidate(tablePtr, x, y, width, height, 0);
    }

    /* get the new value of the active cell into buffer */
    TableGetActiveBuf (tablePtr);

    /* invalidate the new active cell */
    TableCellCoords(tablePtr, tablePtr->activeRow, tablePtr->activeCol,
		     &x, &y, &width, &height);
    TableInvalidate(tablePtr, x, y, width, height, 1);
    /* set the old active row/col for the next time this function is called */
    tablePtr->oldActRow = tablePtr->activeRow;
    tablePtr->oldActCol = tablePtr->activeCol;
  }
  /*
   * now check the new value of topleft cell against the originals,
   * If they changed, invalidate the area, else leave it alone
   */
  if (tablePtr->topRow != tablePtr->oldTopRow ||
      tablePtr->leftCol != tablePtr->oldLeftCol) {
    TableInvalidateAll(tablePtr, 0);
    /* set the old top row/col for the next time this function is called */
    tablePtr->oldTopRow = tablePtr->topRow;
    tablePtr->oldLeftCol = tablePtr->leftCol;
  }
}

/*
 * Toggle the cursor status 
 *
 * Equivalent to EntryBlinkProc
 */
static void
TableCursorEvent (ClientData clientData)
{
  register Table *tablePtr = (Table *) clientData;
  int x, y, width, height;

  if (!(tablePtr->flags & HAS_FOCUS) || (tablePtr->insertOffTime == 0)) {
    return;
  }
  if (tablePtr->cursorTimer != NULL)
    Tcl_DeleteTimerHandler(tablePtr->cursorTimer);
  tablePtr->cursorTimer =
    Tcl_CreateTimerHandler((tablePtr->flags & CURSOR_ON) ?
			   tablePtr->insertOffTime : tablePtr->insertOnTime,
			   TableCursorEvent, (ClientData) tablePtr);
  /* Toggle the cursor */
  tablePtr->flags ^= CURSOR_ON;

  /* invalidate the cell */
  TableCellCoords(tablePtr, tablePtr->activeRow, tablePtr->activeCol,
		   &x, &y, &width, &height);
  TableInvalidate(tablePtr, x, y, width, height, 1);
}

/*
 * Configures the timer depending on the state of the table.
 *
 * Equivalent to EntryFocusProc
 */
static void
TableConfigCursor(tablePtr)
     register Table *tablePtr;	/* Information about widget */
{
  int x, y, width, height;

  /* to get a cursor, we have to have focus and allow */
  if ((tablePtr->flags & HAS_FOCUS) &&
      (tablePtr->state == tkNormalUid)) {
    /* turn the cursor on */
    tablePtr->flags |= CURSOR_ON;

    /* set up the first timer */
    if (tablePtr->insertOffTime != 0)
      tablePtr->cursorTimer = Tcl_CreateTimerHandler(tablePtr->insertOnTime,
						     TableCursorEvent,
						     (ClientData) tablePtr);
  } else {
    /* turn the cursor off */
    tablePtr->flags &= ~CURSOR_ON;

    /* and disable the timer */
    if (tablePtr->cursorTimer != NULL)
      Tcl_DeleteTimerHandler (tablePtr->cursorTimer);
    tablePtr->cursorTimer = NULL;
  }

  /* Invalidate the selection window to show or hide the cursor */
  TableCellCoords(tablePtr, tablePtr->activeRow, tablePtr->activeCol,
		   &x, &y, &width, &height);
  TableInvalidate(tablePtr, x, y, width, height, 1);
}

/* 
 *----------------------------------------------------------------------
 *
 * TableCleanupTag --
 *
 * this releases the resources used by a tag before it is freed up.
 *
 *----------------------------------------------------------------------
 */

static void
TableCleanupTag(Table * tablePtr, TagStruct * tagPtr)
{
  if (tagPtr->image)
    Tk_FreeImage(tagPtr->image);
  /* free the options in the widget */
  Tk_FreeOptions (tagConfig, (char *) tagPtr, tablePtr->display, 0);
}

/*
 *--------------------------------------------------------------
 *
 * TableGetIndex --
 *
 *	Parse an index into a table and return either its value
 *	or an error.
 *
 * Results:
 *	A standard Tcl result.  If all went well, then *row,*col is
 *	filled in with the index corresponding to string.  If an
 *	error occurs then an error message is left in interp->result.
 *	The index returned is in user coords.
 *
 * Side effects:
 *	Sets row,col index to an appropriately constrained user index.
 *
 *--------------------------------------------------------------
 */

static int
TableGetIndex(tablePtr, str, row_p, col_p)
     register Table *tablePtr;	/* Table for which the index is being
				 * specified. */
     char *str;			/* Symbolic specification of cell in table. */
     int *row_p;		/* Where to store converted row. */
     int *col_p;		/* Where to store converted col. */
{
  int r, c, len = strlen(str);

  /*
   * Note that all of these values will be adjusted by row/ColOffset
   */
  if (str[0] == '@') {				/* @x,y coordinate */ 
    int x, y;
    char *p, *end;

    p = str+1;
    x = strtol(p, &end, 0);
    if ((end == p) || (*end != ','))
      goto IndexError;
    p = end+1;
    y = strtol(p, &end, 0);
    if ((end == p) || (*end != '\0'))
      goto IndexError;
    TableWhatCell(tablePtr, x, y, &r, &c);
    r += tablePtr->rowOffset;
    c += tablePtr->colOffset;
  } else if (sscanf(str, "%d,%d", &r,&c) == 2) {
    char buf[INDEX_BUFSIZE];
    TableMakeArrayIndex(r, c, buf);
    /* Make sure it won't work for "2,3extrastuff" */
    if (strcmp(buf, str))
      goto IndexError;
    /* ensure appropriate user index */
    r = MIN(MAX(tablePtr->rowOffset,r),tablePtr->rows-1+tablePtr->rowOffset);
    c = MIN(MAX(tablePtr->colOffset,c),tablePtr->cols-1+tablePtr->colOffset);
  } else if (len > 1 && strncmp(str, "active", len) == 0 ) {	/* active */
    if (tablePtr->flags & HAS_ACTIVE) {
      r = tablePtr->activeRow+tablePtr->rowOffset;
      c = tablePtr->activeCol+tablePtr->colOffset;
    } else {
      Tcl_AppendResult(tablePtr->interp, "no \"active\" cell in table", NULL);
      return TCL_ERROR;
    }
  } else if (len > 1 && strncmp(str, "anchor", len) == 0) {	/* anchor */
    if (tablePtr->flags & HAS_ANCHOR) {
      r = tablePtr->anchorRow+tablePtr->rowOffset;
      c = tablePtr->anchorCol+tablePtr->colOffset;
    } else {
      Tcl_AppendResult(tablePtr->interp, "no \"anchor\" cell in table", NULL);
      return TCL_ERROR;
    }
  } else if (strncmp(str, "end", len) == 0) {		/* end */
    r = tablePtr->rows-1+tablePtr->rowOffset;
    c = tablePtr->cols-1+tablePtr->colOffset;
  } else if (strncmp(str, "origin", len) == 0) {	/* origin */
    r = tablePtr->titleRows+tablePtr->rowOffset;
    c = tablePtr->titleCols+tablePtr->colOffset;
  } else if (strncmp(str, "topleft", len) == 0) {	/* topleft */
    r = tablePtr->topRow+tablePtr->rowOffset;
    c = tablePtr->leftCol+tablePtr->colOffset;
  } else if (strncmp(str, "bottomright", len) == 0) {	/* bottomright */
    TableWhatCell(tablePtr,
		  Tk_Width(tablePtr->tkwin)-2*tablePtr->highlightWidth,
		  Tk_Height(tablePtr->tkwin)-2*tablePtr->highlightWidth,
		  &r, &c);
    r += tablePtr->rowOffset;
    c += tablePtr->colOffset;
  } else {
  IndexError:
    Tcl_AppendResult(tablePtr->interp, "bad table index \"",
		     str, "\"", (char *)NULL);
    return TCL_ERROR;
  }

  /* Note: values are expected to be properly constrained 
   * as a user index by this point */
  if ( row_p ) *row_p = r;
  if ( col_p ) *col_p = c;
  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TableFetchSelectionAux --
 *
 *	This aux procedure is used to handle the minor difference
 *	between FetchSelection and FetchSelectionCtext.
 *
 *----------------------------------------------------------------------
 */

static int
TableFetchSelectionAux(cData, offset, buffer, maxBytes, ctext)
    ClientData cData;	/* Information about table widget. */
    int offset;		/* Offset within selection of first
			 * character to be returned. */
    char *buffer;	/* Location in which to place selection. */
    int maxBytes;	/* Maximum number of bytes to place at buffer,
			 * not including terminating NULL character. */
    int ctext;		/* Specifies whether Ctext should be output. (KANJI) */
{
  register Table *tablePtr = (Table *) cData;
  Tcl_Interp *interp = tablePtr->interp;
  char *value, *rowsep = tablePtr->rowSep, *colsep = tablePtr->colSep;
  Tcl_DString selection;
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  int length, count, lastrow=0, needcs=0, r, c, listArgc, rslen=0, cslen=0;
  int numcols, numrows;
  char **listArgv;
  char *data;
  WCHAR *wstr;

  /* if we are not exporting the selection || we have no data source, return */
  if (!tablePtr->exportSelection ||
      (tablePtr->dataSource == DATA_NONE)) {
    return -1;
  }

  /* First get a sorted list of the selected elements */
  Tcl_DStringInit(&selection);
  for (entryPtr = Tcl_FirstHashEntry(tablePtr->selCells, &search);
       entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
    Tcl_DStringAppendElement(&selection,
			     Tcl_GetHashKey(tablePtr->selCells, entryPtr));
  }
  value = TableCellSort(tablePtr, Tcl_DStringValue(&selection));
  Tcl_DStringFree(&selection);

  if (value == NULL ||
      Tcl_SplitList(interp, value, &listArgc, &listArgv) != TCL_OK) {
    return -1;
  }
  ckfree(value);

  Tcl_DStringInit(&selection);
  rslen = (rowsep?(strlen(rowsep)):0);
  cslen = (colsep?(strlen(colsep)):0);
  numrows = numcols = 0;
  for (count = 0; count < listArgc; count++) {
    TableParseArrayIndex(&r, &c, listArgv[count]);
    if (count) {
      if (lastrow != r) {
	lastrow = r;
	needcs = 0;
	if (rslen) {
	  Tcl_DStringAppend(&selection, rowsep, rslen);
	} else {
	  Tcl_DStringEndSublist(&selection);
	  Tcl_DStringStartSublist(&selection);
	}
	++numrows;
      } else {
	if (++needcs > numcols)
	  numcols = needcs;
      }
    } else {
      lastrow = r;
      needcs = 0;
      if (!rslen)
	Tcl_DStringStartSublist(&selection);
    }
    if (ctext) {
      wstr = ENCODE_WSTR(interp, TableGetCellValue(tablePtr, r, c));
      data = WSTR2CTEXT(wstr, -1);
    } else {
      data = TableGetCellValue(tablePtr, r, c);
    }
    if (cslen) {
      if (needcs)
	Tcl_DStringAppend(&selection, colsep, cslen);
      Tcl_DStringAppend(&selection, data, -1);
    } else {
      Tcl_DStringAppendElement(&selection, data);
    }
#ifdef KANJI	/* KANJI patch allocates everything... */
    ckfree(data);
    Tk_FreeWStr(wstr);
#endif
  }
  if (!rslen && count)
    Tcl_DStringEndSublist(&selection);
  ckfree((char *) listArgv);

  length = Tcl_DStringLength(&selection);

  if (tablePtr->selCmd != NULL) {
    Tcl_DString script;
    Tcl_DStringInit(&script);
    ExpandPercents(tablePtr, tablePtr->selCmd, numrows+1, numcols+1,
		   Tcl_DStringValue(&selection), (char *) NULL,
		   listArgc, &script, CMD_ACTIVATE);
    if (Tcl_GlobalEval(interp, Tcl_DStringValue(&script)) == TCL_ERROR) {
      Tcl_AppendResult(interp, "\n\terror in table selection command\n",
		       (char *)NULL);
      Tcl_DStringFree(&script);
      return -1;
    } else {
      Tcl_DStringGetResult(interp, &selection);
    }
    Tcl_DStringFree(&script);
  }

  if (length == 0)
    return -1;

  /* Copy the requested portion of the selection to the buffer. */
  count = length - offset;
  if (count <= 0) {
    count = 0;
  } else {
    if (count > maxBytes) {
      count = maxBytes;
    }
    memcpy((VOID *) buffer, (VOID *) (Tcl_DStringValue(&selection) + offset),
	   (size_t) count);
  }
  buffer[count] = '\0';
  Tcl_DStringFree(&selection);
  return count;
}

/*
 *----------------------------------------------------------------------
 *
 * TableFetchSelection --
 *
 *	This procedure is called back by Tk when the selection is
 *	requested by someone.  It returns part or all of the selection
 *	in a buffer provided by the caller.
 *
 * Results:
 *	The return value is the number of non-NULL bytes stored
 *	at buffer.  Buffer is filled (or partially filled) with a
 *	NULL-terminated string containing part or all of the selection,
 *	as given by offset and maxBytes.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
TableFetchSelection(clientData, offset, buffer, maxBytes)
    ClientData clientData;		/* Information about table widget. */
    int offset;				/* Offset within selection of first
					 * character to be returned. */
    char *buffer;			/* Location in which to place
					 * selection. */
    int maxBytes;			/* Maximum number of bytes to place
					 * at buffer, not including terminating
					 * NULL character. */
{
  return TableFetchSelectionAux(clientData, offset, buffer, maxBytes, 0);
}

#ifdef KANJI
/*
 *----------------------------------------------------------------------
 *
 * TableFetchSelectionCtext --
 *
 *	Same as TableFetchSelection except it
 *	converts the selection data to compound text encoding.
 *
 *----------------------------------------------------------------------
 */

static int
TableFetchSelectionCtext(clientData, offset, buffer, maxBytes)
     ClientData clientData;	/* Information about table widget. */
     int offset;		/* Offset within selection of first
				 * character to be returned. */
     char *buffer;		/* Location in which to place selection. */
     int maxBytes;		/* Max number of bytes to place at buffer,
				 * not including terminating NULL char. */
{
  return TableFetchSelectionAux(clientData, offset, buffer, maxBytes, 1);
}
#endif /* KANJI */

/*
 *----------------------------------------------------------------------
 *
 * TableLostSelection --
 *
 *	This procedure is called back by Tk when the selection is
 *	grabbed away from a table widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The existing selection is unhighlighted, and the window is
 *	marked as not containing a selection.
 *
 *----------------------------------------------------------------------
 */

static void
TableLostSelection(clientData)
    ClientData clientData;	/* Information about table widget. */
{
  register Table *tablePtr = (Table *) clientData;
  Tcl_HashSearch search;

  if ((tablePtr->exportSelection) &&
      (Tcl_FirstHashEntry(tablePtr->selCells, &search) != NULL)) {
    /* Same as SEL CLEAR ALL */
    Tcl_DeleteHashTable(tablePtr->selCells);
    ckfree ((char *) (tablePtr->selCells));
    tablePtr->selCells = (Tcl_HashTable *)ckalloc(sizeof(Tcl_HashTable));
    Tcl_InitHashTable(tablePtr->selCells, TCL_STRING_KEYS);
    TableInvalidateAll(tablePtr, 0);
  }
}

/*
 *----------------------------------------------------------------------
 *
 * TableRestrictProc --
 *
 *	A Tk_RestrictProc used by TableValidateChange to eliminate any
 *	extra key input events in the event queue that
 *	have a serial number no less than a given value.
 *
 * Results:
 *	Returns either TK_DISCARD_EVENT or TK_DEFER_EVENT.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static Tk_RestrictAction
TableRestrictProc(serial, eventPtr)
    ClientData serial;
    XEvent *eventPtr;
{
  if ((eventPtr->type == KeyRelease || eventPtr->type == KeyPress) &&
      ((eventPtr->xany.serial-(unsigned int)serial) > 0)) {
    return TK_DEFER_EVENT;
  } else {
    return TK_PROCESS_EVENT;
  }
}

/*
 *--------------------------------------------------------------
 *
 * TableValidateChange --
 *
 *	This procedure is invoked when any character is added or
 *	removed from the table widget, or a set has triggered validation.
 *
 * Results:
 *	TCL_OK    if the validatecommand accepts the new string,
 *	TCL_BREAK if the validatecommand rejects the new string,
 *      TCL_ERROR if any problems occured with validatecommand.
 *
 * Side effects:
 *      The insertion/deletion may be aborted, and the
 *      validatecommand might turn itself off (if an error
 *      or loop condition arises).
 *
 *--------------------------------------------------------------
 */

static int
TableValidateChange(tablePtr, r, c, old, new, index)
     register Table *tablePtr;	/* Table that needs validation. */
     int r, c;			/* row,col index of cell in user coords */
     char *old;			/* current value of cell */
     char *new;			/* potential new value of cell */
     int index;			/* index of insert/delete, -1 otherwise */
{
  register Tcl_Interp *interp = tablePtr->interp;
  int code, bool;
  Tk_RestrictProc *restrict;
  ClientData cdata;
  Tcl_DString script;
    
  if (tablePtr->valCmd == NULL || tablePtr->validate == 0) {
    return TCL_OK;
  }

  /* Magic code to make this bit of code synchronous in the face of
   * possible new key events */
  XSync(tablePtr->display, False);
  restrict = Tk_RestrictEvents(TableRestrictProc, (ClientData)
			       NextRequest(tablePtr->display), &cdata);

  /*
   * If we're already validating, then we're hitting a loop condition
   * Return and set validate to 0 to disallow further validations
   * and prevent current validation from finishing
   */
  if (tablePtr->flags & VALIDATING) {
    tablePtr->validate = 0;
    return TCL_OK;
  }
  tablePtr->flags |= VALIDATING;

  /* Now form command string and run through the -validatecommand */
  Tcl_DStringInit(&script);
  ExpandPercents(tablePtr, tablePtr->valCmd, r, c, old, new, index, &script,
		 CMD_VALIDATE);
  code = Tcl_GlobalEval(tablePtr->interp, Tcl_DStringValue(&script));
  Tcl_DStringFree(&script);

  if (code != TCL_OK && code != TCL_RETURN) {
    Tcl_AddErrorInfo(interp, "\n\t(in validation command executed by table)");
    Tk_BackgroundError(interp);
    code = TCL_ERROR;
  } else if (Tcl_GetBoolean(interp, interp->result, &bool) != TCL_OK) {
    Tcl_AddErrorInfo(interp, "\n\tboolean not returned by validation command");
    Tk_BackgroundError(interp);
    code = TCL_ERROR;
  } else {
    if (bool)
      code = TCL_OK;
    else
      code = TCL_BREAK;
  }
  Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);

  /*
   * If ->validate has become VALIDATE_NONE during the validation,
   * it means that a loop condition almost occured.  Do not allow
   * this validation result to finish.
   */
  if (tablePtr->validate == 0) {
    code = TCL_ERROR;
  }

  /* If validate will return ERROR, then disallow further validations */
  if (code == TCL_ERROR) {
    tablePtr->validate = 0;
  }

  Tk_RestrictEvents(restrict, cdata, &cdata);
  tablePtr->flags &= ~VALIDATING;

  return code;
}

/*
 *--------------------------------------------------------------
 *
 * ExpandPercents --
 *
 *	Given a command and an event, produce a new command
 *	by replacing % constructs in the original command
 *	with information from the X event.
 *
 * Results:
 *	The new expanded command is appended to the dynamic string
 *	given by dsPtr.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static void
ExpandPercents(tablePtr, before, r, c, old, new, index, dsPtr, cmdType)
     Table *tablePtr;		/* Table that needs validation. */
     char *before;		/* Command containing percent
				 * expressions to be replaced. */
     int r, c;			/* row,col index of cell */
     char *old;                 /* current value of cell */
     char *new;                 /* potential new value of cell */
     int index;                 /* index of insert/delete */
     Tcl_DString *dsPtr;        /* Dynamic string in which to append
				 * new command. */
     int cmdType;		/* type of command to make %-subs for */
{
  int spaceNeeded, cvtFlags;	/* Used to substitute string as proper Tcl
				 * list element. */
  int number, length;
  char *string;
  char buf[INDEX_BUFSIZE];

  /* This returns the static value of the string as set in the array */
  if (old == NULL && cmdType == CMD_VALIDATE) {
    old = TableGetCellValue(tablePtr, r, c);
  }

  while (1) {
    /*
     * Find everything up to the next % character and append it
     * to the result string.
     */
    for (string = before; (*string != 0) && (*string != '%'); string++) {
      /* Empty loop body. */
    }
    if (string != before) {
      Tcl_DStringAppend(dsPtr, before, string-before);
      before = string;
    }
    if (*before == 0) break;

    /* There's a percent sequence here.  Process it. */
    number = 0;
    string = "??";
    /* cmdType independent substitutions */
    switch (before[1]) {
    case 'c':
      number = c;
      goto doNumber;
    case 'C': /* index of cell */
      TableMakeArrayIndex(r, c, buf);
      string = buf;
      goto doString;
    case 'r':
      number = r;
      goto doNumber;
    case 'i': /* index of cursor OR |number| of cells selected */
      number = index;
      goto doNumber;
    case 's': /* Current cell value */
      string = old;
      goto doString;
    case 'S': /* Potential new value of cell */
      string = (new?new:old);
      goto doString;
    case 'W': /* widget name */
      string = Tk_PathName(tablePtr->tkwin);
      goto doString;
    default:
      buf[0] = before[1];
      buf[1] = '\0';
      string = buf;
      goto doString;
    }
	
  doNumber:
    sprintf(buf, "%d", number);
    string = buf;

  doString:
    spaceNeeded = Tcl_ScanElement(string, &cvtFlags);
    length = Tcl_DStringLength(dsPtr);
    Tcl_DStringSetLength(dsPtr, length + spaceNeeded);
    spaceNeeded = Tcl_ConvertElement(string, Tcl_DStringValue(dsPtr) + length,
				     cvtFlags | TCL_DONT_USE_BRACES);
    Tcl_DStringSetLength(dsPtr, length + spaceNeeded);
    before += 2;
  }
  Tcl_DStringAppend(dsPtr, "", 1);
}

/*
 *----------------------------------------------------------------------
 *
 * TableDeleteChars --
 *
 *	Remove one or more characters from an table widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory gets freed, the table gets modified and (eventually)
 *	redisplayed.
 *
 *----------------------------------------------------------------------
 */

static void
TableDeleteChars(tablePtr, index, count)
    register Table *tablePtr;	/* Table widget to modify. */
    int index;			/* Index of first character to delete. */
    int count;			/* How many characters to delete. */
{
  int x, y, width, height, oldlen;
  char *new;

  TableGetIcursor(tablePtr, "end", &oldlen);

  if ((index+count) > oldlen)
    count = oldlen-index;
  if (count <= 0)
    return;

#ifdef KANJI
  {
    WCHAR *tmp = Tk_DeleteWStr(tablePtr->interp,
			       Tk_GetWStr(tablePtr->interp,
					  tablePtr->activeBuf), index, count);
    new = (char *) ckalloc((unsigned)strlen(DECODE_WSTR(tmp))+1);
    strcpy(new, DECODE_WSTR(tmp));
    FREE_WSTR(tmp);
  }
#else
  new = (char *) ckalloc((unsigned)(oldlen-count+1));
  strncpy(new, tablePtr->activeBuf, (size_t) index);
  strcpy(new+index, tablePtr->activeBuf+index+count);
  /* make sure this string is null terminated */
  new[oldlen-count] = '\0';
#endif

  /* This prevents deletes on BREAK or validation error. */
  if (tablePtr->validate &&
      TableValidateChange(tablePtr, tablePtr->activeRow+tablePtr->rowOffset,
			  tablePtr->activeCol+tablePtr->colOffset,
			  tablePtr->activeBuf, new, index) != TCL_OK) {
    ckfree(new);
    return;
  }
  ckfree(tablePtr->activeBuf);
  tablePtr->activeBuf = new;
  TableSetActiveIndex(tablePtr);

  if (tablePtr->icursor >= index) {
    if (tablePtr->icursor >= (index+count)) {
      tablePtr->icursor -= count;
    } else {
      tablePtr->icursor = index;
    }
  }

  /* mark the text as changed */
  tablePtr->flags |= TEXT_CHANGED;

  TableCellCoords(tablePtr, tablePtr->activeRow, tablePtr->activeCol,
		   &x, &y, &width, &height);
  TableInvalidate(tablePtr, x, y, width, height, 1);
}

/*
 *----------------------------------------------------------------------
 *
 * TableInsertChars --
 *
 *	Add new characters to the active cell of a table widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	New information gets added to tablePtr; it will be redisplayed
 *	soon, but not necessarily immediately.
 *
 *----------------------------------------------------------------------
 */

static void
TableInsertChars(tablePtr, index, string)
    register Table *tablePtr;	/* Table that is to get the new elements. */
    int index;			/* Add the new elements before this element. */
    char *string;		/* New characters to add (NULL-terminated
				 * string). */
{
  int x, y, width, height, oldlen, newlen;
  WCHAR *diff;
  char *new;

  diff = ImpKGetStr(tablePtr->interp, string);
  newlen = WSTRLEN(diff);
  if (newlen == 0) return;

  /* Is this an autoclear and this is the first update */
  /* Note that this clears without validating */
  if (tablePtr->autoClear && !(tablePtr->flags & TEXT_CHANGED)) {
    /* set the buffer to be empty */
    tablePtr->activeBuf = (char *)ckrealloc(tablePtr->activeBuf, 1);
    tablePtr->activeBuf[0] = '\0';
    /* the insert position now has to be 0 */
    index = 0;
  }
  /* mark the text as changed */
  tablePtr->flags |= TEXT_CHANGED;
#ifdef KANJI
  {
    WCHAR *tmp = Tk_InsertWStr(tablePtr->interp,
			       Tk_GetWStr(tablePtr->interp,
					  tablePtr->activeBuf), index, diff);
    new = (char *) ckalloc((unsigned)strlen(DECODE_WSTR(tmp))+1);
    strcpy(new, DECODE_WSTR(tmp));
    FREE_WSTR(tmp);
    FREE_WSTR(diff);
  }
#else              /* KANJI */
  oldlen = strlen(tablePtr->activeBuf);
  /* get the buffer to at least the right length */
  new = (char *) ckalloc((unsigned)(oldlen+newlen+1));
  strncpy(new, tablePtr->activeBuf, (size_t) index);
  strcpy(new+index, diff);
  strcpy(new+index+newlen, (tablePtr->activeBuf)+index);
  /* make sure this string is null terminated */
  new[oldlen+newlen] = '\0';
#endif              /* KANJI */

  /* validate potential new active buffer */
  /* This prevents inserts on either BREAK or validation error. */
  if (tablePtr->validate &&
      TableValidateChange(tablePtr, tablePtr->activeRow+tablePtr->rowOffset,
			  tablePtr->activeCol+tablePtr->colOffset,
			  tablePtr->activeBuf, new, index) != TCL_OK) {
    ckfree(new);
    return;
  }
  ckfree(tablePtr->activeBuf);
  tablePtr->activeBuf = new;
  TableSetActiveIndex(tablePtr);

  if (tablePtr->icursor >= index) {
    tablePtr->icursor += newlen;
  }

  TableCellCoords(tablePtr, tablePtr->activeRow, tablePtr->activeCol,
		   &x, &y, &width, &height);
  TableInvalidate(tablePtr, x, y, width, height, 1);
}

/*
 *----------------------------------------------------------------------
 *
 * TableModifyRCaux --
 *	Helper function that does the core work of moving rows/cols
 *	and associated tags.
 *
 * Side effects:
 *	Moves cell data and possibly tag data
 *
 *----------------------------------------------------------------------
 */

static void
TableModifyRCaux(tablePtr, type, which, movetag, tagTblPtr, dimTblPtr,
		 from, to, lo, hi, check)
    Table *tablePtr;	/* Information about text widget. */
    int type;		/* insert (CMD_INSERT) | delete (CMD_DELETE) */
    int which;		/* rows (MOD_ROWS) or cols (MOD_COLS) */
    int movetag;	/* whether tags are supposed to be moved */
    Tcl_HashTable *tagTblPtr, *dimTblPtr; /* Pointers to the row/col tags
					   * and width/height tags */
    int from, to;	/* the from and to row/col */
    int lo, hi;		/* the lo and hi col/row */
    int check;		/* the boundary check for shifting items */
{
  int j, dummy;
  char buf[INDEX_BUFSIZE], buf1[INDEX_BUFSIZE];
  Tcl_HashEntry *entryPtr, *newEntryPtr;

  /* move row/col style && width/height here */
  if (movetag) {
    if ((entryPtr=Tcl_FindHashEntry(tagTblPtr, (char *)from)) != NULL) {
      Tcl_DeleteHashEntry(entryPtr);
    }
    if ((entryPtr=Tcl_FindHashEntry(dimTblPtr, (char *)from)) != NULL) {
      Tcl_DeleteHashEntry(entryPtr);
    }
    if (!check) {
      if ((entryPtr=Tcl_FindHashEntry(tagTblPtr, (char *)to)) != NULL) {
	newEntryPtr = Tcl_CreateHashEntry(tagTblPtr, (char *)from, &dummy);
	Tcl_SetHashValue(newEntryPtr, Tcl_GetHashValue(entryPtr));
	Tcl_DeleteHashEntry(entryPtr);
      }
      if ((entryPtr=Tcl_FindHashEntry(dimTblPtr, (char *)to)) != NULL) {
	newEntryPtr = Tcl_CreateHashEntry(dimTblPtr, (char *)from, &dummy);
	Tcl_SetHashValue(newEntryPtr, Tcl_GetHashValue(entryPtr));
	Tcl_DeleteHashEntry(entryPtr);
      }
    }
  }
  for (j = lo; j <= hi; j++) {
    if (which == MOD_COLS) {
      TableMakeArrayIndex(j, from, buf);
      TableMakeArrayIndex(j, to, buf1);
      TableSetCellValue(tablePtr, j, from, check ? "" :
			TableGetCellValue(tablePtr, j, to));
    } else {
      TableMakeArrayIndex(from, j, buf);
      TableMakeArrayIndex(to, j, buf1);
      TableSetCellValue(tablePtr, from, j, check ? "" :
			TableGetCellValue(tablePtr, to, j));
    }
    /* move cell style here */
    if (movetag) {
      if ((entryPtr=Tcl_FindHashEntry(tablePtr->cellStyles,buf)) != NULL) {
	Tcl_DeleteHashEntry(entryPtr);
      }
      if (!check &&
	  (entryPtr=Tcl_FindHashEntry(tablePtr->cellStyles,buf1))!=NULL) {
	newEntryPtr = Tcl_CreateHashEntry(tablePtr->cellStyles, buf, &dummy);
	Tcl_SetHashValue(newEntryPtr, Tcl_GetHashValue(entryPtr));
	Tcl_DeleteHashEntry(entryPtr);
      }
    }
  }
}

/*
 *----------------------------------------------------------------------
 *
 * TableModifyRC --
 *	Modify rows/cols of the table (insert or delete)
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Modifies associated row/col data
 *
 *----------------------------------------------------------------------
 */

static int
TableModifyRC(tablePtr, interp, type, which, argc, argv)
    Table *tablePtr;		/* Information about text widget. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int type;			/* insert (CMD_INSERT) | delete (CMD_DELETE) */
    int which;			/* rows (MOD_ROWS) or cols (MOD_COLS) */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
  int i, first, lo, hi, idx, c, argsLeft;
  int maxrow, maxcol, maxkey, minkey, movetitle, movetag, movedim;
  size_t length;
  char *arg;
  Tcl_HashTable *tagTblPtr, *dimTblPtr;
  Tcl_HashSearch search;
  int *dimPtr;

  movetitle	= 1;
  movetag	= 1;
  movedim	= 1;
  maxcol	= tablePtr->cols-1+tablePtr->colOffset;
  maxrow	= tablePtr->rows-1+tablePtr->rowOffset;
  for (i = 3; i < argc; i++) {
    arg = argv[i];
    if (arg[0] != '-') {
      break;
    }
    length = strlen(arg);
    if (length < 2) {
    badSwitch:
      Tcl_AppendResult(interp, "bad switch \"", arg,
		       "\": must be -cols, -keeptitles, -holddimensions, ",
		       "-holdtags, -rows, or --",
		       (char *) NULL);
      return TCL_ERROR;
    }
    c = arg[1];
    if ((c == 'h') && (length > 5) &&
	(strncmp(argv[i], "-holddimensions", length) == 0)) {
      movedim = 0;
    } else if ((c == 'h') && (length > 5) &&
	       (strncmp(argv[i], "-holdtags", length) == 0)) {
      movetag = 0;
    } else if ((c == 'k') && (strncmp(argv[i], "-keeptitles", length) == 0)) {
      movetitle = 0;
    } else if ((c == 'c') && (strncmp(argv[i], "-cols", length) == 0)) {
      if (i >= (argc-1)) {
	interp->result = "no value given for \"-cols\" option";
	return TCL_ERROR;
      }
      if (Tcl_GetInt(interp, argv[++i], &maxcol) != TCL_OK) {
	return TCL_ERROR;
      }
      maxcol = MAX(maxcol, tablePtr->titleCols+tablePtr->colOffset);
    } else if ((c == 'r') && (strncmp(argv[i], "-rows", length) == 0)) {
      if (i >= (argc-1)) {
	interp->result = "no value given for \"-rows\" option";
	return TCL_ERROR;
      }
      if (Tcl_GetInt(interp, argv[++i], &maxrow) != TCL_OK) {
	return TCL_ERROR;
      }
      maxrow = MAX(maxrow, tablePtr->titleRows+tablePtr->rowOffset);
    } else if ((c == '-') && (strncmp(argv[i], "--", length) == 0)) {
      i++;
      break;
    } else {
      goto badSwitch;
    }
  }
  argsLeft = argc - i;
  if (argsLeft != 1 && argsLeft != 2) {
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     (type == CMD_DELETE) ? " delete" : " insert",
		     (which == MOD_COLS) ? " cols" : " rows",
		     " ?switches? index ?count?\"", (char *) NULL);
    return TCL_ERROR;
  }

  c = 1;
  if (strcmp(argv[i], "end") == 0) {
    /* allow "end" to be specified as an index */
    idx = (which == MOD_COLS) ? maxcol : maxrow;
  } else if (Tcl_GetInt(interp, argv[i], &idx) != TCL_OK) {
    return TCL_ERROR;
  }
  if (argsLeft == 2 && Tcl_GetInt(interp, argv[++i], &c) != TCL_OK) {
    return TCL_ERROR;
  }
  if (c == 0)
    return TCL_OK;
  if (which == MOD_COLS) {
    maxkey	= maxcol;
    minkey	= tablePtr->titleCols+tablePtr->colOffset;
    lo		= tablePtr->rowOffset+(movetitle?0:tablePtr->titleRows);
    hi		= maxrow;
    tagTblPtr	= tablePtr->colStyles;
    dimTblPtr	= tablePtr->colWidths;
    dimPtr	= &(tablePtr->cols);
  } else {
    maxkey	= maxrow;
    minkey	= tablePtr->titleRows+tablePtr->rowOffset;
    lo		= tablePtr->colOffset+(movetitle?0:tablePtr->titleCols);
    hi		= maxcol;
    tagTblPtr	= tablePtr->rowStyles;
    dimTblPtr	= tablePtr->rowHeights;
    dimPtr	= &(tablePtr->rows);
  }
  if (type == CMD_DELETE) {
    /* Handle row/col deletion */
    first = MAX(MIN(idx,idx+c), minkey);
    /* (index = i && count = 1) == (index = i && count = -1) */
    if (c < 0) {
      /* if the count is negative, make sure that the col count will delete
       * no greater than the original index */
      c = idx-first;
      first++;
    }
    if (movedim) {
      *dimPtr -= c;
    }
    for (i = first; i <= maxkey; i++) {
      TableModifyRCaux(tablePtr, type, which, movetag, tagTblPtr,
		       dimTblPtr, i, i+c, lo, hi, (i+c>maxkey));
    }
  } else {
    /* Handle row/col insertion */
    first = MAX(MIN(idx, maxkey), minkey);
    /* +count means insert after index, -count means insert before index */
    if (c < 0) {
      c = -c;
    } else {
      first++;
    }
    if (movedim) {
      maxkey += c;
      *dimPtr += c;
    }
    for (i = maxkey; i >= first; i--) {
      /* move row/col style && width/height here */
      TableModifyRCaux(tablePtr, type, which, movetag, tagTblPtr,
		       dimTblPtr, i, i-c, lo, hi, (i-c<first));
    }
  }
  if (Tcl_FirstHashEntry(tablePtr->selCells, &search) != NULL) {
    /* clear selection - forceful, but effective */
    Tcl_DeleteHashTable(tablePtr->selCells);
    ckfree ((char *) (tablePtr->selCells));
    tablePtr->selCells = (Tcl_HashTable *)ckalloc(sizeof(Tcl_HashTable));
    Tcl_InitHashTable(tablePtr->selCells, TCL_STRING_KEYS);
  }

  TableAdjustParams(tablePtr);
  TableInvalidateAll(tablePtr, 0);
  return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * TableTagCommand --
 *
 *	This procedure is invoked to process the tag method
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

static int
TableTagCommand(Table * tablePtr, int argc, char *argv[])
{
  register Tcl_Interp *interp = tablePtr->interp;
  int result = TCL_OK, retval, i, newEntry, value;
  int row, col, x, y, width, height;
  TagStruct *tagPtr;
  Tcl_HashEntry *entryPtr, *scanPtr, *newEntryPtr, *oldEntryPtr;
  Tcl_HashTable *hashTblPtr;
  Tcl_HashSearch search;
  Tk_Image image;
  char buf[INDEX_BUFSIZE], *keybuf, *yes = "1", *no = "0";

  /* parse the next argument */
  retval = parse_command (interp, tag_cmds, argv[2]);
  switch (retval) {
    /* failed to parse the argument, error */
  case 0:
    return TCL_ERROR;

  case TAG_CELLTAG:
    /* tag a (group of) cell(s) */
    if (argc < 4) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " tag cell tag ?arg arg ...?\"", (char *) NULL);
      return TCL_ERROR;
    }
    /* are we deleting */
    if (!(*argv[3]))
      tagPtr = NULL;
    else {
      /* check to see if the tag actually exists */
      if ((entryPtr = Tcl_FindHashEntry(tablePtr->tagTable, argv[3]))==NULL) {
	Tcl_AppendResult (interp, "invalid tag name \"", argv[3],
			  "\"", (char *) NULL);
	return TCL_ERROR;
      }
      /* get the pointer to the tag structure */
      tagPtr = (TagStruct *) Tcl_GetHashValue (entryPtr);
    }

    /* No more args -> display only */
    if (argc == 4) {
      /* Added special handling for tags: active, flash, sel, title */

      if ((tablePtr->flags & HAS_ACTIVE) && strcmp(argv[3], "active") == 0) {
	TableMakeArrayIndex (tablePtr->activeRow+tablePtr->rowOffset,
			     tablePtr->activeCol+tablePtr->colOffset, buf);
	Tcl_AppendElement(interp, buf);
      } else if (tablePtr->flashMode && strcmp(argv[3], "flash") == 0) {
	for (scanPtr = Tcl_FirstHashEntry (tablePtr->flashCells, &search);
	     scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search)) {
	  Tcl_AppendElement(interp,
			    Tcl_GetHashKey(tablePtr->flashCells, scanPtr));
	}
      } else if (strcmp(argv[3], "sel") == 0) {
	for (scanPtr = Tcl_FirstHashEntry (tablePtr->selCells, &search);
	     scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search)) {
	  Tcl_AppendElement(interp,
			    Tcl_GetHashKey(tablePtr->selCells, scanPtr));
	}
      } else if (strcmp(argv[3], "title") == 0 &&
		 (tablePtr->titleRows || tablePtr->titleCols)) {
	for (row = tablePtr->rowOffset;
	     row < tablePtr->rowOffset+tablePtr->rows; row++) {
	  for (col = tablePtr->colOffset;
	       col < tablePtr->colOffset+tablePtr->titleCols; col++) {
	    TableMakeArrayIndex (row, col, buf);
	    Tcl_AppendElement (interp, buf);
	  }
	}
	for (row = tablePtr->rowOffset;
	     row < tablePtr->rowOffset+tablePtr->titleRows; row++) {
	  for (col = tablePtr->colOffset+tablePtr->titleCols;
	       col < tablePtr->colOffset+tablePtr->cols; col++) {
	    TableMakeArrayIndex (row, col, buf);
	    Tcl_AppendElement (interp, buf);
	  }
	}
      } else {
	for (scanPtr = Tcl_FirstHashEntry (tablePtr->cellStyles, &search);
	     scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search)) {
	  /* is this the tag pointer for this cell */
	  if ((TagStruct *) Tcl_GetHashValue(scanPtr) == tagPtr) {
	    Tcl_AppendElement(interp,
			      Tcl_GetHashKey(tablePtr->cellStyles, scanPtr));
	  }
	}
      }
      return TCL_OK;
    }
    /* Now loop through the arguments and fill in the hash table */
    for (i = 4; i < argc; i++) {
      /* can I parse this argument */
      if (TableGetIndex(tablePtr, argv[i], &row, &col) != TCL_OK) {
	return TCL_ERROR;
      }
      /* get the hash key ready */
      TableMakeArrayIndex(row, col, buf);

      /* is this a deletion */
      if (tagPtr == NULL) {
	oldEntryPtr = Tcl_FindHashEntry (tablePtr->cellStyles, buf);
	if (oldEntryPtr != NULL)
	  Tcl_DeleteHashEntry (oldEntryPtr);
      } else {
	/* add a key to the hash table */
	newEntryPtr = Tcl_CreateHashEntry (tablePtr->cellStyles, buf,
					   &newEntry);

	/* and set it to point to the Tag structure */
	Tcl_SetHashValue (newEntryPtr, (ClientData) tagPtr);
      }
      /* now invalidate the area */
      TableCellCoords(tablePtr, row-tablePtr->rowOffset,
		      col-tablePtr->colOffset, &x, &y, &width, &height);
      TableInvalidate(tablePtr, x, y, width, height, 0);
    }
    return TCL_OK;

  case TAG_COLTAG:
  case TAG_ROWTAG:
    /* tag a row or a column */
    if (argc < 4) {
      Tcl_AppendResult (interp, "wrong # args: should be \"",
			argv[0], " tag ", (retval == TAG_ROWTAG) ? "row" :
			"col", " tag ?arg arg ..?\"", (char *) NULL);
      return TCL_ERROR;
    }
    /* if the tag is null, we are deleting */
    if (!(*argv[3]))
      tagPtr = NULL;
    else {			/* check to see if the tag actually exists */
      if ((entryPtr = Tcl_FindHashEntry(tablePtr->tagTable, argv[3]))==NULL) {
	Tcl_AppendResult (interp, "invalid tag name \"", argv[3],
			  "\"", (char *) NULL);
	return TCL_ERROR;
      }
      /* get the pointer to the tag structure */
      tagPtr = (TagStruct *) Tcl_GetHashValue (entryPtr);
    }

    /* and choose the correct hash table */
    hashTblPtr = (retval == TAG_ROWTAG) ?
      tablePtr->rowStyles : tablePtr->colStyles;

    /* No more args -> display only */
    if (argc == 4) {
      for (scanPtr = Tcl_FirstHashEntry (hashTblPtr, &search);
	   scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search)) {
	/* is this the tag pointer on this row */
	if ((TagStruct *) Tcl_GetHashValue (scanPtr) == tagPtr) {
	  sprintf (buf, "%d", (int) Tcl_GetHashKey (hashTblPtr, scanPtr));
	  Tcl_AppendElement(interp, buf);
	}
      }
      return TCL_OK;
    }
    /* Now loop through the arguments and fill in the hash table */
    for (i = 4; i < argc; i++) {
      /* can I parse this argument */
      if (Tcl_GetInt(interp, argv[i], &value) != TCL_OK) {
	return TCL_ERROR;
      }
      /* deleting or adding */
      if (tagPtr == NULL) {
	oldEntryPtr = Tcl_FindHashEntry (hashTblPtr, (char *) value);
	if (oldEntryPtr != NULL)
	  Tcl_DeleteHashEntry (oldEntryPtr);
      } else {
	/* add a key to the hash table */
	newEntryPtr = Tcl_CreateHashEntry (hashTblPtr, (char *) value,
					   &newEntry);

	/* and set it to point to the Tag structure */
	Tcl_SetHashValue (newEntryPtr, (ClientData) tagPtr);
      }
      /* and invalidate the row or column affected */
      if (retval == TAG_ROWTAG) {
	/* get the position of the leftmost cell in the row */
	TableCellCoords(tablePtr, value - tablePtr->rowOffset, 0,
			&x, &y, &width, &height);

	/* Invalidate the row */
	TableInvalidate(tablePtr, 0, y, Tk_Width (tablePtr->tkwin), height, 0);
      } else {
	/* get the position of the topmost cell on the column */
	TableCellCoords(tablePtr, 0, value - tablePtr->colOffset,
			&x, &y, &width, &height);

	/* Invalidate the column */
	TableInvalidate(tablePtr, x, 0, width, Tk_Height(tablePtr->tkwin), 0);
      }
    }
    return TCL_OK;	/* COLTAG && ROWTAG */

  case TAG_CGET:
    if (argc != 5) {
      Tcl_AppendResult(interp, "wrong # args: should be \"",
                       argv[0], " tag cget tagName option\"", (char *) NULL);
      return TCL_ERROR;
    }
    if ((entryPtr=Tcl_FindHashEntry(tablePtr->tagTable, argv[3])) == NULL) {
      Tcl_AppendResult(interp, "invalid tag name \"", argv[3],
		       "\"", (char *) NULL);
      return TCL_ERROR;
    } else {
      tagPtr = (TagStruct *) Tcl_GetHashValue (entryPtr);
      result = Tk_ConfigureValue(interp, tablePtr->tkwin, tagConfig,
				 (char *) tagPtr, argv[4], 0);
    }
    return result;	/* CGET */

  case TAG_CONFIGURE:
    if (argc < 4) {
      Tcl_AppendResult (interp, "wrong # args: should be \"",
			argv[0], " tag configure tagName ?arg arg  ...?\"",
			(char *) NULL);
      return TCL_ERROR;
    }
    /* first see if this is a reconfiguration */
    entryPtr = Tcl_FindHashEntry (tablePtr->tagTable, argv[3]);
    if (entryPtr == NULL) {
      entryPtr = Tcl_CreateHashEntry (tablePtr->tagTable, argv[3], &newEntry);

      /* create the structure */
      tagPtr = TableNewTag (tablePtr);

      /* insert it into the table */
      Tcl_SetHashValue (entryPtr, (ClientData) tagPtr);

      /* configure the tag structure */
      result = Tk_ConfigureWidget(interp, tablePtr->tkwin, tagConfig,
				  argc - 4, argv + 4, (char *) tagPtr, 0);
      if (result == TCL_ERROR)
	return TCL_ERROR;
    } else {
      /* pointer wasn't null, do a reconfig if we have enough arguments */
      /* get the tag pointer from the table */
      tagPtr = (TagStruct *) Tcl_GetHashValue (entryPtr);

      /* 5 args means that there are values to replace */
      if (argc > 5) {
	/* and do a reconfigure */
	result = Tk_ConfigureWidget (interp, tablePtr->tkwin,
				     tagConfig, argc - 4, argv + 4,
				     (char *) tagPtr, TK_CONFIG_ARGV_ONLY);
	if (result == TCL_ERROR)
	  return TCL_ERROR;
      }
    }

    /* handle change of image name */
    if (tagPtr->imageStr) {
      image = Tk_GetImage(interp, tablePtr->tkwin, tagPtr->imageStr,
			  TableImageProc, (ClientData)tablePtr);
      if (image == NULL)
	result = TCL_ERROR;
    } else {
      image = NULL;
    }
    if (tagPtr->image) {
      Tk_FreeImage(tagPtr->image);
    }
    tagPtr->image = image;

    /* 
     * If there were less than 6 args, we need
     * to do a printout of the config, even for new tags
     */
    if (argc < 6) {
      result = Tk_ConfigureInfo (interp, tablePtr->tkwin, tagConfig,
				 (char *) tagPtr, (argc == 5)?argv[4]:0, 0);
    } else {
      /* Otherwise we reconfigured so invalidate the table for a redraw */
      TableInvalidateAll (tablePtr, 0);
    }
    return result;

  case TAG_DELETE:
    /* delete a tag */
    if (argc < 4) {
      Tcl_AppendResult (interp, "wrong # args: should be \"",
	       argv[0], " tag delete tagName ?tagName ...?\"", (char *) NULL);
      return TCL_ERROR;
    }
    /* run through the remaining arguments */
    for (i = 3; i < argc; i++) {
      /* cannot delete the title tag */
      if (strcmp(argv[i], "title") == 0 || strcmp (argv[i], "sel") == 0 ||
	  strcmp(argv[i], "flash") == 0 || strcmp (argv[i], "active") == 0) {
	Tcl_AppendResult (interp, "cannot delete ", argv[i],
			  " tag", (char *) NULL);
	return TCL_ERROR;
      }
      if ((entryPtr = Tcl_FindHashEntry(tablePtr->tagTable, argv[i]))!=NULL) {
	/* get the tag pointer */
	tagPtr = (TagStruct *) Tcl_GetHashValue (entryPtr);

	/* delete all references to this tag in rows */
	scanPtr = Tcl_FirstHashEntry (tablePtr->rowStyles, &search);
	for (; scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search))
	  if ((TagStruct *)Tcl_GetHashValue (scanPtr) == tagPtr)
	    Tcl_DeleteHashEntry (scanPtr);

	/* delete all references to this tag in cols */
	scanPtr = Tcl_FirstHashEntry (tablePtr->colStyles, &search);
	for (; scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search))
	  if ((TagStruct *)Tcl_GetHashValue (scanPtr) == tagPtr)
	    Tcl_DeleteHashEntry (scanPtr);

	/* delete all references to this tag in cells */
	scanPtr = Tcl_FirstHashEntry (tablePtr->cellStyles, &search);
	for (; scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search))
	  if ((TagStruct *)Tcl_GetHashValue (scanPtr) == tagPtr)
	    Tcl_DeleteHashEntry (scanPtr);

	/* release the structure */
	TableCleanupTag (tablePtr, (TagStruct *) Tcl_GetHashValue (entryPtr));
	ckfree ((char *) Tcl_GetHashValue (entryPtr));

	/* and free the hash table entry */
	Tcl_DeleteHashEntry (entryPtr);
      }
    }
    /* since we deleted a tag, redraw the screen */
    TableInvalidateAll (tablePtr, 0);
    return result;

  case TAG_EXISTS:
    if (argc != 4) {
      Tcl_AppendResult (interp, "wrong # args: should be \"",
			argv[0], " tag exists tagName\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (Tcl_FindHashEntry(tablePtr->tagTable, argv[3]) != NULL) {
      Tcl_SetResult(interp, yes, TCL_VOLATILE);
    } else {
      Tcl_SetResult(interp, no, TCL_VOLATILE);
    }
    return TCL_OK;

  case TAG_INCLUDES:
    /* does a tag contain a index ? */
    if (argc != 5) {
      Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			" tag includes tag index\"", (char *) NULL);
      return TCL_ERROR;
    }
    /* check to see if the tag actually exists */
    if ((entryPtr = Tcl_FindHashEntry (tablePtr->tagTable, argv[3])) == NULL) {
      Tcl_AppendResult (interp, "invalid tag name \"", argv[3],
			"\"", (char *) NULL);
      return TCL_ERROR;
    }
    /* parse index */
    if (TableGetIndex (tablePtr, argv[4], &row, &col) != TCL_OK) {
      return TCL_ERROR;
    }
    /* create hash key */
    TableMakeArrayIndex (row, col, buf);
    
    if (strcmp(argv[3], "active") == 0) {
      if (tablePtr->activeRow+tablePtr->rowOffset == row &&
	  tablePtr->activeCol+tablePtr->colOffset == col)
	Tcl_SetResult (interp, yes, TCL_VOLATILE);
      else
	Tcl_SetResult (interp, no, TCL_VOLATILE);
      return TCL_OK;
    } else if (strcmp(argv[3], "flash") == 0) {
      if (tablePtr->flashMode && Tcl_FindHashEntry(tablePtr->flashCells, buf))
	Tcl_SetResult (interp, yes, TCL_VOLATILE);
      else
	Tcl_SetResult (interp, no, TCL_VOLATILE);
      return TCL_OK;
    } else if (strcmp(argv[3], "sel") == 0) {
      if (Tcl_FindHashEntry(tablePtr->selCells, buf))
	Tcl_SetResult (interp, yes, TCL_VOLATILE);
      else
	Tcl_SetResult (interp, no, TCL_VOLATILE);
      return TCL_OK;
    } else if (strcmp(argv[3], "title") == 0) {
      if (row < tablePtr->titleRows+tablePtr->rowOffset ||
	  col < tablePtr->titleCols+tablePtr->colOffset)
	Tcl_SetResult (interp, yes, TCL_VOLATILE);
      else
	Tcl_SetResult (interp, no, TCL_VOLATILE);
      return TCL_OK;
    }

    /* get the pointer to the tag structure */
    tagPtr = (TagStruct *) Tcl_GetHashValue (entryPtr);
    for (scanPtr = Tcl_FirstHashEntry (tablePtr->cellStyles, &search);
	 scanPtr != NULL; scanPtr = Tcl_NextHashEntry (&search)) {
      /* is this the tag pointer for this cell */
      if ((TagStruct *) Tcl_GetHashValue (scanPtr) == tagPtr) {
	Tcl_SetResult (interp, yes, TCL_VOLATILE);
	return TCL_OK;
      }
    }
    Tcl_SetResult (interp, no, TCL_VOLATILE);
    return TCL_OK;

  case TAG_NAMES:
    /* just print out the tag names */
    if (argc != 3 && argc != 4) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " tag names ?pattern?\"", (char *) NULL);
      return TCL_ERROR;
    }
    entryPtr = Tcl_FirstHashEntry (tablePtr->tagTable, &search);
    while (entryPtr != NULL) {
      keybuf = Tcl_GetHashKey(tablePtr->tagTable, entryPtr);
      if (argc == 3 || Tcl_StringMatch(keybuf, argv[3]))
	Tcl_AppendElement(interp, keybuf);
      entryPtr = Tcl_NextHashEntry (&search);
    }
    return TCL_OK;
  }
  return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * TableWidgetCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
static int
TableWidgetCmd(clientData, interp, argc, argv)
     ClientData clientData;		/* Information about listbox widget. */
     Tcl_Interp *interp;		/* Current interpreter. */
     int argc;				/* Number of arguments. */
     char **argv;			/* Argument strings. */
{
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  Tcl_HashTable *hashTablePtr;
  int result, retval, sub_retval, row, col, x, y;
  int i, width, height, dummy, key, value, posn;
  char buf1[INDEX_BUFSIZE], buf2[INDEX_BUFSIZE];

  Table *tablePtr = (Table *) clientData;

  if (argc < 2) {
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " option ?arg arg ...?\"", (char *) NULL);
    return TCL_ERROR;
  }
  Tcl_Preserve(clientData);

  result = TCL_OK;
  /* parse the first parameter */
  retval = parse_command(interp, main_cmds, argv[1]);

  /* Switch on the parameter value */
  switch (retval) {
  case 0:
    /* error, the return string is already set up */
    result = TCL_ERROR;
    break;

  case CMD_ACTIVATE:
    if (argc != 3) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " activate index\"", (char *)NULL);
      result = TCL_ERROR;
    } else if (TableGetIndex(tablePtr,argv[2],&row,&col)==TCL_ERROR) {
      result = TCL_ERROR;
    } else {
      /* convert to valid active index in real coords */
      row = MIN(MAX(tablePtr->titleRows, row-tablePtr->rowOffset),
		tablePtr->rows-1);
      col = MIN(MAX(tablePtr->titleCols, col-tablePtr->colOffset),
		tablePtr->cols-1);
      if (row != tablePtr->activeRow || col != tablePtr->activeCol) {
	if (tablePtr->flags & HAS_ACTIVE)
	  TableMakeArrayIndex(tablePtr->activeRow+tablePtr->rowOffset,
			      tablePtr->activeCol+tablePtr->colOffset, buf1);
	else
	  buf1[0] = '\0';
	tablePtr->flags |= HAS_ACTIVE;
	tablePtr->activeRow = row;
	tablePtr->activeCol = col;
	TableAdjustParams(tablePtr);
	TableConfigCursor(tablePtr);
	if (!(tablePtr->flags & BROWSE_CMD) && tablePtr->browseCmd != NULL) {
	  Tcl_DString script;
	  tablePtr->flags |= BROWSE_CMD;
	  row = tablePtr->activeRow+tablePtr->rowOffset;
	  col = tablePtr->activeCol+tablePtr->colOffset;
	  TableMakeArrayIndex(row, col, buf2);
	  Tcl_DStringInit(&script);
	  ExpandPercents(tablePtr, tablePtr->browseCmd, row, col, buf1, buf2,
			 tablePtr->icursor, &script, CMD_ACTIVATE);
	  result = Tcl_GlobalEval(interp, Tcl_DStringValue(&script));
	  if (result == TCL_OK || result == TCL_RETURN)
	    Tcl_ResetResult(interp);
	  Tcl_DStringFree(&script);
	  tablePtr->flags &= ~BROWSE_CMD;
	}
      }
      tablePtr->flags |= HAS_ACTIVE;
    }
    break;	/* ACTIVATE */

  case CMD_BBOX: {
    /* bounding box of cell(s) */
    if (argc < 3 || argc > 4) {
      Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			" bbox first ?last?\"", (char *) NULL);
      result = TCL_ERROR;
    } else if (TableGetIndex(tablePtr, argv[2], &row, &col) == TCL_ERROR) {
      result = TCL_ERROR;
    } else if (argc == 3) {
      row -= tablePtr->rowOffset; col -= tablePtr->colOffset;
      if (TableCellVCoords(tablePtr, row, col, &x, &y, &width, &height, 0)) {
	sprintf (interp->result, "%d %d %d %d", x, y, width, height);
      }
    } else if (TableGetIndex(tablePtr, argv[3], &x, &y) == TCL_ERROR) {
      result = TCL_ERROR;
    } else {
      int r1, c1, r2, c2, minX = 99999, minY = 99999, maxX = 0, maxY = 0;
      row -= tablePtr->rowOffset; col -= tablePtr->colOffset;
      x -= tablePtr->rowOffset; y -= tablePtr->colOffset;
      r1 = MIN(row,x); r2 = MAX(row,x);
      c1 = MIN(col,y); c2 = MAX(col,y);
      key = 0;
      for (row = r1; row <= r2; row++) {
	for (col = c1; col <= c2; col++) {
	  if (TableCellVCoords(tablePtr, row, col, &x, &y,
			       &width, &height, 0)) {
	    /* Get max bounding box */
	    if (x < minX) minX = x;
	    if (y < minY) minY = y;
	    if (x+width > maxX) maxX = x+width;
	    if (y+height > maxY) maxY = y+height;
	    key++;
	  }
	  /* FIX - This could break on else for speed */
	}
      }
      if (key)
	sprintf (interp->result,"%d %d %d %d",minX,minY,maxX-minX,maxY-minY);
    }
  }
  break;	/* BBOX */

  case CMD_BORDER:
    if (argc > 6) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " border mark|dragto x y ?r|c?\"", (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    sub_retval = parse_command(interp, bd_cmds, argv[2]);
    if (sub_retval == 0 || Tcl_GetInt(interp, argv[3], &x) != TCL_OK ||
	Tcl_GetInt(interp, argv[4], &y) != TCL_OK) {
      result = TCL_ERROR;
      break;
    }
    switch (sub_retval) {
    case BD_MARK:
      /* Use x && y to determine if we are over a border */
      value = TableAtBorder(tablePtr, x, y, &row, &col);
      /* Cache the row && col for use in DRAGTO */
      tablePtr->scanMarkRow = row;
      tablePtr->scanMarkCol = col;
      if (!value) break;
      TableCellCoords(tablePtr, row, col, &x, &y, &dummy, &dummy);
      tablePtr->scanMarkX = x;
      tablePtr->scanMarkY = y;
      if (argc == 5 || argv[5][0] == 'r') {
	if (row < 0)
	  buf1[0] = '\0';
	else
	  sprintf(buf1, "%d", row+tablePtr->rowOffset);
	Tcl_AppendElement(interp, buf1);
      }
      if (argc == 5 || argv[5][0] == 'c') {
	if (col < 0)
	  buf1[0] = '\0';
	else
	  sprintf(buf1, "%d", col+tablePtr->colOffset);
	Tcl_AppendElement(interp, buf1);
      }
      break;	/* BORDER MARK */
    case BD_DRAGTO:
      row = tablePtr->scanMarkRow;
      col = tablePtr->scanMarkCol;
      TableCellCoords(tablePtr, row, col, &width, &height, &dummy, &dummy);
      key = 0;
      if (row >= 0) {
	/* row border was active, move it */
	value = y-height-(2*tablePtr->borderWidth);
	if (value < -1) value = -1;
	if (value != tablePtr->scanMarkY) {
	  entryPtr = Tcl_CreateHashEntry (tablePtr->rowHeights,
					  (char *) row, &dummy);
	  Tcl_SetHashValue (entryPtr, (ClientData) MAX(0,value));
	  tablePtr->scanMarkY = value;
	  key++;
	}
      }
      if (col >= 0) {
	/* col border was active, move it */
	value = (x-width-tablePtr->borderWidth)/tablePtr->charWidth;
	if (value < -1) value = -1;
	if (value != tablePtr->scanMarkX) {
	  entryPtr = Tcl_CreateHashEntry (tablePtr->colWidths,
					  (char *) col, &dummy);
	  Tcl_SetHashValue (entryPtr, (ClientData) MAX(0,value));
	  tablePtr->scanMarkX = value;
	  key++;
	}
      }
      /* Only if something changed do we want to update */
      if (key) {
	TableAdjustParams(tablePtr);
	/* Only rerequest geometry if the basis is the #rows &| #cols */
	if (tablePtr->maxReqCols || tablePtr->maxReqRows)
	  TableGeometryRequest(tablePtr);
	TableInvalidateAll(tablePtr, 0);
      }
      break;	/* BORDER DRAGTO */
    }
    break;	/* BORDER */

  case CMD_CGET:
    if (argc != 3) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " cget option\"", (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    result = Tk_ConfigureValue(interp, tablePtr->tkwin, TableConfig,
			       (char *) tablePtr, argv[2], 0);
    break;	/* CGET */

  case CMD_CONFIGURE:
    /* configure */
    switch (argc) {
    case 2:
      result = Tk_ConfigureInfo (interp, tablePtr->tkwin, TableConfig,
				 (char *) tablePtr, (char *) NULL, 0);
      break;
    case 3:
      result = Tk_ConfigureInfo (interp, tablePtr->tkwin, TableConfig,
				 (char *) tablePtr, argv[2], 0);
      break;
    default:
      result = TableConfigure (interp, tablePtr, argc - 2, argv + 2,
			       TK_CONFIG_ARGV_ONLY, 0);
    }
    break;	/* CONFIGURE */

  case CMD_CURVALUE:
    /* Get current active cell buffer */
    if (argc > 3) {
      Tcl_AppendResult( interp, "wrong # args: should be \"",
                        argv[0], " curvalue ?<value>?\"", (char *)NULL);
      result=TCL_ERROR;
    } else if (tablePtr->flags & HAS_ACTIVE) {
      if (argc == 3 && strcmp(argv[2], tablePtr->activeBuf)) {
	key = TCL_OK;
	/* validate potential new active buffer contents
	 * only accept if validation returns acceptance. */
	if (tablePtr->validate &&
	    TableValidateChange(tablePtr,
				tablePtr->activeRow+tablePtr->rowOffset,
				tablePtr->activeCol+tablePtr->colOffset,
				tablePtr->activeBuf,
				argv[2], tablePtr->icursor) != TCL_OK) {
	  break;
	}
	tablePtr->activeBuf = (char *)ckrealloc(tablePtr->activeBuf,
						strlen(argv[2])+1);
	strcpy(tablePtr->activeBuf, argv[2]);
	/* mark the text as changed */
	tablePtr->flags |= TEXT_CHANGED;
	TableSetActiveIndex(tablePtr);
	/* check for possible adjustment of icursor */
	TableGetIcursor(tablePtr, "insert", (int *)0);
	TableCellCoords (tablePtr, tablePtr->activeRow, tablePtr->activeCol,
			 &x, &y, &width, &height);
	TableInvalidate (tablePtr, x, y, width, height, 1);
	if (key == TCL_ERROR) {
	  result = TCL_ERROR;
	  break;
	}
      }
      Tcl_AppendResult(interp, tablePtr->activeBuf, (char *)NULL);
    }
    break;	/* CURVALUE */

  case CMD_CURSELECTION:
    if (argc != 2 && argc != 4 ||
	(argc == 4 && (argv[2][0] == '\0' ||
		       strncmp(argv[2], "set", strlen(argv[2]))))) {
      Tcl_AppendResult(	interp, "wrong # args: should be \"", argv[0],
			" curselection ?set <value>?\"", (char *)NULL);
      result = TCL_ERROR;
      break;
    }
    /* make sure there is a data source to accept set */
    if (argc == 4 && (tablePtr->state == tkDisabledUid ||
		      (tablePtr->dataSource == DATA_NONE)))
      break;
    for (entryPtr=Tcl_FirstHashEntry(tablePtr->selCells, &search);
	 entryPtr!=NULL; entryPtr=Tcl_NextHashEntry(&search)) {
      if (argc == 2) {
	Tcl_AppendElement(interp,
			  Tcl_GetHashKey(tablePtr->selCells, entryPtr));
      } else {
	TableParseArrayIndex(&row, &col,
			     Tcl_GetHashKey(tablePtr->selCells, entryPtr));
	TableSetCellValue(tablePtr, row, col, argv[3]);
	row -= tablePtr->rowOffset;
	col -= tablePtr->colOffset;
	if (row == tablePtr->activeRow && col == tablePtr->activeCol) {
	  TableGetActiveBuf(tablePtr);
	}
	TableCellCoords(tablePtr, row, col, &x, &y, &width, &height);
	TableInvalidate(tablePtr, x, y, width, height, 0);
      }
    }
    if (argc == 2) {
      interp->result = TableCellSort(tablePtr, interp->result);
      interp->freeProc = TCL_DYNAMIC;
    }
    break;	/* CURSELECTION */

  case CMD_DELETE:
    if (argc < 4) {
      Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			" delete option ?switches? arg ?arg?\"",
			(char *) NULL);
      result = TCL_ERROR;
      break;
    }
    sub_retval = parse_command (interp, mod_cmds, argv[2]);
    switch (sub_retval) {
    case 0:
      result = TCL_ERROR;
      break;
    case MOD_ACTIVE:
      if (argc > 5) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " delete active first ?last?\"", (char *) NULL);
	result = TCL_ERROR;
	break;
      }
      if (TableGetIcursor(tablePtr, argv[3], &posn) == TCL_ERROR) {
	result = TCL_ERROR;
	break;
      }
      if (argc == 4) {
	value = posn+1;
      } else if (TableGetIcursor(tablePtr, argv[4], &value) == TCL_ERROR) {
	result = TCL_ERROR;
	break;
      }
      if (value >= posn && (tablePtr->flags & HAS_ACTIVE) &&
	  tablePtr->state == tkNormalUid)
	TableDeleteChars(tablePtr, posn, value-posn);
      break;	/* DELETE ACTIVE */
    case MOD_COLS:
    case MOD_ROWS:
      result = TableModifyRC(tablePtr, interp, CMD_DELETE, sub_retval,
			     argc, argv);
      break;	/* DELETE ROWS */
    }
    break;	/* DELETE */

  case CMD_GET: {
    int r1, c1, r2, c2;

    if (argc < 3 || argc > 4) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " get first ?last?\"", (char *)NULL);
      result = TCL_ERROR;
    } else if (TableGetIndex(tablePtr, argv[2], &row, &col) == TCL_ERROR) {
      result = TCL_ERROR;
    } else if (argc == 3) {
      interp->result = TableGetCellValue(tablePtr, row, col);
    } else if (TableGetIndex(tablePtr, argv[3], &r2, &c2) == TCL_ERROR) {
      result = TCL_ERROR;
    } else {
      r1 = MIN(row,r2); r2 = MAX(row,r2);
      c1 = MIN(col,c2); c2 = MAX(col,c2);
      for ( row = r1; row <= r2; row++ ) {
	for ( col = c1; col <= c2; col++ ) {
	  Tcl_AppendElement(interp, TableGetCellValue(tablePtr, row, col));
	}
      }
    }
  }
  break;	/* GET */

  case CMD_FLUSH:
    if (argc > 4) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " flush ?first? ?last?\"", (char *) NULL);
      result = TCL_ERROR;
      break;
    } else if (argc == 2) {
      TableFlushCache(tablePtr);
      /* If we were caching and we have no other data source,
       * invalidate all the cells */
      if (tablePtr->dataSource == DATA_CACHE) {
	TableGetActiveBuf(tablePtr);
	TableInvalidateAll(tablePtr, 0);
      }
    } else {
      int r1, r2, c1, c2;
      if (TableGetIndex(tablePtr, argv[2], &row, &col) == TCL_ERROR ||
	  (argc==4 && TableGetIndex(tablePtr,argv[3],&r2,&c2)==TCL_ERROR)) {
	result = TCL_ERROR;
	break;
      }
      if (argc == 3) {
	r1 = r2 = row;
	c1 = c2 = col;
      } else {
	r1 = MIN(row,r2); r2 = MAX(row,r2);
	c1 = MIN(col,c2); c2 = MAX(col,c2);
      }
      for ( row = r1; row <= r2; row++ ) {
	for ( col = c1; col <= c2; col++ ) {
	  TableMakeArrayIndex(row, col, buf1);
	  if ((entryPtr = Tcl_FindHashEntry(tablePtr->cache, buf1)) != NULL) {
	    Tcl_DeleteHashEntry(entryPtr);
	    /* if the cache is our data source,
	     * we need to invalidate the cells changed */
	    if (tablePtr->dataSource == DATA_CACHE) {
	      if (row-tablePtr->rowOffset == tablePtr->activeRow &&
		  col-tablePtr->colOffset == tablePtr->activeCol) {
		TableGetActiveBuf(tablePtr);
	      }
	      TableCellCoords(tablePtr, row-tablePtr->rowOffset,
			      col-tablePtr->colOffset, &x,&y,&width,&height);
	      TableInvalidate(tablePtr, x, y, width, height, 0);
	    }
	  }
	}
      }
    }
    break;	/* FLUSH */

  case CMD_HEIGHT:
  case CMD_WIDTH:
    /* changes the width/height of certain selected columns */
    if (argc != 3 && (argc & 1)) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       (retval == CMD_WIDTH) ?
		       " width ?col? ?width col width ...?\"" :
		       " height ?row? ?height row height ...?\"",
		       (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    if (retval == CMD_WIDTH) {
      hashTablePtr = tablePtr->colWidths;
      key = tablePtr->colOffset;
    } else { 
      hashTablePtr = tablePtr->rowHeights;
      key = tablePtr->rowOffset;
    }

    if (argc == 2) {
      /* print out all the preset column widths or row heights */
      entryPtr = Tcl_FirstHashEntry (hashTablePtr, &search);
      while (entryPtr != NULL) {
	posn = (int) Tcl_GetHashKey (hashTablePtr, entryPtr);
	value = (int) Tcl_GetHashValue (entryPtr);
	posn += key;
	sprintf (buf1, "%d %d", posn, value);
	Tcl_AppendElement(interp, buf1);
	entryPtr = Tcl_NextHashEntry (&search);
      }
    } else if (argc == 3) {
      /* get the width/height of a particular row/col */
      if (Tcl_GetInt(interp, argv[2], &posn) != TCL_OK) {
	result = TCL_ERROR;
	break;
      }
      /* no range check is done, why bother? */
      posn -= key;
      entryPtr = Tcl_FindHashEntry (hashTablePtr, (char *) posn);
      if (entryPtr != NULL)
	sprintf (interp->result, "%d", (int) Tcl_GetHashValue (entryPtr));
      else {
	if (retval == CMD_WIDTH)
	  sprintf (interp->result, "%d", tablePtr->defColWidth);
	else
	  sprintf (interp->result, "%d", tablePtr->defRowHeight);
      }
    } else {
      for (i=2; i<argc; i++) {
	/* set new width|height here */
	if (Tcl_GetInt(interp, argv[i], &posn) != TCL_OK ||
	    Tcl_GetInt(interp, argv[++i], &value) != TCL_OK) {
	  result = TCL_ERROR;
	  break;
	}
	posn -= key;
	if (value < 0) {
	  /* reset that field if value is < 0 */
	  if ((entryPtr = Tcl_FindHashEntry (hashTablePtr,
					     (char *) posn)) != NULL)
	    Tcl_DeleteHashEntry (entryPtr);
	} else {
	  entryPtr = Tcl_CreateHashEntry (hashTablePtr, (char *) posn, &dummy);
	  Tcl_SetHashValue (entryPtr, (ClientData)value);
	}
      }
      TableAdjustParams (tablePtr);
      /* rerequest geometry */
      TableGeometryRequest(tablePtr);
      /*
       * Invalidate the whole window as TableAdjustParams
       * will only check to see if the top left cell has moved
       */
      TableInvalidateAll(tablePtr, 0);
    }
    break;	/* HEIGHT && WIDTH */

  case CMD_ICURSOR:
    /* set the insertion cursor */
    if (!(tablePtr->flags & HAS_ACTIVE) || tablePtr->state == tkDisabledUid)
      break;
    switch (argc) {
    case 2:
      sprintf (interp->result, "%d", tablePtr->icursor);
      break;
    case 3:
      if (TableGetIcursor (tablePtr, argv[2], (int *)0) != TCL_OK) {
	result = TCL_ERROR;
	break;
      }
      TableCellCoords(tablePtr, tablePtr->activeRow, tablePtr->activeCol,
		      &x, &y, &width, &height);
      TableInvalidate(tablePtr, x, y, width, height, 0);
      break;
    default:
      Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			" icursor arg\"", (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    break;	/* ICURSOR */

  case CMD_INDEX:
    if (argc < 3 || argc > 4 ||
	TableGetIndex(tablePtr, argv[2], &row, &col) == TCL_ERROR ||
	(argc == 4 &&
	 (strcmp(argv[3], "row") && strcmp(argv[3], "col")))) {
      if (!strlen(interp->result)) {
	Tcl_AppendResult( interp, "wrong # args: should be \"", argv[0],
			  " index index ?row|col?\"", (char *)NULL);
      }
      result=TCL_ERROR;
      break;
    }
    if (argc==3) {
      TableMakeArrayIndex(row, col, interp->result);
    } else if (argv[3][0] == 'r') { /* INDEX row */
      sprintf(interp->result,"%d",row);
    } else {	/* INDEX col */
      sprintf(interp->result,"%d",col);
    }
    break;	/* INDEX */

  case CMD_INSERT:
    /* are edits enabled */
    if (argc < 4) {
      Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			" insert option ?switches? arg ?arg?\"", (char *)NULL);
      result = TCL_ERROR;
      break;
    }
    sub_retval = parse_command (interp, mod_cmds, argv[2]);
    switch (sub_retval) {
    case 0:
      result = TCL_ERROR;
      break;
    case MOD_ACTIVE:
      if (argc != 5) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " insert active index string\"", (char *)NULL);
	result = TCL_ERROR;
      } else if (TableGetIcursor (tablePtr, argv[3], &posn) != TCL_OK) {
	result = TCL_ERROR;
      } else if ((tablePtr->flags & HAS_ACTIVE) &&
		 tablePtr->state == tkNormalUid) {
	TableInsertChars(tablePtr, posn, argv[4]);
      }
      break;	/* INSERT ACTIVE */
    case MOD_COLS:
    case MOD_ROWS:
      result = TableModifyRC(tablePtr, interp, CMD_INSERT, sub_retval,
			     argc, argv);
      break;
    }
    break;	/* INSERT */

  case CMD_REREAD:
    /* this rereads the selection from the array */
    if (!(tablePtr->flags & HAS_ACTIVE) || tablePtr->state == tkDisabledUid)
      break;
    TableGetActiveBuf (tablePtr);
    TableCellCoords(tablePtr, tablePtr->activeRow, tablePtr->activeCol,
		    &x, &y, &width, &height);
    TableInvalidate(tablePtr, x, y, width, height, 1);
    break;	/* REREAD */

  case CMD_SCAN:
    if (argc != 5 || Tcl_GetInt(interp, argv[3], &x) != TCL_OK ||
	Tcl_GetInt(interp, argv[4], &y) != TCL_OK) {
      if (!strlen(interp->result))
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " scan mark|dragto x y\"", (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    if ((argv[2][0] == 'm')
	&& (strncmp(argv[2], "mark", strlen(argv[2])) == 0)) {
      TableWhatCell(tablePtr, x, y, &row, &col);
      tablePtr->scanMarkRow = row-tablePtr->topRow;
      tablePtr->scanMarkCol = col-tablePtr->leftCol;
      tablePtr->scanMarkX = x;
      tablePtr->scanMarkY = y;
    } else if ((argv[2][0] == 'd')
	       && (strncmp(argv[2], "dragto", strlen(argv[2])) == 0)) {
      int oldTop = tablePtr->topRow, oldLeft = tablePtr->leftCol;
      y += (5*(y-tablePtr->scanMarkY));
      x += (5*(x-tablePtr->scanMarkX));

      TableWhatCell(tablePtr, x, y, &row, &col);

      /* maintain appropriate real index */
      tablePtr->topRow  = MAX(MIN(row-tablePtr->scanMarkRow,
				  tablePtr->rows-1), tablePtr->titleRows);
      tablePtr->leftCol = MAX(MIN(col-tablePtr->scanMarkCol,
				  tablePtr->cols-1), tablePtr->titleCols);

      /* Adjust the table if new top left */
      if (oldTop != tablePtr->topRow || oldLeft != tablePtr->leftCol)
	TableAdjustParams(tablePtr);
    } else {
      Tcl_AppendResult(interp, "bad scan option \"", argv[2],
		       "\": must be mark or dragto", (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    break;	/* SCAN */

  case CMD_SEE:
    if (argc!=3 || TableGetIndex(tablePtr,argv[2],&row,&col)==TCL_ERROR) {
      if (!strlen(interp->result)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " see index\"", (char *)NULL);
      }
      result = TCL_ERROR;
      break;
    }
    /* Adjust from user to master coords */
    row -= tablePtr->rowOffset; col -= tablePtr->colOffset;
    if (!TableCellVCoords(tablePtr, row, col, &x, &x, &x, &x, 1)) {
      tablePtr->topRow  = row-1;
      tablePtr->leftCol = col-1;
      TableAdjustParams(tablePtr);
    }
    break;	/* SEE */

  case CMD_SELECTION:
    if (argc<3) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " selection option args\"", (char *)NULL);
      result=TCL_ERROR;
      break;
    }
    retval=parse_command(interp, sel_cmds, argv[2]);
    switch(retval) {
    case 0: 		/* failed to parse the argument, error */
      return TCL_ERROR;
    case SEL_ANCHOR:
      if (argc != 4 || TableGetIndex(tablePtr,argv[3],&row,&col) != TCL_OK) {
	if (!strlen(interp->result))
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " selection anchor index\"", (char *)NULL);
	result=TCL_ERROR;
	break;
      }
      tablePtr->flags |= HAS_ANCHOR;
      /* maintain appropriate real index */
      tablePtr->anchorRow = MIN(MAX(tablePtr->titleRows,
				    row-tablePtr->rowOffset),tablePtr->rows-1);
      tablePtr->anchorCol = MIN(MAX(tablePtr->titleCols,
				    col-tablePtr->colOffset),tablePtr->cols-1);
      break;
    case SEL_CLEAR:
      if ( argc != 4 && argc != 5 ) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " selection clear all|<first> ?<last>?\"",
			 (char *)NULL);
	result=TCL_ERROR;
	break;
      }
      if (strcmp(argv[3],"all") == 0) {
	for(entryPtr = Tcl_FirstHashEntry(tablePtr->selCells, &search);
	    entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
	  TableParseArrayIndex(&row, &col,
			       Tcl_GetHashKey(tablePtr->selCells,entryPtr));
	  Tcl_DeleteHashEntry(entryPtr);
	  TableCellCoords(tablePtr, row-tablePtr->rowOffset,
			  col-tablePtr->colOffset, &x, &y, &width, &height);
	  TableInvalidate(tablePtr, x, y, width, height,0); 
	}
      } else {
	int clo,chi,r1,c1,r2,c2;
	if (TableGetIndex(tablePtr,argv[3],&row,&col) == TCL_ERROR ||
	    (argc==5 && TableGetIndex(tablePtr,argv[4],&r2,&c2)==TCL_ERROR)) {
	  result = TCL_ERROR;
	  break;
	}
	key = 0;
	if (argc == 4) {
	  r1 = r2 = row;
	  c1 = c2 = col;
	} else {
	  r1 = MIN(row,r2); r2 = MAX(row,r2);
	  c1 = MIN(col,c2); c2 = MAX(col,c2);
	}
	switch (tablePtr->selectType) {
	case SEL_BOTH:
	  clo = c1; chi = c2;
	  c1 = tablePtr->colOffset;
	  c2 = tablePtr->cols-1+c1;
	  key = 1;
	  goto CLEAR_CELLS;
	CLEAR_BOTH:
	  key = 0;
	  c1 = clo; c2 = chi;
	case SEL_COL:
	  r1 = tablePtr->rowOffset;
	  r2 = tablePtr->rows-1+r1;
	  break;
	case SEL_ROW:
	  c1 = tablePtr->colOffset;
	  c2 = tablePtr->cols-1+c1;
	  break;
	}
	/* row/col are in user index coords */
      CLEAR_CELLS:
	for ( row = r1; row <= r2; row++ ) {
	  for ( col = c1; col <= c2; col++ ) {
	    TableMakeArrayIndex(row, col, buf1);
	    if ((entryPtr=Tcl_FindHashEntry(tablePtr->selCells, buf1))!=NULL) {
	      Tcl_DeleteHashEntry(entryPtr);
	      TableCellCoords(tablePtr, row-tablePtr->rowOffset,
			      col-tablePtr->colOffset,&x,&y,&width,&height);
	      TableInvalidate(tablePtr, x, y, width, height, 0);
	    }
	  }
	}
	if (key) goto CLEAR_BOTH;
      }
      break;	/* SELECTION CLEAR */
    case SEL_INCLUDES:
      if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " selection includes index\"", (char *)NULL);
	result = TCL_ERROR;
      } else if (TableGetIndex(tablePtr, argv[3], &row, &col) == TCL_ERROR) {
	result = TCL_ERROR;
      } else {
	TableMakeArrayIndex(row, col, buf1);
	if (Tcl_FindHashEntry(tablePtr->selCells, buf1))
	  interp->result = "1";
	else
	  interp->result = "0";
      }
      break;	/* SELECTION INCLUDES */
    case SEL_SET: {
      int clo,chi,r1,c1,r2,c2;
      if (argc < 4 || argc > 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " selection set first ?last?\"", (char *)NULL);
	result = TCL_ERROR;
	break;
      }
      if (TableGetIndex(tablePtr,argv[3],&row,&col) == TCL_ERROR ||
	  (argc==5 && TableGetIndex(tablePtr,argv[4],&r2,&c2)==TCL_ERROR)) {
	result = TCL_ERROR;
	break;
      }
      key = 0;
      /* maintain appropriate user index */
      row = MIN(MAX(tablePtr->titleRows+tablePtr->rowOffset, row),
		tablePtr->rows-1+tablePtr->rowOffset);
      col = MIN(MAX(tablePtr->titleCols+tablePtr->colOffset, col),
		tablePtr->cols-1+tablePtr->colOffset);
      if (argc == 4) {
	r1 = r2 = row;
	c1 = c2 = col;
      } else {
	r2 = MIN(MAX(tablePtr->titleRows+tablePtr->rowOffset, r2),
		 tablePtr->rows-1+tablePtr->rowOffset);
	c2 = MIN(MAX(tablePtr->titleCols+tablePtr->colOffset, c2),
		 tablePtr->cols-1+tablePtr->colOffset);
	r1 = MIN(row,r2); r2 = MAX(row,r2);
	c1 = MIN(col,c2); c2 = MAX(col,c2);
      }
      switch (tablePtr->selectType) {
      case SEL_BOTH:
	clo = c1; chi = c2;
	c1 = tablePtr->titleCols+tablePtr->colOffset;
	c2 = tablePtr->cols-1+tablePtr->colOffset;
	key = 1;
	goto SET_CELLS;
      SET_BOTH:
	key = 0;
	c1 = clo; c2 = chi;
      case SEL_COL:
	r1 = tablePtr->titleRows+tablePtr->rowOffset;
	r2 = tablePtr->rows-1+tablePtr->rowOffset;
	break;
      case SEL_ROW:
	c1 = tablePtr->titleCols+tablePtr->colOffset;
	c2 = tablePtr->cols-1+tablePtr->colOffset;
	break;
      }
    SET_CELLS:
      entryPtr = Tcl_FirstHashEntry(tablePtr->selCells, &search);
      for ( row = r1; row <= r2; row++ ) {
	for ( col = c1; col <= c2; col++ ) {
	  TableMakeArrayIndex(row, col, buf1);
	  if (Tcl_FindHashEntry(tablePtr->selCells, buf1) == NULL) {
	    Tcl_CreateHashEntry(tablePtr->selCells, buf1, &dummy);
	    TableCellCoords(tablePtr, row-tablePtr->rowOffset,
			    col-tablePtr->colOffset, &x, &y, &width, &height);
	    TableInvalidate(tablePtr, x, y, width, height, 0);
	  }
	}
      }
      if (key) goto SET_BOTH;

      /* Adjust the table for top left, selection on screen etc */
      TableAdjustParams(tablePtr);

      /* If the table was previously empty and we want to export the
       * selection, we should grab it now */
      if (entryPtr==NULL && tablePtr->exportSelection) {
	Tk_OwnSelection(tablePtr->tkwin, XA_PRIMARY, TableLostSelection,
			(ClientData) tablePtr);
      }
    }
    break;	/* SELECTION SET */
    }
    break;	/* SELECTION */

  case CMD_SET:
    /* sets any number of tags/indices to a given value */
    if (argc < 3 || (argc > 3 && (argc & 1))) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " set index ?value? ?index value ...?\"",
		       (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    /* make sure there is a data source to accept set */
    if (tablePtr->dataSource == DATA_NONE)
      break;
    if (argc == 3) {
      if (TableGetIndex(tablePtr, argv[2], &row, &col) != TCL_OK) {
	result = TCL_ERROR;
	break;
      }
      interp->result = TableGetCellValue(tablePtr, row, col);
    } else if (tablePtr->state == tkNormalUid) {
      for (i=2; i<argc; i++) {
	if (TableGetIndex(tablePtr, argv[i], &row, &col) != TCL_OK) {
	  result = TCL_ERROR;
	  break;
	}
	if (TableSetCellValue(tablePtr, row, col, argv[++i]) == TCL_ERROR) {
	  result = TCL_ERROR;
	  break;
	}
	row -= tablePtr->rowOffset;
	col -= tablePtr->colOffset;
	if (row == tablePtr->activeRow && col == tablePtr->activeCol) {
	  TableGetActiveBuf(tablePtr);
	}
	TableCellCoords(tablePtr, row, col, &x, &y, &width, &height);
	TableInvalidate(tablePtr, x, y, width, height, 0);
      }
    }
    break;

  case CMD_TAG:
    /* a veritable plethora of tag commands */
    /* do we have another argument */
    if (argc < 3) {
      Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			" tag option ?arg arg ...?\"", (char *) NULL);
      result = TCL_ERROR;
      break;
    }
    /* all the rest is now done in a separate function */
    result = TableTagCommand (tablePtr, argc, argv);
    break;	/* TAG */

  case CMD_VALIDATE:
    if (argc != 3) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " validate index\"", (char *) NULL);
      result = TCL_ERROR;
    } else if (TableGetIndex(tablePtr, argv[2], &row, &col) == TCL_ERROR) {
      result = TCL_ERROR;
    } else {
      value = tablePtr->validate;
      tablePtr->validate = 1;
      key = TableValidateChange(tablePtr, row, col, (char *) NULL,
				(char *) NULL, -1);
      tablePtr->validate = value;
      sprintf(interp->result, "%d", (key == TCL_OK) ? 1 : 0);
    }
    break;

  case CMD_VERSION:
    if (argc != 2) {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " version\"", (char *) NULL);
      result = TCL_ERROR;
    } else {
      Tcl_SetResult(interp, TBL_VERSION, TCL_VOLATILE);
    }
    break;

  case CMD_XVIEW:
  case CMD_YVIEW:
    if (argc == 2) {
      TableWhatCell(tablePtr,
		    Tk_Width(tablePtr->tkwin)-2*tablePtr->highlightWidth,
		    Tk_Height(tablePtr->tkwin)-2*tablePtr->highlightWidth,
		    &row, &col);
      if ( retval == CMD_YVIEW ) {
	sprintf(interp->result, "%g %g",
		(tablePtr->topRow-tablePtr->titleRows) /
		((double)(tablePtr->rows-tablePtr->titleRows)),
		(MAX(tablePtr->topRow-tablePtr->titleRows+1,
		     row-tablePtr->titleRows)+1) /
		((double)(tablePtr->rows-tablePtr->titleRows)));
      } else {
	sprintf(interp->result, "%g %g",
		(tablePtr->leftCol-tablePtr->titleCols) /
		((double)(tablePtr->cols-tablePtr->titleCols)),
		(MAX(tablePtr->leftCol-tablePtr->titleCols+1,
		     col-tablePtr->titleCols)+1) /
		((double)(tablePtr->cols-tablePtr->titleCols)));
      }
    } else {
      /* cache old topleft to see if it changes */
      int oldTop = tablePtr->topRow, oldLeft = tablePtr->leftCol;
      if (argc == 3) {
	if (Tcl_GetInt(interp, argv[2], &value) != TCL_OK) {
	  result = TCL_ERROR;
	  break;
	}
	if (retval == CMD_YVIEW) {
	  tablePtr->topRow  = value + tablePtr->titleRows;
	} else {
	  tablePtr->leftCol = value + tablePtr->titleCols;
	}
      } else {
	double frac;
	sub_retval = Tk_GetScrollInfo(interp, argc, argv, &frac, &value);
	switch (sub_retval) {
	case TK_SCROLL_ERROR:
	  result = TCL_ERROR;
	  break;
	case TK_SCROLL_MOVETO:
	  if (frac < 0) frac = 0;
	  if (retval == CMD_YVIEW) {
	    tablePtr->topRow = (int)(frac*tablePtr->rows)+tablePtr->titleRows;
	  } else {
	    tablePtr->leftCol = (int)(frac*tablePtr->cols)+tablePtr->titleCols;
	  }
	  break;
	case TK_SCROLL_PAGES:
	  TableWhatCell(tablePtr,
			Tk_Width(tablePtr->tkwin)-2*tablePtr->highlightWidth,
			Tk_Height(tablePtr->tkwin)-2*tablePtr->highlightWidth,
			&row, &col);
	  if (retval == CMD_YVIEW) {
	    tablePtr->topRow  += value * (row-tablePtr->topRow+1);
	  } else {
	    tablePtr->leftCol += value * (col-tablePtr->leftCol+1);
	  }
	  break;
	case TK_SCROLL_UNITS:
	  if (retval == CMD_YVIEW) {
	    tablePtr->topRow  += value;
	  } else {
	    tablePtr->leftCol += value;
	  }
	  break;
	}
      }
      /* maintain appropriate real index */
      tablePtr->topRow  = MAX(tablePtr->titleRows,
			      MIN(tablePtr->topRow, tablePtr->rows-1));
      tablePtr->leftCol = MAX(tablePtr->titleCols,
			      MIN(tablePtr->leftCol, tablePtr->cols-1));
      /* Do the table adjustment if topRow || leftCol changed */	
      if (oldTop != tablePtr->topRow || oldLeft != tablePtr->leftCol)
	TableAdjustParams(tablePtr);
    }
    break;
  }
  Tk_Release(clientData);
  return result;
}


/*
 *----------------------------------------------------------------------
 *
 * TableDestroy --
 *
 *	This procedure is invoked by Tcl_EventuallyFree
 *	to clean up the internal structure of a table at a safe time
 *	(when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the table is freed up (hopefully).
 *
 *----------------------------------------------------------------------
 */

static void
TableDestroy(ClientData clientdata)
{
  Table *tablePtr = (Table *) clientdata;
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;

  /* delete the variable trace */
  if (tablePtr->arrayVar != NULL) {
    Tcl_UntraceVar (tablePtr->interp, tablePtr->arrayVar,
		    TCL_TRACE_WRITES | TCL_TRACE_UNSETS | TCL_GLOBAL_ONLY,
		    (Tcl_VarTraceProc *)TableVarProc, (ClientData) tablePtr);
  }

  /* free the arrays with row/column pixel sizes */
  if (tablePtr->colPixels) ckfree ((char *) (tablePtr->colPixels));
  if (tablePtr->rowPixels) ckfree ((char *) (tablePtr->rowPixels));
  if (tablePtr->colStarts) ckfree ((char *) (tablePtr->colStarts));
  if (tablePtr->rowStarts) ckfree ((char *) (tablePtr->rowStarts));

  /* free the selection buffer */
  if (tablePtr->activeBuf) ckfree(tablePtr->activeBuf);

  /* delete the cache, row, column and cell style hash tables */
  Tcl_DeleteHashTable (tablePtr->cache);
  ckfree ((char *) (tablePtr->cache));
  Tcl_DeleteHashTable (tablePtr->rowStyles);
  ckfree ((char *) (tablePtr->rowStyles));
  Tcl_DeleteHashTable (tablePtr->colStyles);
  ckfree ((char *) (tablePtr->colStyles));
  Tcl_DeleteHashTable (tablePtr->cellStyles);
  ckfree ((char *) (tablePtr->cellStyles));
  Tcl_DeleteHashTable (tablePtr->flashCells);
  ckfree ((char *) (tablePtr->flashCells));
  Tcl_DeleteHashTable(tablePtr->selCells);
  ckfree ((char *) (tablePtr->selCells));
  Tcl_DeleteHashTable (tablePtr->colWidths);
  ckfree ((char *) (tablePtr->colWidths));
  Tcl_DeleteHashTable (tablePtr->rowHeights);
  ckfree ((char *) (tablePtr->rowHeights));

  /* Now free up all the tag information */
  entryPtr = Tcl_FirstHashEntry (tablePtr->tagTable, &search);
  for (; entryPtr != NULL; entryPtr = Tcl_NextHashEntry (&search)) {
    /* free up the GCs, etc */
    TableCleanupTag (tablePtr, (TagStruct *) Tcl_GetHashValue (entryPtr));
    /* free the memory */
    ckfree ((char *) Tcl_GetHashValue (entryPtr));
  }
  /* And delete the actual hash table */
  Tcl_DeleteHashTable (tablePtr->tagTable);
  ckfree ((char *) (tablePtr->tagTable));
  /* free up the stuff in the default tag */
  TableCleanupTag (tablePtr, &(tablePtr->defaultTag));

  /* free the configuration options in the widget */
  Tk_FreeOptions (TableConfig, (char *) tablePtr, tablePtr->display, 0);

  /* and free the widget memory at last! */
  ckfree ((char *) (tablePtr));
}


/*
 *--------------------------------------------------------------
 *
 * TableEventProc --
 *
 *	This procedure is invoked by the Tk dispatcher for various
 *	events on tables.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, it is redisplayed.
 *
 *--------------------------------------------------------------
 */

static void
TableEventProc(clientData, eventPtr)
     ClientData clientData;	/* Information about window. */
     XEvent *eventPtr;		/* Information about event. */
{
  Table *tablePtr = (Table *) clientData;
  ImpEvent(tablePtr);
  switch (eventPtr->type) {
  case Expose:
    TableInvalidate(tablePtr, eventPtr->xexpose.x, eventPtr->xexpose.y,
		    eventPtr->xexpose.width, eventPtr->xexpose.height, 0);
    break;
  case DestroyNotify:
    ImpDestroy(tablePtr);
    /* remove the command from the interpreter */
    if (tablePtr->tkwin) {
#ifdef ITCL_NAMESPACES
      Itk_SetWidgetCommand(tablePtr->tkwin, (Tcl_Command)NULL);
      tablePtr->tkwin = NULL;
      Tcl_DeleteCommand2(tablePtr->interp, tablePtr->widgetCmd);
#else
      Tcl_DeleteCommand (tablePtr->interp, Tk_PathName (tablePtr->tkwin));
      tablePtr->tkwin = NULL;
#endif
    }

    /* cancel any pending update or timer */
    if (tablePtr->flags & REDRAW_PENDING) {
      Tcl_CancelIdleCall (TableDisplay, (ClientData) tablePtr);
      tablePtr->flags &= ~REDRAW_PENDING;
    }
    if (tablePtr->cursorTimer != NULL) {
      Tcl_DeleteTimerHandler (tablePtr->cursorTimer);
      tablePtr->cursorTimer = NULL;
    }
    if (tablePtr->flashTimer != NULL) {
      Tcl_DeleteTimerHandler (tablePtr->flashTimer);
      tablePtr->flashTimer = NULL;
    }
    Tcl_EventuallyFree ((ClientData) tablePtr, (Tcl_FreeProc *) TableDestroy);
    break;

  case ConfigureNotify:
    TableAdjustParams(tablePtr);
    TableInvalidateAll(tablePtr,0);
    break;
  case FocusIn:
    if (eventPtr->xfocus.detail != NotifyInferior) {
      tablePtr->flags |= HAS_FOCUS;
      if (tablePtr->highlightWidth > 0) TableInvalidate(tablePtr,0,0,1,1,0);
      /* Turn the cursor on */
      TableConfigCursor (tablePtr);
      ImpFocusIn(tablePtr,eventPtr);
    }
    break;
  case FocusOut:
    if (eventPtr->xfocus.detail != NotifyInferior) {
      tablePtr->flags &= ~HAS_FOCUS;
      if (tablePtr->highlightWidth > 0) TableInvalidate(tablePtr,0,0,1,1,0);
      /* cancel the timer */
      TableConfigCursor (tablePtr);
      ImpFocusOut(tablePtr,eventPtr);
    }
    break;
  }
}

/*
 *----------------------------------------------------------------------
 *
 * TableConfigure --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a table widget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as colors, border width, etc.
 *	get set for tablePtr; old resources get freed, if there were any.
 *	Certain values might be constrained.
 *
 *----------------------------------------------------------------------
 */

static int
TableConfigure(interp, tablePtr, argc, argv, flags, forceUpdate)
    Tcl_Interp *interp;		/* Used for error reporting. */
    register Table *tablePtr;	/* Information about widget;  may or may
				 * not already have values for some fields. */
    int argc;			/* Number of valid entries in argv. */
    char **argv;		/* Arguments. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
    int forceUpdate;		/* Whether to force an update - required
				 * for initial configuration */
{
  Tcl_HashSearch search;
  int oldUse, oldCaching, oldExport, result = TCL_OK;
  char *oldVar;
#if (TK_MAJOR_VERSION == 8)
  Tk_FontMetrics fm;
#endif
  Tcl_DString error;

  oldExport	= tablePtr->exportSelection;
  oldCaching	= tablePtr->caching;
  oldUse	= tablePtr->useCmd;
  oldVar	= tablePtr->arrayVar;

  /* Do the configuration */
  if (Tk_ConfigureWidget (interp, tablePtr->tkwin, TableConfig, argc, argv,
			  (char *) tablePtr, flags) != TCL_OK)
    return TCL_ERROR;

  Tcl_DStringInit(&error);

  /* Any time we configure, reevaluate what our data source is */
  tablePtr->dataSource = DATA_NONE;
  if (tablePtr->caching) {
    tablePtr->dataSource |= DATA_CACHE;
  }
  if (tablePtr->command && tablePtr->useCmd) {
    tablePtr->dataSource |= DATA_COMMAND;
  } else if (tablePtr->arrayVar) {
    tablePtr->dataSource |= DATA_ARRAY;
  }

  /* Check to see if the array variable was changed */
  if (strcmp((tablePtr->arrayVar?tablePtr->arrayVar:""),(oldVar?oldVar:""))) {
    /* only do the following if arrayVar is our data source */
    if (tablePtr->dataSource & DATA_ARRAY) {
      /* ensure that the cache will flush later so it gets the new values */
      oldCaching = !(tablePtr->caching);
    }
    /* remove the trace on the old array variable if there was one */
    if (oldVar != NULL)
      Tcl_UntraceVar (interp, oldVar,
		      TCL_TRACE_WRITES | TCL_TRACE_UNSETS | TCL_GLOBAL_ONLY,
		      (Tcl_VarTraceProc *)TableVarProc, (ClientData)tablePtr);
    /* Check whether variable is an array and trace it if it is */
    if (tablePtr->arrayVar != NULL) {
      /* does the variable exist as an array? */
      if (Tcl_SetVar2(interp, tablePtr->arrayVar, TEST_KEY, "",
		      TCL_GLOBAL_ONLY) == NULL) {
	Tcl_DStringAppend(&error, "invalid variable value \"", -1);
	Tcl_DStringAppend(&error, tablePtr->arrayVar, -1);
	Tcl_DStringAppend(&error, "\": could not be made an array", -1);
	ckfree(tablePtr->arrayVar);
	tablePtr->arrayVar = NULL;
	tablePtr->dataSource &= ~DATA_ARRAY;
	result = TCL_ERROR;
      } else {
	Tcl_UnsetVar2 (interp, tablePtr->arrayVar, TEST_KEY, TCL_GLOBAL_ONLY);
	/* remove the effect of the evaluation */
	/* set a trace on the variable */
	Tcl_TraceVar (interp, tablePtr->arrayVar,
		      TCL_TRACE_WRITES | TCL_TRACE_UNSETS | TCL_GLOBAL_ONLY,
		      (Tcl_VarTraceProc *)TableVarProc, (ClientData) tablePtr);

	/* only do the following if arrayVar is our data source */
	if (tablePtr->dataSource & DATA_ARRAY) {
	  /* get the current value of the selection */
	  TableGetActiveBuf(tablePtr);
	}
      }
    }
  }
  if ((tablePtr->command && tablePtr->useCmd && !oldUse) ||
      (tablePtr->arrayVar && !(tablePtr->useCmd) && oldUse)) {
    /* our effective data source changed, so flush and
     * retrieve new active buffer */
    TableFlushCache(tablePtr);
    TableGetActiveBuf (tablePtr);
    forceUpdate = 1;
  } else if (oldCaching != tablePtr->caching) {
    /* caching changed, so just clear the cache for safety */
    TableFlushCache(tablePtr);
    forceUpdate = 1;
  }

  /* set up the default column width and row height */
#ifdef KANJI
  tablePtr->charWidth = XTextWidth(tablePtr->defaultTag.asciiFontPtr, "0", 1);
  if (tablePtr->defRowHeight==0)
    tablePtr->defRowHeight = (tablePtr->defaultTag.asciiFontPtr)->ascent + 
      (tablePtr->defaultTag.asciiFontPtr)->descent + 2;
#else
#if (TK_MAJOR_VERSION == 8)
  tablePtr->charWidth = Tk_TextWidth(tablePtr->defaultTag.fontPtr, "0", 1);
  Tk_GetFontMetrics(tablePtr->defaultTag.fontPtr, &fm);
  if (tablePtr->defRowHeight==0)
    tablePtr->defRowHeight = fm.linespace + 2;
#else
  tablePtr->charWidth = XTextWidth(tablePtr->defaultTag.fontPtr, "0", 1);
  if (tablePtr->defRowHeight==0)
    tablePtr->defRowHeight = (tablePtr->defaultTag.fontPtr)->ascent + 
      (tablePtr->defaultTag.fontPtr)->descent + 2;
#endif
#endif

  if (tablePtr->state != tkNormalUid && tablePtr->state != tkDisabledUid) {
    if (Tcl_DStringLength(&error)) Tcl_DStringAppend(&error, "\n", 1);
    Tcl_DStringAppend(&error, "bad state value \"", -1);
    Tcl_DStringAppend(&error, tablePtr->state, -1);
    Tcl_DStringAppend(&error, "\": must be normal or disabled", -1);
    tablePtr->state = tkNormalUid;
    result = TCL_ERROR;
  }

  if (tablePtr->insertWidth <= 0) {
    tablePtr->insertWidth = 2;
  }
  if (tablePtr->insertBorderWidth > tablePtr->insertWidth/2) {
    tablePtr->insertBorderWidth = tablePtr->insertWidth/2;
  }
  tablePtr->highlightWidth = MAX(0,tablePtr->highlightWidth);

  /* Ensure that certain values are within proper constraints */
  tablePtr->rows = MAX(1,tablePtr->rows);
  tablePtr->cols = MAX(1,tablePtr->cols);
  tablePtr->titleRows = MIN(MAX(0,tablePtr->titleRows),tablePtr->rows);
  tablePtr->titleCols = MIN(MAX(0,tablePtr->titleCols),tablePtr->cols);
  tablePtr->padX = MAX(0,tablePtr->padX);
  tablePtr->padY = MAX(0,tablePtr->padY);
  tablePtr->maxReqCols = MAX(0,tablePtr->maxReqCols);
  tablePtr->maxReqRows = MAX(0,tablePtr->maxReqRows);

  /*
   * Claim the selection if we've suddenly started exporting it and
   * there is a selection to export.
   */
  if (tablePtr->exportSelection && !oldExport &&
      (Tcl_FirstHashEntry(tablePtr->selCells, &search) != NULL)) {
    Tk_OwnSelection(tablePtr->tkwin, XA_PRIMARY, TableLostSelection,
		    (ClientData) tablePtr);
  }

  /* only do the full reconfigure if absolutely necessary */
  if (!forceUpdate) {
    int i;
    for (i = 0; i < argc-1; i += 2) {
      if (getCmdValue(update_config, argv[i])) {
	forceUpdate = 1;
	break;
      }
    }
  }
  if (forceUpdate) {
    /* 
     * Calculate the row and column starts 
     * Adjust the top left corner of the internal display 
     */
    TableAdjustParams(tablePtr);
    /* reset the cursor */
    TableConfigCursor(tablePtr);
    /* set up the background colour in the window */
    Tk_SetBackgroundFromBorder(tablePtr->tkwin, tablePtr->defaultTag.bgBorder);
    /* set the geometry and border */
    TableGeometryRequest(tablePtr);
    Tk_SetInternalBorder(tablePtr->tkwin,
			 tablePtr->borderWidth+tablePtr->highlightWidth);
    /* invalidate the whole table */
    TableInvalidateAll(tablePtr, 0);
  }
  /* FIX this is goofy because the result could be munged by other
   * functions.  Needs to be improved */
  Tcl_ResetResult(interp);
  if (result == TCL_ERROR) {
    Tcl_AddErrorInfo(interp, "\t(configuring table widget)");
    Tcl_DStringResult(interp, &error);
  }
  Tcl_DStringFree(&error);
  return result;
}

#ifndef CLASSPATCH
/*
 * As long as we wait for the Function in general
 *
 * This parses the "-class" option for the table.
 */

static void
Tk_ClassOption(Tk_Window tkwin,char *defaultclass, int *argcp,char ***argvp)
{
  char *classname = (((*argcp)<3) || (strcmp((*argvp)[2],"-class"))) ?
    defaultclass : ((*argcp)-=2,(*argcp)+=2,(*argvp)[1]);
  Tk_SetClass(tkwin,classname);
}
#endif

/*
 *--------------------------------------------------------------
 *
 * TableCmd --
 *
 *	This procedure is invoked to process the "table" Tcl
 *	command.  See the user documentation for details on what
 *	it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

static int
TableCmd(clientData, interp, argc, argv)
     ClientData clientData;	/* Main window associated with
				 * interpreter. */
     Tcl_Interp *interp;	/* Current interpreter. */
     int argc;			/* Number of arguments. */
     char **argv;		/* Argument strings. */
{
  register Table *tablePtr;
  Tk_Window main_window = (Tk_Window) clientData;
  Tk_Window tkwin;

  if (argc < 2) {
    Tcl_AppendResult (interp, "wrong # args: should be \"",
		      argv[0], "pathname ?options?\"", (char *) NULL);
    return TCL_ERROR;
  }

  tkwin = Tk_CreateWindowFromPath(interp, main_window, argv[1], (char *)NULL);
  if (tkwin == NULL) return TCL_ERROR;

  tablePtr			= (Table *) ckalloc(sizeof (Table));
  tablePtr->tkwin		= tkwin;
  tablePtr->display		= Tk_Display (tkwin);
  tablePtr->interp		= interp;
  tablePtr->rows		= 0;
  tablePtr->cols		= 0;
  tablePtr->defRowHeight	= 0;
  tablePtr->defColWidth		= 0;
  tablePtr->arrayVar		= NULL;
  tablePtr->borderWidth		= 0;
  tablePtr->defaultTag.anchor	= TK_ANCHOR_CENTER;
  tablePtr->defaultTag.bgBorder	= NULL;
  tablePtr->defaultTag.foreground = NULL;
  tablePtr->defaultTag.fontPtr	= NULL;
#ifdef KANJI
  tablePtr->defaultTag.asciiFontPtr = NULL;
  tablePtr->defaultTag.kanjiFontPtr = NULL;
#endif              /* KANJI */
  tablePtr->defaultTag.image	= NULL;
  tablePtr->defaultTag.imageStr	= NULL;
  tablePtr->defaultTag.relief	= TK_RELIEF_FLAT;
  tablePtr->yScrollCmd		= NULL;
  tablePtr->xScrollCmd		= NULL;
  tablePtr->insertBg		= NULL;
  tablePtr->cursor		= None;
  tablePtr->topRow		= 0;
  tablePtr->leftCol		= 0;
  tablePtr->titleRows		= 0;
  tablePtr->titleCols		= 0;
  tablePtr->anchorRow		= -1;
  tablePtr->anchorCol		= -1;
  tablePtr->activeRow		= -1;
  tablePtr->activeCol		= -1;
  tablePtr->oldTopRow		= -1;
  tablePtr->oldLeftCol		= -1;
  tablePtr->oldActRow		= -1;
  tablePtr->oldActCol		= -1;
  tablePtr->icursor		= 0;
  tablePtr->flags		= 0;
  tablePtr->drawMode		= DRAW_MODE_TK_COMPAT;
  tablePtr->colStretch		= STRETCH_MODE_NONE;
  tablePtr->rowStretch		= STRETCH_MODE_NONE;
  tablePtr->maxWidth		= 0;
  tablePtr->maxHeight		= 0;
  tablePtr->charWidth		= 0;
  tablePtr->colPixels		= (int *) 0;
  tablePtr->rowPixels		= (int *) 0;
  tablePtr->colStarts		= (int *) 0;
  tablePtr->rowStarts		= (int *) 0;
  tablePtr->colOffset		= 0;
  tablePtr->rowOffset		= 0;
  tablePtr->flashTime		= 3;
  tablePtr->cache	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->cache, TCL_STRING_KEYS);
  tablePtr->colWidths	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->colWidths, TCL_ONE_WORD_KEYS);
  tablePtr->rowHeights	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->rowHeights, TCL_ONE_WORD_KEYS);
  tablePtr->tagTable	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->tagTable, TCL_STRING_KEYS);
  tablePtr->rowStyles	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->rowStyles, TCL_ONE_WORD_KEYS);
  tablePtr->colStyles	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->colStyles, TCL_ONE_WORD_KEYS);
  tablePtr->cellStyles	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->cellStyles, TCL_STRING_KEYS);
  tablePtr->flashCells	= (Tcl_HashTable *) ckalloc(sizeof (Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->flashCells, TCL_STRING_KEYS);
  tablePtr->selCells=(Tcl_HashTable *)ckalloc(sizeof(Tcl_HashTable));
  Tcl_InitHashTable(tablePtr->selCells, TCL_STRING_KEYS);
  tablePtr->cursorTimer		= (Tcl_TimerToken)0;
  tablePtr->flashTimer		= (Tcl_TimerToken)0;
  tablePtr->selectMode		= NULL;
  tablePtr->activeBuf		= ckalloc(1);
  *(tablePtr->activeBuf)	= '\0';
  tablePtr->rowTagCmd		= NULL;
  tablePtr->colTagCmd		= NULL;
  tablePtr->highlightWidth	= 0;
  tablePtr->highlightBgColorPtr	= NULL;
  tablePtr->highlightColorPtr	= NULL;
  tablePtr->takeFocus		= NULL;
  tablePtr->state		= tkNormalUid;
  tablePtr->insertWidth		= 0;
  tablePtr->insertBorderWidth	= 0;
  tablePtr->insertOnTime	= 0;
  tablePtr->insertOffTime	= 0;
  tablePtr->autoClear		= 0;
  tablePtr->batchMode		= 0;
  tablePtr->flashMode		= 0;
  tablePtr->exportSelection	= 1;
  tablePtr->rowSep		= NULL;
  tablePtr->colSep		= NULL;
  tablePtr->browseCmd		= NULL;
  tablePtr->command		= NULL;
  tablePtr->selCmd		= NULL;
  tablePtr->valCmd		= NULL;
  tablePtr->validate		= 0;
  tablePtr->useCmd		= 1;
  tablePtr->caching		= 0;
  tablePtr->padX		= 0;
  tablePtr->padY		= 0;
  tablePtr->maxReqCols		= 0;
  tablePtr->maxReqRows		= 0;
  tablePtr->maxReqWidth		= 800;
  tablePtr->maxReqHeight	= 600;
  tablePtr->dataSource		= DATA_NONE;

  /* selection handlers needed here */

  Tk_ClassOption(tkwin, "Table", &argc, &argv);
  Tk_CreateEventHandler(tkwin,
			ExposureMask|StructureNotifyMask|FocusChangeMask,
			TableEventProc, (ClientData) tablePtr);
  Tk_CreateSelHandler(tkwin, XA_PRIMARY, XA_STRING,
		      TableFetchSelection, (ClientData) tablePtr, XA_STRING);
#ifdef KANJI
  {
    Atom textatom  = Tk_InternAtom(tkwin, "TEXT");
    Atom ctextatom = Tk_InternAtom(tkwin, "COMPOUND_TEXT");
    Tk_CreateSelHandler(tablePtr->tkwin, XA_PRIMARY, textatom,
			TableFetchSelectionCtext,
			(ClientData) tablePtr, ctextatom);
    Tk_CreateSelHandler(tablePtr->tkwin, XA_PRIMARY, ctextatom,
			TableFetchSelectionCtext,
			(ClientData) tablePtr, ctextatom);
  }
#endif /* KANJI */
  tablePtr->widgetCmd = Tcl_CreateCommand (interp, Tk_PathName (tkwin),
					   TableWidgetCmd,
					   (ClientData) tablePtr,
					   (Tcl_CmdDeleteProc *) NULL);
#ifdef ITCL_NAMESPACES
  Itk_SetWidgetCommand(tablePtr->tkwin, tablePtr->widgetCmd);
#endif
  if (TableConfigure (interp, tablePtr, argc - 2, argv + 2, 0, 1) != TCL_OK) {
    Tk_DestroyWindow (tkwin);
    return TCL_ERROR;
  }
  InitTagTable(tablePtr);
  interp->result = Tk_PathName (tkwin);
  return TCL_OK;
}

/* Function to call on loading the Table module */
#pragma export on
EXPORT(int,Tktable_Init)(interp)
    Tcl_Interp *interp;
#pragma export reset
{
  static char init_script[] =
    /* FIX - consider including TBL_VERSION in directory path */
#ifdef ITCL_NAMESPACES
    "if [catch {source [lindex $tcl_pkgPath 0]/Tktable/tkTable.itcl}] {\n"
#include "tkTableitcl.h"
#elif defined(MAC_TCL)
    "if [catch {source [lindex $tcl_pkgPath 0]/Tktable/tkTable.tcl}] {\n"
	"source -rsrc tkTable"
#else
    "if [catch {source [lindex $tcl_pkgPath 0]/Tktable/tkTable.tcl}] {\n"
#include "tkTabletcl.h"
#endif
    "}\n";
  if (Tcl_PkgRequire (interp, "Tcl", TCL_VERSION, 0) == NULL) {
    return TCL_ERROR;
  }
  if (Tcl_PkgRequire (interp, "Tk", TK_VERSION, 0) == NULL) {
    return TCL_ERROR;
  }
  if (Tcl_PkgProvide (interp, "Tktable", TBL_VERSION) != TCL_OK) {
    return TCL_ERROR;
  }
  Tcl_CreateCommand (interp, "table", TableCmd,
		     (ClientData) Tk_MainWindow(interp),
		     (Tcl_CmdDeleteProc *) NULL);
#ifndef X_WINDOWS
  tkNormalUid   = Tk_GetUid("normal");
  tkDisabledUid = Tk_GetUid("disabled");
#endif
  return Tcl_Eval(interp, init_script);
}

EXPORT(int,Tktable_SafeInit)(interp)
    Tcl_Interp *interp;
{
  static char init_script[] =
#ifdef ITCL_NAMESPACES
    "if [catch {source [lindex $tcl_pkgPath 0]/Tktable/tkTable.itcl}] {\n"
#include "tkTableitcl.h"
#elif defined(MAC_TCL)
    "if [catch {source [lindex $tcl_pkgPath 0]/Tktable/tkTable.tcl}] {\n"
	"source -rsrc tkTable"
#else
    "if [catch {source [lindex $tcl_pkgPath 0]/Tktable/tkTable.tcl}] {\n"
#include "tkTabletcl.h"
#endif
  "}\n";
  if (Tcl_PkgRequire (interp, "Tcl", TCL_VERSION, 0) == NULL) {
    return TCL_ERROR;
  }
  if (Tcl_PkgRequire (interp, "Tk", TK_VERSION, 0) == NULL) {
    return TCL_ERROR;
  }
  if (Tcl_PkgProvide (interp, "Tktable", TBL_VERSION) != TCL_OK) {
    return TCL_ERROR;
  }
  Tcl_CreateCommand (interp, "table", TableCmd,
		     (ClientData) Tk_MainWindow(interp),
		     (Tcl_CmdDeleteProc *) NULL);
#ifndef X_WINDOWS
  tkNormalUid   = Tk_GetUid("normal");
  tkDisabledUid = Tk_GetUid("disabled");
#endif
  return Tcl_Eval(interp, init_script);
}

#ifdef WIN32
/*
 *----------------------------------------------------------------------
 *
 * DllEntryPoint --
 *
 *	This wrapper function is used by Windows to invoke the
 *	initialization code for the DLL.  If we are compiling
 *	with Visual C++, this routine will be renamed to DllMain.
 *	routine.
 *
 * Results:
 *	Returns TRUE;
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

BOOL APIENTRY
DllEntryPoint(hInst, reason, reserved)
    HINSTANCE hInst;		/* Library instance handle. */
    DWORD reason;		/* Reason this function is being called. */
    LPVOID reserved;		/* Not used. */
{
  return TRUE;
}
#endif /* WIN32 */
