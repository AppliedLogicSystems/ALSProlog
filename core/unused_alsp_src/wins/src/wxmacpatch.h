/*---------------------------------------------------------*
 |			wxmacpatch.h
 |		Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *---------------------------------------------------------*/

WXID wxcListBoxCreate(WXID parentId, char *label, Bool multiple,
  int x, int y, int width, int height, long style, char *name);
WXID wxcDialogBoxCreate(WXID parentId, char *title, Bool modal,
  int x, int y, int width, int height, long style,
  char *name);
WXID wxcMultiTextCreate(WXID parentId, char *label, char *value,
  int x, int y, int width, int height, long style, char *name);
WXID wxcTextCreate(WXID parentId, char *label, char *value,
  int x, int y, int width, int height, long style, char *name);
WXID wxcTextWindowCreate(WXID parentId, int x, int y, int width, int height,
  long style, char *name);
WXID wxcCanvasCreate(WXID parentId, int x, int y, int width, int height, long style,
  char *name);
WXID wxcPanelCreate(WXID parentId, int x, int y, int width, int height, long style, char *name);
WXID wxcMessageCreate(WXID parentId, char *label, int x, int y,
  long style, char *name);
Bool wxcMenuAppend(WXID id, int item_id, char *menu_string, WXID submenu_id, char *help_string, Bool checkable);
WXID wxcMessageCreateFromBitmap(WXID parentId, WXID bitmapId, int x, int y,
  long style, char *name);
WXID wxcFrameCreate(WXID parentId, char *title, int x, int y,
  int width, int height, long style, char *name);
WXID wxcCheckBoxCreate(WXID parentId, char *label,
  int x, int y, int width, int height, long style, char *name);
WXID wxcButtonCreate(WXID parentId, char *label,
  int x, int y, int width, int height, long style, char *name);
WXID wxcButtonCreateFromBitmap(WXID parentId, WXID bitmapId,
  int x, int y, int width, int height, long style, char *name);
WXID wxcIconLoadFromFile(char *file, char *bitmapType);
Bool wxcFrameSetStatusText(WXID id, char *text, int i);
WXID wxcToolbarCreate(long parent, int x, int y, int w, int h, long style,
   int orientation, int rowsOrColumns, Bool createButtons, char *name);
WXID wxcChoiceCreate(WXID parentId, char *label, int x, int y,
  int width, int height, int n, char **choices, long style, char *name);
WXID wxcSliderCreate(WXID parentId, char *label,
  int value, int min_value, int max_value, int width, int x, int y,
  long style, char *name);
WXID wxcRadioBoxCreate(WXID parentId, char *label, int x, int y,
                       int width, int height, int n, char **choices,
                       int majorDim, long style, char *name);
