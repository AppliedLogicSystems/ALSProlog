/*
 * decw.spc
 */

et)
     CompositeWidget  widget;{}

display DwtGetDisplay  (widget)
     Widget  widget;{}

Screen  *DwtGetScreen (widget)
     Widget  widget;{}

window DwtGetWindo (widget)
     Widget  widget; {}

Cardinal DwtNumberChildren (widget)
     CompositeWidget  widget; {}

void DwtResolvePartOffsets (widget_class, offset)
     WidgetClass   widget_class;
     DwtOffsetPtr  *offset; {}

Widget DwtDisplayCsMessage (parent_widget,name,
                            default_position, x, y,
                            style, message_vector,
                            widget, convert_proc,
                            ok_callback,  help_callback)
      Widget          parent_widget;
      char            *name;
      Boolean         default_position;
      Position        x;
      Position        y;
      unsigned char   style;
      int             *message_vector;
      Widget          *widget;
      int             (*convert_proc) ();
      DwtCallbackPtr  ok_callback;
      DwtCallbackPtr  help_callback; {}

Widget DwtDisplayVmsMessage (parent_widget, name,
                             default_position, x, y,
                             style, message_vector,
                             widget, convert_proc,
                             ok_callback, help_callback)
      Widget          parent_widget;
      char            *name;
      Boolean         default_position;
      Position        x;
      Position        y;
      unsigned char   style;
      int             *message_vector;
      Widget          *widget;
      int             (*convert_proc)  ();
      DwtCallbackPtr  ok_callback;
      DwtCallbackPtr  help_callback; {}

int DwtCloseHierachy(hierachy_id)
      DRMHierarchy                hierarchy_id; {}

int DwtDrmFreeResourceContext (context_id)
      DRMResourceContextPtr       context_id; {}

int DwtDrmGetResourceContext (alloc_func, free_func, size,
                                  context_id_return)
      char                        *((*alloc_func) ());
      void                        (*free_func) ();
      DRMSize                     size;
      DRMResourceContextPtr       *context_id_return; {}

int DwtDrmHGetIndexedLiteral (hierarchy_id, index, context_id)
      DRMHierarchy                hierarchy_id;
      String                      index;
      DRMResourceContextPtr       context_id; {}

buffer DwtDrmRCBuffer(context_id)
      DRMResourceContextPtr     context_id; {}

void DwtDrmRCSetType (context_id,type)
      DRMResourceContextPtr    context_id;
      DRMType                  type; {}

DRMsize DwtDrmRCSize (context_id)
      DRMResourceContextPtr       context_id; {}

DRMtype DwtDrmRCType (context_id)
      DRMResourceContextPtr       context_id; {}

int DwtFetchInterrfaceModule (hierarchy_id, module_name,
                              parent_widget, widget_return)
      DRMHierarchy     hierarchy_id;
      char             *module_name;
      Widget           parent_widget;
      Widget           *widget_return; {}

int DwtFetchSetValues (hierarchy_id, widget, args, num_args)
      DRMHierarchy    hierarchy_id;
      Widget          widget;
      ArgList         args;
      Cardinal        num_args; {}

int DwtFetchWidget(hierarchy_id, index, parent_widget,
                   widget_return, class_return)
      DRMHierarchy        hierarchy_id;
      String             index;
      Widget             parent_widget;
      Widget             *widget_return;
      DRMType            *class_return; {}

int DwtFetchWidgetOverride (hierarchy_id, index, parent_widget,
                            override_name, override_args,
                            override_num_args,
                            widget_return, class_return)
      DRMHierarchy      hierarchy_id;
      String            index;
      Widget            parent_widget;
      String            override_name;
      ArgList           override_args;
      int               override_num_args;
      Widget            *widget_return;
      DRMType           *class_return; {}

int DwtOpenHierarchy (num_files, file_names_list,
                      ancillary_strutures_list,
                      hierarchy_id_return)
      DRMCount              num_files;
      String                *file_names_list;
      IDBOSOpenParamPtr     *ancillary_structures_list;
      DRMHierarchy          *hierarchy_id_return; {}

int DwtRegisterClass (class_code, class_name, create_name,
                      create_proc, class_record)
      DRMType          class_code;
      String           class_name;
      String           create_name;
      Widget           (* create_proc)  ();
      WidgetClass      class_record; {}

int DwtRegisterDrmnames  (register_list, register_count)
      DRMRegisterArglist     register_list;
      DRMCount               register_count; {}

DwtFontList DwtAddFontList (list,font,charset)
      DwtFontList         list;
      XFontStruct         *font;
      unsigned long       charset; {}

DwtFontList DwtCreateFontList (font, charset)
      XFontStruct        *font;
      unsigned long      charset; {}

int DwtCSbytecmp (compound_stringl, compound_string2)
      DwtCompString   compound_stringl, compound_string2; {}

DwtCompString DwtCStrcat (compound_string1, compound_string2)
      DwtCompSTring      compound_string1, compound_string2; {}

DwtCompString DwtCStrncat (compound_string1, compound_string2,
                           num_chars)
      DwtCompSTring      compound_string1, compound_string2;
      int                num_chars; {}

DwtCompString DwtCstrcpy (compound_string1)
      DwtCompString      compound_string1; {}

DwtCompString DwtCStrncpy (compound_string1, num_chars)
      DwtCompString      compound_string1;
      int                num_chars; {}

int DwtCSempty (compound_Sting)
      DwtCompString  compound_string; {}

int DwtCStrlen (compound_string)
      DwtCompString  compound_string; {}

DwtCompSTring DwtCSString (text, charset, dir_r_to_l,
                           language,rend)
     char             *text;
     unsigned long    charset;
     int	      dir_r_to_l;
     unsigned long    language;
     DwtRendMask      rend; {}

int DwtGetNextSegment (context, text, charset,
                       dir_r_to_l, lang, rend)
      DwtCompStringContext     *context;
      char                     **text;
      unsigned long	       *charset;
      int                      *dir_r_to_l;
      unsigned long            *lang;
      DwtRendMask              *rend; {}

int DwtInitGetSegment (context, compound_string)
      DwtCompStringContext    *context;
      DwtCompString           compound_string; {}

DwtCompString DwtLatin1String(text)
      char     *text; {}

DwtCompString DwtString(text, charset, dir_r_to_l)
      char           *text;
      unsigned long  charset;
      int            dir_r_to_l; {}

int DwtBeginCopyToClipboard (display, window, clip_label,
                            widget, callback, item_id)
      Display         *display;
      Window          window;
      DwtCompString   clip_lable;
      Widget          widget;
      VoidProc        callback;
      unsigned long   *item_id; {}

int DwtCancelCopyFormat (Display, window, data_id)
      Display        *Display;
      Window         window;
      unsigned long  data_id; {}

void DwtCancelCopyToClipboard(display, window, item_id)
      Display        *display;
      Window         window;
      unsigned long  item_id; {}

int DwtClipboardLock(display, window)
      Display   *display;
      Window    window; {}

int DwtClipboardUnlock (display, window, remove_all_locks)
      Display     *display;
      Window      window;
      Boolean     remove_all_locks; {}

int DwtCopyFromClipboard (display, window, Format_name, buffer,
                          length, num_bytes, private_id)
      Display             *display;
      Window              window;
      char                *format_name;
      char                *buffer;
      unsigned long       length    
      unsigned long       *num_bytes;
      int                 *private_id; {}

int DwtCopyToClipboard (display, window, item_id, format_name,
                        buffer, length, private_id, data_id)
     Display            *display;
     Window             window;
     long               item_id
     char               *format_name;
     char               *buffer;
     unsigned long      length;
     int                private_id;
     unsigned long      *data_id; {}

int DwtEndCopyToClipboard (display, window, item_id)
      Display       *display;
      Window        window;
      unsigned      long item_id; {}

int DwtInquireNextPasteCount (display, window, count,
                              max_format_name_len)
      Display          *display;
      Window           window;
      unsigned long    *count;
      unsigned long    *max_format_anme_len; {}

int DwtInquireNextPasteFormat (display, window, number,
                               format_name_buf, buffer_len,
                               copied_len)
      Display          *display;
      Window           window;
      int              number;
      char             *format_name_buf;
      unsigned long    buffer_len;
      unsigned long    *copied_len; {}

int DwtInquireNextPasteLength (display, window, format_name,
                               length)
      Display          *display;
      Window           window;
      char             *format_name;
      unsigned long    *lenght;  {}

int DwtListPendingItems (display, window, format_name,
                         item_list, item_count)
       Display                   *display;
       Window                    window;
       char                      *format_name;
       DwtClipboardPendingList   *item_list;
       unsigned long             *item_count; {}

int DwtReCopyToClipboard(display, window, data_id,
                         buffer, length, private_id)
      Display        *display;
      Window         window;
      unsigned long  data_id;
      char           *buffer;
      unsigned long  length;
      int            private_id; {}

int DwtUndoCopyToClipboard(display, window)
      Display    *display;
      Window     window; {}

Widget DwtAttachedDB (parent_widget, name, default_position,
                      x, y, title, style, map_callback,
                      help_callback)
      Widget          parent_widget;
      char            *name,
      Boolean         default_position;
      Position        x;
      Position        y;
      DwtCompString   title;
      unsigned char   style;
      DwtCallbackPtr  map_callback;
      DwtCallbackPtr  help_callback; {}

Widget DwtCautionBox (parent_widget, name, default_position,
                      x, y, style, label, yes_label, no_label,
                      cancel_label, default_push_button,
                      callback, help_callback)
      Widget          parent_widget;
      char            *name;
      Boolean         default_position;
      Position        x;
      Position        y;
      unsigned        style;
      DwtCompString   label;
      DwtCompString   yes_label;
      DwtCompString   no_label;
      DwtCompString   cancel_label;
      int             default_push_button;
      DwtCallbackPtr  callback;
      DwtCallbackPtr  help_callback; {}

void DwtCommandAppend (widget,command)
      Widget    widget;
      char      *command; {}

void DwtCommandErrorMessage (widget,error)
     Widget    widget;
     char      *error; {}

void DwtCommandSet (widget, command)
      Widget      widget;
      char        *command; {}

Widget DwtCommandWindow (parent_widget, name, prompt, lines, 
                         callback, help_callback)

      Widget             parent_widget;
      char               *name;
      DwtCompString      prompt;
      int                lines;
      DwtCallbackPtr     callback;
      DwtCallbackPtr     help_callback; {}


Widget DwtDialogBox (parent_widget, name, default_position,
                     x, y, title, style, map_callback,
                     help_callback)
      Widget              parent_widget;
      char                *name;
      Boolean             default_position;
      Position            x;
      Position            y;
      DwtCompString       title;
      unsigned char       style;
      DwtCallbackPtr      map_callback;
      DwtcallbackPtr      help_callback; {}

Widget DwtFileSelection (parent_widget, name, x, y, title,
                         value, dirmask, visible_item_count,
                         format, default_position, callback,
                         help_callback)
      Widget             parent_widget;
      char               *name;
      Position           x;
      Position           y;
      DwtCompString      title;
      DwtCompString      value;
      DwtCompString      dirmask;
      int                visible_item_count;
      int                format;
      Boolean            default_position;
      DwtCallbackPtr     callback;
      DwtCallbackPtr     help_callback; {}

void DwtFileSelectionDoSearch (widget, dirmask)
       FileSelectionWidget      widget;
       DwtCompString            dirmask; {}

Widget DwtHelp (parent_widget, name, default_position, x, y,
                application_name, library_type, library_spec,
                first_topic, overview_topic, glossary_topic,
                unmap_callback)
       Widget              parent_widget;
       DwtCompString       name;
       Boolean             default_position;
       Position            x;
       Position            y;
	DwtCompString		application_name;
	unsigned int		library_type;
	DwtCompString		library_spec;
        DwtCompString 		first_topic;
	DwtCompString		overview_topic;
	DwtCompString		glossary_topic;
	DwtCallbackPtr		unmpa_callback; {}

Widget DwtLabel (parent_widget, name, x, y, label,
		 help_callback)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	DwtCompString		label;
	DwtCallBackPtr		help_callback; {}

Widget DwtListBox (parent_widget, name, x, y, items, item_count,
		   visible_item_count, callback, help_callback,
		   resize, horiz)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	DwtCompString		*items;
	int			item_count;
	int			visible_items_count;
	DwtCallbackPtr		callback;
	DwtCallbackPtr		help_callback;
	unsigned char		resize;
	Boolean			horiz;  {}

void DwtListBoxAddItem (widget, item, position)
	Widget			widget;
	DwtCompString		item;
	int			position; {}

void DwtListBoxDeleteItem (widget, item)
	Widget			widget;
	DwtCompString		item;  {}

void DwtListBoxDeletePos(widget, position)
	Widget			widget;
	int			position;  {}

void DwtListBoxDeselectAllItems(widget)
	Widget			widget;  {}

void DwtListBoxDeselectItem(widget, item)
	Widget			widget;
	DwtCompString		item;  {}

void DwtListBoxDeselectPos(widget, position)
	Widget			Widget;
	int			position;  {}

Boolean DwtListBoxItemExists(widget, item)
	Widget			widget;
	DwtCompString		item;  {}

Void DwtListBoxSelectItem(widget, item, notify)
	Widget			widget;
	DwtCompString		item;
	Boolean			notify;  {}

void DwtListBoxSelectPos (widget, position, notify)
	Widget			widget;
	int			position;
	Boolean			notify;  {}

void DwtListBoxSetHorizePos (widget, position)
	Widget			widget;
	int			position; {}

void DwtListBoxSetItem(widget, item)
	Widget			widget;
	DwtCompString		item;  {}

void DwtListBoxSetPos(widget, position)
	Widget			widget;
	int			position;  {}

Widget	DwtMainWindow (parent_widget, name, x, y, 
		       width, height)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	Dimension		width;
	Dimension		height;  {}

void DwtMainSetAreas (widget, menu bar, work_window,
		      command_window, h_scroll, v_scroll)
	Widget			widget;
	Widget			menubar;
	Widget			work_window;
	Widget			command_window;
	Widget			h_scroll;
	Widget			v_scroll;  {}

Widget DwtMenu (parent_widget, name, x, y, format, orientation, 
		entry_callback, map_callback, help_callback)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	int			format;
	unsigned char		orientation;
	DwtCallbackPtr		entry_callback;
	DwtCallbackPtr		map_callback;
	DwtCallbackPtr		help_callback;  {}

Widget DwtMenuBar(parent_widget, name, entry_callback,
		  help_callback)
	Widget			parent_widget;
	char			*name;
	DwtCallbackPtr		entry_callback;
	DwtCallbackPtr 		help_callback;  {}

void DwtMenuPosition
	Widget			widget;
	XEvent			*event;  {}

Widget DwtMessageBox (parent_widget, name, default_position,
 		      x, y, style, label, ok_label, callback,
		      help_callback)
	Widget			parent_widget;
	char			*name;
	Boolean			default_position;
	Position		x;
	Position		y;
	unsigned char		style;
	DwtCompString		label;
	DwtCompString 		ok_label;
	DwtCallbackPtr		callback;
	DwtCallbacckPtr		help_callback; {}

Widget DwtOptionMenu (parent_widget, name, x, y, label,
		      entry_callback, help_callback)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	DwtCompString		label;
	DwtCompString		entry_callback;
	DwtCallbackPtr		help_callback; {}

Widget DwtPullDownMenuEntry (parent_widget, name, x, y, label,
			     menu_id, callback, help_callback)
	Widget			parent_widget
	char 			*name;
	Position		x;
	Position		y;
	DwtCompString		label;
	Widget			menu_id;
	DwtCallbackPtr		callback;
	DwtCallbackPtr		help_callback;  {}

void DwtPullDownMenuEntryHilite
	Widget			widget;
	int			hilite; {}

Widget DwtPushButton (parent_widget, name, x, y, label,
 		      callback, help_callback)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	DwtCompString		label;
	DwtCallbackPtr		callback;
	DwtCallbackPtr		help_callback;  {}

Widget	DwtRadioBox (parent_widget, name, x, y, entry_callback,
 		     help_callback)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	DwtCallbackPtr		entry_callback;
	DwtCallbackPtr		help_callback;  {}


Widget DwtScale (parent_widget, name, x, y, width,
		height, scale_width, scale_height, title,
		min_value, max_value, decimal_points, value,
		orientation, callback, drag_callback,
		help_callback)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	DwtCompString		title;
	Dimension		width;
	Dimension		height;
	Dimension		scale_width;
	Dimension		scale_height;
	int			min_value;
	int			max_value;
	int			decimal_points;
	int			value;
	unsigned char		orientation;
	DwtCallbackPtr		callback;
	DwtCallbackPtr		drag_callback;
	DwtCallbackPtr		help_callback;  {}

void DwtScaleGetSlider (widget, value)
	Widget			widget;
	int			*value; {}


void DwtScaleSetSlider(widget, value)
	Widget			widget;
	int			value; {}


Widget DwtScrollBar (parent_widget, name, x, y, width, height,
		inc, page_inc, shown, int_value, min_value,
		max_value, orientation, callback,
		help_callback, unit_inc_callback,
		unit_dec_callback, page_inc_callback,
		page_dec_callback, to_top_callback,
		to_bottom_callback, drag_callback)
	Widget			parent_widget;
	char			*name;
	Position		x;
	Position		y;
	Dimension		width;
	Dimension		height;
	int			inc;
	int			page_inc;
	int			shown;
	int			int_value;
	int			min_value;
	int			max_value;
	unsigned char		orientation;
	DwtCallbackPtr		callback;
	DwtCallbackPtr		help_callback;
	DwtCallbackPtr		unit_inc_callback;
	DwtCallbackPtr		unit_inc_callback;
	DwtCallbackPtr		page_inc_callback;
	DwtCallbackPtr		page_dec_callback;
	DwtCallbackPtr		to_top_callback;
	DwtCallbackPtr		to_bottom_callback;
	DwtCallbackPtr		drag_callback;  {}

void DwtScrollBarGetSlider (widget, value, shown, inc, page_inc)
	Widget		widget;
	int		*value;
	int		*shown;
	int		*inc;
	int		*page_inc;  {}

void DwtScrollBarSetSlider (widget, value, shown, inc, page_inc,
		notify)
	Widget		widget;
	int		value;
	int		shown;
	int		inc;
	int		page_inc;
	Boolean		notify; {}

Widget DwtScrollWindow (parent_widget, name, x, y,
		width, height)
	Widget		parent_widget;
	char		*name;
	Position	x;
	Position	y;
	Dimension	width;
	Dimension	height;  {}

void DwtScrollWindowSetAreas (widget, h_scroll, v_scroll,
		work_region)
	Widget		widget;
	Widget		h_scroll;
	Widget		v_scroll;
	Widget		work_region;  {}

Widget DwtFileSelection (parent_widget, name,
		x, y, title, value, items, 
		items_count, visible_item_count, style,
		default_position,
		callback, help_callback)
	Widget		parent_widget;
	char		*name;
	Position	x;
	Position	y;
	DwtCompString	title;
	DwtCompString	value;
	DwtCompString	*items;
	int		item_count;
	int		visible_item_count;
	int		style;
	Boolean		default_position;
	DwtCallbackPtr	callback;
	DwtCallbackPtr	help_callback;  {}

Widget DwtSeparator (parent_widget, name, x, y, orientation)
	Widget		parent_widget;
	char		*name;
	Position	x;
	Position	y;
	unsigned char	orientation; {}

Widget DwtSText (parent_widget, name, x, y, cols, rows, 
		string_value)
	Widget		parent_widget;
	char		*name;
	Position	x;
	Position	y;
	int		cols;
	int		rows;
	char		*string_value;  {}

void DwtSTextClearSelection (widget, time)
	Widget		widget;
	Time		time;  {}

Boolean DwtSTextGetEditable (widget)
	Widget		widget; {}

int DwtSTextGetMaxLength(widget)
	Widget		widget; {}

char *DwtSTextGetSelection(widget)
	char		*selection;
	Widget		widget;  {}

char *DwtSTextGetString (widget)
	Widget		widget;  {}

void DwtXTextReplace (widget, from_pos, to_pos, value)
	Widget		widget;
	int		from_pos;
	int		to_pos;
	char		*value;  {}

void DwtSTextSetEditable (widget, editable)
	Widget 		widget;
	Boolean		editable;  {}

void DwtSTextSetMaxLength (widget, max_len)
	Widget		widget;
	int		max_len;  {}

void DwtXTextSetSelection (widget, first, last, time)
	Widget		widget;
	int		first;
	int		last;
	Time		time;  {}

void DwtSTextSetSTring (widget, string_value)
	Widget		widget;
	char		*string_value;  {}

Widget DwtToggleButton (parent_widget, name, x, y, label,
		value, callback, help_callback)
	Widget		parent_widget;
	char		*name;
	Position	x;
	Position	y;
	DwtCompSting	label;
	Boolean		value;
	DwtCallbackPtr	callback;
	DwtCallbackPtr	help_callback;  {}

Boolean	DwtToggleButtonGetState (widget)
	Widget		widget; {}

void DwToggleButtonSetState (widget, value, notify)
	Widget		widget;
	Boolean		value;
	Boolean		notify; {}

Widget DwtWindow (parent_widget, name, x, y, width,
		height, callback)
	Widget		parent_widget;
	char		*name;
	Position	x;
	Position	y;
	Dimension	width;
	Dimension	height;
	DwtCallbackPtr	callback; {}

Widget DwtWorkBox (parent_widget, name, default_position, 
		x, y, style, label, cancel_label,
		callback, help_callback)
	Widget		parent_widget;
	char		*name;
	Boolean		default_position;
	Position	x;
	Position	y;
	unsigned char	style;
	DwtCompString	label;
	DwtCompString	cancel_label;
	DwtCallbackPtr	callback;
	DwtCallbackPtr	help_callback; {}

Widget DwtAttachedDBCreate (parent_widget, name,
		override_arglist, override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		fraction_base;  {}

Widget DwtAttachedDBPopupCreate (parent_widget, name,
		override_arglist, override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtCautionBoxCreate (aparent_widget, name,
		override_arglist, override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtCommandWindowCreate (parent_widget, name, 
		override_arglist, override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtDialogBoxCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount; {}

Widget DwtDialogboxPopupCreate (parent_widget, name,
		override_arglist, override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtFileSelectionCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtHelpCreat (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtLabelCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtListBoxCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtMainWindow Create (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtMenuBarCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtMenuCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtMenuPopupCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtMenuPulldownCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtMessageBoxCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtOptionMenuCreate (parent_widget, name, override_arglist, 
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtPullDownMenuEntryCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtPushButtonCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtRadioBoxCreate (parent-widget, name, override-arglist, 
		override_argcount)
	Widget		parent-widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtScaleCreate (parent_widget, name, override-arglist,
		override-argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtScrollBarCreat (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtScrollWindowCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	Arglist		override-arglist;
	int		override_argcount;  {}

Widget DwtSelectionCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtSeparatorCreate (parent_widget, name, override_arglist,
		override-argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtSTextCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtToggleButtonCreate (parent-widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtWindowCreate (parent-widget, name, override-arglist,
	 	override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtWorkBoxCreat (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtLabelGadgetCreate(parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {} 

Widget DwtPushButtonGadgetCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent-widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtSeparatorGadgetCreate (parent_widget, name, override_arglist,
		override-argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}

Widget DwtToggleButtonGadgetCreate (parent_widget, name, override_arglist,
		override_argcount)
	Widget		parent_widget;
	char		*name;
	ArgList		override_arglist;
	int		override_argcount;  {}


