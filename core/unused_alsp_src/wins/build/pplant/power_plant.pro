/* The prolog interface file for PowerPlant. */

/* LApplication. */
pp_new_LApplication(App) :- cpp_new('LApplication', App).

pp_Run(X) :- cpp_send(X, 'Run').



cpp_cast_ptr(object(O, 'LPane'), 'LPane', O).

pp_Enable(Pane) :- 
	cpp_cast_ptr(Pane, 'LPane', PanePtr),
	'$LPane::Enable'(PanePtr).


cpp_cast_ptr(object(O, 'LView'), 'LPane', Ptr) :- '$LViewToLPane'(0, Ptr).

cpp_cast_ptr(object(O, 'LWindow'), 'LView', Ptr) :- '$LWindowToLView'(O, Ptr).
cpp_cast_ptr(object(O, 'LWindow'), 'LPane', Ptr) :- '$LWindowToLPane'(O, Ptr).