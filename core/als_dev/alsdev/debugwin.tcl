#!/usr/local/bin/wish
#############################################################################
# Visual Tcl v1.09 Project
#

#################################
# GLOBAL VARIABLES
#
global DebugWinButtonBack; 
set DebugWinButtonBack #cee8e6

global flat_print; 
global widget; 

#################################
# USER DEFINED PROCEDURES
#

global DebugResponse
proc vTclWindow.debugger_win {} {
    set base .debugger_win
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
	global DebugWinButtonBack; 
	global alsstyl;

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 553x391+323+175
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "ALS Prolog Debugger"

    frame $base.cpd17 \
        -borderwidth 1 -relief sunken 

	menu $base.menubar -tearoff 0 -relief sunken

	###########
	# Spy
	###########
	menu $base.menubar.spy -relief raised
	$base.menubar add cascade -label Spy -menu $base.menubar.spy
    $base.menubar.spy add checkbutton \
        -label {Spy [on predicate]} -command toggle_spy_win \
		-variable spywin_showing
    $base.menubar.spy add command \
        -label {Nospy [on predicate]} -state disabled
    $base.menubar.spy add command \
        -label {Spy When} -state disabled
    $base.menubar.spy add separator
    $base.menubar.spy add command \
        -label {Exit Debugger} -command exit_debugger

	###########
	# Settings
	###########
	menu $base.menubar.settings -relief raised
	$base.menubar add cascade -label Settings -menu $base.menubar.settings
    $base.menubar.settings add command \
        -label {Set Print Depth} 

	###########
	# Help
	###########
	menu $base.menubar.help -relief raised
	$base.menubar add cascade -label Help -menu $base.menubar.help

	global DebugResponse

set DBBpady 2

    frame $base.buttons \
        -borderwidth 1 -relief sunken
    button $base.buttons.creep \
        -background $DebugWinButtonBack \
        -padx 4 -text creep -underline 0 \
		-command { set DebugResponse Bc }
    button $base.buttons.skip \
        -background $DebugWinButtonBack \
        -padx 4 -text skip -underline 0 \
		-command { set DebugResponse Bs }
    button $base.buttons.leap \
        -background $DebugWinButtonBack \
        -padx 4 -text leap -underline 0 \
		-command { set DebugResponse Bl }
    button $base.buttons.retry \
        -background $DebugWinButtonBack \
        -padx 0 -text retry -underline 0 \
		-command { set DebugResponse Br }
    button $base.buttons.fail \
        -background $DebugWinButtonBack \
        -padx 4 -text fail -underline 0 \
		-command { set DebugResponse Bf }

    checkbutton $base.buttons.flat_print \
        -relief raised -text {print flat} -variable flat_print -padx 2 \
        -command { set DebugResponse Bm }
    button $base.buttons.statistics \
        -padx 2 -text statistics \
		-command { set DebugResponse Bi }
    button $base.buttons.stack_trace \
        -padx 2 -text stack \
		-command { set DebugResponse Bt }

    button $base.buttons.abort \
        -text abort -underline 0 -padx 4 \
        -command { set DebugResponse Ba }
    button $base.buttons.break \
        -text break -underline 0 -padx 4 \
		-command { set DebugResponse Bb }
    button $base.buttons.exit \
        -text exit -underline 0 -padx 4 \
		-command { set DebugResponse Be }

    frame $base.debug_status \
        -borderwidth 1 -relief sunken 
    label $base.debug_status.port_label \
        -padx 4 -text Port: 
    label $base.debug_status.port \
        -padx 5 -relief ridge -text {         } 
    label $base.debug_status.depth_label \
        -padx 5 -text Depth: 
    label $base.debug_status.depth \
        -padx 5 -relief ridge -text {        } 
    label $base.debug_status.call_num_label \
        -padx 5 -text {Call Num:} 
    label $base.debug_status.call_num \
        -padx 4 -relief ridge -text {         } 

    frame $base.textwin \
        -borderwidth 1 -relief raised 
    scrollbar $base.textwin.02 \
        -borderwidth 1 -command {.debugger_win.textwin.text yview} -orient vert 
    text $base.textwin.text \
        -font alsstyl(text-font) \
        -width 8 -yscrollcommand {.debugger_win.textwin.02 set} 
    ###################
    # SETTING GEOMETRY
    ###################

	$base configure -menu $base.menubar

    pack $base.buttons \
        -anchor center -expand 0 -fill x -side top 
    pack $base.buttons.creep \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.skip \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.leap \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.retry \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.fail \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.flat_print \
        -anchor center -expand 0 -fill y -padx 4 -side left 
    pack $base.buttons.statistics \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.stack_trace \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.abort \
        -anchor center -expand 0 -fill none -side right 
    pack $base.buttons.break \
        -anchor center -expand 0 -fill none -side right 
    pack $base.buttons.exit \
        -anchor center -expand 0 -fill none -side right 
    pack $base.debug_status \
        -anchor center -expand 0 -fill x -side top 
    pack $base.debug_status.port_label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.debug_status.port \
        -anchor center -expand 0 -fill none -side left 
    pack $base.debug_status.depth_label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.debug_status.depth \
        -anchor center -expand 0 -fill none -side left 
    pack $base.debug_status.call_num_label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.debug_status.call_num \
        -anchor center -expand 0 -fill none -side left 

    pack $base.textwin \
        -anchor center -expand 1 -fill both -side top 
    grid columnconf $base.textwin 0 -weight 1
    grid rowconf $base.textwin 0 -weight 1
    grid $base.textwin.02 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.textwin.text \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
}

proc vTclWindow.spy_select {} {
    set base .spy_select
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
	global SpyModuleMenu SpyModule
	global SpyPredMenu SpyPred
	global alsstyl;
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 421x200+278+184
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Spy Point Selection"
    label $base.typein_label \
        -text {Type in predicate info:} 
    entry $base.pred_entry
    label $base.or_label \
        -text {Or select by menus:} 
    frame $base.modules \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.modules.module_label \
        -text module: 
	set SpyModuleMenu [tk_optionMenu $base.modules.mods SpyModule user]
    frame $base.predicates \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.predicates.pred_label \
        -text predicate: 
	set SpyPredMenu [tk_optionMenu $base.predicates.preds SpyPred ??/?]
    frame $base.buttons \
        -borderwidth 1 -height 30 -width 30 
    button $base.buttons.ok \
        -command {spy_point ok} -padx 11 -pady 4 -text OK 
    button $base.buttons.cancel \
        -command {spy_point cancel} -padx 11 -pady 4 -text Cancel 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.typein_label \
        -anchor w -expand 0 -fill none -pady 4 -side top 
    pack $base.pred_entry \
        -anchor center -expand 0 -fill x -padx 12 -side top 
    pack $base.or_label \
        -anchor center -expand 0 -fill none -pady 4 -side top 

    pack $base.modules \
        -anchor w -expand 0 -fill x -padx 25 -pady 4 -side top 
    pack $base.modules.module_label \
        -anchor center -expand 0 -fill none -padx 12 -side left 
	pack $base.modules.mods \
        -anchor center -expand 1 -fill x -padx 12 -side left 

    pack $base.predicates \
        -anchor w -expand 0 -fill x -pady 4 -side top 
    pack $base.predicates.pred_label \
        -anchor center -expand 0 -fill none -padx 12 -side left 
    pack $base.predicates.preds \
        -anchor center -expand 1 -fill x -padx 12 -side left 

    pack $base.buttons \
        -anchor center -expand 0 -fill x -padx 65 -pady 8 -side top 
    pack $base.buttons.ok \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.cancel \
        -anchor center -expand 0 -fill none -side right 
}


