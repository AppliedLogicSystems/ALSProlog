#!/usr/local/bin/wish
#############################################################################
# Visual Tcl v1.09 Project
#

#################################
# GLOBAL VARIABLES
#

global flat_print; 
global widget; 

#################################
# USER DEFINED PROCEDURES
#

global DebugResponse
proc vTclWindow.debugwin {base} {
	global array proenv
	global tcl_platform
	global DebugResponse
	global mod

    set base .debugwin
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 553x391+323+175
    wm maxsize $base 1137 870
    wm minsize $base 1 1
	if {$tcl_platform(platform) != "macintosh"} {
		# This command removes the zoom box from Macintosh windows.
	    wm overrideredirect $base 0
	}
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "ALS Prolog Debugger"
	wm protocol $base WM_DELETE_WINDOW unmap_alsdev_debug

	bind $base <Configure> "debugwin_configure_event $base %h %w %W"

	menu $base.menubar -tearoff 0 -relief sunken

	add_default_menus .$base.menubar
	add_file_menu $base.menubar debugwin $base
	add_edit_menu $base.menubar debugwin $base
	add_prolog_menu $base.menubar debugwin $base
	add_tools_menu $base.menubar debugwin $base
	add_help_menu $base.menubar

    frame $base.buttons \
        -borderwidth 1 -relief sunken
    button $base.buttons.creep \
        -background $proenv(debugwin_button,background) \
        -padx 4 -text creep -underline 0 \
		-command { set DebugResponse Bc }
    button $base.buttons.skip \
        -background $proenv(debugwin_button,background) \
        -padx 4 -text skip -underline 0 \
		-command { set DebugResponse Bs }
    button $base.buttons.leap \
        -background $proenv(debugwin_button,background) \
        -padx 4 -text leap -underline 0 \
		-command { prolog call debugger clear_source_traces ; set DebugResponse Bl }
    button $base.buttons.retry \
        -background $proenv(debugwin_button,background) \
        -padx 0 -text retry -underline 0 \
		-command { set DebugResponse Br }
    button $base.buttons.fail \
        -background $proenv(debugwin_button,background) \
        -padx 4 -text fail -underline 0 \
		-command { set DebugResponse Bf }

    button $base.buttons.statistics \
        -padx 2 -text statistics \
		-command { set DebugResponse Bi }
    button $base.buttons.stack_trace \
        -padx 2 -text stack \
		-command { set DebugResponse Bt }

#    button $base.buttons.abort \
#        -text abort -underline 0 -padx 4 \
#        -command { set DebugResponse Ba }
#    button $base.buttons.break \
#        -text break -underline 0 -padx 4 \
#		-command { set DebugResponse Bb }

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

	if {$tcl_platform(platform) == "macintosh"} {
	    scrollbar $base.vsb \
	        -borderwidth 0 -command {.debugwin.text yview} -orient vert 
	} else {
	    scrollbar $base.vsb \
	        -borderwidth 1 -command {.debugwin.text yview} -orient vert 
	}
    text $base.text \
		-background $proenv(.debugwin,background) \
		-foreground $proenv(.debugwin,foreground) \
		-font $proenv(.debugwin,font) \
        -width 40 -yscrollcommand {.debugwin.vsb set} 

    if {$tcl_platform(platform) == "macintosh"} {
        $base.text configure -highlightthickness 0
    }

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
    pack $base.buttons.statistics \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.stack_trace \
        -anchor center -expand 0 -fill none -side left 

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

	pack $base.vsb -side right -fill both
	pack $base.text -fill both -expand 1 -side left

	bind .debugwin <Unmap> {unmap_alsdev_debug}
	bind .debugwin <Map>   {map_alsdev_debug}

	# accelerators
	bind_accelerators .topals $mod debugwin
}

proc vTclWindow.spywin {base} {
	global array proenv

    set base .spywin
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
	global SpyModuleMenu SpyModule
	global SpyPredMenu SpyPred
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 421x231+278+184
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Spy Point Selection"
	wm protocol $base WM_DELETE_WINDOW hide_spywin

    label $base.typein_label \
        -text {Type in predicate info:} 
    entry $base.pred_entry
    frame $base.spacer1 \
        -borderwidth 1 -height 2 -relief sunken  -background black
    label $base.or_label \
        -text {Or select by menus:} 
    frame $base.modules \
        -borderwidth 1 -height 30 -relief flat -width 30 
    label $base.modules.module_label \
        -text Module: 
	set SpyModuleMenu [tk_optionMenu $base.modules.mods SpyModule user]
    frame $base.spacer2 \
        -borderwidth 1 -height 2 -relief sunken  -background black
	button $base.predicates \
        -command popup_spypoint_choice -padx 11 -pady 4 -text Predicates 
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
    pack $base.spacer1 \
        -anchor center -expand 0 -fill x -side top -pady 8
    pack $base.or_label \
        -anchor w -expand 0 -fill none -pady 4 -side top 

    pack $base.modules \
        -anchor w -expand 0 -fill x -padx 25 -pady 4 -side top 
    pack $base.modules.module_label \
        -anchor center -expand 0 -fill none -padx 12 -side left 
	pack $base.modules.mods \
        -anchor center -expand 1 -fill x -padx 12 -side left 
    pack $base.predicates \
        -anchor w -expand 0 -fill x -pady 4 -padx 35 -side top 

    pack $base.spacer2 \
        -anchor center -expand 0 -fill x -side top -pady 8
    pack $base.buttons \
        -anchor center -expand 0 -fill x -padx 65 -pady 8 -side top 
    pack $base.buttons.ok \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.cancel \
        -anchor center -expand 0 -fill none -side right 
}

proc vTclWindow.debug_source_trace {base Title} {
	global array proenv
	global tcl_platform

    if {$base == ""} {
        set base .debug_source_trace
    }
    if {$Title == ""} {
        set Title "Source Trace: Unknown File"
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    } 
	lappend proenv(debugwin,visible) $base

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 467x542+504+47
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    if {$tcl_platform(platform) != "macintosh"} {
    	# This command removes the zoom box from Macintosh windows.
    	wm overrideredirect $base 0
    }
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base $Title
	wm protocol $base WM_DELETE_WINDOW "wm iconify $base"

	if {$tcl_platform(platform) == "macintosh"} {
	    frame $base.textwin \
	        -borderwidth 0 -relief raised
	    scrollbar $base.textwin.vsb \
	        -borderwidth 0 -command [list $base.textwin.text yview] \
	        -orient vert 
	} else {
	    frame $base.textwin \
	        -borderwidth 1 -relief raised
	    scrollbar $base.textwin.vsb \
	        -borderwidth 1 -command [list $base.textwin.text yview] \
	        -orient vert 
	}
    text $base.textwin.text \
		-background $proenv(.debugwin,background) \
		-foreground $proenv(.debugwin,foreground) \
		-font $proenv(.debugwin,font) \
        -width 8 -yscrollcommand [list $base.textwin.vsb set] 

    if {$tcl_platform(platform) == "macintosh"} {
        $base.textwin.text configure -highlightthickness 0
    }

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.textwin \
        -anchor center -expand 1 -fill both -side top 
    grid columnconf $base.textwin 0 -weight 1
    grid rowconf $base.textwin 0 -weight 1
    grid $base.textwin.vsb \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.textwin.text \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
}

proc vTclWindow.spychoose {base} {
    if {$base == ""} {
        set base .spychoose
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 172x364+226+178
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Spy Points"
	wm protocol $base WM_DELETE_WINDOW { hide_spywin }

    frame $base.modid \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.modid.label \
        -text {Module: } 
    label $base.modid.module \
        -relief groove -text user 
    frame $base.slist \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.slist.listbox \
		-selectmode multiple \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand {.spychoose.slist.hscroll set} \
        -yscrollcommand {.spychoose.slist.vscroll set} 
    scrollbar $base.slist.hscroll \
        -borderwidth 1 -command {.spychoose.slist.listbox xview} -orient horiz \
        -width 10 
    scrollbar $base.slist.vscroll \
        -borderwidth 1 -command {.spychoose.slist.listbox yview} -orient vert \
        -width 10 
    frame $base.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.buttons.ok \
        -padx 11 -pady 4 -text OK -command {do_spychoose ok}
    button $base.buttons.cancel \
        -padx 11 -pady 4 -text Cancel -command {do_spychoose cancel}
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.modid \
        -anchor center -expand 0 -fill x -side top 
    pack $base.modid.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.modid.module \
        -anchor center -expand 0 -fill x -side top 
    pack $base.slist \
        -anchor center -expand 1 -fill both -side top 
    grid columnconf $base.slist 0 -weight 1
    grid rowconf $base.slist 0 -weight 1
    grid $base.slist.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.slist.hscroll \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.slist.vscroll \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    pack $base.buttons \
        -anchor center -expand 0 -fill x -side bottom 
    pack $base.buttons.ok \
        -anchor center -expand 0 -fill none -padx 2 -side left 
    pack $base.buttons.cancel \
        -anchor center -expand 0 -fill none -padx 2 -side right 
}
