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
    wm geometry $base $proenv(.debugwin,geometry)
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

	add_default_menus $base.menubar
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
		-command { send_prolog debugger_mgr clear_for_leap ; set DebugResponse Bl }

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

    frame $base.buttons.sep1 \
        -borderwidth 1 -relief flat -width 4 -background black

    button $base.buttons.abort \
        -padx 2 -text Abort \
		-command { set DebugResponse Ba }
    button $base.buttons.break \
        -padx 2 -text Break \
		-command { set DebugResponse Bb }

    button $base.buttons.interrupt \
        -font {lucida 10 bold} \
        -foreground $proenv(interrupt_button,foreground) \
		-padx 11 -pady 0 -text Interrupt \
        -command interrupt_action

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

    frame $base.stacklabel \
        -borderwidth 1 -relief raised 
    label $base.stacklabel.label \
        -padx 5 -relief flat -text {Predicate Call Stack} 

	listbox $base.stacklist \
		-background $proenv(.debugwin,background) \
		-foreground $proenv(.debugwin,foreground) \
		-font $proenv(.debugwin,font) \
        -yscrollcommand "$base.stacklist_vsb set" 
    scrollbar $base.stacklist_vsb \
        -borderwidth 1 -command "$base.stacklist yview" -orient vert 

    if {$tcl_platform(platform) == "macintosh"} {
        $base.text configure -highlightthickness 0
    }

    ###################
    # SETTING GEOMETRY
    ###################

	$base configure -menu $base.menubar

	grid columnconf $base 0 -weight 1
	grid columnconf $base 1 -weight 0
	grid rowconf $base 0 -weight 0
	grid rowconf $base 1 -weight 0
	grid rowconf $base 2 -weight 1
	grid rowconf $base 3 -weight 0
#	grid rowconf $base 4 -weight 1

    grid $base.buttons -in $base \
        -column 0 -row 0 -columnspan 2 -rowspan 1 -sticky ew

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
    pack $base.buttons.sep1 \
        -anchor center -expand 0 -fill y -side left -padx 4
    pack $base.buttons.abort \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.break \
        -anchor center -expand 0 -fill none -side left 
    pack $base.buttons.interrupt \
        -anchor center -expand 0 -fill none -side right 

    grid $base.debug_status \
        -column 0 -row 1 -columnspan 2 -rowspan 1 -sticky ew

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

	grid $base.text \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky nesw
	grid $base.vsb \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -sticky ns

#    grid $base.stacklabel \
#        -column 0 -row 3 -columnspan 2 -rowspan 1 -sticky ew
#    pack $base.stacklabel.label \
#        -anchor center -expand 0 -fill none -side left -padx 15
#	grid $base.stacklist \
#        -column 0 -row 4 -columnspan 1 -rowspan 1 -sticky nesw
#    grid $base.stacklist_vsb \
#        -column 1 -row 4 -columnspan 1 -rowspan 1 -sticky ns

	bind .debugwin <Unmap> {unmap_alsdev_debug}
	bind .debugwin <Map>   {map_alsdev_debug}
	bind .debugwin.text <KeyPress> {bell; .debugwin.text delete {insert-1 chars} insert}
	bindtags .debugwin.text {Text .debugwin.text .debugwin all}

	# accelerators
	bind_accelerators .debugwin $mod debugwin
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

proc vTclWindow.debug_settings {base} {
	global array proenv

    if {$base == ""} {
        set base .debug_settings
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
#    wm geometry $base 284x51+152+178
#    wm maxsize $base 1137 870
#    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Debugger Settings"
	wm protocol .debug_settings WM_DELETE_WINDOW {unpost_debug_subwin .debug_settings}

    frame $base.depth \
        -borderwidth 1 -relief sunken 
    label $base.depth.label \
        -text {Debug Print Depth:} 
    entry $base.depth.value \
        -textvariable proenv(debug_print_depth) 
	bind $base.depth.value <Return> { reset_print_depth }
    frame $base.flatness \
        -borderwidth 1 -relief sunken 
    label $base.flatness.label \
        -text {Debug Print Flatness:} 
    radiobutton $base.flatness.on \
        -text Flat -value flat -variable proenv(db_flatness) -command toggle_debug_flatness
    radiobutton $base.flatness.off \
        -text NonFlat -value nonflat -variable proenv(db_flatness) -command toggle_debug_flatness

    frame $base.leashing \
        -borderwidth 1 -relief sunken 

    label $base.leashing.label \
        	-text {Leashing}  
	checkbutton $base.leashing.call \
        	-text {call} -variable proenv(leash,call) \
			-command {exec_toggle_leash call} 
	checkbutton $base.leashing.exit \
        	-text {exit} -variable proenv(leash,exit) \
			-command {exec_toggle_leash exit} 
	checkbutton $base.leashing.redo \
        	-text {redo} -variable proenv(leash,redo) \
			-command {exec_toggle_leash redo} 
	checkbutton $base.leashing.fail \
        	-text {fail} -variable proenv(leash,fail) \
			-command {exec_toggle_leash fail} 

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.depth \
        -anchor w -expand 0 -fill x -side top 
    pack $base.depth.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.depth.value \
        -anchor center -expand 0 -fill none -side top 
    pack $base.flatness \
        -anchor n -expand 0 -fill x -side top 
    pack $base.flatness.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.flatness.on \
        -anchor center -expand 0 -fill none -side left 
    pack $base.flatness.off \
        -anchor center -expand 0 -fill none -side left 

    pack $base.leashing \
        -anchor w -expand 0 -fill x -side top 
    pack $base.leashing.label \
        -anchor center -expand 0 -fill none -side left 
	pack $base.leashing.call \
        -anchor center -expand 0 -fill none -side left 
	pack $base.leashing.exit \
        -anchor center -expand 0 -fill none -side left 
	pack $base.leashing.redo \
        -anchor center -expand 0 -fill none -side left 
	pack $base.leashing.fail \
        -anchor center -expand 0 -fill none -side left 

    wm geometry $base ""
	update
    wm resizable $base 0 0
}


proc vTclWindow.pred_info {base} {
    if {$base == ""} {
        set base .pred_info
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm maxsize $base 1137 870
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Predicate Information"
	wm protocol .pred_info WM_DELETE_WINDOW { unpost_debug_subwin .pred_info }

    frame $base.preds \
        -borderwidth 1 -relief sunken  
    label $base.preds.label \
        -relief flat -text Predicates 
    listbox $base.preds.listbox \
        -height 14 \
		-selectmode multiple \
        -yscrollcommand "$base.preds.vscrollbar set" 
    scrollbar $base.preds.vscrollbar \
        -orient vert \
        -command "$base.preds.listbox yview" 

    frame $base.mods \
        -borderwidth 1 -relief sunken  
    label $base.mods.label \
        -relief flat -text Modules 
    frame $base.mods.l2 \
        -borderwidth 1 -relief sunken  
    label $base.mods.l2.l1 \
        -relief flat -text {Focus: } 
    label $base.mods.l2.modfocus \
        -relief sunken -borderwidth 1  -text {   } 
    listbox $base.mods.listbox \
        -height 4 \
		-selectmode browse \
        -yscrollcommand "$base.mods.vscrollbar set" 
    scrollbar $base.mods.vscrollbar \
        -orient vert \
        -command "$base.mods.listbox yview" 

    frame $base.buttons \
        -borderwidth 1 -relief sunken  
    frame $base.buttons.f0 \
        -borderwidth 0 -relief flat -height 10
    label $base.buttons.label1 \
        -relief flat -text {Action On} 
    label $base.buttons.label2 \
        -relief flat -text {Selection} 
    frame $base.buttons.f1 \
        -borderwidth 0 -relief flat -height 10
    frame $base.buttons.spy \
        -borderwidth 0 -relief flat  
    button $base.buttons.spy.b1 \
        -padx 11 -pady 2 -text Spy \
        -command move_to_spying_list
    button $base.buttons.spy.b2 \
        -padx 11 -pady 2 -text arrow -image right_gif \
        -command move_to_spying_list

    frame $base.buttons.spywhen \
        -borderwidth 0 -relief flat  
    button $base.buttons.spywhen.b1 \
        -padx 11 -pady 2 -text {Spy When} \
        -command spywhen_move_to_spying_list -state disabled
    button $base.buttons.spywhen.b2 \
        -padx 11 -pady 2 -text arrow -image right_gif \
        -command spywhen_move_to_spying_list -state disabled

    frame $base.buttons.nospy \
        -borderwidth 0 -relief flat  
    button $base.buttons.nospy.b1 \
        -padx 11 -pady 2 -text arrow -image left_gif \
		-command remove_from_spying_list
    button $base.buttons.nospy.b2 \
        -padx 11 -pady 2 -text NoSpy \
		-command remove_from_spying_list
    button $base.buttons.find \
        -padx 11 -pady 2 -state disabled -text Find 
    button $base.buttons.listing \
        -padx 11 -pady 2 -text Listing \
		-command carry_out_listing
    button $base.buttons.wamlisting \
        -padx 11 -pady 2 -text {WAM Asm} \
		-command carry_out_listasm -state disabled
    button $base.buttons.refreshpreds \
        -padx 1 -pady 2 -text {Refresh Preds} \
		-command refresh_the_preds
    button $base.buttons.refreshmods \
        -padx 1 -pady 2 -text {Refresh Mods} \
		-command refresh_mods_list

    frame $base.spying \
        -borderwidth 1 -relief sunken  
    label $base.spying.label \
        -relief flat -text {Spying On} 
    listbox $base.spying.listbox \
		-selectmode multiple \
        -yscrollcommand "$base.spying.vscrollbar set" 
    scrollbar $base.spying.vscrollbar \
        -orient vert \
        -command "$base.spying.listbox yview" 
    frame $base.spying.buttons \
        -borderwidth 1 -relief sunken  
    button $base.spying.buttons.reset \
        -padx 11 -pady 4 -text {Reset All Spypoints} \
		-command reset_all_spypoints
    button $base.spying.buttons.nospy \
        -padx 11 -pady 4 -text {Remove All Spypoints} \
		-command remove_all_spypoints
    ###################
    # SETTING GEOMETRY
    ###################

	grid rowconf $base 0 -weight 1
	grid rowconf $base 1 -weight 0

	grid columnconf $base 0 -weight 1
	grid columnconf $base 1 -weight 0
	grid columnconf $base 2 -weight 1

    grid $base.preds \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -padx 2 -sticky ewns 
	grid rowconf $base.preds 0 -weight 0
	grid rowconf $base.preds 1 -weight 1
	grid columnconf $base.preds 0 -weight 1
	grid columnconf $base.preds 1 -weight 0
    grid $base.preds.label \
        -column 0 -row 0 -columnspan 2 -rowspan 1 -sticky ew
    grid $base.preds.listbox \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nsew
    grid $base.preds.vscrollbar \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -padx 2 -sticky nse

    grid $base.mods \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ewns
	grid rowconf $base.mods 0 -weight 0
	grid rowconf $base.mods 1 -weight 0
	grid rowconf $base.mods 2 -weight 1
	grid columnconf $base.mods 0 -weight 1
	grid columnconf $base.mods 1 -weight 0

    grid $base.mods.label \
        -column 0 -row 0 -columnspan 2 -rowspan 1 -sticky ew 

    grid $base.mods.l2 \
        -column 0 -row 1 -columnspan 2 -rowspan 1 -sticky ew  -padx 2
    pack $base.mods.l2.l1 \
		 -anchor center -expand 0 -fill none -side left 
    pack $base.mods.l2.modfocus \
		 -anchor center -expand 1 -fill x -side left 

    grid $base.mods.listbox \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky nsew
    grid $base.mods.vscrollbar \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -sticky nse 

		#### Center column buttons:
    grid $base.buttons \
        -column 1 -row 0 -columnspan 1 -rowspan 2 -padx 2 -sticky ns 

    pack $base.buttons.f0 \
		 -anchor center -expand 0 -fill x -side top 
    pack $base.buttons.label1 \
		 -anchor center -expand 0 -fill x -side top -pady 0
    pack $base.buttons.label2 \
		 -anchor center -expand 0 -fill x -side top -pady 0
    pack $base.buttons.f1 \
		 -anchor center -expand 0 -fill x -side top 

    pack $base.buttons.spy \
		 -anchor center -expand 0 -fill x -side top -pady 6
    pack $base.buttons.spy.b2 \
		-anchor w -expand 0 -fill none -side right
    pack $base.buttons.spy.b1 \
		-anchor w -expand 0 -fill none -side right

    pack $base.buttons.spywhen \
		 -anchor center -expand 0 -fill x -side top -pady 6
    pack $base.buttons.spywhen.b2 \
		-anchor w -expand 0 -fill none -side right
    pack $base.buttons.spywhen.b1 \
		-anchor w -expand 0 -fill none -side right

    pack $base.buttons.nospy \
		 -anchor center -expand 0 -fill x -side top -pady 6
    pack $base.buttons.nospy.b1 \
		-anchor w -expand 0 -fill none -side left
    pack $base.buttons.nospy.b2 \
		-anchor w -expand 0 -fill none -side left

    pack $base.buttons.find \
		 -anchor center -expand 0 -fill none -side top -pady 6
    pack $base.buttons.listing \
		 -anchor center -expand 0 -fill none -side top -pady 6
    pack $base.buttons.wamlisting \
		 -anchor center -expand 0 -fill none -side top -pady 6
    pack $base.buttons.refreshpreds \
		 -anchor center -expand 0 -fill none -side bottom -pady 6
    pack $base.buttons.refreshmods \
		 -anchor center -expand 0 -fill none -side bottom -pady 6

    grid $base.spying \
        -column 2 -row 0 -columnspan 1 -rowspan 2 -padx 2 -sticky nsew
	grid rowconf $base.spying 0 -weight 0
	grid rowconf $base.spying 1 -weight 1
	grid rowconf $base.spying 2 -weight 0
	grid columnconf $base.spying 0 -weight 1
	grid columnconf $base.spying 1 -weight 0

    grid $base.spying.label \
        -column 0 -row 0 -columnspan 2 -rowspan 1 -sticky ew 
    grid $base.spying.listbox \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nsew
    grid $base.spying.vscrollbar \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -sticky nse
    grid $base.spying.buttons \
        -column 0 -row 2 -columnspan 2 -rowspan 1 -sticky nsew 
    pack $base.spying.buttons.nospy \
		 -anchor center -expand 0 -fill x -side bottom -pady 6
    pack $base.spying.buttons.reset \
		 -anchor center -expand 0 -fill x -side bottom -pady 6

	bind  $base.mods.listbox <Double-Button-1> \
		{ set_module_focus [ get_selected_module ] }

	bind $base.spying.label <Configure> {refresh_spy_win}
	bind $base.spying.label <Map> {refresh_spy_win}

    bind $base.preds.listbox <Double-Button-1> {move_to_spying_list}
    bind $base.spying.listbox <Double-Button-1> {remove_from_spying_list}

	wm geometry $base ""
	update
	set BaseGeom [wm geometry .pred_info]
	set XPlace [string first "x" $BaseGeom]
	set WinWidth [string range $BaseGeom 0 [expr $XPlace - 1]]
	set WinHeight [string range $BaseGeom [expr $XPlace + 1] \
					[expr [string first "+" $BaseGeom] -1] ]
    wm minsize .pred_info $WinWidth $WinHeight
}



##########################################################

proc spy_preds_in_module {base module} {
	global array proenv
	global array cols2data

    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel 
    wm focusmodel $base passive
    wm geometry $base 350x250+384+076
    wm maxsize $base 1265 994
    wm minsize $base 300 200
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Spy Points in Module $module"

    frame $base.cols_fr \
		-borderwidth 1 -relief sunken 
    frame $base.cols_fr.l \
		-borderwidth 1 -relief sunken 

    label $base.cols_fr.l.label \
        -borderwidth 2 \
        -foreground #000000000000 -relief groove -text {Spying On} 

    frame $base.cols_fr.l.leftlist \
        -borderwidth 1 -relief raised 

    listbox $base.cols_fr.l.leftlist.lstbx \
		-selectmode extended \
        -yscrollcommand "$base.cols_fr.l.leftlist.vsb set" 
    scrollbar $base.cols_fr.l.leftlist.vsb -orient vert \
        -command "$base.cols_fr.l.leftlist.lstbx yview" 

    frame $base.cols_fr.m -borderwidth 1 -relief sunken 
    button $base.cols_fr.m.mv_left_btn \
        -padx 11 -pady 4 -text button -command "move_to_spying $module $base" \
		-image left_gif 
    button $base.cols_fr.m.move_rt_btn \
        -padx 11 -pady 4 -text button -command "move_to_no_spying $module $base" \
		-image right_gif 

    frame $base.cols_fr.r -borderwidth 1 -relief sunken 
    label $base.cols_fr.r.label \
        -borderwidth 2 -relief groove -text {No Spy} 
    frame $base.cols_fr.r.rightlist \
        -borderwidth 1 -relief raised 
    listbox $base.cols_fr.r.rightlist.lstbx \
        -yscrollcommand "$base.cols_fr.r.rightlist.vsb set" \
		-selectmode extended
    scrollbar $base.cols_fr.r.rightlist.vsb \
        -command "$base.cols_fr.r.rightlist.lstbx yview" \
        -orient vert 

	frame $base.btns_fr -borderwidth 1 -relief groove 
#    button $base.btns_fr.ok_btn \
#        -padx 8 -pady 3 -text {Install SpyPoints} \
#		-command "install_spypoints $module $base"
    button $base.btns_fr.dismiss_btn \
        -padx 8 -pady 3 -text Dismiss -command "wm withdraw $base"

    ###################
    # SETTING GEOMETRY
    ###################
    grid columnconf $base 0 -weight 1
    grid rowconf $base 0 -weight 1
    grid rowconf $base 1 -weight 0

    grid $base.cols_fr \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nsew

    grid columnconf $base.cols_fr 0 -weight 1
    grid columnconf $base.cols_fr 1 -weight 0
    grid columnconf $base.cols_fr 2 -weight 1
    grid rowconf $base.cols_fr 0 -weight 1

    grid $base.cols_fr.l \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nsew

    grid columnconf $base.cols_fr.l 0 -weight 1
    grid rowconf $base.cols_fr.l 0 -weight 0
    grid rowconf $base.cols_fr.l 1 -weight 1

    grid $base.cols_fr.l.label \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nsew
    grid $base.cols_fr.l.leftlist \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nsew

    grid columnconf $base.cols_fr.l.leftlist 0 -weight 1
    grid rowconf $base.cols_fr.l.leftlist 0 -weight 1
    grid $base.cols_fr.l.leftlist.lstbx \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.cols_fr.l.leftlist.vsb \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 


    grid $base.cols_fr.m \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nsew
    pack $base.cols_fr.m.mv_left_btn \
        -anchor center -expand 0 -fill none -side top -pady 15
    pack $base.cols_fr.m.move_rt_btn \
        -anchor center -expand 0 -fill none -side top -pady 35

    grid $base.cols_fr.r \
        -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky nsew

    grid columnconf $base.cols_fr.r 0 -weight 1
    grid rowconf $base.cols_fr.r 0 -weight 0
    grid rowconf $base.cols_fr.r 1 -weight 1

    grid $base.cols_fr.r.label \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nsew
    grid $base.cols_fr.r.rightlist \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nsew

    grid columnconf $base.cols_fr.r.rightlist 0 -weight 1
    grid rowconf $base.cols_fr.r.rightlist 0 -weight 1
    grid $base.cols_fr.r.rightlist.lstbx \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.cols_fr.r.rightlist.vsb \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 

	grid $base.btns_fr \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nsew
#    pack $base.btns_fr.ok_btn \
#        -anchor center -expand 0 -fill none -side left -padx 20
    pack $base.btns_fr.dismiss_btn \
        -anchor center -expand 0 -fill none -side right -padx 20
}

proc module_choose {ModsList} {
	global array proenv

	set base .module_choose

    if {[winfo exists $base]} {
		$base.listbox delete 0 end
		eval $base.listbox insert end $ModsList
        wm deiconify $base; return
		raise $base
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 170x150+152+178
    wm maxsize $base 1137 870
    wm minsize $base 170 150
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Choose Module for Spying"
	wm protocol .module_choose WM_DELETE_WINDOW {wm withdraw .module_choose}

    listbox $base.listbox \
		-selectmode browse \
        -yscrollcommand "$base.vsb set" 
    scrollbar $base.vsb -orient vert \
        -command "$base.listbox yview" 

	eval $base.listbox insert end $ModsList

	frame $base.buttons -borderwidth 1 -relief groove 
    button $base.buttons.ok \
        -padx 4 -pady 3 -text OK \
		-command "wm withdraw $base ; spy_preds_choice2 \
			\[$base.listbox get \[lindex \[$base.listbox curselection \] 0 \] \]"
    button $base.buttons.cancel \
        -padx 4 -pady 3 -text Dismiss -command "wm withdraw $base"

    grid columnconf $base 0 -weight 1
    grid columnconf $base 1 -weight 0
    grid rowconf $base 0 -weight 1
    grid rowconf $base 1 -weight 0

    grid $base.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nsew
	grid $base.vsb \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nsew
	grid $base.buttons \
        -column 0 -row 1 -columnspan 2 -rowspan 1 -sticky nsew

    pack $base.buttons.ok \
        -anchor center -expand 0 -fill none -side left -padx 8
    pack $base.buttons.cancel \
        -anchor center -expand 0 -fill none -side right -padx 8

	bind $base.listbox <Double-Button-1> "wm withdraw $base ; spy_preds_choice2 \
			\[$base.listbox get \[lindex \[$base.listbox curselection \] 0 \] \]"
}

##########################################################



proc vTclWindow.sys_mods {base} {
    if {$base == ""} {
        set base .sys_mods
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 144x149+193+203
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Modules:"
	wm protocol .sys_mods WM_DELETE_WINDOW {unpost_debug_subwin .sys_mods}
    ###################
    # SETTING GEOMETRY
    ###################
}

