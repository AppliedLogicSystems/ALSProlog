##=================================================================================
#|				alsdev_main.tcl
#|		Copyright (c) 1997-98 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk main window for the alsdev environment
##=================================================================================

proc vTclWindow.topals {args} {
	global array proenv
	global tcl_platform

    set base .topals
    if {[winfo exists .topals]} {
        wm deiconify .topals ; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel .topals -class Toplevel \
		-background $proenv(win_general,background) 
    wm focusmodel .topals passive
    wm geometry .topals 559x567+149+178
    wm maxsize .topals 1265 994
    wm minsize .topals 1 1
    if {$tcl_platform(platform) != "macintosh"} {
    	# This command removes the zoom box from Macintosh windows.
    	wm overrideredirect .topals 0
    }
    wm resizable .topals 1 1
    wm deiconify .topals
    wm title .topals "ALS Prolog Environment"

	wm protocol $base WM_DELETE_WINDOW {wm iconify .topals ; unmap_alsdev_main }

		##------------------
		## Main menubar:
		##------------------
	if {$tcl_platform(platform) == "macintosh"} {
		set elipsis "…"
	} else {
		set elipsis "..."
	}
	
    menu .topals.mmenb -relief sunken -tearoff 0

	if {$tcl_platform(platform) == "macintosh"} {
		menu .topals.mmenb.apple -tearoff 0
		.topals.mmenb add cascade -menu .topals.mmenb.apple
		.topals.mmenb.apple add command -label "About ALS Prolog…" -command {Window show .about ; raise .about}
	}
	
	## File:
	menu .topals.mmenb.file -relief raised
    .topals.mmenb add cascade -label File -menu .topals.mmenb.file 
    .topals.mmenb.file add command -label New -accelerator "Meta-N" -state disabled
    if {$tcl_platform(platform) == "macintosh"} {
    	.topals.mmenb.file add command -label "New Project" -state disabled
    }
    .topals.mmenb.file add command -label "Open$elipsis" -accelerator "Meta-O" -command open_file
    .topals.mmenb.file add command -label Close -accelerator "Meta-W" -state disabled
    .topals.mmenb.file add separator
    .topals.mmenb.file add command -label Save -accelerator "Meta-S" -command save_file
    .topals.mmenb.file add command -label "Save As$elipsis" -command saveas_file -state disabled
    .topals.mmenb.file add separator
    if {$tcl_platform(platform) == "macintosh"} {
    	.topals.mmenb.file add command -label "Consult$elipsis" -command reconsult
    } else {
    	.topals.mmenb.file add command -label "Reconsult$elipsis" -command reconsult
    }
    .topals.mmenb.file add command \
        -label "Set Directory$elipsis" -command set_directory 
    .topals.mmenb.file add command -label "Source Tcl$elipsis" -command source_tcl -state disabled
	.topals.mmenb.file add command \
		-label "Clear Workspace" -command clear_workspace
    .topals.mmenb.file add separator
    .topals.mmenb.file add command -label "Page Setup$elipsis" \
		-command page_setup -state disabled
    .topals.mmenb.file add command -label "Print$elipsis" -accelerator "Meta-P"\
		-command page_setup -state disabled
    .topals.mmenb.file add separator
    .topals.mmenb.file add command -label Quit -accelerator "Meta-Q" -command exit_prolog

	## Edit:
	menu .topals.mmenb.edit  -relief raised
    .topals.mmenb add cascade -label Edit -menu .topals.mmenb.edit
    .topals.mmenb.edit add command \
        -label Undo -accelerator "Meta-Z" -command undo_action -state disabled
    .topals.mmenb.edit add separator
    .topals.mmenb.edit add command -label Cut \
		-accelerator "Meta-X" -command { cut_text .topals.txwin.text } -state disabled
    .topals.mmenb.edit add command \
        -label Copy -accelerator "Meta-C" -command {copy_text .topals.txwin.text }
    .topals.mmenb.edit add command \
        -label Paste -accelerator "Meta-V" -command {paste_text .topals.txwin.text }
    .topals.mmenb.edit add command \
        -label Clear -command {clear_text .topals.txwin.text } -state disabled
    .topals.mmenb.edit add separator
    .topals.mmenb.edit add command \
        -label {Select All} -accelerator "Meta-A" -command {select_all .topals.txwin.text } -state disabled
    .topals.mmenb.edit add separator
    .topals.mmenb.edit add command -label "Preferences$elipsis" \
			-command {Window show .alsdev_settings}
	if {$tcl_platform(platform) != "macintosh"} {
    	.topals.mmenb.edit add separator
    	.topals.mmenb.edit add command -label {Flush Input} -state disabled
    }

	## Project:


	menu .topals.mmenb.project -relief raised
	.topals.mmenb add cascade -label Project -menu .topals.mmenb.project
	
	if {$tcl_platform(platform) == "macintosh"} {
		.topals.mmenb.project add command -label "Add File" -command {error "not implemented"}
		.topals.mmenb.project add command -label "Remove File" -state disabled
	} else {
		.topals.mmenb.project add command \
			-label "Open Project" -command open_project -state disabled
		.topals.mmenb.project add command \
			-label "New Project" -command new_project -state disabled
		.topals.mmenb.project add command \
			-label "Save Project" -command save_project -state disabled
		.topals.mmenb.project add command \
			-label "Save As Project" -command save_as_project -state disabled
		.topals.mmenb.project add command \
			-label "Close Project" -command close_project -state disabled
	}
    .topals.mmenb.project add separator
    .topals.mmenb.project add command -label "Consult" -accelerator "Meta-K" -command consult_file
    .topals.mmenb.project add separator
	.topals.mmenb.project add command \
		-label "Dynamic Flags" \
		-command show_dynamic_flags
	menu .topals.mmenb.project.static -cursor {} -title "Static Flags"
	.topals.mmenb.project add cascade -label "Static Flags" \
		-menu .topals.mmenb.project.static 

#	.topals.mmenb.file add command \
#		-label "Add File to Project" -command add_file_to_project -state disabled
#	.topals.mmenb.file add command \
#		-label "Delete File from Project" -command delete_file_from_project -state disabled

	## Tools:
	menu .topals.mmenb.tools  -relief raised
    .topals.mmenb add cascade -label Tools -menu .topals.mmenb.tools
		## Debugger:
    .topals.mmenb.tools add checkbutton \
        -label Debugger -command exec_toggle_debugwin -variable proenv(debugwin)

	menu .topals.mmenb.tools.tclshell -relief raised
    .topals.mmenb.tools add cascade \
        -label {Tcl Shell} -menu .topals.mmenb.tools.tclshell -state disabled
    .topals.mmenb.tools.tclshell add command \
		-label "User Defined" -command {prolog_tcltk_shell user_def_choice}
    .topals.mmenb.tools.tclshell add command \
		-label "shl_tcli (System - Danger!)" \
		-command {prolog_tcltk_shell shl_tcli}

    .topals.mmenb.tools add separator 
		## DefStructs:
	menu .topals.mmenb.tools.defstr -relief raised
    .topals.mmenb.tools add cascade \
        -label {Structs} -menu .topals.mmenb.tools.defstr
    .topals.mmenb.tools.defstr add command \
		-label "Define New" -command new_defstruct
    .topals.mmenb.tools.defstr add command \
		-label "Edit" -command edit_defstruct 

	## Help:
	if {$tcl_platform(platform) != "macintosh"} {
		menu .topals.mmenb.help  -relief raised -tearoff 0
	    .topals.mmenb add cascade -label Help -menu .topals.mmenb.help
	    .topals.mmenb.help add command \
	        -label About -command {Window show .about ; raise .about}
	}


		##------------------------------------
		## Directory notif & Interrupt button:
		##------------------------------------
    frame .topals.cpd19 \
		-borderwidth 1 -height 30 -relief raised -width 30 

#    label .topals.cpd19.01 -relief flat -text "Directory:"

#    entry .topals.cpd19.02 \
#		-textvariable proenv(cwd) \
#		-highlightthickness 0 

    label .topals.cpd19.02 -relief flat -text {}

    button .topals.cpd19.03 \
        -font {lucida 10 bold} \
        -foreground $proenv(interrupt_button,foreground) \
		-padx 11 -pady 4 \
		-text Interrupt \
        -command interrupt_action

		##------------------
		## Text Window:
		##------------------
	if {$tcl_platform(platform) == "macintosh"} {
	    frame .topals.txwin \
			-borderwidth 0 -height 30 -relief raised
	} else {
	    frame .topals.txwin \3
			-borderwidth 1 -height 30 -relief raised 
	}
    scrollbar .topals.txwin.02 \
		-command {.topals.txwin.text yview} \
        -orient vert 
    text .topals.txwin.text \
        -height 26 -width 8 \
		-background $proenv(win_general,background) \
		-foreground $proenv(win_general,foreground) \
		-selectbackground $proenv(win_general,selectbackground) \
		-selectforeground $proenv(win_general,selectforeground) \
		-font $proenv(win_general,font) \
		-tabs $proenv(win_general,tabs) \
        -yscrollcommand {.topals.txwin.02 set} \
		-exportselection true

    if {$tcl_platform(platform) == "macintosh"} {
        .topals.txwin.text configure -highlightthickness 0
    }

#		-font {system 10 normal} \
		

    ###################
    # SETTING GEOMETRY
    ###################

	.topals configure -menu .topals.mmenb

    pack .topals.cpd19 \
        -anchor center -expand 0 -fill x -side top 

#    pack .topals.cpd19.01 \
#        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 

    pack .topals.cpd19.02 \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side left 
    pack .topals.cpd19.03 \
        -anchor center -expand 0 -fill x -padx 2 -pady 2 -side right 

    pack .topals.txwin \
        -anchor nw -expand yes -fill both -side top 
    grid columnconf .topals.txwin 0 -weight 1
    grid rowconf .topals.txwin 0 -weight 1
    grid .topals.txwin.02 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid .topals.txwin.text \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 

	bind .topals <Unmap> {unmap_alsdev_main}
	bind .topals <Map> {map_alsdev_main}

}

proc vTclWindow.dyn_flags {base} {
	global array proenv

    set base .dyn_flags
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm resizable $base 0 0
    wm overrideredirect $base 0
    wm deiconify $base
    wm title $base "Changable Prolog Flags"
	wm protocol .dyn_flags WM_DELETE_WINDOW {wm withdraw .dyn_flags}

    ###################
    # SETTING GEOMETRY
    ###################

}

#proc vTclWindow.settings {base} {
#	global array proenv
#
#    set base .settings
#    if {[winfo exists $base]} {
#        wm deiconify $base; return
#    }
#    ###################
#    # CREATING WIDGETS
#    ###################
#    toplevel $base -class Toplevel
#    wm focusmodel $base passive
#    wm geometry $base 412x121+326+186
#    wm maxsize $base 1137 870
#    wm minsize $base 1 1
#    wm overrideredirect $base 0
#    wm resizable $base 1 1
#    wm deiconify $base
#    wm title $base "ALSDEV Settings"
#	wm protocol .settings WM_DELETE_WINDOW {wm withdraw .settings}
#
#    frame $base.obps \
#        -borderwidth 1 -relief sunken 
#    label $base.obps.label \
#        -borderwidth 0 -padx 6 -text {*.obp locations:} 
#	set OBPMenu [tk_optionMenu $base.obps.optmenu proenv(obplcn) \
#		{Generated in Current (gic)} {Generated in Source(gis)} \
#		{Generated in Arch/Current (giac)} {Generated in Arch/Source(gias)}  ]
#	set proenv(obplcn) {Generated in Arch/Source(gias)}
#
#    frame $base.event_intv \
#        -borderwidth 1 -height 30 -relief sunken -width 30 
#    label $base.event_intv.label \
#        -padx 6 -text {Event interval (ms):} 
#    scale $base.event_intv.scale \
#        -font {Helvetica -10 bold} -orient horiz -to 1000.0 \
#        -variable EventInterval -width 10 
#    ###################
#    # SETTING GEOMETRY
#    ###################
#    pack $base.obps \
#        -anchor center -expand 0 -fill x -side top 
#    pack $base.obps.label \
#        -anchor e -expand 0 -fill none -side left 
#    pack $base.obps.optmenu \
#        -anchor center -expand 0 -fill none -side top 
#    pack $base.event_intv \
#        -anchor center -expand 0 -fill x -side top 
#    pack $base.event_intv.label \
#        -anchor center -expand 0 -fill none -side left 
#    pack $base.event_intv.scale \
#        -anchor center -expand 0 -fill x -side top 
#}


proc vTclWindow.about {base} {
    if {$base == ""} {
        set base .about
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 171x130+323+258
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "About alsdev"
	wm protocol .about WM_DELETE_WINDOW {wm withdraw .about}

    label $base.alsdev \
        -font {helvetica 14 {bold italic}} -text alsdev 
    frame $base.f1 \
        -borderwidth 1 -height 3 -relief sunken -width 30 
    label $base.alspro \
        -text {ALS Prolog} 
    label $base.dev \
        -text {Development Environment} 
    frame $base.f2 \
        -borderwidth 1 -height 3 -relief sunken -width 30 
    label $base.created \
        -font {helvetica 9 {}} -text {Copyright (c) 1998} 
    label $base.als \
        -text {Applied Logic Systems Inc.} 
    frame $base.f3 \
        -borderwidth 1 -height 9 -width 30 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.alsdev \
        -anchor center -expand 0 -fill none -pady 4 -side top 
    pack $base.f1 \
        -anchor center -expand 0 -fill none -side top 
    pack $base.alspro \
        -anchor center -expand 0 -fill none -side top 
    pack $base.dev \
        -anchor center -expand 0 -fill none -side top 
    pack $base.f2 \
        -anchor center -expand 0 -fill none -side top 
    pack $base.created \
        -anchor center -expand 0 -fill none -pady 1 -side top 
    pack $base.als \
        -anchor center -expand 0 -fill none -padx 2 -side top 
    pack $base.f3 \
        -anchor center -expand 0 -fill none -side top 
}

proc vTclWindow.break_choices {base} {
    if {$base == ""} {
        set base .break_choices
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 155x260+109+214
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Break Choices"
	wm protocol .break_choices WM_DELETE_WINDOW {wm withdraw .break_choices}

    label $base.label \
        -font {Helvetica -12 {bold italic}} -text {Break Choices} 
    button $base.show \
        -font {Helvetica -12 {}} -padx 11 -pady 4 -text {Show Broken Goal} 
    button $base.stack \
        -font {Helvetica -12 {}} -padx 11 -pady 4 -text {Show Stack Trace} 
    button $base.abort \
        -font {Helvetica -12 {}} -padx 11 -pady 4 -text {Abort Computation} 
    button $base.break_shell \
        -font {Helvetica -12 {}} -padx 11 -pady 4 -text {Enter Break Shell} 
    button $base.previous_level \
        -font {helvetica 9 {}} -padx 1 -pady 4 \
        -text {Return to Previous Break Level} 
    button $base.continue \
        -font {Helvetica -12 {}} -padx 11 -pady 4 \
        -text {Continue Computation} 
    button $base.debug \
        -font {Helvetica -12 {}} -padx 11 -pady 4 -text {Enter Debugger} 
    button $base.fail_goal \
        -font {Helvetica -12 {}} -padx 11 -pady 4 -text {Fail Broken Goal} 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.label \
        -anchor center -expand 0 -fill none -pady 3 -side top 
    pack $base.show \
        -anchor center -expand 0 -fill x -side top 
    pack $base.stack \
        -anchor center -expand 0 -fill x -side top 
    pack $base.abort \
        -anchor center -expand 0 -fill x -side top 
    pack $base.break_shell \
        -anchor center -expand 0 -fill x -side top 
    pack $base.previous_level \
        -anchor center -expand 0 -fill x -side top 
    pack $base.continue \
        -anchor center -expand 0 -fill x -side top 
    pack $base.debug \
        -anchor center -expand 0 -fill x -side top 
    pack $base.fail_goal \
        -anchor center -expand 0 -fill x -side top 
}





