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
		set elipsis "É"
	} else {
		set elipsis "..."
	}
	
    menu .topals.mmenb -relief sunken -tearoff 0

	add_default_menus .topals.mmenb
	add_file_menu .topals.mmenb listener .topals
	add_edit_menu .topals.mmenb listener .topals
	add_project_menu .topals.mmenb listener .topals
	add_tools_menu .topals.mmenb listener .topals
	add_help_menu .topals.mmenb


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
	    frame .topals.txwin \
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





