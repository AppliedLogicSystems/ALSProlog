##=================================================================================
#|				alsdev_main.tcl
#|		Copyright (c) 1997-98 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk main window for the alsdev environment
##=================================================================================

proc vTclWindow.topals {args} {
	global array proenv
	global tcl_platform
	global mod

    set base .topals
    if {[winfo exists .topals]} {
        wm deiconify .topals ; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel .topals -class Toplevel -background $proenv(.topals,background) 
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
	
    menu .topals.mmenb -relief sunken -tearoff 0

	add_default_menus .topals.mmenb
	add_file_menu .topals.mmenb listener .topals
	add_edit_menu .topals.mmenb listener .topals
	add_prolog_menu .topals.mmenb listener .topals
	add_tools_menu .topals.mmenb listener .topals
	add_help_menu .topals.mmenb


		##------------------------------------
		## Directory notif & Interrupt button:
		##------------------------------------
    frame .topals.cpd19 \
		-borderwidth 1 -relief raised 
    label .topals.cpd19.02 -relief flat -pady 0 -text {}
    button .topals.cpd19.03 \
        -font {lucida 10 bold} \
        -foreground $proenv(interrupt_button,foreground) \
		-padx 11 -pady 0 -text Interrupt \
        -command interrupt_action

		##------------------
		## Text Window:
		##------------------
    scrollbar .topals.vsb \
		-command {.topals.text yview} \
        -orient vert 
    text .topals.text \
        -height 26 -width 8 \
		-background $proenv(.topals,background) \
		-foreground $proenv(.topals,foreground) \
		-selectbackground $proenv(.topals,selectbackground) \
		-selectforeground $proenv(.topals,selectforeground) \
		-font $proenv(.topals,font) \
		-tabs $proenv(.topals,tabs) \
        -yscrollcommand {.topals.vsb set} \
		-exportselection true

    if {$tcl_platform(platform) == "macintosh"} {
        .topals.text configure -highlightthickness 0
    }

    ###################
    # SETTING GEOMETRY
    ###################

	.topals configure -menu .topals.mmenb

    pack .topals.cpd19 \
        -anchor center -expand 0 -fill x -side top 
    pack .topals.cpd19.02 \
        -anchor center -expand 1 -fill x -padx 2 -side left 
    pack .topals.cpd19.03 \
        -anchor center -expand 0 -fill x -padx 2 -side right 

    pack .topals.vsb -side right -fill both
    pack .topals.text -fill both -expand 1 -side left

	bind .topals <Unmap> {unmap_alsdev_main}
	bind .topals <Map> {map_alsdev_main}

	# accelerators
	bind_accelerators .topals $mod listener

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

	frame $base.buttons \
		-borderwidth 1 -relief raised 
    button $base.buttons.dismiss \
		-padx 2 -text Dismiss \
        -command {wm withdraw .dyn_flags}
    button $base.buttons.save \
		-padx 2 -text {Save as Defaults} \
        -command {prolog call alsdev save_prolog_flags ; wm withdraw .dyn_flags}

    ###################
    # SETTING GEOMETRY
    ###################

    pack $base.buttons \
        -anchor center -expand 0 -fill x -side bottom 
    pack $base.buttons.dismiss \
        -anchor center -expand 0 -fill none -padx 2 -side left 
    pack $base.buttons.save \
        -anchor center -expand 0 -fill none -padx 2 -side right 

}

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



proc vTclWindow.static_flags {base} {
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
    wm geometry $base 255x260+109+214
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Static Prolog Flags"
	wm protocol .static_flags WM_DELETE_WINDOW {wm withdraw .static_flags}

    ###################
    # SETTING GEOMETRY
    ###################
}




