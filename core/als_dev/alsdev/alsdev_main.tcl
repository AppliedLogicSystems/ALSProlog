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
    wm geometry .topals $proenv(.topals,geometry)
	wm positionfrom .topals user
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
    wm geometry $base 171x205+323+258
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
        -borderwidth 1 -height 3 -relief sunken -width 30 
    label $base.developed_by \
        -font {helvetica 9 {}} -text {Developed by:}
    label $base.developers_1 \
        -font {helvetica 9 {}} -borderwidth 0 \
		-text {Ken Bowen  Kevin Buettner}
    label $base.developers_2 \
        -font {helvetica 9 {}} -borderwidth 0 \
		-text {Ilyas Cicekli  Chuck Houpt}
    label $base.developers_3 \
        -font {helvetica 9 {}} -borderwidth 0 \
		-text {Keith Hughes  Prabu Raman}
    label $base.developers_4 \
        -font {helvetica 9 {}} -borderwidth 0 \
		-text {Andy Turk}
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
    pack $base.developed_by \
        -anchor center -expand 0 -fill none -side top -pady 0 -ipady 0
    pack $base.developers_1 \
        -anchor center -expand 0 -fill none -side top -pady 0 -ipady 0
    pack $base.developers_2 \
        -anchor center -expand 0 -fill none -side top -pady 0 -ipady 0
    pack $base.developers_3 \
        -anchor center -expand 0 -fill none -side top -pady 0 -ipady 0
    pack $base.developers_4 \
        -anchor center -expand 0 -fill none -side top -pady 0 -ipady 0

    wm geometry $base ""
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



proc init_prj_spec \
	{base TextSlots ListOfFilesSlots ListSlots SlotNames FileTypes DfltDirs} {

	global array proenv

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 318x686+290+82
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 0
    wm deiconify $base
    wm title $base "Project Specification"

    frame $base.title \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.title.label \
        -anchor w -text {Project Title:} 
    entry $base.title.entry \
        -cursor {} -highlightthickness 0 

    frame $base.project_file \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.project_file.label \
        -anchor w -text {Project File Name:} 
    entry $base.project_file.entry \
        -cursor {} -highlightthickness 0 

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.title \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.title.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.title.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    pack $base.project_file \
        -anchor center -expand 0 -fill x -pady 2 -side top 
    pack $base.project_file.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.project_file.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    #########################
    # Single entry text slots
    #########################
	foreach TxtSl $TextSlots {
		install_text_slot $TxtSl $base [find_pair_value $TxtSl $SlotNames]
	}
    ###################
    # Search Paths 
    ###################
	create_sd_toggle  $base search_dirs \
		{Search Individual Directories:} \
		[list add_search_dirs $base.search_dirs.listbox] \
		[list del_search_dirs $base.search_dirs.listbox] \
		[list move_selection_up $base.search_dirs.listbox] \
		[list move_selection_down $base.search_dirs.listbox]

    ###################
    # Lists of Files 
    ###################
	foreach FS $ListOfFilesSlots {
		set FTs	[find_pair_value $FS $FileTypes]
		set FN	[find_pair_value $FS $SlotNames] 
		set XFN ""
		set DfltD [find_pair_value $FS $DfltDirs]
		append XFN $FN " (" $FTs "):"
		create_lofs_toggle $base $FS $XFN $FTs \
			[list add_to_files_list $FS $base.$FS.listbox $FTs $FN $DfltD ] \
			[list add_to_files_list_mult $FS $base.$FS.listbox $FTs $FN $DfltD ] \
			[list del_from_files_list $base.$FS.listbox] \
			[list move_selection_up $base.$FS.listbox] \
			[list move_selection_down $base.$FS.listbox]
	}
    ###################
    # Lists of Items
    ###################
	foreach LS $ListSlots {
		create_ls_toggle $base $LS [find_pair_value $LS $SlotNames] \
		[list add_to_list_$LS $base.$LS.listbox] \
		[list del_from_list_$LS $base.$LS.listbox] \
		[list move_selection_up $base.$LS.listbox] \
		[list move_selection_down $base.$LS.listbox]
	}

    ###################
    # CREATING WIDGETS
    ###################
    frame $base.sep_buttons \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $base.buttons \
        -borderwidth 1 -relief sunken 
    button $base.buttons.save \
        -command "save_project" -padx 11 -pady 4 -text Save 
    button $base.buttons.close \
        -command "close_project" -padx 11 -pady 4 -text Close 
    button $base.buttons.load \
        -command "load_this_project" -padx 11 -pady 4 -text {(Re)Load}
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.sep_buttons \
        -anchor center -expand 0 -fill x -side top 
    pack $base.buttons \
        -anchor center -expand 0 -fill x -side top 
    pack $base.buttons.save \
        -anchor center -expand 0 -fill none -padx 5 -side left 
    pack $base.buttons.close \
        -anchor center -expand 0 -fill none -padx 5 -side left 
    pack $base.buttons.load \
        -anchor center -expand 0 -fill none -padx 10 -side right 

	wm geometry $base ""
}

proc find_pair_value { Tag PairList } {
	set Value ""
	foreach Entry $PairList {
		if { [lindex $Entry 0] == $Tag } then {
			return [lindex $Entry 1]
		} 
	}
}

proc install_text_slot { Slot base SlotTitle } {
    frame $base.$Slot \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.$Slot.label \
        -anchor w -text $SlotTitle
    entry $base.$Slot.entry \
        -cursor {} -highlightthickness 0 

    pack $base.$Slot \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.$Slot.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.$Slot.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
}

proc show_text_slot {GuiPath Slot Value} {
	$GuiPath.$Slot.entry delete 0 end
	$GuiPath.$Slot.entry insert end $Value
}

proc show_list_slot {GuiPath Slot ValueList PrjMgrHandle} {
	$GuiPath.$Slot.listbox delete 0 end
	eval $GuiPath.$Slot.listbox insert end $ValueList
	set Last [$GuiPath.$Slot.listbox index end]
	bind $GuiPath.$Slot.listbox <Double-Button-1> \
		[list prj_slot_focus $Slot $GuiPath.$Slot.listbox $PrjMgrHandle]
}

proc create_ls_toggle { Win Which Title Add Del Up Down} {
	global proenv

    ###################
    # CREATING WIDGETS
    ###################
    frame $Win.sep_$Which \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $Win.ctl_$Which \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $Win.ctl_$Which.open_btn \
        -command "toggle_files_list $Win $Which" -image closed_ptr -padx 11 -pady 4 \
        -text button 
    label $Win.ctl_$Which.label \
        -text $Title
    frame $Win.$Which \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $Win.$Which.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand "$Win.$Which.02 set" \
        -yscrollcommand "$Win.$Which.03 set" \
		-height 0
    scrollbar $Win.$Which.02 \
        -borderwidth 1 -command "$Win.$Which.listbox xview" \
        -orient horiz -width 10 
    scrollbar $Win.$Which.03 \
        -borderwidth 1 -command "$Win.$Which.listbox yview" \
        -orient vert -width 10 
    frame $Win.$Which.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 

    button $Win.$Which.buttons.add \
        -command $Add -padx 11 -pady 4 -text {Add} 
    button $Win.$Which.buttons.del \
        -command $Del -padx 11 -pady 4 -text {Delete} 

    button $Win.$Which.buttons.up \
        -command $Up -padx 11 -pady 4 -text {} -image up_arrow_gif
    button $Win.$Which.buttons.down \
        -command $Down -padx 11 -pady 4 -text {} -image down_arrow_gif

	set proenv($Which) closed
    ###################
    # SETTING GEOMETRY
    ###################
    pack $Win.sep_$Which \
        -anchor center -expand 0 -fill x -side top 
    pack $Win.ctl_$Which \
        -anchor center -expand 0 -fill x -side top 
    pack $Win.ctl_$Which.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $Win.ctl_$Which.label \
        -anchor w -expand 0 -fill none -side left 
    pack $Win.$Which \
        -anchor center -expand 0 -fill x -side top 
    grid columnconf $Win.$Which 0 -weight 1
    grid rowconf $Win.$Which 0 -weight 1
    grid $Win.$Which.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $Win.$Which.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $Win.$Which.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $Win.$Which.buttons \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    pack $Win.$Which.buttons.add \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $Win.$Which.buttons.del \
        -anchor center -expand 0 -fill none -padx 10 -side left 

    pack $Win.$Which.buttons.down \
        -anchor center -expand 0 -fill none -padx 2 -side right 
    pack $Win.$Which.buttons.up \
        -anchor center -expand 0 -fill none -padx 2 -side right 

	pack forget $Win.$Which
}

proc create_sd_toggle { Win Which Title Add Del Up Down} {
	global proenv

    ###################
    # CREATING WIDGETS
    ###################
    frame $Win.sep_$Which \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $Win.ctl_$Which \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $Win.ctl_$Which.open_btn \
        -command "toggle_files_list $Win $Which" -image closed_ptr -padx 11 -pady 4 \
        -text button 
    label $Win.ctl_$Which.label \
        -text $Title
    frame $Win.$Which \
        -borderwidth 1 -height 30 -relief raised -width 30 
    frame $Win.$Which.prefs \
        -borderwidth 1 -relief raised 
    frame $Win.$Which.prefs.pathtype \
        -borderwidth 0 -relief flat 
    radiobutton $Win.$Which.prefs.pathtype.rel \
        -padx 10 -text relative -value relative -variable proenv(ppj_pathtype) 
    radiobutton $Win.$Which.prefs.pathtype.abs \
        -padx 10 -text absolute -value absolute -variable proenv(ppj_pathtype) 
	set proenv(ppj_pathtype) relative
    listbox $Win.$Which.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand "$Win.$Which.02 set" \
        -yscrollcommand "$Win.$Which.03 set" \
		-height 0
    scrollbar $Win.$Which.02 \
        -borderwidth 1 -command "$Win.$Which.listbox xview" \
        -orient horiz -width 10 
    scrollbar $Win.$Which.03 \
        -borderwidth 1 -command "$Win.$Which.listbox yview" \
        -orient vert -width 10 
    frame $Win.$Which.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 

    button $Win.$Which.buttons.add \
        -command "add_search_dirs $Win.$Which.listbox \"\$proenv(ppj_pathtype)\"" \
		-padx 11 -pady 4 -text {Add}
    button $Win.$Which.buttons.del \
        -command $Del -padx 11 -pady 4 -text {Delete} 

    button $Win.$Which.buttons.up \
        -command $Up -padx 11 -pady 4 -text {} -image up_arrow_gif
    button $Win.$Which.buttons.down \
        -command $Down -padx 11 -pady 4 -text {} -image down_arrow_gif

	set proenv($Which) closed
    ###################
    # SETTING GEOMETRY
    ###################
    pack $Win.sep_$Which \
        -anchor center -expand 0 -fill x -side top 
    pack $Win.ctl_$Which \
        -anchor center -expand 0 -fill x -side top 
    pack $Win.ctl_$Which.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $Win.ctl_$Which.label \
        -anchor w -expand 0 -fill none -side left 
    pack $Win.$Which \
        -anchor center -expand 0 -fill x -side top 

    pack $Win.$Which.prefs.pathtype \
        -anchor center -expand 0 -fill x -side top 
    pack $Win.$Which.prefs.pathtype.rel \
        -in $Win.$Which.prefs.pathtype -anchor center -expand 0 -fill none \
        -side left 
    pack $Win.$Which.prefs.pathtype.abs \
        -in $Win.$Which.prefs.pathtype -anchor center -expand 0 -fill none \
        -side left 

    grid columnconf $Win.$Which 0 -weight 1
    grid rowconf $Win.$Which 1 -weight 1
    grid $Win.$Which.prefs \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky ew 
    grid $Win.$Which.listbox \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $Win.$Which.02 \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    grid $Win.$Which.03 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -sticky ns 
    grid $Win.$Which.buttons \
        -column 0 -row 3 -columnspan 1 -rowspan 1 -sticky ew 
    pack $Win.$Which.buttons.add \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $Win.$Which.buttons.del \
        -anchor center -expand 0 -fill none -padx 10 -side left 

    pack $Win.$Which.buttons.down \
        -anchor center -expand 0 -fill none -padx 2 -side right 
    pack $Win.$Which.buttons.up \
        -anchor center -expand 0 -fill none -padx 2 -side right 

	pack forget $Win.$Which
}

proc toggle_files_list {Win Which} {
	global proenv 

	if {"$proenv($Which)"=="closed"} then {
    	$Win.ctl_$Which.open_btn configure -image open_ptr
		set proenv($Which) open
    	pack $Win.$Which  \
			-after $Win.ctl_$Which \
        	-anchor center -expand 0 -fill x -side top 
	} else {
    	$Win.ctl_$Which.open_btn configure -image closed_ptr
		set proenv($Which) closed
    	pack forget $Win.$Which 
	}
}



proc create_lofs_toggle { Win Which Title FileTypes Add AddMult Del Up Down} {
	global proenv

    ###################
    # CREATING WIDGETS
    ###################
    frame $Win.sep_$Which \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $Win.ctl_$Which \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $Win.ctl_$Which.open_btn \
        -command "toggle_files_list $Win $Which" -image closed_ptr -padx 11 -pady 4 \
        -text button 
    label $Win.ctl_$Which.label \
        -text $Title
    frame $Win.$Which \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $Win.$Which.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand "$Win.$Which.02 set" \
        -yscrollcommand "$Win.$Which.03 set" \
		-height 0
    scrollbar $Win.$Which.02 \
        -borderwidth 1 -command "$Win.$Which.listbox xview" \
        -orient horiz -width 10 
    scrollbar $Win.$Which.03 \
        -borderwidth 1 -command "$Win.$Which.listbox yview" \
        -orient vert -width 10 
    frame $Win.$Which.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 

    button $Win.$Which.buttons.add \
        -command $Add -padx 11 -pady 4 -text {Add} 
    button $Win.$Which.buttons.add_mult \
        -command $AddMult -padx 11 -pady 4 -text {Add Mult} 
    button $Win.$Which.buttons.del \
        -command $Del -padx 11 -pady 4 -text {Delete} 

    button $Win.$Which.buttons.up \
        -command $Up -padx 11 -pady 4 -text {} -image up_arrow_gif
    button $Win.$Which.buttons.down \
        -command $Down -padx 11 -pady 4 -text {} -image down_arrow_gif

	set proenv($Which) closed
    ###################
    # SETTING GEOMETRY
    ###################
    pack $Win.sep_$Which \
        -anchor center -expand 0 -fill x -side top 
    pack $Win.ctl_$Which \
        -anchor center -expand 0 -fill x -side top 
    pack $Win.ctl_$Which.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $Win.ctl_$Which.label \
        -anchor w -expand 0 -fill none -side left 
    pack $Win.$Which \
        -anchor center -expand 0 -fill x -side top 
    grid columnconf $Win.$Which 0 -weight 1
    grid rowconf $Win.$Which 0 -weight 1
    grid $Win.$Which.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $Win.$Which.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $Win.$Which.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $Win.$Which.buttons \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    pack $Win.$Which.buttons.add \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $Win.$Which.buttons.add_mult \
        -anchor w -expand 0 -fill none -padx 10 -side left 

    pack $Win.$Which.buttons.down \
        -anchor center -expand 0 -fill none -padx 2 -side right 
    pack $Win.$Which.buttons.up \
        -anchor center -expand 0 -fill none -padx 2 -side right 

    pack $Win.$Which.buttons.del \
        -anchor center -expand 0 -fill none -padx 10 -side right 

	pack forget $Win.$Which
}

proc rd_prj_spec {base TextSlots ListOfFilesSlots ListSlots } {
	set Result ""

	set TxtSs ""
    lappend TxtSs [list title [$base.title.entry get] ]
    lappend TxtSs [list project_file [$base.project_file.entry get] ]

	set LstSs ""
    lappend LstSs [list search_dirs [$base.search_dirs.listbox get 0 end] ]
#    lappend LstSs [list search_trees [$base.search_trees.listbox get 0 end] ]

	foreach TS $TextSlots {
    	lappend TxtSs [list $TS [$base.$TS.entry get] ]
	}
	foreach TS $ListOfFilesSlots {
    	lappend LstSs [list $TS [$base.$TS.listbox get 0 end] ]
	}
	foreach TS $ListSlots {
    	lappend LstSs [list $TS [$base.$TS.listbox get 0 end] ]
	}
	lappend Result  $TxtSs $LstSs
	return $Result
}

proc vTclWindow.ide_settings {base} {
	global proenv

    if {$base == ""} {
        set base .ide_settings
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 374x208+269+255
    wm maxsize $base 1137 870
    wm minsize $base 275 1
    wm overrideredirect $base 0
    wm resizable $base 1 0
    wm deiconify $base
    wm title $base "ALS IDE Settings"
    frame $base.heartbeat \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.heartbeat.label \
        -borderwidth 1 -text {Heartbeat: } 
    entry $base.heartbeat.entry \
        -width 5 
    label $base.heartbeat.sec \
        -borderwidth 1 -text {sec.   0.0} 
    label $base.heartbeat.hl \
        -borderwidth 1 -text 5.0 
    scale $base.heartbeat.scale17 \
        -orient horiz -resolution 0.05 -showvalue 0 -from 0.0 -to 5.0 \
		-command slider_change_ide_heartbeat \
		-variable proenv(heartbeat)
	bind $base.heartbeat.entry <Return> {entry_change_ide_heartbeat}
    frame $base.spacer1 \
        -background #000000 -borderwidth 2 -height 4 -relief groove \
        -width 125 
    frame $base.printdepth \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.printdepth.max \
        -borderwidth 1 -relief raised -text 500 
    label $base.printdepth.label \
        -borderwidth 1 -relief raised -text {Print depth: } 
    entry $base.printdepth.entry \
        -width 5 
    label $base.printdepth.zero \
        -borderwidth 1 -relief raised -text {       0} 
    scale $base.printdepth.slider \
        -orient horiz -showvalue 0 -from 0 -to 500 -resolution 1 \
		-command slider_change_ide_printdepth \
		-variable proenv(main_printdepth)
	bind $base.printdepth.entry <Return> {entry_change_ide_printdepth}
    frame $base.dcomptype \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.dcomptype.label \
        -borderwidth 1 -text {Printing depth type: } 
    radiobutton $base.dcomptype.radio_flat \
        -padx 10 -text Flat -value flat -variable proenv(main_depth_type) \
		-command change_ide_depth_type
    radiobutton $base.dcomptype.radio_nonflat \
        -padx 5 -text Non-flat -value nonflat -variable proenv(main_depth_type) \
		-command change_ide_depth_type
    frame $base.spacer2 \
        -background #000000 -borderwidth 2 -height 4 -relief groove \
        -width 125 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.heartbeat \
        -in .ide_settings -anchor center -expand 0 -fill x -side top 
    pack $base.heartbeat.label \
        -in .ide_settings.heartbeat -anchor center -expand 0 -fill none \
        -side left 
    pack $base.heartbeat.entry \
        -in .ide_settings.heartbeat -anchor center -expand 0 -fill none \
        -side left 
    pack $base.heartbeat.sec \
        -in .ide_settings.heartbeat -anchor center -expand 0 -fill none \
        -side left 
    pack $base.heartbeat.hl \
        -in .ide_settings.heartbeat -anchor center -expand 0 -fill none \
        -side right 
    pack $base.heartbeat.scale17 \
        -in .ide_settings.heartbeat -anchor center -expand 1 -fill x \
        -side right 
    pack $base.spacer1 \
        -in .ide_settings -anchor center -expand 0 -fill x -side top 
    pack $base.printdepth \
        -in .ide_settings -anchor center -expand 0 -fill x -side top 
    pack $base.printdepth.max \
        -in .ide_settings.printdepth -anchor center -expand 0 -fill none \
        -side right 
    pack $base.printdepth.label \
        -in .ide_settings.printdepth -anchor center -expand 0 -fill none \
        -side left 
    pack $base.printdepth.entry \
        -in .ide_settings.printdepth -anchor center -expand 0 -fill none \
        -side left 
    pack $base.printdepth.zero \
        -in .ide_settings.printdepth -anchor center -expand 0 -fill none \
        -side left 
    pack $base.printdepth.slider \
        -in .ide_settings.printdepth -anchor center -expand 0 -fill x \
        -side top 
    pack $base.dcomptype \
        -in .ide_settings -anchor center -expand 0 -fill x -side top 
    pack $base.dcomptype.label \
        -in .ide_settings.dcomptype -anchor center -expand 0 -fill none \
        -side left 
    pack $base.dcomptype.radio_flat \
        -in .ide_settings.dcomptype -anchor center -expand 0 -fill none \
        -side left 
    pack $base.dcomptype.radio_nonflat \
        -in .ide_settings.dcomptype -anchor center -expand 0 -fill none \
        -side left 
    pack $base.spacer2 \
        -in .ide_settings -anchor center -expand 0 -fill x -side top 

    wm geometry $base ""
}

proc slider_change_ide_printdepth { Value } {
	.ide_settings.printdepth.entry delete 0 end
	.ide_settings.printdepth.entry insert end $Value
	prolog call builtins change_ide_stream_depth -number $Value
}

proc entry_change_ide_printdepth { } {
	set InitValue [.ide_settings.printdepth.entry get]
	if { $InitValue > 500 } then { 
		set Value 500 
		slider_change_ide_printdepth  $Value 
	} elseif { $InitValue < 0 } then {
		set Value 0
		slider_change_ide_printdepth  $Value 
	} else { 
		set Value $InitValue 
		prolog call builtins change_ide_stream_depth -number $Value
	}
	.ide_settings.printdepth.slider set $Value 
}

proc slider_change_ide_heartbeat { Value } {
	.ide_settings.heartbeat.entry delete 0 end
	.ide_settings.heartbeat.entry insert end $Value
	prolog call builtins change_heartbeat -number $Value
}

proc entry_change_ide_heartbeat { } {
	set InitValue [.ide_settings.heartbeat.entry get]
	if { $InitValue > 500 } then { 
		set Value 500 
		slider_change_ide_heartbeat  $Value 
	} elseif { $InitValue < 0 } then {
		set Value 0
		slider_change_ide_heartbeat  $Value 
	} else { 
		set Value $InitValue 
		prolog call builtins change_heartbeat -number $Value
	}
	.ide_settings.heartbeat.slider set $Value 
}

proc set_heartbeat { Value } {
	.ide_settings.heartbeat.entry delete 0 end
	.ide_settings.heartbeat.entry insert end $Value
	.ide_settings.heartbeat.slider set $Value 
}

proc change_ide_depth_type { } {
	global proenv
	prolog call builtins change_ide_depth_type -atom $proenv(main_depth_type)
}

	#############################################
	######      FIND AND REPLACE
	#############################################

set proenv(searchdirect) forward
set proenv(searchnature) exact

proc vTclWindow.find_repl {base} {
	global proenv

    if {$base == ""} {
        set base .find_repl
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 510x200+140+392
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Find / Replace"
    frame $base.f1 \
        -borderwidth 0 -height 5 -relief flat -width 125 
			## INVISIBLE LABEL:
    label $base.f1.whichwin \
        -borderwidth 0 -relief flat -text $proenv(current_search_window)
    frame $base.search \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.search.label \
        -borderwidth 1 -relief flat -text {Search for:} 
    entry $base.search.entry
    frame $base.replace \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.replace.label \
        -borderwidth 1 -relief flat -text {Replace by:} 
    entry $base.replace.entry 
    frame $base.buttons \
        -borderwidth 0 -relief flat -width 125 
    button $base.buttons.find \
        -padx 11 -pady 4 -text {Find (Next)} -command edit_find_next
    button $base.buttons.replace \
        -padx 11 -pady 4 -text Replace -command edit_replace
    button $base.buttons.replaceall \
        -padx 11 -pady 4 -state disabled -text {Replace All} \
		-command edit_replace_all
    button $base.buttons.findreplace \
        -padx 11 -pady 4 -text {Find & Replace} \
		-command edit_find_replace
    frame $base.spacer2 \
        -background #000000 -height 3 -relief groove -width 125 
    frame $base.options \
        -borderwidth 2 -height 90 -relief groove -width 125 
    label $base.options.label \
        -borderwidth 1 -text {Search Options} 
    frame $base.options.direction \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.options.direction.label \
        -borderwidth 1 -text Direction: 

    radiobutton $base.options.direction.forward \
        -text Forward -value forward -variable proenv(searchdirect) \
		-command [list set \
			proenv([.find_repl.f1.whichwin cget -text],searchdirect) forward]
    radiobutton $base.options.direction.backward \
        -text Backward -value backward -variable proenv(searchdirect) \
		-command [list set \
			proenv([.find_repl.f1.whichwin cget -text],searchdirect) backward]
    frame $base.options2 \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.options2.label \
        -borderwidth 1 -text {Pattern Type:} 
    radiobutton $base.options2.exact \
        -text {Exact Match} -value exact -variable proenv(searchnature) \
		-command [list set \
			proenv([.find_repl.f1.whichwin cget -text],searchnature) exact]
    radiobutton $base.options2.regexp \
        -text {Regular Expression} -value regexp -variable proenv(searchnature) \
		-command [list set \
			proenv([.find_repl.f1.whichwin cget -text],searchnature) regexp]
    frame $base.wintgt \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.wintgt.label1 \
        -borderwidth 1 -text {Target Window:} 
    label $base.wintgt.label \
        -borderwidth 1 -text {} 

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.f1 \
        -in $base -anchor center -expand 0 -fill x -side top 
    pack $base.search \
        -in $base -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $base.search.label \
        -in $base.search -anchor center -expand 0 -fill none -padx 10 \
        -side left 
    pack $base.search.entry \
        -in $base.search -anchor center -expand 1 -fill x -padx 5 \
        -side top 
    pack $base.replace \
        -in $base -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $base.replace.label \
        -in $base.replace -anchor center -expand 0 -fill none -padx 10 \
        -side left 
    pack $base.replace.entry \
        -in $base.replace -anchor center -expand 1 -fill x -padx 5 \
        -side top 
    pack $base.buttons \
        -in $base -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $base.buttons.find \
        -in $base.buttons -anchor center -expand 0 -fill none -padx 10 \
        -side left 
    pack $base.buttons.replace \
        -in $base.buttons -anchor center -expand 0 -fill none -padx 10 \
        -side left 
    pack $base.buttons.replaceall \
        -in $base.buttons -anchor center -expand 0 -fill none -padx 10 \
        -side right 
    pack $base.buttons.findreplace \
        -in $base.buttons -anchor center -expand 0 -fill none -padx 10 \
        -side right 
    pack $base.spacer2 \
        -in $base -anchor center -expand 0 -fill x -side top 
    pack $base.options \
        -in $base -anchor center -expand 0 -fill x -side top 
    pack $base.options.label \
        -in $base.options -anchor center -expand 0 -fill none -padx 10 \
        -side left 
    pack $base.options.direction \
        -in $base.options -anchor center -expand 0 -fill none \
        -side right 
    pack $base.options.direction.label \
        -in $base.options.direction -anchor center -expand 0 -fill none \
        -side left 
    pack $base.options.direction.forward \
        -in $base.options.direction -anchor center -expand 0 -fill none \
        -padx 5 -side left 
    pack $base.options.direction.backward \
        -in $base.options.direction -anchor center -expand 0 -fill none \
        -padx 5 -side left 
    pack $base.options2 \
        -in $base -anchor center -expand 0 -fill x -side top 
    pack $base.options2.label \
        -in $base.options2 -anchor center -expand 0 -fill none -padx 10 \
        -side left 
    pack $base.options2.exact \
        -in $base.options2 -anchor center -expand 0 -fill none -padx 15 \
        -side left 
    pack $base.options2.regexp \
        -in $base.options2 -anchor center -expand 0 -fill none -padx 15 \
        -side left 

    pack $base.wintgt \
        -in $base -anchor center -expand 0 -fill x -side top 
    pack $base.wintgt.label \
        -in $base.wintgt -anchor center -expand 0 -fill none \
        -side right  -padx 10 
    pack $base.wintgt.label1 \
        -in $base.wintgt -anchor center -expand 0 -fill none \
        -side right  -padx 10

	wm geometry $base ""
    wm resizable $base 1 0
}






proc vTclWindow.syn_errors {base} {
    if {$base == ""} {
        set base .syn_errors
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 521x247+261+300
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Syntax Errors"
    frame $base.headers \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.headers.lnum \
        -borderwidth 1 -relief raised -text Line# 
    label $base.headers.desc \
        -borderwidth 1 -relief raised -text Description 
    frame $base.errlist \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.errlist.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand [list $base.errlist.xscrollbar set] \
        -yscrollcommand [list $base.errlist.yscrollbar set] 
    scrollbar $base.errlist.xscrollbar \
        -borderwidth 1 -command [list $base.errlist.listbox xview] -orient horiz \
        -width 10 
    scrollbar $base.errlist.yscrollbar \
        -borderwidth 1 -command [list $base.errlist.listbox yview] -orient vert \
        -width 10 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.headers \
        -in $base -anchor center -expand 0 -fill x -side top 
    pack $base.headers.lnum \
        -in $base.headers -anchor center -expand 0 -fill none -padx 5 \
        -side left 
    pack $base.headers.desc \
        -in $base.headers -anchor center -expand 0 -fill none -side top 
    pack $base.errlist \
        -in $base -anchor center -expand 1 -fill both -side top 
    grid columnconf $base.errlist 0 -weight 1
    grid rowconf $base.errlist 0 -weight 1
    grid $base.errlist.listbox \
        -in $base.errlist -column 0 -row 0 -columnspan 1 -rowspan 1 \
        -sticky nesw 
    grid $base.errlist.xscrollbar \
        -in $base.errlist -column 0 -row 1 -columnspan 1 -rowspan 1 \
        -sticky ew 
    grid $base.errlist.yscrollbar \
        -in $base.errlist -column 1 -row 0 -columnspan 1 -rowspan 1 \
        -sticky ns 
}

