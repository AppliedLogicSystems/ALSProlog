##=================================================================================
#|				alsdev_main.tcl
#|		Copyright (c) 1997-98 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk main window for the alsdev environment
##=================================================================================

#puts [info tclversion]
#puts $tk_version

proc vTclWindow.topals {args} {
	global array proenv
	global mod

    set base .topals
    if {[winfo exists .topals]} {
    	show_window .topals ; return
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
   	wm overrideredirect .topals 0
    wm resizable .topals 1 1
    wm deiconify .topals
    wm title .topals "ALS Prolog Environment"

    wm protocol $base WM_DELETE_WINDOW {exit_prolog}
		##------------------
		## Main menubar:
		##------------------
	
    menu .topals.mmenb -relief sunken -tearoff 0

	add_default_menus .topals.mmenb
	add_file_menu .topals.mmenb listener .topals
	add_edit_menu .topals.mmenb listener .topals
	add_prolog_menu .topals.mmenb listener .topals
	add_tools_menu .topals.mmenb listener .topals
	menu .topals.mmenb.windows -tearoff 0 -title Windows
	.topals.mmenb add cascade -label "Windows" -menu .topals.mmenb.windows -underline 0
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

    if {[tk windowingsystem] == "aqua"} {
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

	# accelerators
#	bind_accelerators .topals $mod listener
#	post_open_document Environment .topals 

}

proc vTclWindow.dyn_flags {base} {
	global array proenv

    set base .dyn_flags
    if {[winfo exists $base]} {
        show_window $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    # Withdraw the window so that it doesn't appear during prolog heartbeat
    wm withdraw $base
    wm focusmodel $base passive
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm resizable $base 0 0
    wm overrideredirect $base 0
    wm title $base "Changable Prolog Flags"
	wm protocol .dyn_flags WM_DELETE_WINDOW {remove_me {Dynamic Flags} .dyn_flags}

	frame $base.buttons \
		-borderwidth 1 -relief raised 
    button $base.buttons.dismiss \
		-padx 11 -text Dismiss \
        -command {wm withdraw .dyn_flags}
    button $base.buttons.save \
		-padx 11 -text {Save as Defaults} \
        -command {prolog call alsdev save_prolog_flags ; wm withdraw .dyn_flags}

    ###################
    # SETTING GEOMETRY
    ###################

    pack $base.buttons \
        -anchor center -expand 0 -fill x -side bottom 
    pack $base.buttons.dismiss \
        -anchor center -expand 0 -fill none -padx 11 -side left 
    pack $base.buttons.save \
        -anchor center -expand 0 -fill none -padx 11 -side right 

	prolog call builtins changable_flags_info -var InfoList
# Find in alsdev.tcl:
	foreach info $InfoList {
		create_dyn_flag_entry $info
	}
	
	# Make window visible
	wm deiconify $base
}

proc change_prolog_flags {} {
	global array proenv

	prolog call builtins changable_flags_info -var InfoList
	foreach info $InfoList {
		set FlagName [lindex $info 0]
		if {[lindex $info 2] != $proenv($FlagName)} then {
			prolog call builtins set_prolog_flag \
				-atom $FlagName -atom $proenv($FlagName)
		}
	}
}

proc vTclWindow.about {base} {
    if {$base == ""} {
        set base .about
    }
    if {[winfo exists $base]} {
        show_window $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 171x215
    center $base
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "About ALS Prolog"
	wm protocol .about WM_DELETE_WINDOW {remove_me About .about}

    label $base.alsdev \
        -font {helvetica 14 {bold italic}} -text {ALS Prolog} 
    frame $base.f1 \
        -borderwidth 1 -height 3 -relief sunken -width 30 
    label $base.alspro \
        -text {ALS Prolog} 
    label $base.dev \
        -text {Development Environment} 
    frame $base.f2 \
        -borderwidth 1 -height 3 -relief sunken -width 30 
    label $base.created \
        -font {helvetica 9 {}} -text {Copyright (c) 1998-99} 
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
}

proc vTclWindow.break_choices {base} {
    if {$base == ""} {
        set base .break_choices
    }
    if {[winfo exists $base]} {
        show_window $base ; return
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
        set base .static_flags
    }
    if {[winfo exists $base]} {
        show_window $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base
    wm withdraw $base

    wm title $base "Static Prolog Flags"
	wm protocol .static_flags WM_DELETE_WINDOW {remove_me {Static Flags} .static_flags}

	prolog call builtins static_flags_info -var InfoList
	foreach info $InfoList {
		create_static_flag_entry $info
	}
	
	update idletask
	wm deiconify $base
}

proc create_static_flag_entry { info } {
	global array proenv

	set flgg [lindex $info 0]
	label .static_flags.$flgg -borderwidth 0 -relief flat -anchor w \
		-text [format "%s  =  %s" $flgg [lindex $info 1]]
	pack .static_flags.$flgg -anchor w -expand 0 -fill x -side top 
}


##=================================================================================
# Project Document fields
# proenv($base,dirty) - true iff document window is dirty.
##=================================================================================
proc init_prj_spec \
	{base TextSlots ListOfFilesSlots ListSlots SlotNames FileTypes DfltDirs AddlTextSlots AddlTextSlotsValues LibFiles} {
	global array proenv

set HHB [list $LibFiles]

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm maxsize $base \
		[expr [winfo screenwidth .] - 80] \
		[expr [winfo screenheight .] - 80]
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 0
    wm deiconify $base
    wm title $base "Project Specification"
    wm protocol $base WM_DELETE_WINDOW "prj_close $base"

    frame $base.title \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.title.label \
        -anchor w -text {Project Title:} 
    entry $base.title.entry \
        -cursor {} -highlightthickness -1 

    frame $base.project_file \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.project_file.label \
        -anchor w -text {Project File Name:} 
    entry $base.project_file.entry \
        -cursor {} -highlightthickness 0 

    set proenv(ppj_pathtype) relative

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
		[list add_search_dirs $base.search_dirs.listbox $proenv(ppj_pathtype) $base] \
		[list del_search_dirs $base.search_dirs.listbox $base] \
		[list move_selection_up $base.search_dirs.listbox $base] \
		[list move_selection_down $base.search_dirs.listbox $base]

    ###################
    # Lists of Files 
    ###################
	foreach FS $ListOfFilesSlots {
		set FTs	[find_pair_valueX $FS $FileTypes]
		set FN	[find_pair_value $FS $SlotNames] 
		set XFN ""
		set DfltD [find_pair_value $FS $DfltDirs]
		append XFN $FN " (" $FTs "):"
		create_lofs_toggle $base $FS {Prolog Files:} $FTs \
			[list add_to_files_list $FS $base.$FS.listbox $FTs $FN $DfltD $base] \
			[list add_to_files_list_mult $FS $base.$FS.listbox $FTs $FN $DfltD  $base] \
			[list del_from_files_list $base.$FS.listbox $base] \
			[list move_selection_up $base.$FS.listbox $base] \
			[list move_selection_down $base.$FS.listbox $base]
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
        -command "save_project $base" -padx 11 -pady 4 -text Save -state disabled
    button $base.buttons.addl \
        -command "addl_project_info $base {$TextSlots} {$SlotNames} {$AddlTextSlots} {$AddlTextSlotsValues } [list $LibFiles]" \
	-padx 11 -pady 4 -text Addl 
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
        -anchor center -expand 0 -fill none -padx 2 -side left 
    pack $base.buttons.addl \
        -anchor center -expand 0 -fill none -padx 35 -side left 
    pack $base.buttons.load \
        -anchor center -expand 0 -fill none -padx 2 -side right 

    focus $base.project_file.entry

    bind $base <Key> "prj_dirty_key $base %K"

	# Init document field
    set proenv($base,dirty) false
    set proenv($base.addlprj,dirty) false

}

proc prj_dirty_key {w k} {
    global array proenv
	if {$k != "Home" && $k != "End" && $k != "Prior" && $k != "Next"
		&& $k != "Left" && $k != "Right" && $k != "Up" && $k != "Down"
		&& $k != "Control_L" && $k != "Control_R"
		&& $k != "Shift_L" && $k != "Shift_R"
		&& $k != "Alt_L" && $k != "Alt_R"
		&& $k != "Meta_L" && $k != "Meta_R"
		&& $k != "Caps_Lock" && $k != "Num_Lock" && $k != "Help"
		} then {
		set proenv($w,dirty) true
    		$w.buttons.save configure -state active
	}
}
proc addl_prj_dirty_key {w k pw} {
    global array proenv
	if {$k != "Home" && $k != "End" && $k != "Prior" && $k != "Next"
		&& $k != "Left" && $k != "Right" && $k != "Up" && $k != "Down"
		&& $k != "Control_L" && $k != "Control_R"
		&& $k != "Shift_L" && $k != "Shift_R"
		&& $k != "Alt_L" && $k != "Alt_R"
		&& $k != "Meta_L" && $k != "Meta_R"
		&& $k != "Caps_Lock" && $k != "Num_Lock" && $k != "Help"
		} then {
		set proenv($w,dirty) true
    		$pw.buttons.save configure -state active
	}
}

# checks whether the project (including addl info) is dirty:
proc prj_perf_isdirtycheck {w} {
    global array proenv

    set addl_project_info_win $w.addlprj

    set addl_isdirtycheck false
    if {[winfo exists $addl_project_info_win] } {
        if {$proenv($addl_project_info_win,dirty)} then {
   	    set addl_isdirtycheck true
	}
    }

# NOTE: isdirtycheck = true iff the project is dirty
# So: isdirtycheck false iff the project is clean
# Ie, project is clean, or project is dirty, but user says to close anyway:

	set isdirtycheck false
        if {$proenv($w,dirty) || $addl_isdirtycheck} then {
    		set isdirtycheck true
	}
    return $isdirtycheck
}

proc prj_close {w} {
    	global array proenv

    	set isdirtycheck [prj_perf_isdirtycheck $w]

	set saveprj [prj_save_check $w $isdirtycheck]

        if {$saveprj == 2} then {
            save_project $w
	    send_prolog als_ide_mgr shutdown_project
	} elseif {$saveprj == 0} then {
	    send_prolog als_ide_mgr shutdown_project
	}

}

#-----------------------------------------------
#  checks project dirty state, and offers user
#  opportunity to save it if project is dirty:
#  Returns 1 if it is ok to close the project:
#    1 if project is clean
#    1 if:
#       -- proj was dirty, user said to save it,
#          and save was successful
#    1 if:
#       -- proj was dirty and 
#          user said not to save it,
#    0 if user hit Cancel on saving prompt
#    0 if:
#       -- proj was dirty, user said to save it,
#          but save was not successful
#-----------------------------------------------
proc prj_save_check {w isdirtycheck} {
        global array proenv

        if {$isdirtycheck} then {
                raise $w
                set title [$w.title.entry get]
                set answer [tk_dialog .document_save_dialog "" \
                        "Save changes to the project \"$title\" before closing?" \
                        {} \
                        2 "Don't Save" "Cancel" "Save"]
		set result $answer
        } else {
                set result 0
        }
        return $result
}
#-----------------------------------------------
#  checks for dirty state of cref panel
#  see above in prj_save_check comment for details
#-----------------------------------------------

proc cref_save_check {w isdirtycheck} {
        global array proenv

        if {$isdirtycheck} then {
                raise $w
#                set title [$w.title.entry get]
                set answer [tk_dialog .document_save_dialog "" \
                        "Save changes to the Cref Panel before closing?" \
                        {} \
                        2 "Don't Save" "Cancel" "Save"]
		set result $answer
        } else {
                set result 0
        }
        return $result
}

proc find_pair_value { Tag PairList } {
	set Value ""
	foreach Entry $PairList {
		if { [lindex $Entry 0] == $Tag } then {
			return [lindex $Entry 1]
		} 
	}
}
proc find_pair_valueX { Tag PairList } {
	set Value ""
	foreach Entry $PairList {
		if { [lindex $Entry 0] == $Tag } then {
			return $Entry
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
	bind $GuiPath.$Slot.listbox <Double-Button-1> \
		[list prj_slot_focus $Slot $GuiPath.$Slot.listbox $PrjMgrHandle]
}

proc create_ls_toggle { Win Which Title Add Del Up Down} {
	global array proenv

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
	global array proenv

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
        -command "add_search_dirs $Win.$Which.listbox \"\$proenv(ppj_pathtype)\" $Win" \
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
	global array proenv 

	if {$proenv($Which) == "closed"} then {
#$Win.ctl_$Which.entry configure -state normal
    	$Win.ctl_$Which.open_btn configure -image open_ptr
		set proenv($Which) open
    	pack $Win.$Which  \
			-after $Win.ctl_$Which \
        	-anchor center -expand 0 -fill x -side top 
		set XX [winfo x $Win]
		set YY [winfo y $Win]
		set Ht [expr [winfo screenheight .] - $YY - 80] 
		wm geometry $Win ""
		update
		set RHt [winfo reqheight $Win]
		if { $RHt < $Ht } then { set Ht $RHt }
		set Wd [winfo width $Win]
		append GG $Wd x $Ht + $XX + $YY
		wm geometry $Win $GG
	} else {
#$Win.ctl_$Which.entry configure -state disabled
    	$Win.ctl_$Which.open_btn configure -image closed_ptr
		set proenv($Which) closed
    	pack forget $Win.$Which 
		wm geometry $Win ""
	}
}

######## For Load Project:
#clofs_t:  Win=.project_1483293097_0 Which=prolog_files Title=Prolog Files:
#clofs_t2: FileTypes=
#clofs_t3: Add=add_to_files_list prolog_files .project_1483293097_0.prolog_files.listbox {} {Prolog Files:} {} .project_1483293097_0
#clofs_t4: AddMult=add_to_files_list_mult prolog_files .project_1483293097_0.prolog_files.listbox {} {Prolog Files:} {} .project_1483293097_0
#clofs_t5: Del=del_from_files_list .project_1483293097_0.prolog_files.listbox .project_1483293097_0
#clofs_t6: Up=move_selection_up .project_1483293097_0.prolog_files.listbox .project_1483293097_0
#clofs_t7: Down=move_selection_down .project_1483293097_0.prolog_files.listbox .project_1483293097_0
#

proc create_lofs_toggle { Win Which Title FileTypes Add AddMult Del Up Down} {
	global array proenv

#puts "clofs_t:  Win=$Win Which=$Which Title=$Title"
#puts "clofs_t2: FileTypes=$FileTypes"
#puts "clofs_t3: Add=$Add"
#puts "clofs_t4: AddMult=$AddMult"
#puts "clofs_t5: Del=$Del"
#puts "clofs_t6: Up=$Up"
#puts "clofs_t7: Down=$Down"

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
	
    entry $Win.ctl_$Which.entry -state disabled
	bind $Win.ctl_$Which.entry <Return> "add_file_entry_to_list $Win.ctl_$Which.entry $Win.$Which.listbox"

    frame $Win.$Which \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $Win.$Which.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand "$Win.$Which.02 set" \
        -yscrollcommand "$Win.$Which.03 set" \
		-height 0
    scrollbar $Win.$Which.02 \
        -borderwidth 1 -command "$Win.$Which.listbox xview" \
        -orient horiz 
    scrollbar $Win.$Which.03 \
        -borderwidth 1 -command "$Win.$Which.listbox yview" \
        -orient vert 
    frame $Win.$Which.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 

    button $Win.$Which.buttons.add \
        -command $Add -padx 11 -pady 1 -text {Add} 
    button $Win.$Which.buttons.del \
        -command $Del -padx 11 -pady 1 -text {Delete} 

    button $Win.$Which.buttons.up \
        -command $Up -padx 11 -pady 1 -text {} -image up_arrow_gif \
		-borderwidth 0
    button $Win.$Which.buttons.down \
        -command $Down -padx 11 -pady 1 -text {} -image down_arrow_gif \
		-borderwidth 0

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
    pack $Win.ctl_$Which.entry \
        -anchor w -expand 1 -fill x -side left 

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

    pack $Win.$Which.buttons.down \
        -anchor center -expand 0 -fill none -padx 2 -side right 
    pack $Win.$Which.buttons.up \
        -anchor center -expand 0 -fill none -padx 2 -side right 

    pack $Win.$Which.buttons.del \
        -anchor center -expand 0 -fill none -padx 10 -side right 

	pack forget $Win.$Which
}

proc rd_prj_spec {base TextSlots ListOfFilesSlots ListSlots AddlTextSlots} {
	set Result ""

	set TxtSs ""
    	lappend TxtSs [list title [$base.title.entry get] ]
    	lappend TxtSs [list project_file [$base.project_file.entry get] ]

	set LstSs ""
    	lappend LstSs [list search_dirs [$base.search_dirs.listbox get 0 end] ]

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

    	if {[winfo exists $base.addlprj]} {
		set AddlInfo [rd_prj_spec_addl $base.addlprj $AddlTextSlots]
		lappend Result  $AddlInfo
    	}

	return $Result
}

proc rd_prj_spec_addl {base AddlTextSlots} {
	set AddlTxtSsVs ""
	foreach TS $AddlTextSlots {
    		set sss [$base.$TS get]
		set bb [list $TS $sss]
    		lappend AddlTxtSsVs [list $TS [$base.$TS get]]
	}
	set LibFiles [list "library_files" [$base.cpd17.01 get 0 end]]
    	lappend AddlTxtSsVs $LibFiles
	return $AddlTxtSsVs
}

proc addl_project_info { parent_base TextSlots SlotNames AddlTextSlots AddlTextSlotsValues LibFiles} {
	set proj_title [$parent_base.title.entry get]  
	if {$proj_title == ""} then {
		tk_messageBox -message "Missing project title!" -icon error \
			-title "Missing info" -type ok
		return
	}
	addl_project_info_win $parent_base.addlprj $proj_title $parent_base $TextSlots $SlotNames $AddlTextSlots $AddlTextSlotsValues $LibFiles
}

proc addl_project_info_win {base proj_title parent_base TextSlots SlotNames AddlTextSlots AddlTextSlotsValues LibFiles} {
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 326x413+320+148
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Additional Project Information"
#    wm protocol $base WM_DELETE_WINDOW "wm iconify $base"
    wm protocol $base WM_DELETE_WINDOW "addl_project_info_close $base"

    label $base.title_label \
        -borderwidth 1 -text {Project Title:} 
    label $base.title_value -text $proj_title -relief sunken

    frame $base.cref_btns \
        -borderwidth 2 -height 75 -relief groove -width 125 
    button $base.cref_btns.add \
        -command "run_cref_on_prj $base" -padx 11 -pady 4 -text {Run CREF on project} 

    label $base.lab_library \
        -borderwidth 1 -text {Library Files:} 
    frame $base.cpd17 \
        -borderwidth 1 -height 8 -relief raised -width 30 
    listbox $base.cpd17.01 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-140-*-*-*-*-*-* -height 6 \
        -xscrollcommand "$base.cpd17.02 set" \
        -yscrollcommand "$base.cpd17.03 set" 
    scrollbar $base.cpd17.02 \
        -borderwidth 1 -command "$base.cpd17.01 xview" -orient horiz \
        -width 10 
    scrollbar $base.cpd17.03 \
        -borderwidth 1 -command "$base.cpd17.01 yview" -orient vert \
        -width 10 
    frame $base.lib_btns \
        -borderwidth 2 -height 75 -relief groove -width 125 
    button $base.lib_btns.add \
        -command "add_lib_file" -padx 11 -pady 4 -text {Add Lib File} 
    button $base.lib_btns.del \
        -command "delete_lib_file $base.cpd17.01 $base $parent_base" -padx 11 -pady 4 -text {Delete Lib File} 
    label $base.production_goal_label \
        -borderwidth 1 -text {} 
    entry $base.production_goal
    label $base.debug_goal_label \
        -borderwidth 1 -text {Debug Goal:} 
    entry $base.debug_goal
    label $base.executable_name_label \
        -borderwidth 1 -text {Executable File Name:} 
    entry $base.executable_name
    label $base.stub_name_label \
        -borderwidth 1 -text {Stub File Name:} 
    entry $base.stub_name
    label $base.distdir_name_label \
        -borderwidth 1 -text {Distribution Dir Name:} 
    entry $base.distdir_name
    frame $base.dist_btns \
        -borderwidth 2 -height 75 -relief groove -width 125 
    button $base.dist_btns.mkexec \
        -command make_executable -padx 11 -pady 4 -text {Make Executable} 
    button $base.dist_btns.mkdist \
        -command make_dist -padx 11 -pady 4 -text {Make Distribution} 

    set LLL "_label"
    foreach TxtSl $AddlTextSlots {
		set xLab $TxtSl$LLL
		set xSN [find_pair_value $TxtSl $SlotNames]
		set xVV [find_pair_value $TxtSl $AddlTextSlotsValues]
    		$base.$xLab configure -text $xSN
		$base.$TxtSl insert 0 $xVV

}
    set Listbox $base.cpd17.01
    foreach Ff $LibFiles {
	$Listbox insert end $Ff
    }
	
    ###################
    # SETTING GEOMETRY
    ###################
    grid columnconf $base 1 -weight 1
    grid rowconf $base 4 -weight 1
    grid $base.title_label \
        -in $base -column 0 -row 0 -columnspan 1 -rowspan 1 -padx 5 -sticky e 
    grid $base.title_value \
        -in $base -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ew 

    grid $base.cref_btns \
        -in $base -column 0 -row 1 -columnspan 2 -rowspan 1 -sticky ew 
    grid $base.cref_btns.add \
        -in $base.cref_btns -column 0 -row 0 -columnspan 1 -rowspan 1 \
        -padx 10 -sticky w 

    grid $base.lab_library \
        -in $base -column 0 -row 2 -columnspan 1 -rowspan 1 -padx 3 \
        -sticky w 

    grid $base.cpd17 \
        -in $base -column 0 -row 3 -columnspan 2 -rowspan 1 -sticky nesw 
    grid columnconf $base.cpd17 0 -weight 1
    grid rowconf $base.cpd17 0 -weight 1
    grid $base.cpd17.01 \
        -in $base.cpd17 -column 0 -row 0 -columnspan 1 -rowspan 1 \
        -sticky nesw 
    grid $base.cpd17.02 \
        -in $base.cpd17 -column 0 -row 1 -columnspan 1 -rowspan 1 \
        -sticky ew 
    grid $base.cpd17.03 \
        -in $base.cpd17 -column 1 -row 0 -columnspan 1 -rowspan 1 \
        -sticky ns 

    grid $base.lib_btns \
        -in $base -column 0 -row 6 -columnspan 2 -rowspan 1 -sticky ew 
    grid $base.lib_btns.add \
        -in $base.lib_btns -column 0 -row 0 -columnspan 1 -rowspan 1 \
        -padx 10 -sticky w 
    grid $base.lib_btns.del \
        -in $base.lib_btns -column 1 -row 0 -columnspan 1 -rowspan 1 \
        -padx 10 -pady 2 -sticky e 


    grid $base.production_goal_label \
        -in $base -column 0 -row 7 -columnspan 1 -rowspan 1 -padx 5 \
        -sticky e 
    grid $base.production_goal \
        -in $base -column 1 -row 7 -columnspan 1 -rowspan 1 -sticky ew 

    grid $base.debug_goal_label \
        -in $base -column 0 -row 8 -columnspan 1 -rowspan 1 -padx 5 \
        -sticky e 
    grid $base.debug_goal \
        -in $base -column 1 -row 8 -columnspan 1 -rowspan 1 -sticky ew 

    grid $base.executable_name_label \
        -in $base -column 0 -row 9 -columnspan 1 -rowspan 1 -padx 5 \
        -sticky e 
    grid $base.executable_name \
        -in $base -column 1 -row 9 -columnspan 1 -rowspan 1 -sticky ew 


    grid $base.stub_name_label \
        -in $base -column 0 -row 10 -columnspan 1 -rowspan 1 -padx 5 \
        -sticky e 
    grid $base.stub_name \
        -in $base -column 1 -row 10 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.distdir_name_label \
        -in $base -column 0 -row 11 -columnspan 1 -rowspan 1 -padx 5 \
        -sticky e 
    grid $base.distdir_name \
        -in $base -column 1 -row 11 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.dist_btns \
        -in $base -column 0 -row 12 -columnspan 2 -rowspan 1 -sticky ew 
    grid $base.dist_btns.mkexec \
        -in $base.dist_btns -column 0 -row 0 -columnspan 1 -rowspan 1 \
        -padx 10 -sticky w 
    grid $base.dist_btns.mkdist \
        -in $base.dist_btns -column 1 -row 0 -columnspan 1 -rowspan 1 \
        -padx 10 -pady 2 -sticky e 

    bind $base <Key> "addl_prj_dirty_key $base %K $parent_base"

	# Init document field
    set proenv($base,dirty) false

}

proc addl_project_info_close {w} {
	global array proenv
puts "addl_project_info_close: NEED TO ENSURE DATA CHANGES are captured into the project"


	 wm withdraw $w
}

proc vTclWindow.ide_settings {base} {
	global array proenv

    if {$base == ""} {
        set base .ide_settings
    }
    if {[winfo exists $base]} {
        show_window $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base
    #wm focusmodel $base passive
    #wm geometry $base 374x208+269+255
    #wm maxsize $base 1137 870
    #wm minsize $base 275 1
    #wm overrideredirect $base 0
    wm resizable $base 1 0
    wm title $base "ALS IDE Settings"
    wm protocol $base WM_DELETE_WINDOW {remove_me {IDE Settings} .ide_settings}

    frame $base.heartbeat \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.heartbeat.label \
        -borderwidth 1 -text {Heartbeat: } 
    entry $base.heartbeat.entry -width 5 -textvariable proenv(heartbeat)
    label $base.heartbeat.sec \
        -borderwidth 1 -text {sec.   0.0} 
    label $base.heartbeat.hl \
        -borderwidth 1 -text 5.0 
    scale $base.heartbeat.scale \
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
        -borderwidth 1 -relief flat -text 500 
    label $base.printdepth.label \
        -borderwidth 1 -relief flat -text {Print depth: } 
    entry $base.printdepth.entry -width 5 -textvariable proenv(main_printdepth) 
    label $base.printdepth.zero \
        -borderwidth 1 -relief flat -text {       0} 
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
    pack $base.heartbeat.scale \
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
	.ide_settings.heartbeat.scale set $Value 
}

proc set_heartbeat { Value } {
	.ide_settings.heartbeat.entry delete 0 end
	.ide_settings.heartbeat.entry insert end $Value
	.ide_settings.heartbeat.scale set $Value 
}

proc change_ide_depth_type { } {
	global array proenv
	prolog call builtins change_ide_depth_type -atom $proenv(main_depth_type)
}

	#############################################
	######      FIND AND REPLACE
	#############################################

set proenv(searchdirect) forward
set proenv(searchnature) exact

proc vTclWindow.find_repl {base} {
	global array proenv

    if {$base == ""} {
        set base .find_repl
    }
    if {[winfo exists $base]} {
        show_window $base; return
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
    wm protocol $base WM_DELETE_WINDOW {remove_me Find .find_repl}

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
	bind $base.search.entry <Return> {edit_find_next}
    frame $base.replace \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.replace.label \
        -borderwidth 1 -relief flat -text {Replace by:} 
    entry $base.replace.entry 
	bind $base.replace.entry <Return> {edit_find_next}
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

	#wm geometry $base ""
    wm resizable $base 1 0
}






proc vTclWindow.syn_errors {base} {
    if {$base == ""} {
        set base .syn_errors
    }
    if {[winfo exists $base]} {
        show_window $base; return
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

###################################################################
## CREF

##=================================================================================
# Project Document fields
# proenv($base,dirty) - true iff document window is dirty.
##=================================================================================
proc cref_panel \
	{base} {
	global array proenv

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm maxsize $base \
		[expr [winfo screenwidth .] - 80] \
		[expr [winfo screenheight .] - 80]
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 0
    wm deiconify $base
    wm title $base "CREF"
    wm protocol $base WM_DELETE_WINDOW "cref_close $base"

    frame $base.suite_file \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.suite_file.label \
        -anchor w -text {Suite Spec (*.crf):} 
    entry $base.suite_file.entry \
        -cursor {} -highlightthickness -1 -vcmd "crefSteFileEntryCmd $base" -validate focusout

    frame $base.suite_dir \
        -borderwidth 1 -height 30 -relief raised -width 30 
    button $base.suite_dir.folder_choose \
        -command "folder_choose_dir $base suite_dir" -image openfolder -padx 11 -pady 4 
    label $base.suite_dir.label \
        -anchor w -text {Spec Dir:} 
    entry $base.suite_dir.entry \
        -cursor {} -highlightthickness -1

    frame $base.title \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.title.label \
        -anchor w -text {Suite Title:} 
    entry $base.title.entry \
        -cursor {} -highlightthickness 0 

    frame $base.src_dir \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.src_dir.label \
        -anchor w -text {Source Dir:} 
    button $base.src_dir.folder_choose \
        -command "folder_choose_dir $base src_dir" -image openfolder -padx 11 -pady 4 
    entry $base.src_dir.entry \
        -cursor {} -highlightthickness -1 

    frame $base.target \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.target.label \
        -anchor w -text {Targets:} 
    entry $base.target.entry \
        -cursor {} -highlightthickness -1 -vcmd "crefTgtEntryCmd $base" -validate focusout

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.suite_file \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.suite_file.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.suite_file.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    pack $base.suite_dir \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.suite_dir.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.suite_dir.folder_choose \
        -anchor w -expand 0 -fill none -side left 
    pack $base.suite_dir.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    pack $base.title \
        -anchor center -expand 0 -fill x -pady 2 -side top 
    pack $base.title.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.title.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    pack $base.src_dir \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.src_dir.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.src_dir.folder_choose \
        -anchor w -expand 0 -fill none -side left 
    pack $base.src_dir.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    pack $base.target \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.target.label \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.target.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    ###################
    # Lists of Files 
    ###################
	create_lofs_toggle $base "prolog_files" {Prolog Files:} "" \
		[list add_to_files_list prolog_files $base.prolog_files.listbox {} {Prolog Files:} {} $base] \
		[list add_to_files_list_mult prolog_files $base.prolog_files.listbox {} {Prolog Files:} {}  $base] \
		[list del_from_files_list $base.prolog_files.listbox $base] \
		[list move_selection_up $base.prolog_files.listbox $base] \
		[list move_selection_down $base.prolog_files.listbox $base]


    ###################
    # Lists of Items
    ###################
#	foreach LS $ListSlots {
#		create_ls_toggle $base $LS [find_pair_value $LS $SlotNames] \
#		[list add_to_list_$LS $base.$LS.listbox] \
#		[list del_from_list_$LS $base.$LS.listbox] \
#		[list move_selection_up $base.$LS.listbox] \
#		[list move_selection_down $base.$LS.listbox]
#	}
    ###################
    # CREATING WIDGETS
    ###################
   frame $base.sep_buttons \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $base.buttons \
        -borderwidth 1 -relief sunken 
    button $base.buttons.save \
        -command "save_cref_suite $base" -padx 11 -pady 4 -text Save -state disabled
    button $base.buttons.close \
        -command "cref_close $base" \
	-padx 11 -pady 4 -text Close 
    button $base.buttons.run_cref \
        -command "run_cref_on_suite $base" -padx 11 -pady 4 -text {Run Cref}
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.sep_buttons \
        -anchor center -expand 0 -fill x -side top 
    pack $base.buttons \
        -anchor center -expand 0 -fill x -side top 
    pack $base.buttons.save \
        -anchor center -expand 0 -fill none -padx 2 -side left 
    pack $base.buttons.close \
        -anchor center -expand 0 -fill none -padx 35 -side left 
    pack $base.buttons.run_cref \
        -anchor center -expand 0 -fill none -padx 2 -side right 

    ##########################
    # CREATING REPORTS BUTTONS
    ##########################
   frame $base.sep_reports \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $base.report_buttons \
        -borderwidth 1 -relief sunken 
    button $base.report_buttons.html \
        -command html_report -padx 11 -pady 4 -text {HTML Report} 
    button $base.report_buttons.xrf \
        -command xrf_report -padx 11 -pady 4 -text {XRF Report} 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.sep_reports \
        -anchor center -expand 0 -fill x -side top 
    pack $base.report_buttons \
        -anchor center -expand 0 -fill x -side top 
    pack $base.report_buttons.html \
        -anchor center -expand 0 -fill none -padx 2 -side left 
    pack $base.report_buttons.xrf \
        -anchor center -expand 0 -fill none -padx 2 -side right 

    focus $base.suite_file.entry

    bind $base <Key> "prj_dirty_key $base %K"

	# Init document field
    set proenv($base,dirty) false
    set proenv($base.addlprj,dirty) false
    send_prolog als_ide_mgr exist_reports
}

proc rd_cref_panel {base} {
	set Result ""
    lappend Result [list title [$base.title.entry get] ]
    lappend Result [list suite_file [$base.suite_file.entry get] ]
    lappend Result [list suite_dir [$base.suite_dir.entry get] ]

    lappend Result [list list_of_files [$base.prolog_files.listbox get 0 end] ]

    lappend Result [list src_dir [$base.src_dir.entry get] ]
    lappend Result [list target [$base.target.entry get] ]

    return $Result
}

proc folder_choose_dir {base Which} {
    	global array proenv
	set CWD [pwd]
		# cf: https://www.tcl.tk/man/tcl8.3/TkCmd/chooseDirectory.htm
	set NewDir [tk_chooseDirectory -initialdir $CWD]
	cd $CWD
	if {($NewDir == "") || ($CWD == $NewDir)} then {
		return
	}
	$base.$Which.entry delete 0 end
	$base.$Which.entry insert end $NewDir
	set proenv($base,dirty) true
    	$base.buttons.save configure -state active
}

proc disable_cref_btns {} {
    global elipsis
    .topals.mmenb.tools entryconfigure "Open Cref Suite$elipsis" -state disabled
    .topals.mmenb.tools entryconfigure "New Cref Suite" -state disabled
}
proc enable_cref_btns {} {
    global elipsis
    .topals.mmenb.tools entryconfigure "Open Cref Suite$elipsis" -state active
    .topals.mmenb.tools entryconfigure "New Cref Suite" -state active
}

proc crefSteFileEntryCmd {base} {
    set curSuiteFile [$base.suite_file.entry get]
    set sfx [file extension $curSuiteFile]

    if {! [string equal $sfx ".crf"]} then {
	if {! [string equal $curSuiteFile ""]} {
		$base.suite_file.entry insert end ".crf"
	}
    } 
    set specDir [file dirname $curSuiteFile]
    if {[string equal $specDir ""]} {
	$base.suite_dir.entry insert end $specDir
    }
    set suiteFileTail [file tail [$base.suite_file.entry get]]
    set suiteFileTailSplit [split $suiteFileTail .]
    set sFTSLen [llength $suiteFileTailSplit]

    if {[expr $sFTSLen > 1]} {
	set baseFile [lindex $suiteFileTailSplit 0]
    } else {
	set baseFile $suiteFileTail
    }
    if {[expr [expr [llength [$base.title.entry get]]] == 0]} {
        $base.title.entry insert end  $baseFile
    }
    if {[expr [expr [llength [$base.target.entry get]]] == 0]} {
	$base.target.entry insert end  $baseFile.\[xrf,html\]
    }
    
    return true;
}

proc crefTgtEntryCmd {base} {
#puts "crefTgtEntryCmd=$base"
#puts "suite_file: |[$base.suite_file.entry get]|"
return true;
}
