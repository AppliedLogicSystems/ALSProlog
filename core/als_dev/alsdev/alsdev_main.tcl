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




proc vTclWindow.ppj_spec {base} {

    if {$base == ""} {
        set base .ppj_spec
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 318x686+290+82
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Prolog Project Specification"
    frame $base.prj_title \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.prj_title.01 \
        -anchor w -text {Project Title:} 
    entry $base.prj_title.entry \
        -cursor {} -highlightthickness 0 
    frame $base.filename \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.filename.01 \
        -anchor w -text {Project File Name:} 
    entry $base.filename.entry \
        -cursor {} -highlightthickness 0 
    frame $base.startup \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.startup.01 \
        -anchor w -text {Startup Goal:} 
    entry $base.startup.entry \
        -cursor {} -highlightthickness 0 
    frame $base.sep_prolog_files \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $base.prolog_files_ctl \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.prolog_files_ctl.label \
        -text {Prolog (*.pro *.pl) Files:} 
	global prolog_files_list ; set prolog_files_list closed
    button $base.prolog_files_ctl.open_btn \
        -command toggle_prolog_files_list -image closed_ptr -padx 11 -pady 4 \
        -text button 
    frame $base.prolog_files \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.prolog_files.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand {.ppj_spec.prolog_files.02 set} \
        -yscrollcommand {.ppj_spec.prolog_files.03 set} \
		-height 0
    scrollbar $base.prolog_files.02 \
        -borderwidth 1 -command {.ppj_spec.prolog_files.listbox xview} -orient horiz \
        -width 10 
    scrollbar $base.prolog_files.03 \
        -borderwidth 1 -command {.ppj_spec.prolog_files.listbox yview} -orient vert \
        -width 10 
    frame $base.prolog_files.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.prolog_files.buttons.add_file \
        -command {add_new_proj_file pro} -padx 11 -pady 4 -text {Add} 
    button $base.prolog_files.buttons.add_mult_file \
        -command {add_mult_files} -padx 11 -pady 4 -text {Add Multiple} 
    button $base.prolog_files.buttons.del_files \
        -command {delete_proj_files} -padx 11 -pady 4 -text {Delete} 

    frame $base.sep_search_dirs \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 

    frame $base.search_dirs_ctl \
        -borderwidth 1 -height 30 -relief sunken -width 30 
	global search_dirs_list ; set search_dirs_list closed
    button $base.search_dirs_ctl.open_btn \
        -command toggle_search_dirs_list -image closed_ptr -padx 11 -pady 4 \
        -text button 
    label $base.search_dirs_ctl.label \
        -text {Search Directory Paths:} 
    frame $base.search_dirs \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.search_dirs.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand {.ppj_spec.search_dirs.02 set} \
        -yscrollcommand {.ppj_spec.search_dirs.03 set} \
		-height 0
    scrollbar $base.search_dirs.02 \
        -borderwidth 1 -command {.ppj_spec.search_dirs.listbox xview} \
        -orient horiz -width 10 
    scrollbar $base.search_dirs.03 \
        -borderwidth 1 -command {.ppj_spec.search_dirs.listbox yview} \
        -orient vert -width 10 
    frame $base.search_dirs.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.search_dirs.buttons.add_dir \
        -command add_directory -padx 11 -pady 4 -text {Add} 
    button $base.search_dirs.buttons.del_dir \
        -command delete_directory -padx 11 -pady 4 -text {Delete} 

    frame $base.sep_oop_files \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $base.oop_files_ctl \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.oop_files_ctl.label \
        -text {ObjectPro (*.oop) Files:} 
	global oop_files_list ; set oop_files_list closed
    button $base.oop_files_ctl.open_btn \
        -command toggle_oop_files_list -image closed_ptr -padx 11 -pady 4 \
        -text button 
    frame $base.oop_files \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.oop_files.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand {.ppj_spec.oop_files.02 set} \
        -yscrollcommand {.ppj_spec.oop_files.03 set} \
		-height 0
    scrollbar $base.oop_files.02 \
        -borderwidth 1 -command {.ppj_spec.oop_files.listbox xview} -orient horiz \
        -width 10 
    scrollbar $base.oop_files.03 \
        -borderwidth 1 -command {.ppj_spec.oop_files.listbox yview} -orient vert \
        -width 10 
    frame $base.oop_files.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.oop_files.buttons.add_file \
        -command {add_new_proj_file oop} -padx 11 -pady 4 -text {Add} 
    button $base.oop_files.buttons.del_files \
        -command {delete_proj_files oop} -padx 11 -pady 4 -text {Delete} 

    frame $base.sep_typ_files \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $base.typ_files_ctl \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.typ_files_ctl.label \
        -text {TypeDef (*.typ) Files:} 
	global typ_files_list ; set typ_files_list closed
    button $base.typ_files_ctl.open_btn \
        -command toggle_typ_files_list -image closed_ptr -padx 11 -pady 4 \
        -text button 
    frame $base.typ_files \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.typ_files.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand {.ppj_spec.typ_files.02 set} \
        -yscrollcommand {.ppj_spec.typ_files.03 set} \
		-height 0
    scrollbar $base.typ_files.02 \
        -borderwidth 1 -command {.ppj_spec.typ_files.listbox xview} -orient horiz \
        -width 10 
    scrollbar $base.typ_files.03 \
        -borderwidth 1 -command {.ppj_spec.typ_files.listbox yview} -orient vert \
        -width 10 
    frame $base.typ_files.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.typ_files.buttons.add_file \
        -command {add_new_proj_file typ} -padx 11 -pady 4 -text {Add} 
    button $base.typ_files.buttons.del_files \
        -command {delete_proj_files typ} -padx 11 -pady 4 -text {Delete} 

    frame $base.sep_lib_files \
        -background #000000 -borderwidth 1 -height 3 -relief sunken -width 30 
    frame $base.lib_files_ctl \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.lib_files_ctl.label \
        -text {Prolog Library Files:} 
	global lib_files_list ; set lib_files_list closed
    button $base.lib_files_ctl.open_btn \
        -command toggle_lib_files_list -image closed_ptr -padx 11 -pady 4 \
        -text button 
    frame $base.lib_files \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.lib_files.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand {.ppj_spec.lib_files.02 set} \
        -yscrollcommand {.ppj_spec.lib_files.03 set} \
		-height 0
    scrollbar $base.lib_files.02 \
        -borderwidth 1 -command {.ppj_spec.lib_files.listbox xview} -orient horiz \
        -width 10 
    scrollbar $base.lib_files.03 \
        -borderwidth 1 -command {.ppj_spec.lib_files.listbox yview} -orient vert \
        -width 10 
    frame $base.lib_files.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.lib_files.buttons.add_file \
        -command {add_new_proj_file lib} -padx 11 -pady 4 -text {Add} 
    button $base.lib_files.buttons.del_files \
        -command {delete_proj_files lib} -padx 11 -pady 4 -text {Delete} 

    frame $base.sep_bld_ctl \
        -background #000000 -borderwidth 1 -height 3 -relief sunken 
    frame $base.bld_ctl_ctl -borderwidth 1 -relief sunken 
    label $base.bld_ctl_ctl.label -text {Package Build:} 
	global bld_ctl_list ; set bld_ctl_list closed
    button $base.bld_ctl_ctl.open_btn \
        -command toggle_bld_ctl_list -image closed_ptr -padx 11 -pady 4 \
        -text button 
    frame $base.bld_ctl -borderwidth 1 -relief sunken 
    frame $base.bld_ctl.os_info -borderwidth 1 -relief sunken
    label $base.bld_ctl.os_info.l1 -text { OS:  }
    label $base.bld_ctl.os_info.os_label -text {   } \
		-borderwidth 1 -relief sunken
    label $base.bld_ctl.os_info.l2 -text { Minor OS:  }
    label $base.bld_ctl.os_info.minor_os_label \
		-text {   } -borderwidth 1 -relief sunken
    frame $base.bld_ctl.bld_cmd -borderwidth 1 -relief sunken
    label $base.bld_ctl.bld_cmd.l1 \
        -anchor w -text {Build Cmd:}
    entry $base.bld_ctl.bld_cmd.skel \
        -cursor {} -highlightthickness 0 
    frame $base.bld_ctl.buttons -borderwidth 1 -relief sunken
    button $base.bld_ctl.buttons.build \
        -command build_the_project -padx 11 -pady 4 -text Build 


    frame $base.buttons3 \
        -borderwidth 1 -height 30 -width 30 
    button $base.buttons3.save_proj \
        -command save_project -padx 11 -pady 4 -text Save 
    button $base.buttons3.load_open_proj \
        -command load_open_project -padx 11 -pady 4 -text Load 
    button $base.buttons3.cancel \
        -command dismiss_project_spec -padx 11 -pady 4 -text Cancel 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.prj_title \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.prj_title.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.prj_title.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
    pack $base.filename \
        -anchor center -expand 0 -fill x -pady 2 -side top 
    pack $base.filename.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.filename.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
    pack $base.startup \
        -anchor center -expand 0 -fill x -side top 
    pack $base.startup.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack $base.startup.entry \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    pack $base.sep_prolog_files \
        -anchor center -expand 0 -fill x -side top 
    pack $base.prolog_files_ctl \
        -anchor center -expand 0 -fill x -side top 
    pack $base.prolog_files_ctl.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $base.prolog_files_ctl.label \
        -anchor w -expand 0 -fill none -side left 
    pack $base.prolog_files \
        -anchor center -expand 0 -fill x -side top 
    grid columnconf $base.prolog_files 0 -weight 1
    grid rowconf $base.prolog_files 0 -weight 1
    grid $base.prolog_files.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.prolog_files.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.prolog_files.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.prolog_files.buttons \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    pack $base.prolog_files.buttons.add_file \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $base.prolog_files.buttons.add_mult_file \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $base.prolog_files.buttons.del_files \
        -anchor e -expand 0 -fill none -padx 10 -side right 
	pack forget $base.prolog_files

    pack $base.sep_search_dirs \
        -anchor center -expand 0 -fill x -side top 
    pack $base.search_dirs_ctl \
        -anchor center -expand 0 -fill x -side top 
    pack $base.search_dirs_ctl.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $base.search_dirs_ctl.label \
        -anchor w -expand 0 -fill none -side left 
    pack $base.search_dirs \
        -anchor center -expand 0 -fill x -side top 
    grid columnconf $base.search_dirs 0 -weight 1
    grid rowconf $base.search_dirs 0 -weight 1
    grid $base.search_dirs.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.search_dirs.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.search_dirs.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.search_dirs.buttons \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    pack $base.search_dirs.buttons.add_dir \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $base.search_dirs.buttons.del_dir \
        -anchor center -expand 0 -fill none -padx 10 -side right 
	pack forget $base.search_dirs

    pack $base.sep_oop_files \
        -anchor center -expand 0 -fill x -side top 
    pack $base.oop_files_ctl \
        -anchor center -expand 0 -fill x -side top 
    pack $base.oop_files_ctl.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $base.oop_files_ctl.label \
        -anchor w -expand 0 -fill none -side left 
    pack $base.oop_files \
        -anchor center -expand 0 -fill x -side top 
    grid columnconf $base.oop_files 0 -weight 1
    grid rowconf $base.oop_files 0 -weight 1
    grid $base.oop_files.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.oop_files.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.oop_files.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.oop_files.buttons \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    pack $base.oop_files.buttons.add_file \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $base.oop_files.buttons.del_files \
        -anchor e -expand 0 -fill none -padx 10 -side right 
	pack forget $base.oop_files

    pack $base.sep_typ_files \
        -anchor center -expand 0 -fill x -side top 
    pack $base.typ_files_ctl \
        -anchor center -expand 0 -fill x -side top 
    pack $base.typ_files_ctl.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $base.typ_files_ctl.label \
        -anchor w -expand 0 -fill none -side left 
    pack $base.typ_files \
        -anchor center -expand 0 -fill x -side top 
    grid columnconf $base.typ_files 0 -weight 1
    grid rowconf $base.typ_files 0 -weight 1
    grid $base.typ_files.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.typ_files.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.typ_files.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.typ_files.buttons \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    pack $base.typ_files.buttons.add_file \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $base.typ_files.buttons.del_files \
        -anchor e -expand 0 -fill none -padx 10 -side right 
	pack forget $base.typ_files

    pack $base.sep_lib_files \
        -anchor center -expand 0 -fill x -side top 
    pack $base.lib_files_ctl \
        -anchor center -expand 0 -fill x -side top 
    pack $base.lib_files_ctl.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $base.lib_files_ctl.label \
        -anchor w -expand 0 -fill none -side left 
    pack $base.lib_files \
        -anchor center -expand 0 -fill x -side top 
    grid columnconf $base.lib_files 0 -weight 1
    grid rowconf $base.lib_files 0 -weight 1
    grid $base.lib_files.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.lib_files.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.lib_files.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.lib_files.buttons \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew 
    pack $base.lib_files.buttons.add_file \
        -anchor w -expand 0 -fill none -padx 10 -side left 
    pack $base.lib_files.buttons.del_files \
        -anchor e -expand 0 -fill none -padx 10 -side right 
	pack forget $base.lib_files

    pack $base.sep_bld_ctl \
        -anchor center -expand 0 -fill x -side top 
    pack $base.bld_ctl_ctl \
        -anchor center -expand 0 -fill x -side top 
    pack $base.bld_ctl_ctl.open_btn \
        -anchor center -expand 0 -fill none -side left 
    pack $base.bld_ctl_ctl.label \
        -anchor w -expand 0 -fill none -side left 
    pack $base.bld_ctl \
        -anchor center -expand 0 -fill x -side top 


    pack $base.bld_ctl.os_info \
        -anchor center -expand 0 -fill x -side top 
    pack $base.bld_ctl.os_info.l1 \
        -anchor center -expand 0 -fill none -side left 
    pack $base.bld_ctl.os_info.os_label  \
        -anchor center -expand 0 -fill x -side left 
    pack $base.bld_ctl.os_info.minor_os_label  \
        -anchor center -expand 0 -fill x -side right 
    pack $base.bld_ctl.os_info.l2  \
        -anchor center -expand 0 -fill none -side right 
    pack $base.bld_ctl.bld_cmd \
        -anchor center -expand 0 -fill x -side top 
    pack $base.bld_ctl.bld_cmd.l1  \
        -anchor center -expand 0 -fill none -side left 
    pack $base.bld_ctl.bld_cmd.skel  \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
    pack $base.bld_ctl.buttons  \
        -anchor center -expand 0 -fill x -side top 
    pack $base.bld_ctl.buttons.build  \
        -anchor center -expand 0 -fill none -padx 15 -side top 

	pack forget $base.bld_ctl


    pack $base.buttons3 \
        -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $base.buttons3.save_proj \
        -anchor center -expand 0 -fill none -padx 15 -side left 
    pack $base.buttons3.load_open_proj \
        -anchor center -expand 0 -fill none -padx 15 -side left 
    pack $base.buttons3.cancel \
        -anchor center -expand 0 -fill none -padx 15 -side right 
	
	wm geometry $base ""
}



