#|==================================================================
#|				als_projects.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl support for project management in the 
#|		ALS Development Environment
#|
#|		"$Id: als_projects.tcl,v 1.25 2004/02/14 20:56:37 ken Exp $"
#|==================================================================

proc load_project {} {
	send_prolog als_ide_mgr load_project
}

proc load_this_project {} {
	send_prolog als_ide_mgr load_this_project
}

proc post_open_project {ProjTitle Win} {
    set ELabel [.topals.mmenb.prolog entrycget end -label]
	if { $ProjTitle == $ELabel } then { return }
    .topals.mmenb.prolog add separator
	.topals.mmenb.prolog add command \
			-label "Active Project:" -font {Helvetica 10 italic} 
	.topals.mmenb.prolog add command \
		-label $ProjTitle -font {Helvetica 10 italic} -command "show_window $Win"
}

proc unpost_open_project {ProjTitle} {
	if {$ProjTitle == ""} then {return}
	set PrjIdx [.topals.mmenb.prolog index $ProjTitle]
	.topals.mmenb.prolog delete $PrjIdx
	.topals.mmenb.prolog delete last
	.topals.mmenb.prolog delete last
}


proc open_project {} {
	send_prolog als_ide_mgr open_project
}

proc select_project_file {} {
	set Projectfile [tk_getOpenFile \
		-filetypes {{"Prolog Project Files" {.ppj}}} \
		-title "Prolog Project File to Open"]
	set File [file tail $Projectfile]
	set Dir [file dirname $Projectfile]

	if {$Projectfile != ""} then {
		return [list $File [file split $Dir ]]
	} else { return "" }
}

proc save_project {w} {
	global array proenv
	send_prolog als_ide_mgr save_project
	set proenv($w,dirty) false
    	set proenv($w.addlprj,dirty) false
    	$w.buttons.save configure -state disabled
	return 1
}

proc close_project {} {
	send_prolog als_ide_mgr close_project
}

proc new_project {} {
	global array proenv

#    	set isdirtycheck [prj_perf_dirtycheck $w]
	send_prolog als_ide_mgr start_new_project
}


proc add_to_files_list { FS Listbox FileTypes FileKind  DfltDir w } {
	global array proenv
	set types {{"Prolog Files" {.pro .pl}} {"Tcl/Tk Files" {.tcl}} {{All Files} *}}
	set DFT [list -filetypes $types]
		# DFT currently not used.
		# -title "Files to Add to Project" not used.
		# cf: https://www.tcl.tk/man/tcl8.4/TkCmd/getOpenFile.htm
	set Choices [tk_getOpenFile -initialdir $DfltDir -multiple true -filetypes $FileTypes ]
	if {$Choices != ""} then {
		set Prev [$Listbox get 0 end]
		foreach Entry $Choices {
			if {$Entry != ""} then {
				set BaseNewFile [file tail $Entry]
				if {[lsearch -exact $Prev $BaseNewFile] == -1} then {
					$Listbox insert end $BaseNewFile
				}
			}
		}
		set proenv($w,dirty) true
    		$w.buttons.save configure -state active
	}
}

proc add_file_entry_to_list { Entry Listbox } {
    global array proenv
    set NewFile [$Entry get]
	if {$NewFile != ""} then {
		set Prev [$Listbox get 0 end]
		set BaseNewFile [file tail $NewFile]
			if {[lsearch -exact $Prev $BaseNewFile] == -1} then {
				$Listbox insert end $BaseNewFile
			}
	}
    $Entry delete 0 end
}



#proc add_to_files_list_mult { FS Listbox FileTypes FileKind DfltDir w} {
#    global array proenv
#
#	prolog call alsdev choose_mult_files \
#		-list $FileTypes -atom $FileKind -atom $DfltDir -var Choices
#	if {$Choices != ""} then {
#		set Prev [$Listbox get 0 end]
#		foreach Entry $Choices {
#			if {[lsearch -exact $Prev $Entry] == -1} then {
#				$Listbox insert end $Entry
#			}
#		}
#		set proenv($w,dirty) true
#    		$w.buttons.save configure -state active
#	}
#}

proc del_from_files_list { Listbox w } {
    	global array proenv
	set SelNums [$Listbox curselection]
	set N [llength $SelNums]
	if {$N == 0} then {
		bell
		return
	}
	set ans [tk_messageBox -icon warning -title "Delete Paths?" \
		-message "Delete the $N selected files?" -type yesno -default yes]
	if {$ans == "yes"} then {
		foreach i $SelNums {
			$Listbox delete $i
		}
		set proenv($w,dirty) true
    		$w.buttons.save configure -state active
	}
}

proc move_selection_up {Listbox w} {
    	global array proenv
	set SelIdx [lindex [$Listbox curselection] 0]
	if {$SelIdx == 0} then {
		bell
		return
	}
	set Item [$Listbox get $SelIdx]
	set NewIdx [expr $SelIdx - 1]
	$Listbox delete $SelIdx
	$Listbox insert $NewIdx $Item
	$Listbox selection set $NewIdx
	set proenv($w,dirty) true
    	$w.buttons.save configure -state active
}

proc move_selection_down {Listbox w} {
    	global array proenv
	set SelIdx [lindex [$Listbox curselection] 0]
	set Last [expr [$Listbox index end] - 1]
	if {$SelIdx == $Last} then {
		bell
		return
	}
	set Item [$Listbox get $SelIdx]
	set NewIdx [expr $SelIdx + 1]
	$Listbox delete $SelIdx
	$Listbox insert $NewIdx $Item
	$Listbox selection set $NewIdx
	set proenv($w,dirty) true
    	$w.buttons.save configure -state active
}


proc add_search_dirs {Listbox PathType w} {
    	global array proenv
	set CWD [pwd]
		# cf: https://www.tcl.tk/man/tcl8.3/TkCmd/chooseDirectory.htm
	set NewDir [tk_chooseDirectory -initialdir $CWD]
	cd $CWD
	set PrevEntries [$Listbox get 0 end]
	if {($NewDir == "") || ($CWD == $NewDir)} then {
		return
	}
	if {$PathType == "absolute"} then {
		if {[lsearch -exact $PrevEntries $NewDir]<0} then { 
			$Listbox insert end $NewDir
		}
	} else {
		set CWDList [file split $CWD]
		set NEWList [file split $NewDir]
		if {[lindex $CWDList 0] != [lindex $NEWList 0]} then {
			if {[lsearch -exact $PrevEntries $NewDir]<0} then { 
				$Listbox insert end $NewDir
			}
		} else {
			set RelPathList [relpathlist_from_to $CWDList $NEWList]
			set NewPath [eval [concat {file join} $RelPathList]]
			if {[lsearch -exact $PrevEntries $NewPath]<0} then { 
				$Listbox insert end $NewPath
			}
		}
	}
	set proenv($w,dirty) true
    	$w.buttons.save configure -state active
}

proc del_search_dirs {Listbox w} {
    	global array proenv
	set SelNums [$Listbox curselection]
	set N [llength $SelNums]
	if {$N == 0} then {
		bell
		return
	}
	set ans [tk_messageBox -icon warning -title "Delete Paths?" \
		-message "Delete the $N selected search paths?" -type yesno -default yes]
	if {$ans == "yes"} then {
		foreach i $SelNums {
			$Listbox delete $i
		}
		set proenv($w,dirty) true
    		$w.buttons.save configure -state active
	}
}


proc prj_slot_focus { Slot Listbox PrjMgrHandle } {
	global proenv
	set Item [$Listbox get [lindex [$Listbox curselection] 0] ]
	prolog call $proenv(dflt_mod) send \
		-number $PrjMgrHandle  -list [list prj_slot_focus $Slot $Item]
}

proc relpathlist_from_to {StartList EndList} {
	set SL $StartList
	set EL $EndList
	set RL {}
	while {($SL != {}) && ($EL != {}) && ([lindex $SL 0] == [lindex $EL 0])} {
		set SL [lrange $SL 1 end]
		set EL [lrange $EL 1 end]
	}
	while {$SL != {}} {
		lappend RL ".."
		set SL [lrange $SL 1 end]
	}
	return [concat $RL $EL]
}

proc do_new_app_top {} {
	set base .new_app_top
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 422x245+132+132
    wm maxsize $base 1028 753
    wm minsize $base 104 1
    wm overrideredirect $base 0
    wm resizable $base 1 0
    wm deiconify $base
    wm title $base "New  Application Setup"
	wm protocol $base WM_DELETE_WINDOW "wm iconify $base"
    frame $base.fra25 \
        -borderwidth 2 -height 10 -width 125 
    label $base.lab23 \
        -borderwidth 1 -font {{MS Sans Serif}  8 italic} -justify left \
        -text {The only required item is "Application Name"} 
    label $base.lab24 \
        -borderwidth 1 -font {{MS Sans Serif} 8 italic} -justify left \
        -text {Any others left blank will be given defaults.} 
    frame $base.shortname \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.shortname.01 \
        -anchor w -text {Application Name: } 
    entry $base.shortname.entry \
        -cursor {} 
    frame $base.exename \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.exename.01 \
        -anchor w -text {Executable Name (no ext.): } 
    entry $base.exename.entry \
        -cursor {} 
    frame $base.docext \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.docext.01 \
        -anchor w -text {Document Extension: } 
    entry $base.docext.entry \
        -cursor {} 
    frame $base.company \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.company.01 \
        -anchor w -text {Company: } 
    entry $base.company.entry \
        -cursor {} 
    frame $base.targetfolder \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.targetfolder.01 \
        -anchor w -text {Target Folder: } 
    button $base.targetfolder.but33 \
        -text Browse -command "set_target_folder_nat .new_app_top.targetfolder.entry"
    entry $base.targetfolder.entry \
        -cursor {} 
    frame $base.fra29 \
        -borderwidth 2 -height 75 -width 125 
    button $base.fra29.but30 \
        -text {Generate Application Setup} -command gen_app_setup
    button $base.fra29.but32 \
        -text {Apply Defaults} -command apply_new_app_defaults
    button $base.fra29.but31 \
        -anchor e -text Exit -command "wm withdraw $base"
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.fra25 \
        -in .new_app_top -anchor center -expand 0 -fill none -side top 
    pack $base.lab23 \
        -in .new_app_top -anchor w -expand 0 -fill none -side top 
    pack $base.lab24 \
        -in .new_app_top -anchor w -expand 0 -fill none -side top 

#    pack $base.fra26 \ -in .new_app_top -anchor center -expand 0 -fill none -side top 

    pack $base.shortname \
        -in .new_app_top -anchor center -expand 0 -fill x -pady 3 -side top 
    pack $base.shortname.01 \
        -in .new_app_top.shortname -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $base.shortname.entry \
        -in .new_app_top.shortname -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    pack $base.exename \
        -in .new_app_top -anchor center -expand 0 -fill x -pady 3 -side top 
    pack $base.exename.01 \
        -in .new_app_top.exename -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $base.exename.entry \
        -in .new_app_top.exename -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    pack $base.docext \
        -in .new_app_top -anchor w -expand 0 -fill none -pady 3 -side top 
    pack $base.docext.01 \
        -in .new_app_top.docext -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $base.docext.entry \
        -in .new_app_top.docext -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    pack $base.company \
        -in .new_app_top -anchor w -expand 0 -fill x -pady 3 -side top 
    pack $base.company.01 \
        -in .new_app_top.company -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $base.company.entry \
        -in .new_app_top.company -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    pack $base.targetfolder \
        -in .new_app_top -anchor center -expand 0 -fill x -side top 
    pack $base.targetfolder.01 \
        -in .new_app_top.targetfolder -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $base.targetfolder.but33 \
        -in .new_app_top.targetfolder -anchor center -expand 0 -fill none -padx 2 \
        -side right 
    pack $base.targetfolder.entry \
        -in .new_app_top.targetfolder -anchor center -expand 1 -fill x -padx 4 -pady 2 \
        -side right 
    pack $base.fra29 \
        -in .new_app_top -anchor center -expand 0 -fill x -side bottom 
    pack $base.fra29.but30 \
        -in .new_app_top.fra29 -anchor w -expand 0 -fill none -padx 25 -side left 
    pack $base.fra29.but32 \
        -in .new_app_top.fra29 -anchor w -expand 0 -fill none -padx 15 -side left 
    pack $base.fra29.but31 \
        -in .new_app_top.fra29 -anchor center -expand 0 -fill none -padx 25 \
        -side right 
}

proc set_target_folder { EntryPath } {
	set CWD [pwd]
	set NewDir [getDirectory]
	if {$NewDir != ""} {
		$EntryPath delete 0 end
		$EntryPath insert end $NewDir
	}
	cd $CWD
}

proc set_target_folder_nat { EntryPath } {
	set_target_folder $EntryPath
	set base .new_app_top
    set ESN [$base.shortname.entry get]
	if { $ESN == "" } {
    	$base.shortname.entry insert end [file tail [$EntryPath get]]
        apply_new_app_defaults
	}
}

proc pwl { S } {
	prolog call builtins write -atom $S
	prolog call builtins nl
}

proc apply_new_app_defaults {} {
	set SN [.new_app_top.shortname.entry get]
	if { $SN == "" } {
		return
	}
	set LCSN [string tolower [string trim $SN]]
	set Result [do_shrink $LCSN]
    if {[.new_app_top.exename.entry get] == ""} {
    	.new_app_top.exename.entry insert end $Result
	}
	if {[.new_app_top.docext.entry get] == ""} {
    	.new_app_top.docext.entry insert end $Result
	}
    if {[.new_app_top.targetfolder.entry get] == ""} {
    	.new_app_top.targetfolder.entry insert end [pwd]
	}
	update
}

proc do_shrink {S} {
	set Result ""
	set L [string length $S]
	for {set n 0} {$n < $L} {incr n} {
		set c [string index $S $n]
		if {$c != "_"} {
			if {$c != " "} {
				append Result $c
			}
		}
	}
	return $Result
}


proc gen_app_setup {} {
	set SN [.new_app_top.shortname.entry get]
	if {$SN == ""} {
		bell
		return
	}
	apply_new_app_defaults
    set EN [.new_app_top.exename.entry get]
    set DE [.new_app_top.docext.entry get]
    set CN [.new_app_top.company.entry get]
    set TGT [.new_app_top.targetfolder.entry get]
	prolog call app_gui_gen create_new_app \
		-atom $SN -atom $EN -atom $DE -atom $CN -atom $TGT
}

proc do_app_gui_gen_dialog {} {
	set base .app_gui_gen_dialog
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 400x145+132+132
    wm maxsize $base 1028 753
    wm minsize $base 104 1
    wm overrideredirect $base 0
    wm resizable $base 1 0
    wm deiconify $base
    wm title $base "GUI Generator"
	wm protocol $base WM_DELETE_WINDOW "wm iconify $base"

    frame $base.sourcefile \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.sourcefile.01 \
        -anchor w -text {Spec File: } 
    entry $base.sourcefile.entry \
        -cursor {} 
    button $base.sourcefile.browse \
        -text Browse \
		-command "set_spec_file $base.sourcefile.entry $base.targetfolder.entry"
    frame $base.targetfolder \
        -borderwidth 1 -height 30 -relief raised -width 30 
    label $base.targetfolder.01 \
        -anchor w -text {Target Folder: } 
    entry $base.targetfolder.entry \
        -cursor {} 
    button $base.targetfolder.browse \
        -text Browse -command "set_target_folder $base.targetfolder.entry"
    frame $base.buttons \
        -borderwidth 2 -height 75 -width 125 
    button $base.buttons.generate \
        -text {Generate GUI} -command do_gen_gui
    button $base.buttons.exit \
        -anchor e -text Exit -command "wm withdraw $base"
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.sourcefile \
        -in $base -anchor center -expand 0 -fill x -side top -pady 4
    pack $base.sourcefile.01 \
        -in $base.sourcefile -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $base.sourcefile.entry \
        -in $base.sourcefile -anchor center -expand 1 -fill x -padx 4 -pady 2 \
        -side left 
    pack $base.sourcefile.browse \
        -in $base.sourcefile -anchor center -expand 0 -fill none -padx 2 \
        -side right 

    pack $base.targetfolder \
        -in $base -anchor center -expand 0 -fill x -side top -pady 4
    pack $base.targetfolder.01 \
        -in $base.targetfolder -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $base.targetfolder.entry \
        -in $base.targetfolder -anchor center -expand 1 -fill x -padx 4 -pady 2 \
        -side left 
    pack $base.targetfolder.browse \
        -in $base.targetfolder -anchor center -expand 0 -fill none -padx 2 \
        -side right 

    pack $base.buttons \
        -in $base -anchor center -expand 0 -fill x -side bottom -pady 4
    pack $base.buttons.generate \
        -in $base.buttons -anchor w -expand 0 -fill none -padx 25 -side left 
    pack $base.buttons.exit \
        -in $base.buttons -anchor center -expand 0 -fill none -padx 25 \
        -side right 
}

proc set_spec_file { SpecEntryPath TgtEntryPath } {
	set SpecFile [tk_getOpenFile \
		-filetypes {{"Application Spec Files" {.spec}}} \
		-title "Application Spec File to Open"]
	if {$SpecFile != ""} {
		set SpecDir [file dirname $SpecFile]
		$SpecEntryPath delete 0 end
		$SpecEntryPath insert end $SpecFile
		set InitTgt [$TgtEntryPath get]
		if { $InitTgt == "" } {
			$TgtEntryPath insert end $SpecDir
		} else {
			set ans [tk_dialog .quit_dialog \
					"Change Target?" \
					"Change Target to:\n$SpecDir ?" "" 0 \
					"Yes" "No"
			]
			if { $ans == 0 } {
				$TgtEntryPath delete 0 end
				$TgtEntryPath insert end $SpecDir
			} 
		}
	}
}

proc start_app_gui_gen {} {
	set ProjectFile [tk_getOpenFile \
		-filetypes {{"Application Spec Files" {.spec}}} \
		-title "Application Spec File to Open"]
	if {$ProjectFile != ""} then {
		prolog call app_gui_gen start_app_gui_gen -atom $ProjectFile
	}
}

proc do_gen_gui {} {
	set base .app_gui_gen_dialog
    set SpecFile [$base.sourcefile.entry get]
	set SpecFolder [file dirname $SpecFile]
    set TgtFolder [$base.targetfolder.entry get]

	if {$SpecFile != ""} then {
		prolog call app_gui_gen start_app_gui_gen -atom $SpecFile \
			-atom $SpecFolder -atom $TgtFolder
	}
}


