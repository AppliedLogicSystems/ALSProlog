#|==================================================================
#|				als_projects.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl support for project management in the 
#|		ALS Development Environment
#|
#|		"$Id: als_projects.tcl,v 1.16 1998/11/18 21:50:09 ken Exp $"
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

proc save_project {} {
	send_prolog als_ide_mgr save_project
}

proc close_project {} {
	send_prolog als_ide_mgr close_project
}

proc new_project {} {
	send_prolog als_ide_mgr start_new_project
}


proc add_to_files_list { FS Listbox FileTypes FileKind  DfltDir } {
	global tcl_platform

#	if {$tcl_platform(platform) == "macintosh"} {
#		set types {{"Text Files" * TEXT} {"Prolog Files" {.pro .pl} TEXT} {"Tcl/Tk Files" {.tcl} TEXT}}
#	} else {
#		set types {{"Prolog Files" {.pro .pl}} {"Tcl/Tk Files" {.tcl}} {{All Files} *}}
#		}
#
#	set DFT [list -filetypes $types]
###	set ID ""
#	set NewFilePath [eval tk_getOpenFile $DFT \
#			{-title "Project File to Open"} \
#			[list "-initialdir" $DfltDir] ]

if {$tcl_platform(platform) != "unix"} {
	set NewFilePaths [eval getFiles  \
			{-prompt "Project File to Open"} ]

	if {$NewFilePaths == ""} then {
		return
	}
	foreach NewFilePath $NewFilePaths {
	set BaseNewFile [file tail $NewFilePath]
	set Prev [$Listbox get 0 end]
	if {[lsearch -exact $Prev $BaseNewFile] == -1} then {
		$Listbox insert end $BaseNewFile
	}
	}
} else {
	
	set types {{"Prolog Files" {.pro .pl}} {"Tcl/Tk Files" {.tcl}} {{All Files} *}}
	set DFT [list -filetypes $types]
	set NewFilePath [eval tk_getOpenFile $DFT \
			{-title "Project File to Open"} \
			[list "-initialdir" $DfltDir] ]
	set BaseNewFile [file tail $NewFilePath]
	set Prev [$Listbox get 0 end]
	if {[lsearch -exact $Prev $BaseNewFile] == -1} then {
		$Listbox insert end $BaseNewFile
	}
}

}

proc add_to_files_list_mult { FS Listbox FileTypes FileKind DfltDir} {
	global tcl_platform

	prolog call alsdev choose_mult_files \
		-list $FileTypes -atom $FileKind -atom $DfltDir -var Choices
	if {$Choices != ""} then {
		set Prev [$Listbox get 0 end]
		foreach Entry $Choices {
			if {[lsearch -exact $Prev $Entry] == -1} then {
				$Listbox insert end $Entry
			}
		}
	}
}

proc del_from_files_list { Listbox } {
	set SelNums [$Listbox curselection]
	set N [llength $SelNums]
	if {$N == 0} then {
		bell
		return
	}
	set ans [tk_messageBox -icon warning -title "Delete Paths?" \
		-message "Delete the $N selected files?" -type yesno -default yes]
	if {$ans == "yes"} then {
		for {set i [expr $N - 1] } { $i>=0 } { incr i -1 } {
			$Listbox delete $i
		}
	}
}

proc move_selection_up {Listbox} {
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
}

proc move_selection_down {Listbox} {
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
}


proc add_search_dirs {Listbox PathType} {
	set CWD [pwd]
	set NewDir [getDirectory -initialdir $CWD]
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
}

proc del_search_dirs {Listbox} {
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
