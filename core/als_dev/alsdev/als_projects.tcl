#|==================================================================
#|				als_projects.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl support for project management in the 
#|		ALS Development Environment
#|
#|		"$Id: als_projects.tcl,v 1.9 1998/08/25 02:21:35 ken Exp $"
#|==================================================================

proc load_project {} {
	send_prolog als_ide_mgr load_project
}

proc load_this_project {} {
	send_prolog als_ide_mgr load_this_project
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

	if { "$Projectfile"!="" } then {
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
	set ID ""
	set DFT "-filetypes [list [list [list $FileKind $FileTypes] {\"All Files\" {*}} ] ]"

	set NewFilePath [eval tk_getOpenFile $DFT \
			{-title "Project File to Open"} \
			[list "-initialdir" $DfltDir] ]
	if {"$NewFilePath"==""} then {
		return
	}
	set BaseNewFile [file tail $NewFilePath]
	set Prev [$Listbox get 0 end]
	if {[lsearch -exact $Prev $BaseNewFile] == -1 } then {
		$Listbox insert end $BaseNewFile
	}
}

proc add_to_files_list_mult { FS Listbox FileTypes FileKind DfltDir} {

	prolog call alsdev choose_mult_files \
		-list $FileTypes -atom $FileKind -atom $DfltDir -var Choices
	if { "$Choices"!="" } then {
		set Prev [$Listbox get 0 end]
		foreach Entry $Choices {
			if {[lsearch -exact $Prev $Entry] == -1 } then {
				$Listbox insert end $Entry
			}
		}
	}
}

proc del_from_files_list { FS Listbox FileTypes FileKind } {

}

proc add_search_dirs {Listbox} {
	set CWD [pwd]
	set NewDir [getDirectory]
	$Listbox insert end $NewDir
	cd $CWD
}

proc prj_slot_focus { Slot Listbox PrjMgrHandle } {
	global proenv
	set Item [$Listbox get [lindex [$Listbox curselection] 0] ]
	prolog call $proenv(dflt_mod) send \
		-number $PrjMgrHandle  -list [list prj_slot_focus $Slot $Item]
}
