#|==================================================================
#|				als_projects.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl support for project management in the 
#|		ALS Development Environment
#|
#|		"$Id: als_projects.tcl,v 1.7 1998/08/17 20:29:45 ken Exp $"
#|==================================================================

proc load_project {} {
	send_prolog als_ide_mgr load_project
}

#	prolog call alsdev load_project

proc load_this_project {} {
	send_prolog als_ide_mgr load_this_project
}

#	prolog call alsdev load_this_project

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
	prolog call alsdev save_project
}

proc close_project {} {
	prolog call alsdev close_project
}

proc new_project {} {
	prolog call alsdev start_new_project
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
	$Listbox insert end $BaseNewFile
}





proc add_to_files_list_mult { FS Listbox FileTypes FileKind DfltDir} {

	prolog call alsdev choose_mult_files \
		-list $FileTypes -atom $FileKind -atom $DfltDir -var Choices
puts "$Choices"
	foreach Entry $Choices {
		$Listbox insert end $Entry
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

