#|==================================================================
#|				als_projects.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl support for project management in the 
#|		ALS Development Environment
#|
#|		"$Id: als_projects.tcl,v 1.4 1998/05/18 01:37:47 ken Exp $"
#|==================================================================

proc load_project {} {
    global pmv
	 
	set Projectfile [tk_getOpenFile \
		-filetypes {{"Prolog Project Files" {.ppj}}} \
		-title "Prolog Project File to Open"]
	set File [file tail $Projectfile]
	set Dir [file dirname $Projectfile]
#puts "Tcl:Calling open_project_file"
	if { "$Projectfile"!="" } then {
		prolog call alsdev load_project_file  \
			-list [file split $Dir] -atom $File
	}
}

proc open_project {} {
    global pmv
	 
	set Projectfile [tk_getOpenFile \
		-filetypes {{"Prolog Project Files" {.ppj}}} \
		-title "Prolog Project File to Open"]
	set File [file tail $Projectfile]
	set Dir [file dirname $Projectfile]
	if { "$Projectfile"!="" } then {
		prolog call alsdev open_project_file  \
			-list [file split $Dir] -atom $File
	}
}

proc load_open_project {} {
    global pmv

    set BaseFile [.ppj_spec.filename.entry get]
	prolog call alsdev load_project_file  \
			-list [file split [pwd]] -atom $BaseFile
}

proc new_project {} {
    global pmv
	Window show .ppj_spec 
}



proc display_project {Title PFN Start FilesList DirsList OS MinorOS BldCmd } {
	
	Window show .ppj_spec
    .ppj_spec.prj_title.entry delete 0 end 
    .ppj_spec.prj_title.entry insert end $Title 

    .ppj_spec.filename.entry delete 0 end 
    .ppj_spec.filename.entry insert end $PFN 

    .ppj_spec.startup.entry delete 0 end 
    .ppj_spec.startup.entry insert end $Start 


    .ppj_spec.prolog_files.listbox delete 0 end 
	set SortedFiles [lsort $FilesList]
	foreach File $SortedFiles {
    	.ppj_spec.prolog_files.listbox insert end $File 
	}

    .ppj_spec.search_dirs.listbox delete 0 end 
	foreach Dir $DirsList {
    	.ppj_spec.search_dirs.listbox insert end \
			[eval file join $Dir]
	}

    .ppj_spec.bld_ctl.os_info.os_label configure -text $OS
    .ppj_spec.bld_ctl.os_info.minor_os_label  configure -text $MinorOS
    .ppj_spec.bld_ctl.bld_cmd.skel delete 0 end 
    .ppj_spec.bld_ctl.bld_cmd.skel insert end $BldCmd

}

proc add_new_file {} {
	set NewFilePath [tk_getOpenFile \
		-filetypes {{"Prolog Files" {.pro .pl}} {"All Files" {*}} } \
		-title "Project File to Open" ]
	set NewFile [file tail $NewFilePath]
	set List [.ppj_spec.prolog_files.listbox get 0 end]
	lappend List $NewFile
	set NewList [lsort $List]
	.ppj_spec.prolog_files.listbox delete 0 end
	foreach FF $NewList {
		.ppj_spec.prolog_files.listbox insert end $FF
	}
}

proc add_mult_files {} {
	set PrevFiles [.ppj_spec.prolog_files.listbox get 0 end]

	prolog call alsdev add_mult_files \
						-list $PrevFiles  \
						-atom .ppj_spec.prolog_files.listbox
}

proc delete_files {} {
	set SelectionIndicies [.ppj_spec.prolog_files.listbox curselection]
	set List [.ppj_spec.prolog_files.listbox get 0 end]

	.ppj_spec.prolog_files.listbox delete 0 end
	set i 0
	foreach j $SelectionIndicies {
		for {set i $i} {$i < $j} {incr i} {
			.ppj_spec.prolog_files.listbox insert end [lindex $List 0]
			set List [lrange $List 1 end]
		}
		incr i
		set List [lrange $List 1 end]
	}
	foreach FN $List {
		.ppj_spec.prolog_files.listbox insert end $FN
	}

}

proc add_directory {} {
	bell
}

proc delete_directory {} {
	set SelectionIndicies [.ppj_spec.search_dirs.listbox curselection]
	set List [.ppj_spec.search_dirs.listbox get 0 end]

	.ppj_spec.search_dirs.listbox delete 0 end
	set i 0
	foreach j $SelectionIndicies {
		for {set i $i} {$i < $j} {incr i} {
			.ppj_spec.search_dirs.listbox insert end [lindex $List 0]
			set List [lrange $List 1 end]
		}
		incr i
		set List [lrange $List 1 end]
	}
	foreach DN $List {
		.ppj_spec.search_dirs.listbox insert end $DN
	}

}

proc save_project {} {
    set Title [.ppj_spec.prj_title.entry get]
    set ProjFile [.ppj_spec.filename.entry get] 
    set Start [.ppj_spec.startup.entry  get]
    set ProFiles [.ppj_spec.prolog_files.listbox get 0 end] 
    set SDirs [.ppj_spec.search_dirs.listbox get 0 end ]
	Window hide .ppj_spec
	prolog call alsdev save_project -atom $Title -atom $ProjFile -atom $Start \
		-list $ProFiles -list $SDirs
}
 
proc dismiss_project_spec {} {
	Window hide .ppj_spec
    .ppj_spec.prj_title.entry delete 0 end 
    .ppj_spec.filename.entry delete 0 end 
    .ppj_spec.startup.entry delete 0 end 
    .ppj_spec.prolog_files.listbox delete 0 end 
    .ppj_spec.search_dirs.listbox delete 0 end 
}
  
proc toggle_prolog_files_list {} {
	global prolog_files_list 

	if {"$prolog_files_list"=="closed"} then {
    	.ppj_spec.prolog_files_ctl.open_btn configure -image open_ptr
		set prolog_files_list open
    	pack .ppj_spec.prolog_files  \
			-after .ppj_spec.prolog_files_ctl \
        	-anchor center -expand 0 -fill x -side top 
	} else {
    	.ppj_spec.prolog_files_ctl.open_btn configure -image closed_ptr
		set prolog_files_list closed
    	pack forget .ppj_spec.prolog_files 

	}
}
  
proc toggle_search_dirs_list {} {
	global search_dirs_list 

	if {"$search_dirs_list"=="closed"} then {
    	.ppj_spec.search_dirs_ctl.open_btn configure -image open_ptr
		set search_dirs_list open
    	pack .ppj_spec.search_dirs  \
			-after .ppj_spec.search_dirs_ctl \
        	-anchor center -expand 0 -fill x -side top 
	} else {
    	.ppj_spec.search_dirs_ctl.open_btn configure -image closed_ptr
		set search_dirs_list closed
    	pack forget .ppj_spec.search_dirs 

	}
}

proc toggle_oop_files_list {} {
	global oop_files_list 

	if {"$oop_files_list"=="closed"} then {
    	.ppj_spec.oop_files_ctl.open_btn configure -image open_ptr
		set oop_files_list open
    	pack .ppj_spec.oop_files  \
			-after .ppj_spec.oop_files_ctl \
        	-anchor center -expand 0 -fill x -side top 
	} else {
    	.ppj_spec.oop_files_ctl.open_btn configure -image closed_ptr
		set oop_files_list closed
    	pack forget .ppj_spec.oop_files 

	}
}

proc toggle_typ_files_list {} {
	global typ_files_list 

	if {"$typ_files_list"=="closed"} then {
    	.ppj_spec.typ_files_ctl.open_btn configure -image open_ptr
		set typ_files_list open
    	pack .ppj_spec.typ_files  \
			-after .ppj_spec.typ_files_ctl \
        	-anchor center -expand 0 -fill x -side top 
	} else {
    	.ppj_spec.typ_files_ctl.open_btn configure -image closed_ptr
		set typ_files_list closed
    	pack forget .ppj_spec.typ_files 

	}
}

proc merge_into_listbox {Items ListBoxWin} {

	set Prev [$ListBoxWin get 0 end]
	foreach Item $Items {
		lappend CleanItems [string trim $Item "'"]
	}
	append Prev " " $CleanItems
	set New [lsort $Prev]
	$ListBoxWin delete 0 end
	foreach Item $New {
		$ListBoxWin insert end $Item
	}
}

proc toggle_lib_files_list {} {
	global lib_files_list 

	if {"$lib_files_list"=="closed"} then {
    	.ppj_spec.lib_files_ctl.open_btn configure -image open_ptr
		set lib_files_list open
    	pack .ppj_spec.lib_files  \
			-after .ppj_spec.lib_files_ctl \
        	-anchor center -expand 0 -fill x -side top 
	} else {
    	.ppj_spec.lib_files_ctl.open_btn configure -image closed_ptr
		set lib_files_list closed
    	pack forget .ppj_spec.lib_files 

	}
}

proc toggle_bld_ctl_list {} {
	global bld_ctl_list 

	if {"$bld_ctl_list"=="closed"} then {
    	.ppj_spec.bld_ctl_ctl.open_btn configure -image open_ptr
		set bld_ctl_list open
    	pack .ppj_spec.bld_ctl  \
			-after .ppj_spec.bld_ctl_ctl \
        	-anchor center -expand 0 -fill x -side top 
	} else {
    	.ppj_spec.bld_ctl_ctl.open_btn configure -image closed_ptr
		set bld_ctl_list closed
    	pack forget .ppj_spec.bld_ctl 

	}
}

proc build_the_project {} {
	
	prolog call alsdev build_project 
}
