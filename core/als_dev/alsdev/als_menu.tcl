


# Menu accelerator modifier key and elipsis string.

if {"$tcl_platform(platform)" == "macintosh"} {
	set mod "Cmd"
	set elipsis "…"
} else {
	set mod "Ctrl"
	set elipsis "..."
}


proc add_default_menus {menubar} {
	global tcl_platform

	if {$tcl_platform(platform) == "macintosh"} {
		menu $menubar.apple -tearoff 0
		$menubar.apple add command -label "About ALS Prolog…" -command {Window show .about ; raise .about}
		$menubar add cascade -menu $menubar.apple
	}
}


proc add_file_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	
	menu $menubar.file
	
    $menubar.file add command -label New -accelerator "$mod-N" -command document.new
    if {$tcl_platform(platform) == "macintosh"} {
    	$menubar.file add command -label "New Project" -state disabled
    }
    $menubar.file add command -label "Open$elipsis" -accelerator "$mod-O" -command document.open
    $menubar.file add command -label Close -accelerator "$mod-W" -command "$type.close $window"
    $menubar.file add separator
    $menubar.file add command -label Save -accelerator "$mod-S" -command "$type.save $window"
    $menubar.file add command -label "Save As$elipsis" -command "$type.save_as $window"
    $menubar.file add separator
    $menubar.file add command -label "Consult$elipsis" -command reconsult
    $menubar.file add command \
        -label "Set Directory$elipsis" -command set_directory 
    $menubar.file add command -label "Source Tcl$elipsis" -command source_tcl -state disabled
	$menubar.file add command \
		-label "Clear Workspace" -command clear_workspace
    $menubar.file add separator
    $menubar.file add command -label "Page Setup$elipsis" \
		-command "$type.page_setup $window" -state disabled
    $menubar.file add command -label "Print$elipsis" -accelerator "$mod-P"\
		-command "$type.print $window" -state disabled
    $menubar.file add separator
    $menubar.file add command -label "Quit" -accelerator "$mod-Q" -command exit_prolog

	$menubar add cascade -menu $menubar.file -label "File"
}

proc add_edit_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis

	menu $menubar.edit
	
    $menubar.edit add command \
        -label Undo -accelerator "$mod-Z" -command "$type.undo $window" -state disabled
    $menubar.edit add separator
    $menubar.edit add command -label Cut \
		-accelerator "$mod-X" -command "$type.cut $window"
    $menubar.edit add command \
        -label Copy -accelerator "$mod-C" -command "$type.copy $window"
    $menubar.edit add command \
        -label Paste -accelerator "$mod-V" -command "$type.paste $window"
    $menubar.edit add command \
        -label Clear -command "$type.clear $window"
    $menubar.edit add separator
    $menubar.edit add command \
        -label {Select All} -accelerator "$mod-A" -command "$type.select_all $window"
    $menubar.edit add separator
    $menubar.edit add command -label "Preferences$elipsis" \
			-command "$type.preference $window"
	if {$tcl_platform(platform) != "macintosh"} {
    	.topals.mmenb.edit add separator
    	.topals.mmenb.edit add command -label {Flush Input} -state disabled
    }

	$menubar add cascade -menu $menubar.edit -label "Edit"
}

proc add_project_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis

	menu $menubar.project

	if {$tcl_platform(platform) == "macintosh"} {
		$menubar.project add command -label "Add File" -state disabled
		$menubar.project add command -label "Remove File" -state disabled
	} else {
		$menubar.project add command \
			-label "Open Project" -command open_project -state disabled
		$menubar.project add command \
			-label "New Project" -command new_project -state disabled
		$menubar.project add command \
			-label "Save Project" -command save_project -state disabled
		$menubar.project add command \
			-label "Save As Project" -command save_as_project -state disabled
		$menubar.project add command \
			-label "Close Project" -command close_project -state disabled
	}
    $menubar.project add separator
    $menubar.project add command -label "Consult" -accelerator "$mod-K" -command "$type.consult $window"
    $menubar.project add separator
	$menubar.project add command \
		-label "Dynamic Flags" \
		-command show_dynamic_flags
	menu $menubar.project.static -cursor {} -title "Static Flags"
	$menubar.project add cascade -label "Static Flags" -menu $menubar.project.static 

	$menubar add cascade -label "Project" -menu $menubar.project

}

proc add_tools_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis

	menu $menubar.tools

	$menubar.tools add checkbutton \
	-label Debugger -command exec_toggle_debugwin -variable proenv(debugwin)

	menu $menubar.tools.tclshell -relief raised
	$menubar.tools add cascade \
	-label {Tcl Shell} -menu $menubar.tools.tclshell -state disabled
	$menubar.tools.tclshell add command \
	-label "User Defined" -command {prolog_tcltk_shell user_def_choice}
	$menubar.tools.tclshell add command \
	-label "shl_tcli (System - Danger!)" \
	-command {prolog_tcltk_shell shl_tcli}

	$menubar.tools add separator 
	## DefStructs:
	menu $menubar.tools.defstr -relief raised
	$menubar.tools add cascade \
	-label {Structs} -menu $menubar.tools.defstr
	$menubar.tools.defstr add command \
	-label "Define New" -command new_defstruct
	$menubar.tools.defstr add command \
	-label "Edit" -command edit_defstruct 


	$menubar add cascade -label "Project" -menu $menubar.tools


}

proc add_help_menu {menubar} {
	global tcl_platform
	global mod
	global elipsis

	if {$tcl_platform(platform) != "macintosh"} {
		menu $menubar.help
		
		$menubar.help add command -label "About ALS Prolog$elipsis" \
			-command {Window show .about ; raise .about}
		
		$menubar add cascade -label "Help" -menu $menubar.help
	}

}
