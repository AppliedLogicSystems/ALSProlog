##=================================================================================
#|				als_menu.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk procedures for IDE menu code
#|
#|	Author: Chuck Houpt
#|	Date:	January 1998
##=================================================================================

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
	
	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.file -tearoff $TearOff
	
    $menubar.file add command -label New -accelerator "$mod-N" -command document.new
    if {$tcl_platform(platform) == "macintosh"} {
    	$menubar.file add command -label "New Project" -state disabled
    }
    $menubar.file add command -label "Open$elipsis" -accelerator "$mod-O" -command document.open
    $menubar.file add command -label Close -accelerator "$mod-W" -command "$type.close $window"
    $menubar.file add separator
    $menubar.file add command -label Save -accelerator "$mod-S" -command "$type.save $window"
    $menubar.file add command -label "Save As$elipsis" -command "$type.save_as $window"

#    $menubar.file add separator
#    $menubar.file add command -label "Page Setup$elipsis" \
#		-command "$type.page_setup $window" -state disabled
#    $menubar.file add command -label "Print$elipsis" -accelerator "$mod-P"\
#		-command "$type.print $window" -state disabled

    $menubar.file add separator
    $menubar.file add command -label "Quit" -accelerator "$mod-Q" -command exit_prolog

	$menubar add cascade -menu $menubar.file -label "File"
}

proc add_edit_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	global proenv

	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.edit -tearoff $TearOff
	
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
    $menubar.edit add command \
		-label {Preferences} -command "fonts_and_colors $window"

	$menubar add cascade -menu $menubar.edit -label "Edit"
}

proc add_prolog_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis

	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.prolog -tearoff $TearOff

    $menubar.prolog add command -label "Consult" -accelerator "$mod-K" -command "$type.consult $window"
	$menubar.prolog add command \
		-label "Clear Workspace" -command clear_workspace

	if {"$type"=="listener"} then { 
    	$menubar.prolog add separator
    	$menubar.prolog add command \
        	-label "Set Directory$elipsis" -command set_directory 

    	$menubar.prolog add separator
		$menubar.prolog add command \
			-label "Dynamic Flags" -command show_dynamic_flags
		$menubar.prolog add command \
			-label "Static Flags" -command show_static_flags

#		menu $menubar.prolog.static -cursor {} -title "Static Flags"
#		$menubar.prolog add cascade -label "Static Flags" -menu $menubar.prolog.static 

	} elseif {"$type"=="debugwin"} then {
    	$menubar.prolog add separator
    	$menubar.prolog add command -label {Abort} \
        	-command { set DebugResponse Ba }
    	$menubar.prolog add command -label {Break} \
			-command { set DebugResponse Bb }
	}
	$menubar add cascade -label "Prolog" -menu $menubar.prolog
}

proc add_tools_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	global proenv

	if {"$type"=="document"} then { return }	

	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.tools -tearoff $TearOff

	if {"$type"=="listener"} then {
		$menubar.tools add checkbutton \
			-label Debugger -command exec_toggle_debugwin -variable proenv(debugwin)
    	$menubar.tools add command -label "Source Tcl$elipsis" -command source_tcl 
#    	$menubar.tools add command -label "Tcl Shell" -command tcl_shell 

		$menubar.tools add separator 
		## DefStructs:
		menu $menubar.tools.defstr -relief raised -tearoff 0
		$menubar.tools add cascade \
			-label {Structs} -menu $menubar.tools.defstr
		$menubar.tools.defstr add command \
			-label "Define New" -command new_defstruct
#		$menubar.tools.defstr add command \
#			-label "Edit" -command edit_defstruct 
	} else {

		# Spy
		$menubar.tools add checkbutton  -label {Spy/NoSpy} \
			-command exec_toggle_spywin -variable proenv(spywin)
		$menubar.tools add command  -label {NoSpy all } \
			-command {prolog call debugger nospy } 
		$menubar.tools add command  -label {Spy When} -state disabled

		$menubar.tools add separator

#		menu $menubar.tools.settings -relief raised
#		$menubar.tools add cascade -label Settings -menu $menubar.tools.settings
#    	$menubar.tools.settings add command \
#        	-label {Set Print Depth} 
#		$menubar.tools.settings add command \
#			-label {Toggle Flat Print} -command { set DebugResponse Bm }

		$menubar.tools add command  -label {Debug Settings } \
			-command {show_debug_settings}

		menu $menubar.tools.leashing -relief raised
    	$menubar.tools add cascade \
        	-label {Leashing}  -menu $menubar.tools.leashing
		$menubar.tools.leashing add checkbutton \
        	-label {call} -variable proenv(leash,call) \
			-command {exec_toggle_leash call} 
		$menubar.tools.leashing add checkbutton \
        	-label {exit} -variable proenv(leash,exit) \
			-command {exec_toggle_leash exit} 
		$menubar.tools.leashing add checkbutton \
        	-label {redo} -variable proenv(leash,redo) \
			-command {exec_toggle_leash redo} 
		$menubar.tools.leashing add checkbutton \
        	-label {fail} -variable proenv(leash,fail) \
			-command {exec_toggle_leash fail} 
	}
	$menubar add cascade -label "Tools" -menu $menubar.tools
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

