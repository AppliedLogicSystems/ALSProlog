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

if {$tcl_platform(platform) == "macintosh"} {
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
	menu $menubar.file -tearoff $TearOff -title File
	
    $menubar.file add command -label New -accelerator "$mod-N" -command {re document.new}
    $menubar.file add command -label "Open$elipsis" -accelerator "$mod-O" -command {re document.open}
    $menubar.file add command -label Close -accelerator "$mod-W" -command "re {$type.close $window}"
    $menubar.file add separator
    $menubar.file add command -label Save -accelerator "$mod-S" -command "re {$type.save $window}"
    $menubar.file add command -label "Save As$elipsis" -command "re {$type.save_as $window}"

#    $menubar.file add separator
#    $menubar.file add command -label "Page Setup$elipsis" \
#		-command "re {$type.page_setup $window}" -state disabled
#    $menubar.file add command -label "Print$elipsis" -accelerator "$mod-P"\
#		-command "re {$type.print $window}" -state disabled

    $menubar.file add separator
    $menubar.file add command -label "Quit" -accelerator "$mod-Q" -command {re exit_prolog}

	$menubar add cascade -menu $menubar.file -label "File"
}

proc add_edit_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	global proenv

	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.edit -tearoff $TearOff -title Edit
	
    $menubar.edit add command \
        -label Undo -accelerator "$mod-Z" -command "re {$type.undo $window}" -state disabled
    $menubar.edit add separator
    $menubar.edit add command -label Cut \
		-accelerator "$mod-X" -command "re {$type.cut $window}"
    $menubar.edit add command \
        -label Copy -accelerator "$mod-C" -command "re {$type.copy $window}"
    $menubar.edit add command \
        -label Paste -accelerator "$mod-V" -command "re {$type.paste $window}"
    $menubar.edit add command \
        -label Clear -command "re {$type.clear $window}"
    $menubar.edit add separator
    $menubar.edit add command \
        -label {Select All} -accelerator "$mod-A" -command "re {$type.select_all $window}"
    $menubar.edit add separator
    $menubar.edit add command \
		-label "Preferences$elipsis" -command "re {fonts_and_colors $window}"

	$menubar add cascade -menu $menubar.edit -label "Edit"
}

proc add_prolog_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis

	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.prolog -tearoff $TearOff -title Prolog

    $menubar.prolog add command -label "Consult" -accelerator "$mod-K" -command "re {$type.consult $window}"
	$menubar.prolog add command \
		-label "Clear Workspace" -command {re clear_workspace}

	if {"$type"=="listener"} then { 
    	$menubar.prolog add separator
    	$menubar.prolog add command \
        	-label "Set Directory$elipsis" -command {re set_directory} 

    	$menubar.prolog add separator
		$menubar.prolog add command \
			-label "Dynamic Flags" -command {re show_dynamic_flags}
		$menubar.prolog add command \
			-label "Static Flags" -command {re show_static_flags}
	} elseif {"$type"=="debugwin"} then {
    	$menubar.prolog add separator
    	$menubar.prolog add command -label {Abort} \
        	-command {re { set DebugResponse Ba }}
    	$menubar.prolog add command -label {Break} \
			-command {re { set DebugResponse Bb }}
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
	menu $menubar.tools -tearoff $TearOff -title Tools

	if {"$type"=="listener"} then {
		$menubar.tools add checkbutton \
			-label Debugger -command exec_toggle_debugwin -variable proenv(debugwin)
    	$menubar.tools add command -label "Source Tcl$elipsis" -command {re source_tcl} 
#    	$menubar.tools add command -label "Tcl Shell" -command {re tcl_shell} 

		$menubar.tools add separator 
		## DefStructs:
		menu $menubar.tools.defstr -relief raised -tearoff 0
		$menubar.tools add cascade \
			-label {Structs} -menu $menubar.tools.defstr
		$menubar.tools.defstr add command \
			-label "Define New" -command {re new_defstruct}
#		$menubar.tools.defstr add command \
#			-label "Edit" -command {re edit_defstruct} 
	} else {

		# Spy
		$menubar.tools add command  -label "Spy$elipsis" \
			-command {re toggle_spywin} 
#		$menubar.tools add checkbutton  -label {Spy$elipsis} \
#			-command exec_toggle_spywin -variable proenv(spywin)
		$menubar.tools add command  -label {NoSpy all } \
			-command {re {prolog call debugger nospy }} 

#		$menubar.tools add command  -label {Spy When} -state disabled

		$menubar.tools add separator
		$menubar.tools add command  -label {Debug Settings } \
			-command {re {show_debug_settings}}
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

		# listener -- edit menu:
proc listener.cut {xw} {
	set w .topals
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
		$w.text delete sel.first sel.last
	}
}

proc listener.copy {xw} {
	set w .topals
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
	}
}

proc listener.paste {xw} {
	set w .topals
	catch {$w.text delete sel.first sel.last}
	$w.text insert insert [selection get -displayof $w -selection CLIPBOARD]
	set proenv($w,dirty) true
}

proc listener.clear {xw} {
	set w .topals
	global array proenv
	catch {$w.text delete sel.first sel.last}
	set proenv($w,dirty) true
}

proc listener.select_all {xw} {
	set w .topals
	$w.text tag add sel 1.0 end
}

proc debugwin.undo {w}  { bell }
proc debugwin.cut {xw}   { 
	set w .debugwin
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
		$w.text delete sel.first sel.last
	}
}
proc debugwin.copy {w} { 
	set w .debugwin
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
	}
}
proc debugwin.paste {xw} { 
	set w .debugwin
	catch {$w.text delete sel.first sel.last}
	$w.text insert insert [selection get -displayof $w -selection CLIPBOARD]
	set proenv($w,dirty) true
}
proc debugwin.clear {xw} {
	set w .debugwin
	global array proenv
	catch {$w.text delete sel.first sel.last}
	set proenv($w,dirty) true
}
proc debugwin.select_all {xw} {
	set w .debugwin
	$w.text tag add sel 1.0 end
}

