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
	set elipsis "�"
} else {
	set mod "Ctrl"
	set elipsis "..."
}

proc add_default_menus {menubar} {
	global tcl_platform

	if {$tcl_platform(platform) == "macintosh"} {
		menu $menubar.apple -tearoff 0
		$menubar.apple add command -label "About ALS Prolog�" -command {Window show .about ; raise .about}
		$menubar add cascade -menu $menubar.apple
	}
}

proc add_file_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	
	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.file -tearoff $TearOff -title File
	
    $menubar.file add command -label New -underline 0 -accelerator "$mod-N" -command {re document.new}
    $menubar.file add command -label "Open$elipsis" -underline 0 -accelerator "$mod-O" -command {re document.open}
    $menubar.file add command -label Close -underline 0 -accelerator "$mod-W" -command "re {$type.close $window}"
    $menubar.file add separator
    $menubar.file add command -label Save -underline 0 -accelerator "$mod-S" -command "re {$type.save $window}"
    $menubar.file add command -label "Save As$elipsis" -underline 5 -command "re {$type.save_as $window}"

#    $menubar.file add separator
#    $menubar.file add command -label "Page Setup$elipsis" \
#		-command "re {$type.page_setup $window}" -state disabled
#    $menubar.file add command -label "Print$elipsis" -accelerator "$mod-P"\
#		-command "re {$type.print $window}" -state disabled

    $menubar.file add separator
    
	if {$tcl_platform(platform) == "windows"} {
    	$menubar.file add command -label "Exit" -underline 1 -accelerator "$mod-Q" -command {re exit_prolog}
    } else {
    	$menubar.file add command -label "Quit" -accelerator "$mod-Q" -command {re exit_prolog}
	}

	$menubar add cascade -menu $menubar.file -label "File" -underline 0
}

proc add_edit_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	global proenv

	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.edit -tearoff $TearOff -title Edit
	
    $menubar.edit add command \
        -label Undo -underline 0 -accelerator "$mod-Z" -command "re {$type.undo $window}" -state disabled
    $menubar.edit add separator
    $menubar.edit add command -label Cut -underline 2 \
		-accelerator "$mod-X" -command "re {$type.cut $window}"
    $menubar.edit add command \
        -label Copy -underline 0 -accelerator "$mod-C" -command "re {$type.copy $window}"
    $menubar.edit add command \
        -label Paste -underline 0 -accelerator "$mod-V" -command "re {$type.paste $window}"
    $menubar.edit add command \
        -label Clear -underline 2 -command "re {$type.clear $window}"
    $menubar.edit add separator
    $menubar.edit add command \
        -label {Select All} -underline 8 -accelerator "$mod-A" -command "re {$type.select_all $window}"
    $menubar.edit add separator
    $menubar.edit add command \
		-label "Preferences$elipsis" -underline 3 -command "re {fonts_and_colors $window}"

	$menubar add cascade -menu $menubar.edit -label "Edit" -underline 0
}

proc add_prolog_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis

	if {"$type"=="listener"} then { set TearOff 1 } else { set TearOff 0 }
	menu $menubar.prolog -tearoff $TearOff -title Prolog

    $menubar.prolog add command -label "Consult" -underline 0 -accelerator "$mod-K" -command "re {$type.consult $window}"
	$menubar.prolog add command \
		-label "Clear Workspace" -underline 2 -command {re clear_workspace}

	if {"$type"=="listener"} then { 
    	$menubar.prolog add separator
    	$menubar.prolog add command \
        	-label "Set Directory$elipsis" -underline 0 -command {re set_directory} 

    	$menubar.prolog add separator
		$menubar.prolog add command \
			-label "Dynamic Flags" -underline 0 -command {re show_dynamic_flags}
		$menubar.prolog add command \
			-label "Static Flags" -underline 1 -command {re show_static_flags}
	} elseif {"$type"=="debugwin"} then {
    	$menubar.prolog add separator
    	$menubar.prolog add command -label {Abort} -underline 0 \
        	-command {re { set DebugResponse Ba }}
    	$menubar.prolog add command -label {Break} -underline 0 \
			-command {re { set DebugResponse Bb }}
	}
	$menubar add cascade -label "Prolog" -underline 0 -menu $menubar.prolog
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
			-label Debugger -underline 0 -command exec_toggle_debugwin -variable proenv(debugwin)
    	$menubar.tools add command -label "Source Tcl$elipsis" -underline 0 -command {re source_tcl} 
#    	$menubar.tools add command -label "Tcl Shell" -underline 0 -command {re tcl_shell} 

		$menubar.tools add separator 
		## DefStructs:
		menu $menubar.tools.defstr -relief raised -tearoff 0
		$menubar.tools add cascade \
			-label {Structs} -underline 1 -menu $menubar.tools.defstr
		$menubar.tools.defstr add command \
			-label "Define New" -underline 0 -command {re new_defstruct}
#		$menubar.tools.defstr add command \
#			-label "Edit" -underline 0 -command {re edit_defstruct} 
	} else {

		# Spy
		$menubar.tools add command  -label "Spy$elipsis" \
			-underline 0 -command {re toggle_spywin} 
#		$menubar.tools add checkbutton  -label {Spy$elipsis} \
#			-command exec_toggle_spywin -variable proenv(spywin)
		$menubar.tools add command  -label {NoSpy all } \
			-underline 0 -command {re {prolog call debugger nospy }} 

#		$menubar.tools add command  -label {Spy When} -underline 4 -state disabled

		$menubar.tools add separator
		$menubar.tools add command  -label {Debug Settings } \
			-underline 0 -command {re {show_debug_settings}}
	}
	$menubar add cascade -label "Tools" -menu $menubar.tools -underline 0
}

proc add_help_menu {menubar} {
	global tcl_platform
	global mod
	global elipsis

	if {$tcl_platform(platform) != "macintosh"} {
		menu $menubar.help
		
		$menubar.help add command -label "About ALS Prolog$elipsis" \
			-underline 0 -command {Window show .about ; raise .about}
		
		$menubar add cascade -label "Help" -underline 0 -menu $menubar.help
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

