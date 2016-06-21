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

switch [tk windowingsystem] {
	aqua  { set mod "Command" ; set elipsis "…"   }
	win32 { set mod "Ctrl"    ; set elipsis "..." }
	x11   { set mod "Ctrl"    ; set elipsis "..." }
}

proc add_default_menus {menubar} {
	if {[tk windowingsystem] == "aqua"} {
		menu $menubar.apple -tearoff 0
		$menubar.apple add command -label "About ALS Prolog…" -command {display_me About .about}
		$menubar add cascade -menu $menubar.apple
	}
}

#$menubar.apple add command -label "About ALS Prolog…" -command {Window show .about}

proc add_file_menu {menubar type window} {
	global mod
	global elipsis
	
	set TearOff 0
	menu $menubar.file -tearoff $TearOff -title File
	
    $menubar.file add command -label New -underline 0 -accelerator "$mod-N" -command {re document.new}
    $menubar.file add command -label "Open$elipsis" -underline 0 -accelerator "$mod-O" -command {re document.open}
    $menubar.file add command -label Close -underline 0 -accelerator "$mod-W" -command "re {$type.close $window}"
    $menubar.file add separator
    $menubar.file add command -label Save -underline 0 -accelerator "$mod-S" -command "re {$type.save $window}"
    $menubar.file add command -label "Save As$elipsis" -underline 5 -command "re {$type.save_as $window}"
    $menubar.file add separator
    $menubar.file add command -label "Examples$elipsis" -underline 5 -command {re document.open_examps}


#    $menubar.file add separator
#    $menubar.file add command -label "Page Setup$elipsis" \
#		-command "re {$type.page_setup $window}" -state disabled
#    $menubar.file add command -label "Print$elipsis" -accelerator "$mod-P"\
#		-command "re {$type.print $window}" -state disabled

#	if {$type == "listener"} then { 
#    	$menubar.file add separator
#    	$menubar.file add separator
#	}

    $menubar.file add separator
	
	switch [tk windowingsystem] {
		aqua  {}
		win32 { $menubar.file add command -label "Exit" -underline 1 -accelerator "$mod-Q" -command {re exit_prolog} }
		x11   { $menubar.file add command -label "Quit"              -accelerator "$mod-Q" -command {re exit_prolog} }
	}

	$menubar add cascade -menu $menubar.file -label "File" -underline 0
}

proc add_edit_menu {menubar type window} {
	global mod
	global elipsis
	global proenv

	set TearOff 0
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
        -label Delete -underline 2 -command "re {$type.delete $window}"
    $menubar.edit add separator
    $menubar.edit add command \
        -label {Select All} -underline 8 -accelerator "$mod-A" -command "re {$type.select_all $window}"
    $menubar.edit add separator
    $menubar.edit add command \
        -label "Find$elipsis" -underline 0 -accelerator "$mod-F" -command "re {$type.find $window}"
    $menubar.edit add separator
    $menubar.edit add command \
		-label "Preferences$elipsis" -underline 3 -command "re {fonts_and_colors $window}"

	$menubar add cascade -menu $menubar.edit -label "Edit" -underline 0
}

proc add_prolog_menu {menubar type window} {
	global proenv
	global mod
	global elipsis
	global proenv

	set TearOff 0
	menu $menubar.prolog -tearoff $TearOff -title Prolog

    $menubar.prolog add command -label "Consult$elipsis" -underline 0 -accelerator "$mod-K" -command "re {$type.consult $window}"
	$menubar.prolog add command \
		-label "Clear Workspace" -underline 2 -command {re clear_workspace}

	if {$type == "listener"} then { 
    	$menubar.prolog add separator
    	$menubar.prolog add command \
        	-label "Load Project$elipsis" -underline 0 -command {re load_project} -state $proenv(edition)
    	$menubar.prolog add command \
        	-label "Open Project$elipsis" -underline 0 -command {re open_project} -state $proenv(edition)
    	$menubar.prolog add command \
        	-label "Close Project" -underline 0 -command {re close_project} -state $proenv(edition)
    	$menubar.prolog add command \
        	-label "New Project" -underline 0 -command {re new_project} -state $proenv(edition)

    	$menubar.prolog add separator
    	$menubar.prolog add command \
        	-label "Set Directory$elipsis" -underline 0 -command {re set_directory} 

    	$menubar.prolog add separator
		$menubar.prolog add command \
			-label "IDE Settings$elipsis" -underline 0 -command {re "display_me {IDE Settings} .ide_settings"}
		$menubar.prolog add command \
			-label "Dynamic Flags$elipsis" -underline 0 -command {re "display_me {Dynamic Flags} .dyn_flags"}
		$menubar.prolog add command \
			-label "Static Flags$elipsis" -underline 1 -command {re "display_me {Static Flags} .static_flags"}

	} elseif {$type == "debugwin"} then {
    	$menubar.prolog add separator
    	$menubar.prolog add command -label {Abort} -underline 0 \
        	-command {re { set DebugResponse Ba }}
    	$menubar.prolog add command -label {Break Shell} -underline 0 \
			-command {re { set DebugResponse Bb }}
	}
	$menubar add cascade -label "Prolog" -underline 0 -menu $menubar.prolog
}

proc add_tools_menu {menubar type window} {
	global mod
	global elipsis
	global proenv

	if {$type == "document"} then { return }	

	set TearOff 0
	menu $menubar.tools -tearoff $TearOff -title Tools

	if {$type == "listener"} then {

#		$menubar.tools add checkbutton \
#			-label Debugger -underline 0 -command exec_toggle_debugwin -variable proenv(debugwin)

		$menubar.tools add command \
			-label Debugger -underline 0 -command ensure_db_showing 

		$menubar.tools add separator 
    	$menubar.tools add command -label "Visual Tcl" -underline 0 \
			-command {re { prolog call alsdev start_visual_tcl } }
    	$menubar.tools add command -label "Source Tcl$elipsis" -underline 0 -command {re source_tcl} -state $proenv(edition)
    	$menubar.tools add command -label "Tcl Debugger$elipsis" -underline 0 -command {re tcl_debugger} -state disabled
    	$menubar.tools add command -label "Kill Tcl Interps" -underline 0 -command {re kill_tcl_interps} -state $proenv(edition)
#    	$menubar.tools add command -label "Tcl Shell$elipsis" -underline 0 -command {re tcl_shell} 

#		$menubar.tools add separator 
		## Cref
#    	$menubar.tools add command -label "Cref$elipsis" -underline 0 \
#			-command {re {prolog call cref0 start_cref}} 
#		$menubar.tools add separator 
		## Application GUI Generator
#    	$menubar.tools add command -label "Application Gui Generator$elipsis" -underline 0 \
#			-command {re do_app_gui_gen_dialog } 
#    	$menubar.tools add command -label "Setup New Application Framework$elipsis" -underline 0 \
#			-command {re do_new_app_top} 
	} else {
	##  must be debugger:
		# Spy
		$menubar.tools add command  -label "Spy$elipsis" \
			-underline 0 -command {re show_pred_info } 
		$menubar.tools add separator
		$menubar.tools add command  -label "Clear Debugger Win" \
                        -underline 0 -command {re clear_debug_win}
		$menubar.tools add command  -label "Debug Settings$elipsis" \
			-underline 0 -command {re show_debug_settings}
		$menubar.tools add command  -label "Statistics" \
			-underline 0 -command {re {set DebugResponse Bi}}
		$menubar.tools add command  -label "System Modules$elipsis" \
			-underline 0 -command {re {set_system_modules_showing}} -state disabled
	}
	$menubar add cascade -label "Tools" -menu $menubar.tools -underline 0
}

proc add_windows_menu {menubar type window} {
	global mod
	global elipsis
	global proenv

#	if {$type == "document"} then { return }	

	set TearOff 0
#	menu $menubar.windows -tearoff $TearOff -title Windows

#	$menubar add cascade -label "Windows" -menu $menubar.windows -underline 0
	$menubar add cascade -label "Windows" -menu .topals.mmenb.windows -underline 0
}

proc add_help_menu {menubar} {
	global mod
	global elipsis

	if {[tk windowingsystem] != "aqua"} {
		menu $menubar.help -tearoff 0
		$menubar.help add command -label "About ALS Prolog$elipsis" \
			-underline 0 -command {display_me About .about}
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
	global proenv
	
	set w .topals
	$w.text insert end [selection get -displayof $w -selection CLIPBOARD]
	set proenv($w,dirty) true
	$w.text see end
	$w.text mark set insert end
	focus $w.text
}

proc listener.delete {xw} {
	set w .topals
	global array proenv
	catch {$w.text delete sel.first sel.last}
	set proenv($w,dirty) true
}

proc listener.select_all {xw} {
	set w .topals
	$w.text tag add sel 1.0 end
}


proc listener.copy_paste { xw } {
	global proenv
	set w .topals

	if  {[tk windowingsystem] == "x11"} {
		set WhichSel PRIMARY
	} else {
		set WhichSel CLIPBOARD
	}
	set selflag [catch "selection get -displayof $w -selection $WhichSel" data]
	if {$selflag == "0" } then {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
		$w.text insert end [selection get -displayof $w -selection $WhichSel]
		set proenv($w,dirty) true
		$w.text see end
		$w.text mark set insert end
		focus $w.text
	}
}

proc listener.find {xw} {
	start_edit_find .topals 
}

proc debugwin.find {xw} {
	start_edit_find .debugwin
}

proc debugwin.delete {xw} {
	set w .debugwin
	global array proenv
	catch {$w.text delete sel.first sel.last}
	set proenv($w,dirty) true
}
proc debugwin.select_all {xw} {
	set w .debugwin
	$w.text tag add sel 1.0 end
}
proc clear_debug_win {} {
	set w .debugwin
	$w.text tag add sel 1.0 end
	global array proenv
	catch {$w.text delete sel.first sel.last}
	set proenv($w,dirty) true
}


proc debugwin.copy {w} { 
	set w .debugwin
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
	}
}
proc debugwin.undo {w}  { bell }
proc debugwin.cut {xw}   { bell }
proc debugwin.paste {xw} { bell }

