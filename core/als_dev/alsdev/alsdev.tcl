##=================================================================================
#|				alsdev.tcl
#|		Copyright (c) 1997 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk procedures supporting the top-level Tk-based
#|		ALS Prolog shell
#|
#|
#|	Author: Ken Bowen
#|	Date:	July 1997
#|
#|	This file is sourced from setup_shell_windows/2 which called by
#|	start_shell0/1 in blt_shl.pro.  It should source any other *.tcl
#|	files required by the Tk-GUI ALS Prolog shell.
#|
#|	The main window is:  .topals.txwin.text
#|	This is hard-coded in the following.
##=================================================================================

set argc 0
set argv ""

set CurrentDirectory [pwd]

if {[info exists ALSTCLPATH]==0} then { set ALSTCLPATH . }
puts "LOADING ALSDEV.TCL: ALSTCLPATH=$ALSTCLPATH"

source [file join $ALSTCLPATH alsdev_main.tcl]
source [file join $ALSTCLPATH debugwin.tcl]

Window show .debugger_win
Window hide .debugger_win
Window show .spy_select
Window hide .spy_select

	## source any other needed files here.....

#################################################
#####				MAIN WINDOW				#####
#################################################

	## Bindings for the main window:

proc set_top_bindings { WinPath StreamAlias WaitVar DataVar } {
	bind $WinPath <Return> \
		"xmit_line_plain $WinPath $StreamAlias "
	bind $WinPath <Control-d> \
		"ctl-d_action $WinPath $StreamAlias "
	bind $WinPath <Control-c> \
		"prolog call builtins forceCtlC"
	bind $WinPath <Control-u> \
		"ctl-u_action $WinPath"
	bindtags $WinPath "Text $WinPath .topals all"
}
#		"xmit_line $WinPath $StreamAlias "


	## Variable on which we execute 'tkwait variable ...' when we really
	## have to wait for input (e.g., getting the user's response during 
	## showanswers after a goal was submitted & an answer was displayed
	## In such a case, we rebind <Return> with
	## bind .topals.txwin.text <Return> { xmit_line_plalin .topals.txwin.text  }
	## and that sets this global variable when it transmits a line:

global WaitForLine

proc insert_prompt { TxtWin Prompt } {
	$TxtWin insert end $Prompt
	$TxtWin mark set lastPrompt {insert -1 chars}
	$TxtWin see end
}

	# Called from prolog; sets 'lastPrompt' to mark the point
	# in the text just after the prompt we just printed (etc.);
	# this establishes the starting point of the 'line' (text
	# segment) which will be transmitted after the next <Return>

proc set_prompt_mark { TxtWin } {
	$TxtWin mark set lastPrompt {insert -1 chars}
	$TxtWin see end
}

	# Transmits a 'line' when the user hits <Return> at the
	# top-level:

proc xmit_line { TxtWin StreamAlias } {
	global WaitForLine

	set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -2 chars} ]
	set WaitForLine ''
	prolog call builtins als_exec -atom $ThisLine\n -atom $StreamAlias -atom tcltk
	$TxtWin mark set lastPrompt {insert -1 chars}
}

proc xmit_line0 { TxtWin StreamAlias WaitVar DataVar} {
	global $WaitVar 

	set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -2 chars} ]
#	set WaitForLine ''
#	prolog call builtins als_exec -atom $ThisLine\n -atom $StreamAlias -atom tcltk

	prolog call builtins add_to_stream_buffer -atom $StreamAlias -atom $ThisLine\n
	$TxtWin mark set lastPrompt {insert -1 chars}
	set WaitVar 1
}

proc wait_for_line0 { } {
	global WaitForLine

	while { "$WaitForLine"!=1 } { dooneevent wait }
	set WaitForLine 0
}

	# Transmits a 'line' when the user hits <Return> at points
	# other than the top-level:

proc xmit_line_plain { TxtWin StreamAlias } {
	global WaitForLine

	set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -2 chars} ]
	set WaitForLine 1

#	bind $TxtWin <Return> [list xmit_line $TxtWin $StreamAlias ]
	prolog call builtins add_to_stream_buffer -atom $StreamAlias -atom $ThisLine\n
}

proc ctl-d_action { TxtWin StreamAlias } {
	global WaitForLine

	set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -2 chars} ]
	if { [llength $ThisLine]>0 } then {
		return
	} 
	prolog call sio set_extra_eof -atom $StreamAlias
	set WaitForLine 1
}

proc ctl-u_action { WinPath } {
	set EndIndex [$WinPath index end]
	set LastLineLN [expr [string range $EndIndex 0 [expr [string first "." $EndIndex] - 1]] - 1]
	set PromptIndex [$WinPath index lastPrompt]
	set PromptPtIdx [string first "." $PromptIndex]
	set PromptLN [string range $PromptIndex 0 [expr $PromptPtIdx - 1]]
	if { $PromptLN < $LastLineLN } then {
		$WinPath delete $LastLineLN.0 $LastLineLN.end
	} else {
		set PromptCX [expr 1 + [string range $PromptIndex [expr $PromptPtIdx + 1] end]]
		$WinPath delete $LastLineLN.$PromptCX $LastLineLN.end
	}
}


global LocalClipboard ; set LocalClipboard ""

proc copy_text { TxtWin } {
	global LocalClipboard
	if { [ $TxtWin tag nextrange sel 1.0 end ]!= "" } then {
		set LocalClipboard [ $TxtWin get sel.first sel.last ]
	}
}

proc paste_text { TxtWin } {
	global LocalClipboard
	$TxtWin insert insert $LocalClipboard 
}

proc reconsult { } {
	set file [tk_getOpenFile \
		-defaultextension pro \
		-title "Consult File" \
		-filetypes {{"Prolog Files" {.pro .pl } } {{All Files} {*} } } ]
	if { "$file"== "" } then { return }
	prolog call builtins reconsult -atom $file
	insert_prompt  .topals.txwin.text "\n?-" 
}

proc source_tcl { } {
	set file [tk_getOpenFile \
		-defaultextension tcl \
		-title "Consult File" \
		-filetypes {{"Tcl/Tk Files" {.tcl } } {{All Files} {*} } } ]
	if { "$file"== "" } then { return }
	source $file
	.topals.txwin.text insert end "File $file sourced."
	insert_prompt  .topals.txwin.text "\n?-" 
}

proc set_directory { } {
}

proc exit_prolog { } {
	
	set ans [tk_dialog .quit_dialog "Exit Prolog?" \
		"Really Exit ALS Prolog?" "" 0 Yes No ]
	if {"$ans"==1} then {
		return 0
	} else {
		prolog call builtins halt
	}
}

wm withdraw .topals 
update idletasks
after 500 {wm deiconify .topals}

proc input_item { } {
	global  input_popup_wait
	Window show .input_popup
	tk_wait variable input_popup_wait
	return $input_popup_wait
}

proc mk_labeled_option_button { Label ParentWin Vals TopVal GlblVar } {

	append Frame $ParentWin . $Label _frame
	frame $Frame -borderwidth 1 -relief sunken
	label $Frame.label -text $Label

	set $GlblVar $TopVal
	eval tk_optionMenu $Frame.btn $GlblVar $Vals

	pack $Frame -anchor center -expand 0 -fill x -side top
	pack $Frame.label  -anchor center -expand 0 -fill none -side left
	pack $Frame.btn -anchor center -expand 0 -fill none -padx 7 -side right
}

#################################################
#####				PROJECTS				#####
##                                             ##
##	Project files have extensions: *.ppj       ##
#################################################

proc open_project { } {
	set file [tk_getOpenFile \
		-defaultextension ppj \
		-title "Open Prolog Project" \
		-filetypes {{"Prolog Files" {.ppj} } {{All Files} {*} } } ]
	if { "$file"== "" } then { return }
	prolog call builtins open_project -atom $file
}

proc new_project { } {
	prolog call builtins new_project 
}

proc save_project { } {
	prolog call builtins save_project 
}

proc save_as_project { } {
	set file [tk_getSaveFile \
		-defaultextension ppj \
		-title "Open Prolog Project" \
		-filetypes {{"Prolog Files" {.ppj} } {{All Files} {*} } } ]
	if { "$file"== "" } then { return }
	prolog call builtins save_as_project -atom $file
}

proc close_project { } {
	prolog call builtins close_project 
}

proc add_file_to_project { } {
	set file [tk_getOpenFile \
		-defaultextension pro \
		-title "Open Prolog Project" \
		-filetypes {{"Prolog Files" {.pro .pl } } {{All Files} {*} } } ]
	if { "$file"== "" } then { return }
	prolog call builtins add_file_to_project -atom $file
}

proc delete_file_from_project { } {
	prolog call builtins delete_file_from_project 
}


#################################################
#####				DEBUGGER				#####
#################################################

proc toggle_debugwin {} {
	global debugwin_showing

	if {"$debugwin_showing"==0} then {
		Window hide .debugger_win
		prolog call builtins change_debug_io -atom console
	} else {
		Window show .debugger_win
		raise .debugger_win
		prolog call builtins change_debug_io -atom debugwin
	}
}

proc exit_debugger {} {
	global debugwin_showing

	prolog call debugger exit_debugger
	set debugwin_showing 0
	toggle_debugwin
}

global SpyModule SpyModuleMenu 
global SpyPred SpyPredMenu

proc toggle_spy_win {} {
	global spywin_showing
	global SpyModule SpyModuleMenu SpyPred SpyPredMenu
	
	if {"$spywin_showing"==0} then {
		Window hide .spy_select
	} else {
		prolog call builtins non_sys_modules -var NonSysMods
		destroy .spy_select.modules.mods
		destroy $SpyModuleMenu 
		set SpyModuleMenu [eval "tk_optionMenu .spy_select.modules.mods SpyModule" $NonSysMods]
		set LL [llength $NonSysMods]
		for {set LI 0} {$LI < $LL} {incr LI} {
			$SpyModuleMenu entryconfigure $LI \
				-command "chng_spy_preds_menu [lindex $NonSysMods $LI]"
		}
		pack .spy_select.modules.mods \
			-after .spy_select.modules.module_label \
			-anchor center -expand 1 -fill x -padx 12 -side left
		set SpyModule user

		prolog call builtins module_preds -atom $SpyModule -var PredsList
		destroy .spy_select.predicates.preds
		destroy $SpyPredMenu
		set SpyPred [lindex $PredsList 0]
		set SpyPredMenu [eval "tk_optionMenu .spy_select.predicates.preds SpyPred" $PredsList]
		pack .spy_select.predicates.preds \
		-after .spy_select.predicates.pred_label \
		-anchor center -expand 1 -fill x -padx 12 -side left

		Window show .spy_select
		raise .spy_select
	}
}

proc chng_spy_preds_menu {Mod} {
	global SpyModule SpyPred SpyPredMenu

	prolog call builtins module_preds -atom $SpyModule -var PredsList
	set SpyPred [lindex $PredsList 0]
	destroy .spy_select.predicates.preds
	destroy $SpyPredMenu
	set SpyPredMenu [eval "tk_optionMenu .spy_select.predicates.preds SpyPred" $PredsList]
	pack .spy_select.predicates.preds \
		-after .spy_select.predicates.pred_label \
		-anchor center -expand 1 -fill x -padx 12 -side left
}

proc set_status_debugwin {Port Box Depth} {
    .debugger_win.debug_status.port configure -text $Port
    .debugger_win.debug_status.call_num configure -text $Box
    .debugger_win.debug_status.depth configure -text $Depth
}

proc wait_for_debug_response {} {
	global DebugResponse

	tkwait variable DebugResponse
	return $DebugResponse
}
