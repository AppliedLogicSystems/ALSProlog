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

global array proenv

#
#	proenv(...)					Usage
#	------------			-------------------------
#	proenv(defstr_ld)		Defstruct loaded/not [true/false]
#	proenv(cwd)				Current Working Directory
#	proenv(debugger_ld)		Debugger loaded/not [true/false]
#	proenv(debugwin)		Debug Win showing/not (1/0) 
#	proenv(spywin)			Spypoint Win showing/not (1/0) 



set proenv(cwd) 		[pwd]
set proenv(debugger_ld)	false
set	proenv(debugwin)	0
set	proenv(spywin)		0

set proenv(defstr_ld)	false


if {[info exists ALSTCLPATH]==0} then { set ALSTCLPATH . }
#puts "LOADING ALSDEV.TCL: ALSTCLPATH=$ALSTCLPATH"

source [file join $ALSTCLPATH alsdev_main.tcl]
source [file join $ALSTCLPATH debugwin.tcl]
source [file join $ALSTCLPATH defstr.tcl]

Window show .debugwin
Window hide .debugwin
Window show .spywin
Window hide .spywin
#Window show .dyn_flags
#Window hide .dyn_flags

#Window show .
Window show .topals
update idletasks
raise .topals

#wm withdraw .topals 
#after 500 {wm deiconify .topals}

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
		"ctl-c_action_during_read $WinPath $StreamAlias $WaitVar"
	bind $WinPath <Control-u> \
		"ctl-u_action $WinPath"
	bindtags $WinPath "Text $WinPath .topals all"

	bind Text <ButtonRelease-2> {} 
	bind $WinPath <ButtonRelease-2> [list copy_paste_text $WinPath]
}

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
	prolog call builtins add_to_stream_buffer -atom $StreamAlias -atom $ThisLine\n
	$TxtWin mark set lastPrompt {insert -1 chars}
	set WaitVar 1
}

proc wait_for_line0 { } {
	global WaitForLine

	while { "$WaitForLine"==0 } { dooneevent wait }
	set ReturnValue $WaitForLine
	set WaitForLine 0
	return $ReturnValue
}

	# Transmits a 'line' when the user hits <Return> at points
	# other than the top-level:

proc xmit_line_plain { TxtWin StreamAlias } {
	global WaitForLine

	set InsertIndex [$TxtWin index insert]
	set InsertLine [string range $InsertIndex 0 [expr [string first "." $InsertIndex] - 1 ]]
	incr InsertLine
	set EndIndex [$TxtWin index end]
	set EndLine [string range $EndIndex 0 [expr [string first "." $EndIndex] - 1 ]]

	if { $EndLine == $InsertLine } then {
		set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -1 chars} ]
		set WaitForLine 1
		prolog call builtins add_to_stream_buffer -atom $StreamAlias -atom $ThisLine\n
	} 
	$TxtWin see end
	$TxtWin mark set insert end
}

proc ctl-c_action_during_read { WinPath StreamAlias WaitVar } {
	global WaitForLine

	set WaitForLine -1
}

global WakeUpLookAround; set WakeUpLookAround 0

proc wake_up_look_around { } {
	global WakeUpLookAround

	set CountDown 100
	set NEvnt [dooneevent dont_wait]
	while {$CountDown>0} {
		if {$WakeUpLookAround!=0} then {
			set SaveIt $WakeUpLookAround
			set WakeUpLookAround 0
			return $SaveIt
		}
		if {$NEvnt==0} then {
			set SaveIt $WakeUpLookAround
			set WakeUpLookAround 0
			return $WakeUpLookAround
		}
		set NEvnt [dooneevent dont_wait]
		incr CountDown -1
	}
	return $WakeUpLookAround
}

proc interrupt_action {} {
	global WakeUpLookAround
	set WakeUpLookAround -1
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
	$TxtWin insert end $LocalClipboard 
}

proc copy_paste_text { TxtWin } {
	if { [ $TxtWin tag nextrange sel 1.0 end ]!= "" } then {
		$TxtWin insert end [ $TxtWin get sel.first sel.last ]
		$TxtWin see end
		$TxtWin mark set insert end
	}
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
	set CWD [pwd]
	set NewDir [tkFDialog]
	if { "$NewDir" !="" } {
		cd $NewDir
		.topals.cpd19.02 delete 0 end
		.topals.cpd19.02 insert end $NewDir
	}
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
#####			Utilities				       ##
#################################################

proc iconify_me {Win} {
	wm iconify $Win
}
	 
proc hide_me {Win} {
	 Window hide $Win
}

#################################################
#####			Prolog Flags				   ##
##                                             ##
##			Dynamic Prolog Flags			   ##
#################################################

proc show_dynamic_flags {} {
	global array proenv

	if {[winfo exists .dyn_flags]} then {
		Window show .dyn_flags
	} else {
		Window show .dyn_flags
		prolog call builtins changable_flags_info -var InfoList
		foreach info $InfoList {
			create_dyn_flag_entry $info
		}
	}
}

proc create_dyn_flag_entry { info } {
	global array proenv

	set FlagName [lindex $info 0]
	set PosVals [lindex $info 1]
	set CurVal [lindex $info 2]

	set ff [frame .dyn_flags.$FlagName -borderwidth 1 -relief sunken]
	label $ff.label \
        -borderwidth 0 -font {lucida 10 bold} \
        -relief flat -width 18 -justify right \
        -text $FlagName

	set Cmd [concat tk_optionMenu "$ff.opts_menu" proenv($FlagName) $PosVals]
	set proenv($FlagName) $CurVal  
	set MM [eval $Cmd]

	pack $ff  \
        -anchor center -expand 0 -fill x -side top 
	pack $ff.label  \
        -anchor center -expand 0 -fill none -side left 
	pack $ff.opts_menu  \
        -anchor center -expand 0 -fill x -side left 

	set Last [$MM index end]
	for {set ii 0} {$ii <= $Last} {incr ii} {
		set Cmd [list prolog call builtins \
			set_prolog_flag -atom $FlagName -atom [lindex $PosVals $ii] ]
		$MM entryconfigure $ii -command $Cmd
	}

}

proc change_prolog_flags {} {
	global array proenv

	prolog call builtins changable_flags_info -var InfoList
	foreach info $InfoList {
		set FlagName [lindex $info 0]
		if {[lindex $info 2]!=$proenv($FlagName)} then {
			prolog call builtins set_prolog_flag \
				-atom $FlagName -atom $proenv($FlagName)
		}
	}
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
	global array proenv

	set proenv(debugwin) [expr 1 - $proenv(debugwin)]
	exec_toggle_debugwin
}

proc exec_toggle_debugwin {} {
	global array proenv

	if {"$proenv(debugwin)"==0} then {
		hide_debugwin
	} else {
		show_debugwin
	}
}

proc ensure_db_showing {} {
	global array proenv
		
	.debugwin.textwin.text delete 1.0 end
	show_debugwin
}

proc show_debugwin {} {
	global array proenv
		
	Window show .debugwin
	raise .debugwin
	prolog call builtins change_debug_io -atom debugwin
	check_leashing
	set proenv(debugwin) 1

#	prolog call xconsult change_source_level_debugging -atom on

}

proc hide_debugwin {} {
	global array proenv

	hide_spywin
	Window hide .debugwin
	set proenv(debugwin) 0

#	prolog call xconsult change_source_level_debugging -atom off
}

proc exit_debugger {} {
	global array proenv

	prolog call debugger exit_debugger
	hide_spywin
	hide_debugwin
}

		###############################
		######### SPYPOINTS
		###############################
global SpyModule SpyModuleMenu 
global SpyPred SpyPredMenu

proc toggle_spywin {} {
	global array proenv

	set proenv(spywin) [expr 1 - $proenv(spywin)]
	exec_toggle_spywin
}

proc hide_spywin {} {
	global array proenv

	set proenv(spywin) 0
	Window hide .spywin
}

proc exec_toggle_spywin {} {
	global array proenv
	global SpyModule SpyModuleMenu SpyPred SpyPredMenu
	
	if {"$proenv(spywin)"==0} then {
		Window hide .spywin
	} else {
		prolog call builtins non_sys_modules -var NonSysMods
		destroy .spywin.modules.mods
		destroy $SpyModuleMenu 
		set SpyModuleMenu [eval "tk_optionMenu .spywin.modules.mods SpyModule" $NonSysMods]
		set LL [llength $NonSysMods]
		for {set LI 0} {$LI < $LL} {incr LI} {
			$SpyModuleMenu entryconfigure $LI \
				-command "chng_spy_preds_menu [lindex $NonSysMods $LI]"
		}
		pack .spywin.modules.mods \
			-after .spywin.modules.module_label \
			-anchor center -expand 1 -fill x -padx 12 -side left
		set SpyModule user

		prolog call builtins module_preds -atom $SpyModule -var PredsList
		destroy .spywin.predicates.preds
		destroy $SpyPredMenu
		set SpyPred [lindex $PredsList 0]
		set SpyPredMenu [eval "tk_optionMenu .spywin.predicates.preds SpyPred" $PredsList]
		pack .spywin.predicates.preds \
		-after .spywin.predicates.pred_label \
		-anchor center -expand 1 -fill x -padx 12 -side left

		Window show .spywin
		raise .spywin
	}
}

proc chng_spy_preds_menu {Mod} {
	global SpyModule SpyPred SpyPredMenu

	prolog call builtins module_preds -atom $SpyModule -var PredsList
	set SpyPred [lindex $PredsList 0]
	destroy .spywin.predicates.preds
	destroy $SpyPredMenu
	set SpyPredMenu [eval "tk_optionMenu .spywin.predicates.preds SpyPred" $PredsList]
	pack .spywin.predicates.preds \
		-after .spywin.predicates.pred_label \
		-anchor center -expand 1 -fill x -padx 12 -side left
}

proc spy_point {Action} {
	global SpyModule SpyPred 

	switch $Action {
	cancel { toggle_spywin }
	ok {
		prolog call debugger gui_spy -atom $SpyModule -atom $SpyPred
		toggle_spywin
	}
	}
}

proc check_leashing {} {
	global array proenv

	prolog call debugger check_leashing -var Call -var Exit -var Redo -var Fail
	set proenv(leash,call) $Call
	set proenv(leash,exit) $Exit
	set proenv(leash,redo) $Redo
	set proenv(leash,fail) $Fail

}

#proc show_ll {} {
#	global array proenv
#
#puts "call=$proenv(leash,call) exit=$proenv(leash,exit) redo=$proenv(leash,redo) fail=$proenv(leash,fail)"
#}

proc exec_toggle_leash {Which} {
	global array proenv

	prolog call debugger exec_toggle_leash -atom $Which
}
		###############################
		######### TRACING,ETC.
		###############################

proc set_status_debugwin {Port Box Depth} {
    .debugwin.debug_status.port configure -text $Port
    .debugwin.debug_status.call_num configure -text $Box
    .debugwin.debug_status.depth configure -text $Depth
}

proc wait_for_debug_response {} {
	global DebugResponse

	tkwait variable DebugResponse
	return $DebugResponse
}

		###############################
		######### SOURCE TRACE
		###############################

proc win_exists {WinName} {
	if {[winfo exists $WinName]==1} then {
		return true
	} else {
		return false
	}
}
proc reset_st_win {WinName} {
	clear_tag head_tag $WinName.textwin.text
	$WinName.textwin.text tag delete head_tag
	clear_tag call_tag $WinName.textwin.text
	$WinName.textwin.text tag delete call_tag
	wm deiconify $WinName
	raise $WinName
}

proc load_file_to_win {FileName TextWinName} {
	set SI [open $FileName r]
	set NumRows [load_stream_to_win $SI $TextWinName]
	close $SI
	return $NumRows

}

proc load_stream_to_win {Stream TextWinName} {
	set LineCount 1
	set LineCsList ""
	while {  [eof $Stream]==0 } {
		set NumCs [gets $Stream Line]
		if { $NumCs >= 0 } then {
			$TextWinName insert end $Line
			$TextWinName insert end "\n"
			lappend LineCsList $NumCs
			incr LineCount
		}
	}
	return [list [expr $LineCount - 1] $LineCsList]
}

proc assign_tag {TextWinName TagName StartLine StartChar EndLine EndChar} {
	$TextWinName tag add $TagName $StartLine.$StartChar $EndLine.$EndChar
}

proc clear_tag { TagName TextWinName } {
	$TextWinName tag configure $TagName -background [$TextWinName cget -background]
}

proc see_text {TextWin StartLine StartChar EndLine EndChar} {
	$TextWin see $StartLine.$StartChar
	$TextWin see $EndLine.$EndChar
}

