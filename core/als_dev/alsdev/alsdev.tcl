##=================================================================================
#|				alsdev.tcl
#|		Copyright (c) 1997-98 Applied Logic Systems, Inc.
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
#|	The main window is:  .topals.text
#|	This is hard-coded in the following.
##=================================================================================

set argc 0
set argv ""

if {[info exists ALSTCLPATH]==0} then { set ALSTCLPATH . }
#puts "LOADING ALSDEV.TCL: ALSTCLPATH=$ALSTCLPATH"

#################################
# GLOBAL VARIABLES
#--------------------------------
global widget; 
global array proenv

#---------------------------------------------------------------
#	proenv(...)					Usage
#	------------			-------------------------
#	proenv(cwd)				Current Working Directory
#	proenv(debugger_ld)		Debugger loaded/not [true/false]
#	proenv(debugwin)		Debug Win showing/not (1/0) 
#	proenv(spywin)			Spypoint Win showing/not (1/0) 
#	proenv(defstr_ld)		Defstruct loaded/not [true/false]

set proenv(cwd) 				[pwd]
set proenv(debugger_ld)			false
set	proenv(debugwin)			0
set	proenv(debugwin,visible)	{}
set	proenv(spywin)				0
set proenv(defstr_ld)			false

	## window appearance stuff - initial defaults:

set proenv(debugwin_button,background)	#cee8e6
set proenv(interrupt_button,foreground)	#ff0000
set proenv(.topals,background)       #d9d9d9
set proenv(.topals,font) {user 10 normal}
set proenv(debugwin,font)   {user 10 normal}
set proenv(edit,font)       {user 10 normal}

set	proenv(edit,visible)		{}

#---------------------------------------------------------------

#################################
# VTcl Top PROCEDURES
#--------------------------------
proc main {argc argv} {
}

proc Window {args} {
	global array proenv
global vTcl

    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set rest [lrange $args 2 end]
    if {$name == "" || $cmd == ""} {return}
    set exists [winfo exists $name]
    switch $cmd {
        show { eval "vTclWindow$name $name" ; raise $name }
        hide    { if $exists {wm withdraw $name; return} }
        iconify { if $exists {wm iconify $name; return} }
        destroy { if $exists {destroy $name; return} }
    }
}

proc vTclWindow. {args} {
    set base .
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel . passive
    wm geometry . 200x200+0+0
    wm maxsize . 1265 994
    wm minsize . 1 1
    wm overrideredirect . 0
    wm resizable . 1 1
    wm withdraw .
    wm title . "vt.tcl"
    ###################
    # SETTING GEOMETRY
    ###################
}

#################################
# 		INITIAL SETUP
#--------------------------------

switch $tcl_platform(platform) {
	unix {
		set MainFont system
	}
	windows {
		set MainFont system
	}
	macintosh {
		set MainFont application
	}
	default {
		set MainFont system
	}
}

proc grab_defaults {Win} {
	global array proenv
	set CFGDF [$Win configure]
	set SEEKTAGS {-background -foreground -selectbackground -selectforeground -font -tabs}
	foreach Item $CFGDF {
		if "[lsearch -exact $SEEKTAGS [lindex $Item 0]] > -1" then {
			lappend SEEKRESULTS [list [string range [lindex $Item 0] 1 end] [lindex $Item 3]]
		}
	}
	return $SEEKRESULTS
}

proc establish_defaults {} {
    text .tmp_test_text -height 2 -width 2 
	set Window .tmp_test_text
	set Background [$Window cget -background ]
	set Foreground [$Window cget -foreground ]
	set SelectBackground [$Window cget -selectbackground ]
	set SelectForeground [$Window cget -selectforeground ]
	set Font [$Window cget -font ]
	set Tabs [$Window cget -tabs]
	destroy .tmp_test_text

	set Vals [list $Background $Foreground $SelectBackground $SelectForeground $Font $Tabs]
	prolog call alsdev alsdev_default_setup -list $Vals
}

proc set_proenv {Left Right Value} {
	global array proenv
	set proenv($Left,$Right) $Value
}

proc return_proenv_defaults {} {
	global array proenv
	lappend Defs \
		$proenv(.topals,background) \
		$proenv(.topals,foreground) \
		$proenv(.topals,selectbackground) \
		$proenv(.topals,selectforeground) \
		$proenv(.topals,font) \
		$proenv(.topals,tabs) \

	return $Defs
}

establish_defaults

proc unmap_alsdev_main {} {
	global array proenv

	Window hide .dyn_flags
	Window hide .alsdev_settings
	Window hide .about
	Window hide .break_choices

	unmap_alsdev_debug
}
proc map_alsdev_main {} {
	global array proenv

	map_alsdev_debug
}

proc unmap_alsdev_debug {} {
	global array proenv
	if {[winfo exists .debugwin]} then {
		if {"$proenv(debugwin)"==1} then { hide_debugwin }
	}
}
proc map_alsdev_debug {} {
	global array proenv
	if {[winfo exists .debugwin]} then {
		if {"$proenv(debugwin)"==0} then { show_debugwin }
	} 
}


if {$tcl_platform(platform) == "macintosh"} {
	source -rsrc {alsdev_main}
	source -rsrc {als_settings}
	source -rsrc {debugwin}
	source -rsrc {defstr}
	source -rsrc {als_menu}
	source -rsrc {als_document}
} else {
	source [file join $ALSTCLPATH alsdev_main.tcl]
	source [file join $ALSTCLPATH als_settings.tcl]
	source [file join $ALSTCLPATH debugwin.tcl]
	source [file join $ALSTCLPATH defstr.tcl]
	source [file join $ALSTCLPATH als_menu.tcl]
	source [file join $ALSTCLPATH als_document.tcl]
}

	## source any other needed files here.....

#################################################
#####										#####
#################################################

lappend auto_path /usr/local/lib/jstools

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
	## bind .topals.text <Return> { xmit_line_plalin .topals.text  }
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

proc copy_text { TxtWin } {
	if { [ $TxtWin tag nextrange sel 1.0 end ]!= "" } then {
		clipboard clear
		clipboard append [ $TxtWin get sel.first sel.last ]
	}
}

proc paste_text { TxtWin } {
	global tcl_platform

	if {"$tcl_platform(platform)" == "windows"} {
		$TxtWin insert end [ selection get -selection CLIPBOARD ]
	} else {
		$TxtWin insert end [ selection get ]
	}
	$TxtWin see end
	$TxtWin mark set insert end
	focus $TxtWin
}

proc copy_paste_text { TxtWin } {
	global tcl_platform

	if {"$tcl_platform(platform)" == "windows"} {
		$TxtWin insert end [ selection get -selection CLIPBOARD ]
	} else {
		$TxtWin insert end [ selection get ]
	}
	$TxtWin see end
	$TxtWin mark set insert end
	focus $TxtWin
}

proc source_tcl { } {
	set file [tk_getOpenFile \
		-defaultextension tcl \
		-title "Consult File" \
		-filetypes {{"Tcl/Tk Files" {.tcl } } {{All Files} {*} } } ]
	if { "$file"== "" } then { return }
	source $file
	.topals.text insert end "File $file sourced."
	insert_prompt  .topals.text "\n?-" 
}

proc set_directory { } {
	set CWD [pwd]
	set NewDir [tkFDialog]
	if { "$NewDir" !="" } {
		cd $NewDir
		show_dir_on_main $NewDir
	}
}

proc show_dir_on_main { Dir } {
	.topals.cpd19.02 configure -text $Dir -anchor w
}

proc exit_prolog { } {
	
	set ans [tk_dialog .quit_dialog "Exit Prolog?" \
		"Really Exit ALS Prolog?" "" 0 Yes No ]
	if {"$ans"==1} then {
		return 0
	} else {
		destroy .
		exit
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

proc listener.consult { Window } {
	consult_file
}
proc debugwin.consult { Window } {
	consult_file
}

proc consult_file {} {
	set file [tk_getOpenFile \
		-defaultextension pro \
		-title "Consult File" \
		-filetypes {{"Prolog Files" {.pro .pl } } {{All Files} {*} } } ]
	if { "$file"== "" } then { return }
	prolog call alsdev do_reconsult -atom $file
#	insert_prompt  .topals.text "\n?-" 
}

proc clear_workspace { } {
	prolog call alsdev clear_workspace 
#	insert_prompt  .topals.text "\n?-" 
}

#################################################
#####	Utilities & Environment Settings       ##
#################################################

proc iconify_me {Win} {
	wm iconify $Win
}
	 
proc careful_withdraw {Win} {
	if "[winfo exists $Win]>1" then { wm withdraw $Win }
}

proc choose_background_color {Window} {
	global array proenv

	set COLOR [tk_chooseColor \
		-title "Choose Background Color" -initialcolor $proenv(.topals,background)]
	if {"$COLOR" == ""} then {return}
	.alsdev_settings.background configure -background $COLOR
	$Window.text configure -background $COLOR
}

proc choose_foreground_color {Window} {
	global array proenv

	set COLOR [tk_chooseColor \
		-title "Choose Foreground Color" -initialcolor $proenv(.topals,foreground)]
	if {"$COLOR" == ""} then {return}
	.alsdev_settings.foreground configure -foreground $COLOR
	$Window.text configure -foreground $COLOR
}

proc font_family_choice { Family Window } {
	global array proenv

	set PrevFont [$Window.text cget -font]
	set NewFont [list $Family [lindex $PrevFont 1] [lindex $PrevFont 2]]
	$Window.text configure -font $NewFont
}

proc font_size_choice { Size Window } {
	global array proenv

	set PrevFont [$Window.text cget -font]
	set NewFont [list [lindex $PrevFont 0] $Size [lindex $PrevFont 2]]
	$Window.text configure -font $NewFont
}

proc font_style_choice { Style Window } {
	global array proenv

	set PrevFont [$Window.text cget -font]
	set NewFont [list [lindex $PrevFont 0] [lindex $PrevFont 1] $Style ]
	$Window.text configure -font $NewFont
}

proc text_front_win {} {
	set Kids [winfo children .]
	set FrontWin [lindex $Kids end]
	if {"$FrontWin"==".alsdev_settings"} then {
		set FrontWin \
			[lindex $Kids [expr [llength $Kids] - 2]]
	}
	if {"$FrontWin"=="" } then {
		bell ; return ""
	} else {
		return $FrontWin
	}
}

proc save_alsdev_settings {} {
	global array proenv
	set FrontWin [text_front_win]
	if {[string range $FrontWin 1 4]=="edit"} then {
		set Grp edit
	} else {
		set Grp $FrontWin
	}
	set TgtTextWin $FrontWin.text

	set proenv($Grp,background)         [lindex $Vals 0]
	set proenv($Grp,foreground)         [lindex $Vals 1]
	set proenv($Grp,selectbackground)   [lindex $Vals 2]
	set proenv($Grp,selectbackground)   [lindex $Vals 3]
	set proenv($Grp,font)               [lindex $Vals 4]
	set proenv($Grp,tabs)               [lindex $Vals 5]
	prolog call alsdev change_settings -list $Vals -atom $Grp

	Window hide .alsdev_settings
}

proc fonts_and_colors { Window } {
	global proenv

	set proenv(fonts_and_colors) $Window

	set SelectBackground [$Window.text cget -selectbackground ]
	set SelectForeground [$Window.text cget -selectforeground ]
	set Font [$Window.text cget -font ]
	set proenv(text,family)  [lindex $Font 0]
	set proenv(text,size)  [lindex $Font 1]
	set proenv(text,style)  [lindex $Font 2]
	if {"$proenv(text,style)"==""} then {set proenv(text,style) normal}
	set Tabs [$Window.text cget -tabs]

	Window show .alsdev_settings
	.alsdev_settings.buttons.save_settings configure \
		-command "save_fonts_and_colors $Window"
	wm title .alsdev_settings "Fonts&Colors: $Window"
    .alsdev_settings.background configure -background [$Window.text cget -background ]
    .alsdev_settings.foreground configure -foreground [$Window.text cget -foreground ]

	grab set .alsdev_settings
}

proc cancel_fonts_and_colors { } {
	grab release .alsdev_settings
	Window hide .alsdev_settings
}

proc save_fonts_and_colors { Window } {
	global array proenv
	
	switch $Window {
		.topals { set Grp .topals }
		.debugwin { set Grp .debugwin }
		default { set Grp .document }
	}
	set Background [$Window.text cget -background ]
	set Foreground [$Window.text cget -foreground ]
	set SelectBackground [$Window.text cget -selectbackground ]
	set SelectForeground [$Window.text cget -selectforeground ]
	set Font [$Window.text cget -font ]
	set Tabs [$Window.text cget -tabs]

	set Vals [list $Background $Foreground $SelectBackground $SelectForeground $Font $Tabs]

	set proenv($Grp,background)         $Background
	set proenv($Grp,foreground)         $Foreground
	set proenv($Grp,selectbackground)   $SelectBackground
	set proenv($Grp,selectbackground)   $SelectForeground
	set proenv($Grp,font)               $Font
	set proenv($Grp,tabs)               $Tabs

	grab release .alsdev_settings
	Window hide .alsdev_settings

	prolog call alsdev change_settings -list $Vals -atom $Grp
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
		
	if {[winfo exists .debugwin]==0} then { Window show .debugwin }
	.debugwin.text delete 1.0 end
	show_debugwin
}

proc show_debugwin {} {
	global array proenv
		
	Window show .debugwin
	raise .debugwin
	prolog call builtins change_debug_io -atom debugwin
	check_leashing
	foreach Win  $proenv(debugwin,visible) {
		wm deiconify $Win
	}
	prolog call alsdev setup_for_debugging -atom on
	set proenv(debugwin) 1
}

proc hide_debugwin {} {
	global array proenv

	hide_spywin
	foreach Win  $proenv(debugwin,visible) {
		Window hide $Win
	}
	prolog call alsdev setup_for_debugging -atom off
	Window hide .debugwin
	set proenv(debugwin) 0
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
		if {[winfo exists .spywin]==0} then {
			Window show .spywin
		}
		prolog call builtins non_sys_modules -var NonSysMods

		if {[info exists SpyModuleMenu] > 0} then {
			if {[winfo exists $SpyModuleMenu] > 0} then {
				destroy $SpyModuleMenu
				destroy .spywin.modules.mods
			}
		}
		set SpyModuleMenu \
			[eval "tk_optionMenu .spywin.modules.mods SpyModule" $NonSysMods]

		set SpyModule user
		set LL [llength $NonSysMods]
		for {set LI 0} {$LI < $LL} {incr LI} {
			$SpyModuleMenu entryconfigure $LI -command chng_spy_preds_menu
		}
		pack .spywin.modules.mods \
			-after .spywin.modules.module_label \
			-anchor center -expand 1 -fill x -padx 12 -side left

		chng_spy_preds_menu

		Window show .spywin
		raise .spywin
	}
}

proc chng_spy_preds_menu {} {
	global SpyModule SpyPred SpyPredMenu

	prolog call builtins module_preds -atom $SpyModule -var PredsList

#	destroy .spywin.predicates.preds
#	destroy $SpyPredMenu
#	set SpyPred [lindex $PredsList 0]
#	set SpyPredMenu [eval "tk_optionMenu .spywin.predicates.preds SpyPred" $PredsList]

#	pack .spywin.predicates.preds \
#		-after .spywin.predicates.pred_label \
#		-anchor center -expand 1 -fill x -padx 12 -side left
}

proc popup_spypoint_choice {} {
	global SpyModule 

	Window show .spychoose
	.spychoose.modid.module configure -text $SpyModule
	prolog call builtins module_preds -atom $SpyModule -var PredList
	.spychoose.slist.listbox delete 0 end
	foreach P $PredList {
		.spychoose.slist.listbox insert end $P
	}
}

proc do_spychoose {Which} {
	global SpyModule
	 
	Window hide .spychoose
	if {"$Which"=="cancel"} then { return }

	set SelIndicies [.spychoose.slist.listbox curselection]
	if {"$SelIndicies"==""} then { return }

	foreach PI $SelIndicies {
		lappend SelItems [.spychoose.slist.listbox get $PI]
	}
	prolog call debugger set_chosen_spypoints -list $SelItems -atom $SpyModule 
	.spychoose.slist.listbox delete 0 end
}


proc spy_point {Action} {
	global SpyModule SpyPred 

#	switch $Action {
#	cancel { toggle_spywin }
#	ok {
#		prolog call debugger gui_spy -atom $SpyModule -atom $SpyPred
#		toggle_spywin
#	}
#	}

	toggle_spywin

}

proc check_leashing {} {
	global array proenv

	prolog call debugger check_leashing -var Call -var Exit -var Redo -var Fail
	set proenv(leash,call) $Call
	set proenv(leash,exit) $Exit
	set proenv(leash,redo) $Redo
	set proenv(leash,fail) $Fail

}

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
	clear_tag head_tag $WinName.text
	$WinName.text tag delete head_tag
	clear_tag call_tag $WinName.text
	$WinName.text tag delete call_tag
	wm deiconify $WinName
	raise $WinName
}

proc load_file_to_win {FileName TextWinName} {
	set SI [open $FileName r]
	set NumRows [load_stream_to_win $SI $TextWinName]
	close $SI
	return $NumRows
}

proc save_win_to_file {TextWinName FileName} {
	set SI [open $FileName w]
	save_stream_to_win $SI $TextWinName
	close $SI
	file attributes $FileName -creator ALS4 -type TEXT
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

proc line_index_file {FileName } {
	set SI [open $FileName r]
	set Index [line_index_stream $SI ]
	close $SI
	return $Index
}

proc line_index_stream {Stream } {
	set LineCount 1
	set LineCsList ""
	while {  [eof $Stream]==0 } {
		set NumCs [gets $Stream Line]
		if { $NumCs >= 0 } then {
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

proc source_trace_closedown {STWin} {
	global DebugResponse 

	set DebugResponse Ba
	destroy $STWin
}

proc debugwin_configure_event {Win Ht Wd WW} {
	if {"$WW"==".debugwin.text"} then {
		set FD [.debugwin.text cget -font]
		set FM [font measure .debugwin.text mmmmm]
		set WWD [.debugwin.text cget -width]
		prolog call debugger set_debugwin_width -number $FM -number $Wd
	}
}




proc tkOpenDocument args {
	foreach file $args {
		document.open $file
	}
}

###############________________________________##################
###############________________________________##################

Window show .topals
if {$tcl_platform(platform) == "macintosh"} {
	# Make .topals.mmenb the default menu for all windows.
	. configure -menu .topals.mmenb
}
update idletasks
raise .topals

