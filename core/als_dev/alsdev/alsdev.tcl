##=================================================================================
#|				alsdev.tcl
#|		Copyright (c) 1997-98 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk procedures supporting the top-level Tk-based
#|		ALS Prolog shell
#|
#|		"$Id: alsdev.tcl,v 1.39 1998/03/11 02:50:05 ken Exp $"
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

proc xpe { What } {
	global array proenv
	return $proenv($What)
}

proc xpe2 { What1 What2 } {
	global array proenv
	return $proenv($What1,$What2)
}

set argc 0
set argv ""

# Error handling Procedures

# re - Report Errors: Forces the reporting of errors
proc re {a} {
	if {[catch {uplevel $a} result]} then {
		bgerror $result
		error $result
	}
}

# Basic try control structure for handling exceptions. Two forms are supported:
# try A always B = Always execute B after A, even if A fails (ie causes exception).
# try A fail B   = Execute B only when A fails (ie causes exeception).

proc try {a selector b} {
	if {$selector != "always" && $selector != "fail"} then {
		error "bad option \"$selector\": must be always or fail"
	}
	if {[catch {uplevel $a} result] == 0} then {
		if {$selector == "always"} then {uplevel $b}
	} else {
		uplevel $b
		error $result
	}
}

if {[info exists ALSTCLPATH]==0} then { set ALSTCLPATH . }
#puts "LOADING ALSDEV.TCL: ALSTCLPATH=$ALSTCLPATH"

if {$tcl_platform(platform) == "macintosh"} {
	load {} {appleevents}
}
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

if {$tcl_platform(platform) == "macintosh"} then {
	set proenv(.topals,geometry)	400x300+3+22
	set proenv(.debugwin,geometry)	400x300+300+22
} else {
	set proenv(.topals,geometry)	400x300+0+0
	set proenv(.debugwin,geometry)	400x300+300+0
}

set proenv(.topals,foreground)	black
set proenv(.debugwin,foreground)	black
set proenv(.document,foreground)	black

set proenv(.topals,background)	#ffffff
set proenv(.debugwin,background)	#ffffff
set proenv(.document,background)	#ffffff

set proenv(.topals,selectforeground)	systemHighlightText
set proenv(.debugwin,selectforeground)	systemHighlightText
set proenv(.document,selectforeground)	systemHighlightText

set proenv(.topals,selectbackground)	systemHighlight
set proenv(.debugwin,selectbackground)	systemHighlight
set proenv(.document,selectbackground)	systemHighlight

set proenv(.topals,font)	{user 10 normal}
set proenv(.debugwin,font)	{user 10 normal}
set proenv(.document,font)	{user 10 normal}

set proenv(.topals,tabs)	{}
set proenv(.debugwin,tabs)	{}
set proenv(.document,tabs)	{}

if {$tcl_platform(platform) == "macintosh"} {
	set proenv(.topals,font)		{Monaco 9 normal}
	set proenv(.debugwin,font)		{Monaco 9 normal}
	set proenv(.document,font)		{Monaco 9 normal}
} elseif {$tcl_platform(platform) == "windows"} {
	set proenv(.topals,selectbackground)	SystemHighlight
	set proenv(.debugwin,selectbackground)	SystemHighlight
	set proenv(.document,selectbackground)	SystemHighlight
} elseif {$tcl_platform(platform) == "unix"} {
	set proenv(.topals,background)	#d9d9d9	
	set proenv(.debugwin,background)	#d9d9d9
	set proenv(.document,background)	#d9d9d9
	set proenv(.topals,selectforeground)	black
	set proenv(.debugwin,selectforeground)	black
	set proenv(.document,selectforeground)	black
	set proenv(.topals,selectbackground)	#c3c3c3
	set proenv(.debugwin,selectbackground)	#c3c3c3
	set proenv(.document,selectbackground)	#c3c3c3
}

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

proc establish_defaults {} {
	global array proenv

	prolog call alsdev alsdev_ini_defaults  \
		-var DefaultVals -var TopGeom -var DebugGeom
	reset_default_values $DefaultVals
	if {"$TopGeom"!=""} then { set proenv(.topals,geometry) $TopGeom }
	if {"$DebugGeom"!=""} then { set proenv(.debugwin,geometry) $DebugGeom }
}

proc reset_default_values { DefaultValsList } {
	set TopVals [lindex $DefaultValsList 0] 
	set DebugVals [lindex $DefaultValsList 1] 
	set EditVals [lindex $DefaultValsList 2] 

	if {"$TopVals"!=""} then { reset_def_vals .topals $TopVals }
	if {"$DebugVals"!=""} then { reset_def_vals .debugwin $DebugVals }
	if {"$EditVals"!=""} then { reset_def_vals .document $EditVals }
}

proc reset_def_vals { Which ValsList } {
	global proenv

 	set proenv($Which,background) [lindex $ValsList  0]
 	set proenv($Which,foreground) [lindex $ValsList  1]
 	set proenv($Which,selectbackground) [lindex $ValsList  2]
 	set proenv($Which,selectforeground) [lindex $ValsList  3]
 	set proenv($Which,font) [lindex $ValsList  4]
 	set proenv($Which,tabs) [lindex $ValsList  5]
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
		if {"$proenv(debugwin)"==0} then { 
			wm geometry .debugwin $proenv(.debugwin,geometry)
			show_debugwin 
		}
	} 
}


proc load_source {path name} {
	global tcl_platform
	if {$tcl_platform(platform) == "macintosh"} {
		uplevel "source -rsrc {$name}"
	} else {
		uplevel "source \[file join {$path} {$name.tcl}]"
	}
}


load_source $ALSTCLPATH {alsdev_main}
load_source $ALSTCLPATH {als_settings}
load_source $ALSTCLPATH {debugwin}
load_source $ALSTCLPATH {defstr}
load_source $ALSTCLPATH {als_menu}
load_source $ALSTCLPATH {als_document}
load_source $ALSTCLPATH {als_tkfbox}


proc load_photo {image_name base_name} {
	global tcl_platform ALSTCLPATH
	if {$tcl_platform(platform) == "macintosh"} {
		image create photo $image_name -format gif -data [resource read GIFf $base_name]
	} else {
		image create photo $image_name -file [file join $ALSTCLPATH .. images $base_name.gif]
	}
}

load_photo up_arrow_gif up-arrow-blue
load_photo down_arrow_gif down-arrow-blue
load_photo right_gif right-arrow-blue
load_photo left_gif left-arrow-blue

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
	bind $WinPath <Control-c> "listener.copy .topals"
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
	global tcl_platform
	if {$tcl_platform(platform) == "macintosh"} {
		prolog interrupt
	} else {
		set WakeUpLookAround -1
	}
}

proc ctl-d_action { TxtWin StreamAlias } {
	global WaitForLine

	set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -2 chars} ]
	if { [llength $ThisLine]>0 } then {
		return
	} 
	exit_prolog 
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
		-title "Source File" \
		-filetypes {{"Tcl/Tk Files" {.tcl} TEXT} {{All Files} {*} TEXT} } ]
	if { "$file"== "" } then { bell ; return }
	set TclInterp [do_popup_input "Input the name of the Tcl interpreter:" "Tcl Interp?"]
	if { "$TclInterp"== "" } then { bell ; return }
	if {[interp exists $TclInterp]==0} then {
		prolog call alsdev do_source_tcl -atom $TclInterp -atom $file
	} else { bell }
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
	global WaitForLine

	set ans [tk_dialog .quit_dialog "Exit Prolog?" \
		"Really Exit ALS Prolog?" "" 0 Yes No ]
	if {"$ans"==1} then {
		return 0
	} else {
		if {[document.close_all]} then {
			save_window_positions
			exit
#			set WaitForLine -3
		}
	}
}

proc save_window_positions {} {
	global proenv
	set TopGeom [wm geometry .topals]	
	if {[winfo exists .debugwin]==1} then {
		set DebugGeom [wm geometry .debugwin]	
	} else {
		set DebugGeom $proenv(.debugwin,geometry)
	}
	prolog call alsdev win_positions_for_exit -atom $TopGeom -atom $DebugGeom
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
}

proc clear_workspace { } {
	prolog call alsdev clear_workspace 
}

		# file menu:
proc listener.new {}   { document.new }
proc listener.open {}  { document.open }
proc listener.close {w} { bell }
proc listener.save {w}  { bell }

proc debugwin.new {}   { document.new }
proc debugwin.open {}  { document.open }
proc debugwin.close {w} { bell }
proc debugwin.save {w}  { bell }


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

	grab release .alsdev_settings
	set COLOR [tk_chooseColor \
		-title "Choose Background Color" -initialcolor $proenv(.topals,background)]
	if {"$COLOR" == ""} then {return}
	.alsdev_settings.background configure -background $COLOR
	$Window.text configure -background $COLOR
	grab set -global .alsdev_settings
}

proc choose_foreground_color {Window} {
	global array proenv

	grab release .alsdev_settings
	set COLOR [tk_chooseColor \
		-title "Choose Foreground Color" -initialcolor $proenv(.topals,foreground)]
	if {"$COLOR" == ""} then {return}
	.alsdev_settings.foreground configure -foreground $COLOR
	$Window.text configure -foreground $COLOR
	grab set -global .alsdev_settings
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

	grab set -global .alsdev_settings
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

	prolog call alsdev change_window_settings -list $Vals -atom $Grp
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
	label $ff.label -borderwidth 0 \
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
#####			STATIC FLAGS				#####
#################################################

proc show_static_flags {} {
	global array proenv

	if {[winfo exists .static_flags]} then {
		Window show .static_flags
	} else {
		Window show .static_flags
		prolog call builtins static_flags_info -var InfoList
		foreach info $InfoList {
			create_static_flag_entry $info
		}
	}
	wm geometry .static_flags ""
}

proc create_static_flag_entry { info } {
	global array proenv

	set flgg [lindex $info 0]
	label .static_flags.$flgg -borderwidth 0 -relief flat -anchor w \
		-text [format "%s  =  %s" $flgg [lindex $info 1]]
	pack .static_flags.$flgg -anchor w -expand 0 -fill x -side top 
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
		
	if {[winfo exists .debugwin]==0} then { 
		Window show .debugwin 
	}
	.debugwin.text delete 1.0 end
	show_debugwin
}

proc show_debugwin {} {
	global array proenv
		
	Window show .debugwin
	wm geometry .debugwin $proenv(.debugwin,geometry)
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

#	hide_spywin
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
#	hide_spywin
	hide_debugwin
}

proc switch_debug_setup {Which} {
	global array proenv

	if {"$Which"=="on"} then {
		if {"$proenv(debugwin)"==0} then { toggle_debugwin }
	} else {
		if {"$proenv(debugwin)"==1} then { toggle_debugwin }
	}
}

		###############################
		######### SPYPOINTS
		###############################
proc toggle_spywin {} {
	global array proenv
	prolog call builtins non_sys_modules -var NonSysMods
	module_choose [lsort $NonSysMods]
}

proc spy_preds_choice2 {Mod} {
	global array proenv

    set base .spy_on_$Mod

	prolog call builtins module_preds -atom $Mod -var Spying -var Rest
	set proenv(prev_spying,$Mod) $Spying
	set proenv(prev_notspying,$Mod) $Rest
	set proenv(spying,$Mod) $Spying
	set proenv(notspying,$Mod) $Rest

	spy_preds_in_module $base $Mod
	$base.cols_fr.l.leftlist.lstbx delete 0 end
	$base.cols_fr.r.rightlist.lstbx delete 0 end

	disp_list $proenv(spying,$Mod)    $base.cols_fr.l.leftlist.lstbx
	disp_list $proenv(notspying,$Mod) $base.cols_fr.r.rightlist.lstbx
}

proc move_to_spying {Module Base} {
	set NewSpying [move_col_left $Base]
	prolog call debugger install_new_spypoints -list $NewSpying -atom $Module
}

proc move_to_no_spying {Module Base} {
	set NewNoSpying [move_col_right $Base]
	prolog call debugger remove_old_spypoints -list $NewNoSpying -atom $Module
}

#proc install_spypoints {module base} {
#	set Left [$base.cols_fr.l.leftlist.lstbx get 0 end]
#	set Right [$base.cols_fr.r.rightlist.lstbx get 0 end]
#	prolog call debugger reset_spypoints -atom $module -list $Left -list $Right
#}


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

proc line_index_file {FileName } {
	set SI [open $FileName r]
	try {
		set Index [line_index_stream $SI ]
	} always {
		close $SI
	}
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

proc show_debug_settings {} {
	global array proenv

	prolog call debugger get_maxdepth -atom debugger_output -var CurDepth
	set proenv(debug_print_depth) $CurDepth

	prolog call debugger get_depth_computation -atom debugger_output -var DC
	set proenv(db_flatness) $DC

	Window show .debug_settings
}

proc reset_print_depth {} {
	global array proenv

    set proenv(debug_print_depth) [.debug_settings.depth.value get]
	prolog call debugger set_maxdepth -atom debugger_output -number $proenv(debug_print_depth)
}

proc toggle_debug_flatness {} {
	global array proenv

	prolog call debugger set_depth_computation -atom debugger_output -atom $proenv(db_flatness)
}


##############################





##############################


proc tkOpenDocument args {
	foreach file $args {
		document.open $file
	}
}

proc do_2col {base} {
 	vTclWindow.columns2_select $base
	window show $base

}

###############________________________________##################
###############________________________________##################


if {$tcl_platform(platform) == "macintosh"} {
	# Make .topals.mmenb the default menu for all windows.
	. configure -menu .topals.mmenb
}
Window show .topals
wm positionfrom .topals user
wm geometry .topals $proenv(.topals,geometry)

Window show .debug_settings
Window hide .debug_settings

Window show .alsdev_settings
Window hide .alsdev_settings

Window show .topals

update idletasks
raise .topals
focus .topals.text

