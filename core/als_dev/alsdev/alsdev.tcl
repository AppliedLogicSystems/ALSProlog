#=================================================================================
#|				alsdev.tcl
#|		Copyright (c) 1997-98 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk procedures supporting the top-level Tk-based
#|		ALS Prolog shell
#|
#|		"$Id: alsdev.tcl,v 1.90 2009/06/06 21:24:14 chuck Exp $"
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

# Load custom packages for each windowing system.
#
# OpenDocument is an ALS-custom package for Windows.  This package contains
# the AttachOpenDocumentHandler procedure which should be called when
# .topals is fully opened.


switch [tk windowingsystem] {
	win32   { load {} OpenDocument }
	default {}
}

proc xpe { What } {
	global array proenv
	return $proenv($What)
}

proc xpe2 { What1 What2 } {
	global array proenv
	return $proenv($What1,$What2)
}


	#--------------------------------------------------------------
	#   Effectively calls prolog with:
	#       Mod:send(Obj, Msg)
	# where:
	#       Mod = $proenv(dflt_mod)
	#       $proenv($Obj) = a number which is an object handle for Obj
	#	    where Obj is a prolog object (e.g., ALS_IDE_Mgr, line 154 in blt_dvsh.pro
	#       Msg is an atom
	#
	#### WARNING #### WARNING #### WARNING 
	#
	#	This procedure is identical with one in tk_alslib,
	#	EXCEPT EXCEPT EXCEPT for the global variable "proenv"
	#	{ which is agv in the tk_alslib version}.  The one
	#	here is intended only for use in the shell tcl interpreter
	#	shl_tcli, while the one from tk_alslib is intended to be
	#	loaded into user-specificied tcl interpreters.
	#--------------------------------------------------------------
proc send_prolog {Obj Msg} {
	global proenv
	prolog call $proenv(dflt_mod) send -number $proenv($Obj) -atom $Msg
}

	#--------------------------------------------------------------
	#   Effectively calls prolog with:
	#       Mod:send(Obj, Msg)
	# where:
	#       Mod = $proenv(dflt_mod)
	#       $proenv($Obj) = a number which is an object handle for Obj
	#	    where Obj is a prolog object (e.g., ALS_IDE_Mgr, line 154 in blt_dvsh.pro
	#       Msg is of type $Type
	#               (normally use this with $Type = "list" )
	#
	#### WARNING #### WARNING #### WARNING 
	#		..... SAME AS ABOVE
	#--------------------------------------------------------------
proc send_prolog_t {Obj Msg Type} {
	global proenv
	prolog call $proenv(dflt_mod) send -number $proenv($Obj) -$Type $Msg
}

set argc 0
set argv ""

	#--------------------------------------------------------------
	# Error handling Procedures
	#
	# re - Report Errors: Forces the reporting of errors
	#--------------------------------------------------------------
proc re {a} {
	if {[catch {uplevel $a} result]} then {
		bgerror $result
		error $result
	}
}

	#--------------------------------------------------------------
	# Basic try control structure for handling exceptions. 
	# Two forms are supported:
	#
	# try A always B = Always execute B after A, even if A 
	#				   fails (ie causes exception).
	# try A fail B   = Execute B only when A fails 
	#				   (ie causes exeception).
	#--------------------------------------------------------------
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

set proenv(edition) 			normal
set proenv(cwd) 				[pwd]
set proenv(debugger_ld)			false
set	proenv(debugwin)			0
set	proenv(debugwin,visible)	{}
set	proenv(spywin)				0
set proenv(defstr_ld)			false
set proenv(dflt_mod)			alsdev
set proenv(untitled_counter)	0
set proenv(posted_vis)			{}

set proenv(alsdev_history_file)		""
set proenv(do_load_prev_alsdev_history)	true

	## window appearance stuff - initial defaults:

# Define cross-platform version of systemHighlight
switch [tk windowingsystem] {
    x11 {
	set systemHighlightText black
	set systemHighlight #c3c3c3
    }
    default {
	set systemHighlightText systemHighlightText
	set systemHighlight systemHighlight
    }
}

set proenv(debugwin_button,background)	#cee8e6
set proenv(interrupt_button,foreground)	#ff0000

set proenv(.topals,geometry)	600x500
set proenv(.debugwin,geometry)	600x300

set proenv(.topals,foreground)	black
set proenv(.debugwin,foreground)	black
set proenv(.document,foreground)	black

set proenv(.topals,background)	#ffffff
set proenv(.debugwin,background)	#ffffff
set proenv(.document,background)	#ffffff

set proenv(.topals,selectforeground)	$systemHighlightText
set proenv(.debugwin,selectforeground)	$systemHighlightText
set proenv(.document,selectforeground)	$systemHighlightText

set proenv(.topals,selectbackground)	$systemHighlight
set proenv(.debugwin,selectbackground)	$systemHighlight
set proenv(.document,selectbackground)	$systemHighlight

set proenv(.topals,font)	{user 14 normal}
set proenv(.debugwin,font)	{user 14 normal}
set proenv(.document,font)	{user 14 normal}

set proenv(.topals,tabs)	{}
set proenv(.debugwin,tabs)	{}
set proenv(.document,tabs)	{}

set	proenv(edit,visible)		{}

set proenv(heartbeat) 1.05
set proenv(main_printdepth) 50
set proenv(main_depth_type) nonflat
#---------------------------------------------------------------

#################################
# VTcl Top PROCEDURES
#--------------------------------
proc main {argc argv} {
}

proc show_window {w} {
	raise $w
	wm deiconify $w
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
        show 	{vTclWindow$name $name}
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

proc establish_defaults {} {
	global array proenv

	prolog call alsdev alsdev_ini_defaults  -var DefaultVals -var TopGeom -var DebugGeom -var DebugVis

	reset_default_values $DefaultVals
	if {$TopGeom != ""} then { set proenv(.topals,geometry) $TopGeom }
	if {$DebugGeom != ""} then { set proenv(.debugwin,geometry) $DebugGeom }
	set proenv(debugwin) $DebugVis 
}

proc reset_default_values { DefaultValsList } {
	set TopVals [lindex $DefaultValsList 0] 
	set DebugVals [lindex $DefaultValsList 1] 
	set EditVals [lindex $DefaultValsList 2] 

	if {$TopVals != ""} then { reset_def_vals .topals $TopVals }
	if {$DebugVals != ""} then { reset_def_vals .debugwin $DebugVals }
	if {$EditVals != ""} then { reset_def_vals .document $EditVals }
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

#Called from builtins/blt_dvsh.pro > alsdev(Shared, ALS_IDE_Mgr):
#	 tcl_call(shl_tcli, [do_main_als_bindings],_),
proc do_main_als_bindings {} {
	bind .topals.text <Unmap> {unmap_alsdev_main}
	bind .topals.text <Map> {map_alsdev_main}
}

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
		wm iconify .debugwin
	}
	set proenv(debugwin) 0
	foreach Win  $proenv(debugwin,visible) {
		wm iconify $Win
	}
	send_prolog als_ide_mgr unmap_edit_wins
}

proc map_alsdev_debug {} {
	global array proenv
	if {[winfo exists .debugwin]} then {
		wm deiconify .debugwin
	} 
	set proenv(debugwin) 1
	foreach Win  $proenv(debugwin,visible) {
		wm deiconify $Win
	}
	send_prolog als_ide_mgr map_edit_wins
}

proc load_source {path name} {
	uplevel "source \[file join {$path} {$name.tcl}]"
}

# Load Tcl source

load_source $ALSTCLPATH {alsdev_main}
load_source $ALSTCLPATH {als_settings}
load_source $ALSTCLPATH {debugwin}
load_source $ALSTCLPATH {defstr}
load_source $ALSTCLPATH {als_menu}
load_source $ALSTCLPATH {als_document}
load_source $ALSTCLPATH {als_projects}

proc load_photo {image_name base_name} {
	global ALSTCLPATH
	image create photo $image_name -file [file join $ALSTCLPATH .. images $base_name.gif]
}

# Load images

load_photo up_arrow_gif up-arrow-blue
load_photo down_arrow_gif down-arrow-blue
load_photo right_gif right-arrow-blue
load_photo left_gif left-arrow-blue
load_photo openfolder openfolder
switch [tk windowingsystem] {
	x11 {
		load_photo closed_ptr closed_unix
		load_photo open_ptr open_unix
	}
	win32 {
		load_photo closed_ptr closed_wins
		load_photo open_ptr open_wins
	}
	aqua {
		load_photo closed_ptr closed_mac
		load_photo open_ptr open_mac
	}
}

	## source any other needed files here.....

#################################################
#####		MAIN WINDOW		    #####
#################################################

	## Bindings for the main window:

###################################################################################
# set_top_bindings is called from 
# builtins/blt_dvsh.pro > alsdev(Shared, ALS_IDE_Mgr):
#	tcl_call(shl_tcli,
#                [set_top_bindings,'.topals.text',shl_tk_in_win,WaitVar,DataVar],_)
###################################################################################

proc set_top_bindings { WinPath StreamAlias WaitVarName DataVar } {

 	bind $WinPath <Return> \
 		"xmit_line_plain $WinPath $StreamAlias $WaitVarName"
        bind $WinPath <Control-d> \
                "ctl-d_action $WinPath $StreamAlias "
        bind $WinPath <Control-c> "listener.copy .topals"
        bind $WinPath <Control-u> \
                "ctl-u_action $WinPath"
 
                ## This is intended to be totally global in the IDE:
        bind all <Control-period> interrupt_action

                # Use of break from: http://wiki.tcl.tk/1152 : Replacing the Text Bindings
        bind $WinPath <BackSpace> {
        if {[%W compare insert != 1.0] && \
                [%W compare insert > {lastPrompt + 1c}]} {
                %W delete insert-1c
                %W see insert
            }
            break
        }
        bind $WinPath <Control-h> [bind .topals.text <BackSpace>]

        bind $WinPath <Left> {
        if {[%W compare insert != 1.0] && \
                [%W compare insert > {lastPrompt + 1c}]} {
                %W mark set insert insert-1c
                %W see insert
            }
            break
        }

        bind $WinPath <Up> {
            do_history %W
            break
        }

        bind $WinPath <Down> {
            do_rev_history %W
            break
        }

        bind $WinPath <KeyPress> {
                if {[%W compare insert <= lastPrompt]} {
                	%W mark set insert end
            		break
		}
	}

}

#### The original set_top_bindings (as of June 2016):
# All KeyPress events write the key char into .topals.text, so:
# delete will consume prompt; but listener.copy_paste keeps Left and Up from more than 1 action
# proc set_top_bindings { WinPath StreamAlias WaitVarName DataVar } {
# 
# 	bind $WinPath <Return> \
# 		"xmit_line_plain $WinPath $StreamAlias $WaitVarName"
# 	bind $WinPath <Control-d> \
# 		"ctl-d_action $WinPath $StreamAlias "
# 	bind $WinPath <Control-c> "listener.copy .topals"
# 	bind $WinPath <Control-u> \
# 		"ctl-u_action $WinPath"
# 
# 		## This is intended to be totally global in the IDE:
# 	bind all <Control-period> interrupt_action
# 
# 	bind $WinPath <KeyPress> "listener.check_at_end .topals"
# 	bind $WinPath <ButtonPress-2> "listener.copy_paste .topals"
# 	bind Text <ButtonRelease-2> " " 
# }
#proc listener.check_at_end {xw} {
#	set w .topals
#	$w.text mark set insert end 
#puts "listener.check_at_end: end= [$w.text index end] insert= [$w.text index insert]"
#}

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

#proc xmit_line0 { TxtWin StreamAlias WaitVar DataVar} {
#	global $WaitVar 
#
#	set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -2 chars} ]
#	prolog call builtins add_to_stream_buffer -atom $StreamAlias -atom $ThisLine\n
#	$TxtWin mark set lastPrompt {insert -1 chars}
#	set WaitVar 1
#}

#proc wait_for_line0 { } {
#	global WaitForLine
#
#	while {$WaitForLine == 0} { dooneevent wait }
#	set ReturnValue $WaitForLine
#	set WaitForLine 0
#	return $ReturnValue
#}

proc wait_for_line1 { WaitVar } {
	upvar #0 $WaitVar TheWaitVar

	while {$TheWaitVar == 0} { dooneevent wait }
	set ReturnValue $TheWaitVar
	set TheWaitVar 0
	return $ReturnValue
}


	# Transmits a 'line' when the user hits <Return> at points
	# other than the top-level:

proc xmit_line_plain { TxtWin StreamAlias WaitVarName} {
	upvar #0 $WaitVarName WaitForLine
	global array histArray
	global histPosition
	global showPosition

	set InsertIndex [$TxtWin index insert]
	set InsertLine [string range $InsertIndex 0 [expr [string first "." $InsertIndex] - 1 ]]
	incr InsertLine
	set EndIndex [$TxtWin index end]
	set EndLine [string range $EndIndex 0 [expr [string first "." $EndIndex] - 1 ]]
	if {$EndLine == $InsertLine} then {
		set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -1 chars} ]
		incr histPosition 
		set histArray($histPosition) $ThisLine
		set showPosition $histPosition
		set WaitForLine 1
		prolog call builtins add_to_stream_buffer -atom $StreamAlias -atom $ThisLine\n
	} 
	$TxtWin see end
	$TxtWin mark set insert end
}

global array histArray
array set histArray {}
global histPosition
set histPosition 0
global showPosition
set showPosition 0

proc manage_alsdev_history {} {
    global proenv
    global histArray
    global histPosition
    global showPosition
    set histPosition 0

    if {$proenv(do_load_prev_alsdev_history)} {
 
	if {[file exists $proenv(alsdev_history_file)]} {
		set showPosition 1
		set fp [open $proenv(alsdev_history_file) r]
		set file_data [read $fp]
		close $fp
		set data [split $file_data "\n"]
		foreach line $data {
	    		if {[string length $line] >0} {
				incr histPosition 
				set histArray($histPosition) $line
	    		}
	    	}
	    	set showPosition [lindex [array statistics histArray] 0]
         }
    }
}

proc do_history {WinPath} {
	global histArray
	global histPosition
	global showPosition

	if {$showPosition > 0} {
		ctl-u_action .topals.text
		$WinPath insert end $histArray($showPosition)
 		incr showPosition -1 
	}
}

proc do_rev_history {WinPath} {
	global histArray
	global histPosition
	global showPosition

	set rLimit [lindex [array statistics histArray] 0]
	if {$showPosition < $rLimit} {
		ctl-u_action .topals.text
 		incr showPosition 1 
		$WinPath insert end $histArray($showPosition)
	}
}


proc dumpHist {} {
    global histArray
    global histPosition

    puts "dumpHist: histPosition=$histPosition size=[lindex [array statistics histArray] 0]"

    for {set i 1} {$i <= $histPosition} {incr i} {
        puts "$i: $histArray($i)"
    }
}

proc save_history {} {
    global proenv
    global histArray
    global histPosition

	set fo [open $proenv(alsdev_history_file) "w"]
	for {set i 1} {$i <= $histPosition} {incr i} {
    		puts $fo $histArray($i)
	}
	close $fo
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
		if {$WakeUpLookAround != 0} then {
			set SaveIt $WakeUpLookAround
			set WakeUpLookAround 0
#puts "wake_up_look_around return = $SaveIt"

			return $SaveIt
		}
		if {$NEvnt == 0} then {
			set SaveIt $WakeUpLookAround
			set WakeUpLookAround 0
#puts "wake_up_look_around return = $WakeUpLookAround"

			return $WakeUpLookAround
		}
		set NEvnt [dooneevent dont_wait]
		incr CountDown -1
	}
#puts "wake_up_look_around return = $WakeUpLookAround"

	return $WakeUpLookAround
}

proc interrupt_action {} {
	global WakeUpLookAround
	set WakeUpLookAround -1
}

proc ctl-d_action { TxtWin StreamAlias } {
	global WaitForLine.topals.text

	set ThisLine [ $TxtWin get {lastPrompt +1 chars} {end -2 chars} ]
	if { [llength $ThisLine]>0 } then {
		return
	}
#puts "d_action: TxtWin=$TxtWin StreamAlias=$StreamAlias"
	set  WaitForLine.topals.text -3
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

proc high_tide_flotsam { WinPath Text } {
	$WinPath insert end $Text
	$WinPath see end
	update
}

proc source_tcl { } {
	set file [tk_getOpenFile \
		-defaultextension tcl \
		-title "Source File" \
		-filetypes {{"Tcl/Tk Files" {.tcl} TEXT} {{All Files} {*} TEXT} } ]
	if {$file == ""} then { return }
	set TclInterp [do_popup_input "Input the name of the Tcl interpreter:" "Tcl Interp?"]
	if {$TclInterp == ""} then { bell ; return }
	if {[interp exists $TclInterp] == 0} then {
		prolog call alsdev do_source_tcl -atom $TclInterp -atom $file
	} else { bell }
}


proc tcl_debugger {} {
	global ALSTCLPATH
	set TclInterp [do_popup_input "Input the name of the Tcl interpreter:" "Tcl Interp?"]
	if {$TclInterp == ""} then { bell ; return }
	if {[interp exists $TclInterp] == 0} then {
		prolog call alsdev init_tcl_debugger -atom $TclInterp -atom $ALSTCLPATH
	} else { bell }
}

proc do_debug_setup { ALSTCLPATH } {
	source [file join $ALSTCLPATH prodebug.tcl]
	debugger_init
}

proc kill_tcl_interps  { } {
	prolog call tk_alslib destroy_all_tcl_interpreters
}

proc set_directory { } {
	set CWD [pwd]
		# cf: https://www.tcl.tk/man/tcl8.3/TkCmd/chooseDirectory.htm
	set NewDir [tk_chooseDirectory]
	if {$NewDir != ""} {
		cd $NewDir
		show_dir_on_main $NewDir
	}
}

proc show_dir_on_main { Dir } {
	.topals.cpd19.02 configure -text $Dir -anchor w
}

proc exit_prolog { } {
	global WaitForLine
	global proenv

	set ans [tk_messageBox -icon warning -parent .topals \
				-title "Exit Prolog?" -message "Really Exit ALS Prolog?" \
				-type yesno -default yes]
	if {$ans == "yes"} then {
		prolog call alsdev possible_save_project

		if {[document.close_all]} then {
			if {[catch {
					save_window_positions
					prolog call alsdev save_prolog_flags
				} result]} then {
				# maybe do "bgerror $result" or some other message here
			}
			set WaitForLine -3
		}
		save_history
		exit
	} else {
		return 0
	}
}

proc save_window_positions {} {
	global proenv
	set TopGeom [wm geometry .topals]	
	if {[winfo exists .debugwin] == 1} then {
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
		-initialdir [pwd] \
		-filetypes {{"Prolog Files" {.pro .pl } } {{All Files} {*} } } ]
	if {$file == ""} then { return }
	prolog call alsdev do_reconsult -atom $file
	refresh_spy_preds_if_showing
	return true
}

proc clear_workspace { } {
	prolog call alsdev clear_workspace 
}

		# file menu:
proc listener.new {}   { document.new }
proc listener.open {}  { document.open }
proc listener.close {w} { exit_prolog }
proc listener.save {w}  { listener.save_as $w }

#proc listener.save_as {w}  { bell }


proc listener.save_as {w} {
	global array proenv
	
	set file [tk_getSaveFile -initialfile prolog_env \
		-defaultextension .txt ]
	if {$file != ""} then {
		store_text $w.text $file
		return true
	} else {
		return false
	}
}




proc debugwin.new {}   { document.new }
proc debugwin.open {}  { document.open }
proc debugwin.close {w} { bell }
proc debugwin.save {w}  { bell }


#################################################
#####	Utilities & Environment Settings       ##
#################################################

proc display_me {Title Win} {
	Window show $Win
	post_open_document $Title $Win
}

proc remove_me {Title Win} {
	wm withdraw $Win
	un_post_open_document $Title
}

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
	if {$COLOR == ""} then {return}
	.alsdev_settings.background configure -background $COLOR
	$Window.text configure -background $COLOR
}

proc choose_foreground_color {Window} {
	global array proenv

	set COLOR [tk_chooseColor \
		-title "Choose Foreground Color" -initialcolor $proenv(.topals,foreground)]
	if {$COLOR == ""} then {return}
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
	if {$FrontWin == ".alsdev_settings"} then {
		set FrontWin \
			[lindex $Kids [expr [llength $Kids] - 2]]
	}
	if {$FrontWin == ""} then {
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
	if {$proenv(text,style) == ""} then {set proenv(text,style) normal}
	set Tabs [$Window.text cget -tabs]

	Window show .alsdev_settings
	.alsdev_settings.buttons.save_settings configure \
		-command "save_fonts_and_colors $Window"
	wm title .alsdev_settings "Fonts&Colors: $Window"
    .alsdev_settings.background configure -background [$Window.text cget -background ]
    .alsdev_settings.foreground configure -foreground [$Window.text cget -foreground ]
	post_open_document Preferences .alsdev_settings

}

proc cancel_fonts_and_colors { } {
	Window hide .alsdev_settings
	un_post_open_document Preferences 
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

	Window hide .alsdev_settings
	un_post_open_document Preferences 

	prolog call alsdev change_window_settings -list $Vals -atom $Grp
}

#################################################
#####			Prolog Flags				   ##
##                                             ##
##			Dynamic Prolog Flags			   ##
#################################################
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
        -anchor w -expand 0 -fill none -side left 
	pack $ff.opts_menu  \
        -anchor e -expand 0 -fill none -side right 

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


#################################################
#####				DEBUGGER				#####
#################################################

proc toggle_debugwin {} {
	global array proenv

	set proenv(debugwin) [expr 1 - $proenv(debugwin)]
	exec_toggle_debugwin
}

#proc exec_toggle_debugwin {} {
#	global array proenv
#	if {$proenv(debugwin) == 0} then {
#		hide_debugwin
#		set FlagVal off
#	} else {
#		if {[winfo exists .debugwin] == 0} then {
#			vTclWindow.debugwin ""
#			show_debugwin
#		} else {
#			show_debugwin
#		}
#		set FlagVal on
#	}
#	send_prolog debugger_mgr toggle_visibility
##	prolog call builtins do_set_prolog_flag -atom debug -atom $FlagVal
#	set proenv(debug) $FlagVal
#}

proc exec_toggle_debugwin {} {
	global array proenv
	if {$proenv(debugwin) == 0} then {
		hide_debugwin
		set FlagVal off
	} else {
		if {[winfo exists .debugwin] == 0} then {
			vTclWindow.debugwin ""
			show_debugwin
		} else {
			show_debugwin
		}
		set FlagVal on
	}
	send_prolog debugger_mgr toggle_visibility
#	prolog call builtins do_set_prolog_flag -atom debug -atom $FlagVal
	set proenv(debug) $FlagVal
}


proc ensure_db_showing {} {
	global array proenv

	if {[winfo exists .debugwin] == 0} then { 
		Window show .debugwin 
	}
	show_debugwin
}

proc hide_debugwin {} {
	global array proenv

	un_post_open_document Debugger 
	foreach Win  $proenv(debugwin,visible) {
		wm withdraw $Win
	}
	set proenv(debugwin) 0
	wm withdraw .debugwin
}


proc show_debugwin {} {
	global array proenv
    show_window .debugwin
	prolog call builtins change_debug_io -atom debugwin
	post_open_document Debugger .debugwin
	check_leashing
	foreach Win  $proenv(debugwin,visible) {
		wm deiconify $Win
	}
	set proenv(debugwin) 1
}

proc exit_debugger {} {
	global array proenv

	prolog call alsdev exit_debugger
	hide_debugwin
}

proc switch_debug_setup {Which} {
	global array proenv
	if {$Which == "on"} then {
		if {"$proenv(debugwin)"==0} then { toggle_debugwin }
	} else {
		if {"$proenv(debugwin)"==1} then { toggle_debugwin }
	}
}

proc post_debug_subwin {Title Win} {
	global array proenv

	if {[lsearch -exact $proenv(debugwin,visible) $Win] < 0 } then {
		set proenv(debugwin,visible) [concat $Win $proenv(debugwin,visible)]
	}
	post_open_document $Title $Win
}

proc unpost_debug_subwin {Title Win} {
	global array proenv

	set Idx [lsearch -exact $proenv(debugwin,visible) $Win]
	if {$Idx >= 0} then {
		set proenv(debugwin,visible) [lreplace $proenv(debugwin,visible) $Idx $Idx]
	}
	Window hide $Win
	un_post_open_document $Title 
}

		###############################
		######### SPYPOINTS
		###############################
proc toggle_spywin {} {
	global array proenv
	prolog call builtins non_sys_modules -var NonSysMods
	module_choose [lsort $NonSysMods]
}


proc refresh_mods_list {}  {
	global array proenv
	prolog call builtins non_sys_modules -var NonSysMods
	.pred_info.mods.listbox delete 0 end 
	set SortedNonSysMods [lsort $NonSysMods]

	foreach Module $SortedNonSysMods {
		.pred_info.mods.listbox insert end $Module
	}
	set PrevFocus [get_module_focus]
	if {[llength $SortedNonSysMods] == 1} then {
		set_module_focus user
	} elseif { [lsearch -exact $SortedNonSysMods $PrevFocus] < 0 } then {
		set_module_focus [lindex $SortedNonSysMods 0]
	} else {
		set_module_focus $PrevFocus
	}
}

proc set_module_focus {Mod} {
	global array proenv

    .pred_info.mods.l2.modfocus configure -text $Mod
	refresh_preds_list $Mod
}

proc get_module_focus {} {
	global array proenv

    return [.pred_info.mods.l2.modfocus cget -text]
}

proc get_selected_module {} {
	global array proenv

	return [ .pred_info.mods.listbox get \
			[lindex [.pred_info.mods.listbox curselection ] 0] ]
}


proc refresh_the_preds {} {
	refresh_preds_list [get_module_focus]
}

proc refresh_preds_list {Mod} {
	global array proenv

	prolog call builtins module_preds -atom $Mod -var Spying -var Rest

	.pred_info.preds.listbox delete 0 end 
	foreach Pred $Rest {
		.pred_info.preds.listbox insert end $Pred
	}

    .pred_info.spying.listbox  delete 0 end
	foreach Pred $Spying {
		.pred_info.spying.listbox insert end $Pred
	}
}

proc refresh_spy_win0 {} {
	refresh_spy_win
	set_module_focus user
	refresh_preds_list user

}
proc refresh_spy_win {} {
	refresh_mods_list
	set MF [get_module_focus]
	if  {$MF != ""} then { refresh_preds_list $MF }
}

proc refresh_spy_preds_if_showing {} {
	global array proenv

	if {[winfo exists .pred_info] == 1} then { 
		refresh_spy_win
	}
}

proc move_to_spying_list {} {
	global array proenv

	set PrevSpying [.pred_info.spying.listbox get 0 end]
	set IXs [.pred_info.preds.listbox curselection]
	.pred_info.preds.listbox selection clear 0 end
	set NewSpying {}
	foreach  Idx $IXs {
		lappend NewSpying [set Tmp [.pred_info.preds.listbox get $Idx]]
		lappend PrevSpying $Tmp
	}
	set RIXs [lsort -decreasing $IXs]
	foreach  Idx $RIXs {
		.pred_info.preds.listbox delete $Idx
	}
	prolog call debugger install_new_spypoints \
							-list $NewSpying \
							-atom [get_module_focus]

	.pred_info.spying.listbox delete 0 end
	set SpyingNow [lsort $PrevSpying]
	foreach PD $SpyingNow {
		.pred_info.spying.listbox insert end $PD
	}
}

proc remove_from_spying_list {} {
	global array proenv

	set PrevNonSpying [.pred_info.preds.listbox get 0 end]
	set NewNonSpying {}
	set IXs [.pred_info.spying.listbox curselection]
	.pred_info.spying.listbox selection clear 0 end
	foreach  Idx $IXs {
		lappend NewNonSpying [set Tmp [.pred_info.spying.listbox get $Idx]]
		lappend PrevNonSpying $Tmp
	}

	set RIXs [lsort -decreasing $IXs]
	foreach  Idx $RIXs {
		.pred_info.spying.listbox delete $Idx
	}
	prolog call debugger remove_old_spypoints \
							-list $NewNonSpying \
							-atom [get_module_focus]
	.pred_info.preds.listbox delete 0 end
	set NonSpyingNow [lsort $PrevNonSpying]
	foreach PD $NonSpyingNow {
		.pred_info.preds.listbox insert end $PD
	}
}

proc reset_all_spypoints {} {
	prolog call debugger reset_all_spypoints 
}

#	set Spying [.pred_info.spying.listbox get 0 end]
#	prolog call debugger install_new_spypoints -list $Spying -atom [get_module_focus]

proc remove_all_spypoints {} {
	set Mod [get_module_focus]
	set ans [tk_dialog .quit_dialog \
				"Remove Spypoints?" \
				"Remove spypoints in:" "" 0 \
				"All Modules" "Module $Mod Only" "No Modules"]

	set Spying [.pred_info.spying.listbox get 0 end]

    switch $ans {
	0 " prolog call debugger remove_all_spypoints "
	1 " prolog call debugger remove_old_spypoints -list $Spying -atom $Mod "
	2 " return "
	}
	.pred_info.spying.listbox delete 0 end
	set NonSpying [.pred_info.preds.listbox get 0 end]
	.pred_info.preds.listbox delete 0 end
	lappend NonSpying $Spying
	set NonSpyingNow [lsort $NonSpying]
	foreach PD $NonSpyingNow {
		.pred_info.preds.listbox insert end $PD
	}
}

proc carry_out_listing {} {
	set Mod [get_module_focus]
	set IXs [.pred_info.spying.listbox curselection]

	if {$IXs != ""} then {
		set Pred [.pred_info.spying.listbox get [lindex $IXs 0] ]
	} else {
		set IXs [.pred_info.preds.listbox curselection]
		if {$IXs != ""} then {
			set Pred [.pred_info.preds.listbox get [lindex $IXs 0] ]
		} else {
			set SelOwner [selection own]
			puts "Must try looking in section owner = $SelOwner"
			return
		}
	}
	prolog call builtins carry_out_listing -atom "$Mod:$Pred"
}

proc carry_out_listasm {} {
	set Mod [get_module_focus]
	set IXs [.pred_info.spying.listbox curselection]

	if {$IXs != ""} then {
		set Pred [.pred_info.spying.listbox get [lindex $IXs 0] ]
	} else {
		set IXs [.pred_info.preds.listbox curselection]
		if {$IXs != ""} then {
			set Pred [.pred_info.preds.listbox get [lindex $IXs 0] ]
		} else {
			set SelOwner [selection own]
			puts "Must try looking in section owner = $SelOwner"
			return
		}
	}
#puts "calling list_asm on $Mod:$Pred"
	prolog call builtins carry_out_listasm -atom "$Mod:$Pred"
}

##################################
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

##################################

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
	if {[winfo exists $WinName] == 1} then {
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
	show_window $WinName
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
	while {[eof $Stream] == 0} {
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

proc see_text {TextWin LTextWin StartLine StartChar EndLine EndChar} {
	set HH [$TextWin cget -height]
	set Slop2 [expr ($HH - ($EndLine - $StartLine)) / 2]
	$TextWin see $StartLine.$StartChar
	$LTextWin see $StartLine.0
	$TextWin see $EndLine.$EndChar
	$LTextWin see $EndLine.0
	$TextWin see [expr $EndLine + $Slop2].$EndChar
	$LTextWin see [expr $EndLine + $Slop2].0
}

proc source_trace_closedown {STWin} {
	global DebugResponse 

	set DebugResponse Ba
	destroy $STWin
}

proc debugwin_configure_event {Win Ht Wd WW} {
	set FD [$WW cget -font]
	set FM [font measure $FD -displayof $WW m]
	set WWD [winfo width $WW]
	prolog call debugger set_debugwin_width -number $FM -number $WWD
}

proc show_debug_settings {} {
	global array proenv

	prolog call debugger get_maxdepth -atom debugger_output -var CurDepth
	set proenv(debug_print_depth) $CurDepth

	prolog call debugger get_depth_computation -atom debugger_output -var DC
	set proenv(db_flatness) $DC

	Window show .debug_settings 
	post_debug_subwin {Debug Settings} .debug_settings
}

proc show_pred_info {} {
	global array proenv

	Window show .pred_info 
	post_debug_subwin {Predicate Info} .pred_info
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

global array sys_mods
proc set_system_modules_showing {} {
	global array sys_mods
	Window show .sys_mods
	prolog call builtins sys_mods_status -var ML
	foreach Item $ML {
		set Mod [lindex $Item 0]
		set S [lindex $Item 1]
		set sys_mods($Mod) $S
		checkbutton .sys_mods.$Mod -text $Mod -variable sys_mods($Mod) \
			-anchor w -command "prolog call debugger toggle_mod_show -atom $Mod" 
	 	pack .sys_mods.$Mod  -side top -anchor w -expand 0 -fill x 
	}
	wm geometry .sys_mods ""
	update
	set BaseGeom [wm geometry .sys_mods]
	set XPlace [string first "x" $BaseGeom]
	set WinWidth [string range $BaseGeom 0 [expr $XPlace - 1]]
	set WinHeight [string range $BaseGeom [expr $XPlace + 1] \
					[expr [string first "+" $BaseGeom] -1] ]
    wm minsize .sys_mods $WinWidth $WinHeight
	post_debug_subwin {System Modules} .sys_mods
}

##############################

proc err_indic {w Line StartPos CaratPos P3 } {
	global array proenv

	$w.text tag configure syntax_err_head -background #acec86
	$w.text tag add syntax_err_head $Line.0 $Line.$CaratPos

	$w.text tag configure syntax_err_tail -background #febad4
	$w.text tag add syntax_err_tail $Line.$CaratPos $Line.end

	$w.ltext insert $Line.end " >"
	$w.ltext tag add error_line $Line.0 $Line.end 
	set CurFont $proenv(.document,font)
	set EmpFont [list [lindex $CurFont 0] [lindex $CurFont 1] bold]
	$w.ltext tag configure error_line -foreground #ec5648 -font $EmpFont
}

proc syn_err_msg {EW LN Msg TxtW} {
#	.syn_errors.errlist.listbox insert end [format "% 8d  -  %s" $LN $Msg]
	$EW insert end [format "% 4d  -  %s" $LN $Msg]
}

proc error_focus_attn {w} {
	set EE [$w.listbox get [lindex [$w.listbox curselection] 0]]
	set LineNum [string range $EE 0 [expr [string first "-" $EE] - 1 ] ]
	set HH [expr ([$w.text cget -height] - 1) / 2]
	$w.text yview [expr $LineNum - $HH] 
	$w.ltext yview [expr $LineNum - $HH] 
}

proc err_indic0 {w Line} {
	global array proenv

	$w.text tag configure syntax_err_tail -background #febad4
	$w.text tag add syntax_err_tail $Line.0 $Line.end

	$w.ltext insert $Line.end " >"
	$w.ltext tag add error_line $Line.0 $Line.end 
	set CurFont $proenv(.document,font)
	set EmpFont [list [lindex $CurFont 0] [lindex $CurFont 1] bold]
	$w.ltext tag configure error_line -foreground #ec5648 -font $EmpFont
}
##############################

proc start_edit_find { w } {
	global proenv
	set proenv(current_search_window) $w
	set proenv($w,searchpos) 1.0
	set proenv($w,searchdirect) forward
	set proenv($w,searchnature) exact
	Window show .find_repl
	.find_repl.wintgt.label configure -text [wm title $w]
	.find_repl.f1.whichwin configure -text $w
	post_open_document Find .find_repl
}

proc edit_find_next {} {
	global proenv
	set w [.find_repl.f1.whichwin cget -text]
	set Pattern [.find_repl.search.entry get]
	if {$Pattern == ""} then {
		bell
		tk_messageBox -icon info -parent .find_repl -title "Bad Pattern" \
			-message "Can't search for the empty string!" -type ok
		return
	}
	set proenv(searchdirect) $proenv($w,searchdirect)
	set Direct -$proenv($w,searchdirect)
	set Nature -$proenv($w,searchnature)
	set StartIndex $proenv($w,searchpos)

	if {$proenv($w,searchdirect) == "forward"} then {
		set sresult [$w.text search $Direct $Nature -count MLen --  $Pattern $StartIndex ]
	} else {
		set sresult [$w.text search $Direct $Nature -count MLen --  $Pattern $StartIndex ]
	}
	if {$sresult == ""} then {
		bell
		set proenv($w,searchpos) 1.0
	} else {
		set MLNum [string range $sresult 0 [expr [string first "." $sresult] -1 ] ]
		set MLCharStart [string range $sresult [expr [string first "." $sresult] + 1] end]
		set MatchEnd [expr $MLCharStart + $MLen] 
		raise $w
		focus $w.text
		$w.text see $sresult
		$w.text tag remove sel 1.0 end 
		$w.text tag add sel $sresult $MLNum.$MatchEnd
		$w.text mark set insert $sresult
		if {$proenv($w,searchdirect) == "forward"} then {
			set proenv($w,searchpos) $MLNum.$MatchEnd
		} else {
			if { $MLCharStart > 0 } then {
				set proenv($w,searchpos) $MLNum.[expr $MLCharStart - 1]
			} elseif { ($MLCharStart == 0) && ($MLNum > 1) } then {
				set proenv($w,searchpos) [expr $MLNum - 1].end
			} else {
				set proenv($w,searchpos) 1.0
			}
		}
	}
}

proc edit_replace {} {
	set w [.find_repl.f1.whichwin cget -text]
	if { [$w.text tag ranges sel] != "" } then {
    	set New [.find_repl.replace.entry  get]
		$w.text mark set insert sel.first
 		if {![catch {$w.text delete sel.first sel.last}]} {
			$w.text insert insert $New
			set proenv($w,dirty) true
		}
	}
}

proc edit_find_replace {} {
	edit_find_next
	edit_replace
}


##############################

proc process_typ {} {
	prolog call alsdev process_typ
}

proc process_oop {} {
	prolog call alsdev process_oop
}




##############################
proc run_cref  {} {
	prolog call alsdev run_cref
}

###########################################
# Mac OS X Support via tk::mac functions
#------------------------------------------

if {[tk windowingsystem] == "aqua"} {
	proc ::tk::mac::Quit {} {
		exit_prolog
	}

	proc ::tk::mac::OpenDocument {args} {
		foreach file $args {
			if { [file extension "$file"] == ".ppj" } then {
				prolog call alsdev launch_project -atom $file
			} else {
				document.open $file
			}
		}
	}
}

proc do_2col {base} {
 	vTclWindow.columns2_select $base
	window show $base

}

###############________________________________##################
###############________________________________##################


if {[tk windowingsystem] == "aqua"} {
	# Make .topals.mmenb the default menu for all windows.
	. configure -menu .topals.mmenb
}

Window show .topals

# update idletasks seems to push .topals behind other windows on
# Windows, so just call update.

if {[tk windowingsystem] == "win32"} then {
	update
} else {
	update idletasks
}

raise .topals
wm positionfrom .topals user
wm geometry .topals $proenv(.topals,geometry)
focus .topals.text

# Call AttachOpenDocumentHandler from the OpenDocument package to
# install a custom window procedure on .topals window for handling
# document opened form the Explorer.

if {[tk windowingsystem] == "win32"} {
	AttachOpenDocumentHandler
}
