#!/usr/local/bin/wish
#############################################################################
# Visual Tcl v1.08 Project
#

#################################
# GLOBAL VARIABLES
#
global widget; 
#################################
# USER DEFINED PROCEDURES
#
proc init {argc argv} {
}

init $argc $argv

proc main {argc argv} {
}

proc Window {args} {
global vTcl
    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set rest [lrange $args 2 end]
    if {$name == "" || $cmd == ""} {return}
    set exists [winfo exists $name]
    switch $cmd {
        show {
            if {[info procs vTclWindow(pre)$name] != ""} {
                vTclWindow(pre)$name $rest
            }
            if {[info procs vTclWindow$name] != ""} {
                vTclWindow$name
            }
            if {[info procs vTclWindow(post)$name] != ""} {
                vTclWindow(post)$name $rest
            }
        }
        hide    { if $exists {wm withdraw $name; return} }
        iconify { if $exists {wm iconify $name; return} }
        destroy { if $exists {destroy $name; return} }
    }
}

#################################
# VTCL GENERATED GUI PROCEDURES
#

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

set DefaultGray #d9d9d9
set LightGray #f5f5f5
set DarkGray #b0b0b0
set PureBlue #0000fe

set alsstyl(label-font) $MainFont
set alsstyl(label-background) $DefaultGray
set alsstyl(label-foreground) #000000

set alsstyl(entry-font) $MainFont
set alsstyl(entry-background) $DefaultGray
set alsstyl(entry-foreground) #000000

set alsstyl(text-font) $MainFont
set alsstyl(text-background) $DefaultGray
set alsstyl(text-foreground) #000000

set alsstyl(button-font) $MainFont
set alsstyl(button-background) $DefaultGray
set alsstyl(button-foreground) #000000
set alsstyl(button-activebackground) #d6ccd6
#fdfd65

set alsstyl(frame-background) $DefaultGray

proc vTclWindow.topals {args} {
    set base .topals
    if {[winfo exists .topals]} {
        wm deiconify .topals; return
    }
	global alsstyl PureBlue
    ###################
    # CREATING WIDGETS
    ###################
    toplevel .topals -class Toplevel \
        -background $alsstyl(button-background) 
    wm focusmodel .topals passive
    wm geometry .topals 559x567+149+178
    wm maxsize .topals 1265 994
    wm minsize .topals 1 1
    wm overrideredirect .topals 0
    wm resizable .topals 1 1
    wm deiconify .topals
    wm title .topals "ALS Prolog Environment"

		##------------------
		## Main menubar:
		##------------------

    menu .topals.mmenb -relief sunken -tearoff 0

	## Project:
	menu .topals.mmenb.project -relief raised
    .topals.mmenb add cascade -label Project -menu .topals.mmenb.project 
	.topals.mmenb.project add command \
		-label "Open Project" -command open_project
	.topals.mmenb.project add command \
		-label "New Project" -command new_project
	.topals.mmenb.project add command \
		-label "Save Project" -command save_project
	.topals.mmenb.project add command \
		-label "Save As Project" -command save_as_project
	.topals.mmenb.project add command \
		-label "Close Project" -command close_project
    .topals.mmenb.project add separator
    .topals.mmenb.project add command -label Halt -command exit_prolog

	## File:
	menu .topals.mmenb.file -relief raised
    .topals.mmenb add cascade -label File -menu .topals.mmenb.file 
	.topals.mmenb.file add command \
		-label "Add File to Project" -command add_file_to_project
	.topals.mmenb.file add command \
		-label "Delete File from Project" -command delete_file_from_project
    .topals.mmenb.file add separator
    .topals.mmenb.file add command -label Reconsult -command reconsult
    .topals.mmenb.file add command \
        -label {Set Directory} -command set_directory -state disabled
    .topals.mmenb.file add command -label {Source Tcl} -command source_tcl
    .topals.mmenb.file add separator
    .topals.mmenb.file add command -label Open -state disabled
    .topals.mmenb.file add command -label Close -state disabled

	## Edit:
	menu .topals.mmenb.edit  -relief raised
    .topals.mmenb add cascade -label Edit -menu .topals.mmenb.edit
    .topals.mmenb.edit add command \
        -label Copy -command {copy_text .topals.txwin.text }
    .topals.mmenb.edit add command \
        -label Paste -command {paste_text .topals.txwin.text }
    .topals.mmenb.edit add command -label Cut -state disabled
    .topals.mmenb.edit add separator
    .topals.mmenb.edit add command -label {Flush Input} -state disabled

	## Settings:
	menu .topals.mmenb.settings   -relief raised
    .topals.mmenb add cascade -label Settings -menu .topals.mmenb.settings
    .topals.mmenb.settings add cascade \
        -label "Obp Location" \
		-menu .topals.mmenb.settings.obp_lcns 
	menu .topals.mmenb.settings.obp_lcns -cursor {}
	.topals.mmenb.settings.obp_lcns add command \
		-label "Generated in Source/Arch" -command set_gias
	.topals.mmenb.settings.obp_lcns add command \
		-label "Generated in Current Dir/Arch" -command set_giac
    .topals.mmenb.settings add cascade \
        -label "Prolog Flags" \
		-menu .topals.mmenb.settings.flags 
	menu .topals.mmenb.settings.flags -cursor {}
	.topals.mmenb.settings.flags add command \
		-label "Dynamic Flags" -command {Window show .dyn_flags }
	.topals.mmenb.settings.flags add cascade \
		-label "Static Flags" -menu .topals.mmenb.settings.flags.static
	menu .topals.mmenb.settings.flags.static -cursor {}
    .topals.mmenb.settings add separator 

	## Tools:
	menu .topals.mmenb.tools  -relief raised
    .topals.mmenb add cascade -label Tools -menu .topals.mmenb.tools
    .topals.mmenb.tools add checkbutton \
        -label Debugger -command toggle_debugwin -variable debugwin_showing
	menu .topals.mmenb.tools.tclshell -relief raised
    .topals.mmenb.tools add cascade \
        -label {Tcl Shell} -menu .topals.mmenb.tools.tclshell
    .topals.mmenb.tools.tclshell add command \
		-label "User Defined" -command {prolog_tcltk_shell user_def_choice}
    .topals.mmenb.tools.tclshell add command \
		-label "shl_tcli (System - Danger!)" -command {prolog_tcltk_shell shl_tcli}
    .topals.mmenb.tools add separator 

	## Help:
	menu .topals.mmenb.help  -relief raised
    .topals.mmenb add cascade -label Help -menu .topals.mmenb.help

		##------------------
		## Directory notif:
		##------------------
    frame .topals.cpd19 \
		-borderwidth 1 -height 30 -relief raised -width 30 
    label .topals.cpd19.01 -relief flat -text "Directory:"
    entry .topals.cpd19.02 \
		-textvariable CurrentDirectory \
		-highlightthickness 0 

		##------------------
		## Text Window:
		##------------------
    frame .topals.txwin \
		-borderwidth 1 -height 30 -relief raised 
    scrollbar .topals.txwin.02 \
        -background $alsstyl(button-background) -command {.topals.txwin.text yview} \
        -orient vert 
    text .topals.txwin.text \
        -height 26 -width 8 \
        -yscrollcommand {.topals.txwin.02 set} \
		-exportselection true

    ###################
    # SETTING GEOMETRY
    ###################

	.topals configure -menu .topals.mmenb

    pack .topals.cpd19 \
        -anchor center -expand 0 -fill x -side top 
    pack .topals.cpd19.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack .topals.cpd19.02 \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 

    pack .topals.txwin \
        -anchor nw -expand yes -fill both -side top 
    grid columnconf .topals.txwin 0 -weight 1
    grid rowconf .topals.txwin 0 -weight 1
    grid .topals.txwin.02 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid .topals.txwin.text \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 

}

proc vTclWindow.input_popup {} {

    set base .input_popup
	global  input_popup_wait

    ###################
    # CREATING WIDGETS
    ###################

    toplevel $base -class Toplevel 
    wm focusmodel $base passive
    wm geometry $base 407x130+50+317
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "input_popup"
    frame $base.input_p \
        -borderwidth 1 -height 30 -relief sunken
    label $base.input_p.input_popup_head \
        -borderwidth 2 \
        -font {lucida 10 bold} \
        -foreground #000000000000 -relief groove \
        -text {Type the name of the configuration:} 
    entry $base.input_p.input_popup_entry \
        -font {lucida 10 bold} \
        -foreground #000000000000 
	bind $base.input_p.input_popup_entry <Return> \
		"set input_popup_wait [$base.input_p.input_popup_entry get]"
    frame $base.input_p.btns \
        -borderwidth 1 -height 30 -relief flat
    button $base.input_p.btns.ok \
        -command "set input_popup_wait [$base.input_p.input_popup_entry get]" \
        -font {lucida 10 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text OK 
    button $base.input_p.btns.cancel \
        -command {set input_popup_wait ""} \
        -font {lucida 10 bold} \
        -foreground #ffffff -padx 11 -pady 4 -text Cancel 
    ###################
    # SETTING GEOMETRY
    ###################

    pack $base.input_p \
		-anchor w -expand 1 -fill both  -side top -padx 4 -pady 4

    pack $base.input_p.input_popup_head \
        -anchor center -expand 0 -fill none -side top -pady 8 

    pack $base.input_p.input_popup_entry \
        -anchor center -expand 0 -fill x -side top -pady 8 -padx 8

    pack $base.input_p.btns \
        -anchor w -expand 1 -fill x -side top -padx 25 

    pack $base.input_p.btns.ok \
        -anchor center -expand 0 -fill none -padx 4 -side left 

    pack $base.input_p.btns.cancel \
        -anchor center -expand 0 -fill none -padx 4 -side right 
}

proc vTclWindow.dyn_flags {} {
    set base .dyn_flags
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 210x381+55+90
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Changable Prolog Flags"

    frame $base.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.buttons.accept \
        -padx 11 -pady 4 -text Accept \
		-command change_prolog_flags
    button $base.buttons.cancel \
        -padx 11 -pady 4 -text Cancel \
		-command {Window hide .dyn_flags}
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.buttons \
        -anchor center -expand 0 -fill x -side bottom 
    pack $base.buttons.accept \
        -anchor center -expand 0 -fill none -padx 5 -side left 
    pack $base.buttons.cancel \
        -anchor center -expand 0 -fill none -padx 5 -side right 

}

Window show .
Window show .topals
Window show .input_popup
Window hide .input_popup
Window show .dyn_flags
Window hide .dyn_flags

main $argc $argv
