#!/usr/local/bin/wish
#############################################################################
# Visual Tcl v1.08 Project
#

#################################
# GLOBAL VARIABLES
#
global widget; 
global array proenv; 

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
        show { eval "vTclWindow$name $name" }
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
	global array proenv

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

	wm protocol $base WM_DELETE_WINDOW {hide_me .topals}

		##------------------
		## Main menubar:
		##------------------

    menu .topals.mmenb -relief sunken -tearoff 0

	## Project:
	menu .topals.mmenb.project -relief raised
    .topals.mmenb add cascade -label Project -menu .topals.mmenb.project 
	.topals.mmenb.project add command \
		-label "Open Project" -command open_project -state disabled
	.topals.mmenb.project add command \
		-label "New Project" -command new_project -state disabled
	.topals.mmenb.project add command \
		-label "Save Project" -command save_project -state disabled
	.topals.mmenb.project add command \
		-label "Save As Project" -command save_as_project -state disabled
	.topals.mmenb.project add command \
		-label "Close Project" -command close_project -state disabled
    .topals.mmenb.project add separator
    .topals.mmenb.project add command -label Halt -command exit_prolog

	## File:
	menu .topals.mmenb.file -relief raised
    .topals.mmenb add cascade -label File -menu .topals.mmenb.file 
	.topals.mmenb.file add command \
		-label "Add File to Project" -command add_file_to_project -state disabled
	.topals.mmenb.file add command \
		-label "Delete File from Project" -command delete_file_from_project -state disabled
    .topals.mmenb.file add separator
    .topals.mmenb.file add command -label Reconsult -command reconsult
    .topals.mmenb.file add command \
        -label {Set Directory} -command set_directory 
    .topals.mmenb.file add command -label {Source Tcl} -command source_tcl -state disabled
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
    .topals.mmenb.edit add command -label Cut \
		-command {testitall}
    .topals.mmenb.edit add separator
    .topals.mmenb.edit add command -label {Flush Input} -state disabled

	## Settings:
	menu .topals.mmenb.settings   -relief raised -tearoff 0
    .topals.mmenb add cascade -label Settings -menu .topals.mmenb.settings 
    .topals.mmenb.settings add cascade \
        -label "Prolog Flags" \
		-menu .topals.mmenb.settings.flags -state active
	menu .topals.mmenb.settings.flags -cursor {} -tearoff 0
	.topals.mmenb.settings.flags add command \
		-label "Dynamic Flags" \
		-command show_dynamic_flags
	.topals.mmenb.settings.flags add cascade \
		-label "Static Flags" \
		-menu .topals.mmenb.settings.flags.static -state active
	menu .topals.mmenb.settings.flags.static -cursor {} -title "Static Flags"
    .topals.mmenb.settings add command \
			-label {Other Settings} -command {Window show .settings}

	## Tools:
	menu .topals.mmenb.tools  -relief raised
    .topals.mmenb add cascade -label Tools -menu .topals.mmenb.tools
		## Debugger:
    .topals.mmenb.tools add checkbutton \
        -label Debugger -command exec_toggle_debugwin -variable proenv(debugwin)
		## DefStructs:
	menu .topals.mmenb.tools.tclshell -relief raised
    .topals.mmenb.tools add cascade \
        -label {Tcl Shell} -menu .topals.mmenb.tools.tclshell -state disabled
    .topals.mmenb.tools.tclshell add command \
		-label "User Defined" -command {prolog_tcltk_shell user_def_choice}
    .topals.mmenb.tools.tclshell add command \
		-label "shl_tcli (System - Danger!)" \
		-command {prolog_tcltk_shell shl_tcli}

    .topals.mmenb.tools add separator 
		## DefStructs:
	menu .topals.mmenb.tools.defstr -relief raised
    .topals.mmenb.tools add cascade \
        -label {Structs} -menu .topals.mmenb.tools.defstr
    .topals.mmenb.tools.defstr add command \
		-label "Define New" -command new_defstruct
    .topals.mmenb.tools.defstr add command \
		-label "Edit" -command edit_defstruct -state disabled

	## Help:
	menu .topals.mmenb.help  -relief raised
    .topals.mmenb add cascade -label Help -menu .topals.mmenb.help

		##------------------------------------
		## Directory notif & Interrupt button:
		##------------------------------------
    frame .topals.cpd19 \
		-borderwidth 1 -height 30 -relief raised -width 30 
    label .topals.cpd19.01 -relief flat -text "Directory:"
    entry .topals.cpd19.02 \
		-textvariable proenv(cwd) \
		-highlightthickness 0 
    button .topals.cpd19.03 \
        -font {lucida 10 bold} \
        -foreground #ff0000 -padx 11 -pady 4 \
		-text Interrupt \
        -command interrupt_action

		##------------------
		## Text Window:
		##------------------
    frame .topals.txwin \
		-borderwidth 1 -height 30 -relief raised 
    scrollbar .topals.txwin.02 \
        -background $alsstyl(button-background) \
		-command {.topals.txwin.text yview} \
        -orient vert 
    text .topals.txwin.text \
        -height 26 -width 8 \
		-font {system 10 normal} \
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
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side left 
    pack .topals.cpd19.03 \
        -anchor center -expand 0 -fill x -padx 2 -pady 2 -side right 

    pack .topals.txwin \
        -anchor nw -expand yes -fill both -side top 
    grid columnconf .topals.txwin 0 -weight 1
    grid rowconf .topals.txwin 0 -weight 1
    grid .topals.txwin.02 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid .topals.txwin.text \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 

}



proc vTclWindow.dyn_flags {base} {
	global array proenv

    set base .dyn_flags
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm resizable $base 0 0
    wm overrideredirect $base 0
    wm deiconify $base
    wm title $base "Changable Prolog Flags"
	wm protocol .dyn_flags WM_DELETE_WINDOW {wm withdraw .dyn_flags}

    ###################
    # SETTING GEOMETRY
    ###################

}

proc vTclWindow.settings {base} {
	global array proenv

    set base .settings
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 412x121+326+186
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "ALSDEV Settings"
	wm protocol $base WM_DELETE_WINDOW {hide_me .settings}

    frame $base.obps \
        -borderwidth 1 -relief sunken 
    label $base.obps.label \
        -borderwidth 0 -padx 6 -text {*.obp locations:} 
	set OBPMenu [tk_optionMenu $base.obps.optmenu proenv(obplcn) \
		{Generated in Current (gic)} {Generated in Source(gis)} \
		{Generated in Arch/Current (giac)} {Generated in Arch/Source(gias)}  ]
	set proenv(obplcn) {Generated in Arch/Source(gias)}

    frame $base.event_intv \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.event_intv.label \
        -padx 6 -text {Event interval (ms):} 
    scale $base.event_intv.scale \
        -font {Helvetica -10 bold} -orient horiz -to 1000.0 \
        -variable EventInterval -width 10 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.obps \
        -anchor center -expand 0 -fill x -side top 
    pack $base.obps.label \
        -anchor e -expand 0 -fill none -side left 
    pack $base.obps.optmenu \
        -anchor center -expand 0 -fill none -side top 
    pack $base.event_intv \
        -anchor center -expand 0 -fill x -side top 
    pack $base.event_intv.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.event_intv.scale \
        -anchor center -expand 0 -fill x -side top 
}







Window show .
Window show .topals
raise .topals

#main $argc $argv
