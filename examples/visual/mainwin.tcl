#============================================================================
#		mainwin.tcl
#		Copyright (c) 1998-99 Applied Logic Systems, Inc.
#
#		Tk side of a "main window" for a Prolog application...
#	
#		Initially developed using:
# 			Visual Tcl v1.09 
#		to provide initial drawings and specifications of components
#============================================================================

#--------------------------------------------------------------------
#	.mainwin
#--------------------------------------------------------------------

	## The global array "agv" provides values for such things as
	## background colors, fonts, etc., in a uniform way:

global array agv
 
	 ## Colors & Fonts:
	  
proc setup_font {} {
	global agv
	if {"$agv(text,sizeunits)"=="pixels"} then {
		set Size [ expr 0 - $agv(text,size) ]
	} else {
		set Size $agv(text,size)
	}
	set Font [list $agv(text,family) $Size $agv(text,style) ]
	set agv(text,font) $Font
}

set BasicGray #d9d9d9
set agv(frame,background)   $BasicGray
set agv(label,background)   #dcd48e
set agv(menubar,background) $BasicGray
set agv(menu,background)    $BasicGray
set agv(menubtn,background) $BasicGray
set agv(text,highlightcolor) #e0588e
set agv(text,background)    $BasicGray
set agv(text,foreground)    black
set agv(text,family)        helvetica
set agv(text,size)          12
set agv(text,sizeunits)     pixels
set agv(text,style)         normal
set agv(entry,background)   $BasicGray
set agv(dismiss,background) #fe0000
set agv(dismiss,foreground) #ffffff
set agv(ok,background)      #10fe2c

setup_font

proc vTclWindow.mainwin {base} {
	global agv

    if {$base == ""} {
        set base .mainwin
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -background $agv(frame,background) 

    wm focusmodel $base passive
    wm geometry $base 496x394+473+157
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Samples Main Window for ALS Prolog Applications"

    menu $base.menubar -tearoff 0 -background $agv(menubar,background) -relief sunken 

    menu $base.menubar.projects -background $agv(menu,background) -relief raised
    $base.menubar add cascade -label {Projects} -menu $base.menubar.projects
    $base.menubar.projects add command \
        -background $agv(menubtn,background) -label {Load Project File}  \
		-command load_existing_project 
    $base.menubar.projects add command \
        -background $agv(menubtn,background) -label {New Project}  \
		-command start_new_project 
    $base.menubar.projects add command \
        -background $agv(menubtn,background) -label {Save Project}  \
		-command save_project 
    $base.menubar.projects add command \
        -background $agv(menubtn,background) -label {Save Project As}  \
		-command save_project_as 
    $base.menubar.projects add command \
        -background $agv(menubtn,background) -label {Delete Project}  \
		-command delete_project 
    $base.menubar.projects add command \
        -background $agv(menubtn,background) -label {Whatever Project}  \
		-command whatever_project 
    $base.menubar.projects add separator
    $base.menubar.projects add command \
        -background $agv(dismiss,background) -foreground $agv(dismiss,foreground) \
		-label Quit -command exit_mainwin 

		### Look in als_menu.tcl for how we actually set up the
		### edit commands for the ALS IDE.....
    menu $base.menubar.rules -background $agv(menu,background) -relief raised
    $base.menubar add cascade -label {Edit} -menu $base.menubar.rules
    $base.menubar.rules add command \
        -background $agv(menubtn,background) -label {Edit} \
		-command edit_existing_whatever -state disabled
    $base.menubar.rules add command \
        -background $agv(menubtn,background) -label {Clone} \
		-command clone_existing_whatever -state disabled
    $base.menubar.rules add command \
        -background $agv(menubtn,background) -label {Delete} \
		-command delete_existing_whatever -state disabled
    $base.menubar.rules add separator \
        -background $agv(menubtn,background) 

    menu $base.menubar.configs -background $agv(menubtn,background) -relief raised
    $base.menubar add cascade -label {Configurations} -menu $base.menubar.configs 
	$base.menubar.configs add command \
		-background $agv(menubtn,background) -label {Window Settings} \
		-command {Window show .my_app_settings} -state disabled
    $base.menubar.configs add separator
	$base.menubar.configs add command \
		-background $agv(menubtn,background) -label {Clear Main Window} \
		-command {.mainwin.txtwin.text delete 1.0 end}

    menu $base.menubar.help -background $agv(menubtn,background) -relief raised
    $base.menubar add cascade -label {Help} -menu $base.menubar.help
	$base.menubar.help add command \
		-background $agv(menubtn,background) -label {About} \
		-command about_my_app
	$base.menubar.help add command \
		-background $agv(menubtn,background) -label {Acrobat *.pdf} \
		-command {exec "acroread" "./my_app_sample.pdf" & } -state disabled
	$base.menubar.help add command \
		-background $agv(menubtn,background) -label {Netscape HTML} \
		-command {exec "netscape" "./my_app_sample.html" & } -state disabled

    frame $base.datasrc \
		-borderwidth 2 -height 30 -relief sunken 
    label $base.datasrc.label \
        -anchor w -text {Data Source:} 
    label $base.datasrc.value \
        -background $agv(entry,background) -anchor w -text {} -relief sunken 

    frame $base.txtwin \
		-borderwidth 1 -relief raised 
    scrollbar $base.txtwin.hsb \
		-command {.mainwin.txtwin.text xview} -orient horiz 
    scrollbar $base.txtwin.vsb \
		-command {.mainwin.txtwin.text yview} -orient vert 
    text $base.txtwin.text \
        -font $agv(text,font) \
		-background $agv(text,background) -foreground $agv(text,foreground) \
        -width 8 -xscrollcommand {.mainwin.txtwin.hsb set} \
        -yscrollcommand {.mainwin.txtwin.vsb set} 

    ###################
    # SETTING GEOMETRY
    ###################

	$base configure -menu $base.menubar

    pack $base.txtwin \
        -anchor nw -expand 1 -fill both -side left 
    grid columnconf $base.txtwin 0 -weight 1
    grid rowconf $base.txtwin 0 -weight 1
    grid $base.txtwin.hsb \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.txtwin.vsb \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.txtwin.text \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 

		## Binding for getting lines from main window via get_line/2:
	bind .mainwin.txtwin.text <Return>  \
		"xmit_myline .mainwin.txtwin.text  main_in"

}
global WaitForLine.mainwin.txtwin.text ; set WaitForLine.mainwin.txtwin.text 0

	## End mainwin....

proc about_my_app {} {
	tk_dialog .warn_dialog "About My Sample App"  \
		"My Sample App\nVersion 0.1\nDeveloped by\nMe and My Shadow" \
		"" 0 OK }

proc exit_mainwin {} {
		## do whatever....
	exit
}


		## Stub code....
		## Show some samples of using popups, etc:
proc load_existing_project {} {
		## simple call into prolog....
	prolog call user load_existing_project
}

proc start_new_project  {} {
		## 1st run a dialog in Tcl, then call into prolog with answer:

	set Ans [tk_dialog .pm_warn_dialog "Choice Interaction"  \
		"Please choose an option..." \
		"" 0 "Choice 1" "Choice 2" ]

	prolog call user start_new_project -number $Ans
}

proc save_project  {} {
	## Call directly into prolog & have it run a dialog:

	prolog call user save_project
}

proc save_project_as  {} {
		## a final example:
	prolog call user save_project_as
}

proc delete_project  {} {
		## a final example:
	prolog call user delete_project
}

proc whatever_project  {} {
		## a final example:
	prolog call user whatever_project
}

	## Utilities borrowed from files generated by VTCL:

proc Window {args} {
global vTcl
	set cmd [lindex $args 0]
	set name [lindex $args 1]
	set newname [lindex $args 2]
	set rest [lrange $args 3 end]
	if {$name == "" || $cmd == ""} {return}
	if {$newname == ""} {
		set newname $name
	}
	set exists [winfo exists $newname]
	switch $cmd {
		show {
			if {[info procs vTclWindow(pre)$name] != ""} {
				eval "vTclWindow(pre)$name $newname $rest"
			}
			if {[info procs vTclWindow$name] != ""} {
				eval "vTclWindow$name $newname $rest"
			}
			if {[info procs vTclWindow(post)$name] != ""} {
				eval "vTclWindow(post)$name $newname $rest"
			}
		}
		hide    { if $exists {wm withdraw $newname; return} }
		iconify { if $exists {wm iconify $newname; return} }
		destroy { if $exists {destroy $newname; return} }
	}
}

proc vTclWindow. {base} {
	if {$base == ""} {
	set base .
	}
	###################
	# CREATING WIDGETS
	###################
	wm focusmodel $base passive
	wm geometry $base 200x200+0+0
	wm maxsize $base 1137 870
	wm minsize $base 1 1
	wm overrideredirect $base 0
	wm resizable $base 1 1
	wm withdraw $base
	wm title $base "vt.tcl"
	###################
	# SETTING GEOMETRY
	###################
	}

	##############################################

proc xmit_myline {TxtWin StreamAlias} {
			## Return true last text line (index "end" gives
			## line after last newline):
	set EndIndex [$TxtWin index end]
	set EndLine [string range $EndIndex 0 [expr [string first "." $EndIndex] - 1 ]]
	set EndTextLine [expr $EndLine -1]
	set ThisLine [ $TxtWin get $EndTextLine.0 $EndTextLine.end]
	$TxtWin see end
	prolog call user activate_main_in -atom $ThisLine
}

