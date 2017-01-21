#!/usr/local/bin/wish
############################################################################
# Visual Tcl v1.09 Project


################################
# GLOBAL VARIABLES

global widget; 

#################################
# USER DEFINED PROCEDURES

proc init {argc argv} {

}

init $argc $argv


proc main {argc argv} {

}

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

################################
# VTCL GENERATED GUI PROCEDURES


proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $base passive
    wm geometry $base 1x1+0+0
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt.tcl"
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.vdc {base} {
    if {$base == ""} {
        set base .vdc
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
set CmnBack brown
    toplevel $base -class Toplevel -background $CmnBack
    wm focusmodel $base passive
    wm geometry $base 286x432+251+303
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "vdc"
    frame $base.in -relief flat -background $CmnBack
    label $base.in.inlbl \
        -background #dbb2c9 -borderwidth 3 -font {lucida 12 bold} \
        -foreground #000000000000 -relief groove -text Input: -width 7
    entry $base.in.indisp \
        -background #cec9c9 -font {lucida 12 bold} -foreground #000000000000 
    frame $base.out -relief flat -background $CmnBack 
    label $base.out.anslbl \
        -background #dbb0c9 -borderwidth 3 -font {lucida 12 bold} \
        -foreground #000000000000 -relief groove -text Answer: -width 7
    entry $base.out.ansdisp \
        -background #d2c9c9 -font {lucida 12 bold} -foreground #000000000000 

set BBack #b88cad

    frame $base.nums -relief groove -background $CmnBack
    button $base.nums.btn0 \
        -background $BBack -command {cbutton 0} -font {lucida 12 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text 0 
    button $base.nums.btn1 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 1 -command {cbutton 1}
    button $base.nums.btn2 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 2 -command {cbutton 2}
    button $base.nums.btn3 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 3 -command {cbutton 3}
    button $base.nums.btn4 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 4 -command {cbutton 4}
    button $base.nums.btn5 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 5 -command {cbutton 5}
    button $base.nums.btn6 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 6 -command {cbutton 6}
    button $base.nums.btn7 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 7 -command {cbutton 7}
    button $base.nums.btn8 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 8 -command {cbutton 8}
    button $base.nums.btn9 \
        -background $BBack -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 9 -command {cbutton 9}

    frame $base.ops -relief groove -background $CmnBack
    button $base.ops.btn_eq \
        -background $BBack -command "get_answer_in $base.in.indisp" \
	-font {lucida 14 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text = 
    button $base.ops.btn_plus \
        -background $BBack -command {cbutton +} -font {lucida 14 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text + 
    button $base.ops.btn_minus \
        -background $BBack -font {lucida 14 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text - -command {cbutton -}
    button $base.ops.btn_times \
        -background $BBack -font {lucida 14 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text * -command {cbutton *}
    button $base.ops.btn_div \
        -background $BBack -font {lucida 14 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text / -command {cbutton /}

    button $base.ops.clear_btn \
        -background $BBack -command clear_all -font {lucida 10 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text Clear
    button $base.ops.quitbtn \
        -background #f81616 -command quit_vdc -font {lucida 10 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text Quit 

    frame $base.defs -relief groove -background $CmnBack
    button $base.defs.define \
        -background $BBack -command "get_answer_def $base.defs.definition" \
	-font {lucida 12 bold}  -borderwidth 2 \
        -foreground #000000000000 -padx 2 -pady 0 -text Define -width 7
    entry $base.defs.definition \
        -background #d2c9c9 -font {lucida 12 bold} -foreground #000000000000 

    frame $base.deflist -relief groove -background $CmnBack
    listbox $base.deflist.listbox \
        -background #d2c9c9 -font {lucida 10 bold} -foreground #000000000000 \
	-height 3 -yscrollcommand "$base.deflist.vsb set" 
    scrollbar $base.deflist.vsb \
	-command "$base.deflist.listbox yview" -orient vert
	

    ###################
    # SETTING GEOMETRY
    ###################


    pack $base.in \
	-anchor center -side top -expand 0 -fill x
    pack $base.in.inlbl \
	-anchor center -side left -expand 0 -fill none
    pack $base.in.indisp \
	-anchor center -side left -expand 1 -fill x
    pack $base.out \
	-anchor center -side top -expand 0 -fill x
    pack $base.out.anslbl \
	-anchor center -side left -expand 0 -fill none
    pack $base.out.ansdisp \
	-anchor center -side left -expand 1 -fill x

    pack $base.nums -pady 8 \
	-anchor center -side top -expand 0 -fill none

	grid columnconf $base.nums 0 -weight 1 -pad 5
	grid columnconf $base.nums 1 -weight 1 -pad 5
	grid columnconf $base.nums 3 -weight 1 -pad 5
	grid columnconf $base.nums 4 -weight 1 -pad 5
	grid columnconf $base.nums 5 -weight 1 -pad 5

	grid rowconf $base.nums 0 -weight 1 -pad 4
	grid rowconf $base.nums 1 -weight 1 -pad 4
	grid rowconf $base.nums 2 -weight 1 -pad 4

    grid $base.nums.btn0 \
	-column 0 -row 0 -columnspan 1 -rowspan 1 
    grid $base.nums.btn1 \
	-column 1 -row 0 -columnspan 1 -rowspan 1 
    grid $base.nums.btn2 \
	-column 2 -row 0 -columnspan 1 -rowspan 1 
    grid $base.nums.btn3 \
	-column 3 -row 0 -columnspan 1 -rowspan 1 
    grid $base.nums.btn4 \
	-column 4 -row 0 -columnspan 1 -rowspan 1 
    grid $base.nums.btn5 \
	-column 0 -row 1 -columnspan 1 -rowspan 1 
    grid $base.nums.btn6 \
	-column 1 -row 1 -columnspan 1 -rowspan 1 
    grid $base.nums.btn7 \
	-column 2 -row 1 -columnspan 1 -rowspan 1 
    grid $base.nums.btn8 \
	-column 3 -row 1 -columnspan 1 -rowspan 1 
    grid $base.nums.btn9 \
	-column 4 -row 1 -columnspan 1 -rowspan 1 

    pack $base.ops -pady 4 \
	-anchor center -side top -expand 0 -fill none

	grid columnconf $base.ops 0 -weight 1 -pad 5
	grid columnconf $base.ops 1 -weight 1 -pad 5
	grid columnconf $base.ops 2 -weight 1 -pad 5
	grid columnconf $base.ops 3 -weight 1 -pad 5

	grid rowconf $base.ops 0 -weight 1 -pad 4
	grid rowconf $base.ops 1 -weight 1 -pad 4

    grid $base.ops.btn_plus \
	-column 0 -row 0 -columnspan 1 -rowspan 1 
    grid $base.ops.btn_minus \
	-column 1 -row 0 -columnspan 1 -rowspan 1 
    grid $base.ops.btn_times \
	-column 2 -row 0 -columnspan 1 -rowspan 1 
    grid $base.ops.btn_div \
	-column 3 -row 0 -columnspan 1 -rowspan 1 
    grid $base.ops.quitbtn \
	-column 0 -row 1 -columnspan 1 -rowspan 1 
    grid $base.ops.clear_btn \
	-column 1 -row 1 -columnspan 2 -rowspan 1 
    grid $base.ops.btn_eq \
	-column 3 -row 1 -columnspan 1 -rowspan 1 

    pack $base.defs -pady 4 \
	-anchor center -side top -expand 0 -fill x
    pack $base.defs.define \
	-anchor center -side left -expand 0 -fill none
    pack $base.defs.definition \
	-anchor center -side left -expand 1 -fill both

    pack $base.deflist -pady 2 \
	-anchor center -side top -expand 0 -fill x

	grid columnconf $base.deflist 0 -weight 1
	grid columnconf $base.deflist 1 -weight 0
	grid rowconf $base.deflist 0 -weight 1
    grid $base.deflist.listbox \
	-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nsew
    grid $base.deflist.vsb \
	-column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	
	wm geometry $base ""
}


####################################################################

proc cbutton {Arg} {
    .vdc.in.indisp insert end $Arg
}

proc quit_vdc {} {
	destroy .vdc
}

proc clear_all {} {
    .vdc.in.indisp delete 0 end 
    .vdc.out.ansdisp delete 0 end 
    .vdc.defs.definition delete 0 end 
}

proc get_answer_in {Source} {
    	set input [$Source get]
	if {"$input"== "" } then {
		bell
		return
	}
	prolog call desk_calc vdc -atom $input -var answer
    .vdc.out.ansdisp delete 0 end 
    .vdc.out.ansdisp insert 0 $answer
}

proc get_answer_def {Source} {
    	set input [$Source get]
	if {"$input"== "" } then {
		bell
		return
	}
	prolog call desk_calc vdc -atom $input -var answer
    .vdc.out.ansdisp delete 0 end 
    .vdc.out.ansdisp insert 0 $answer
	if {$answer != "bad_input" } then {
    	.vdc.deflist.listbox insert end $input
	}
}

Window show .
Window show .vdc
