#----------------------------------------------------------------------#
#		twigs_id.tcl
#	Copyright (c) 1998 Applied Logic Systems, Inc.
#
#	Visual interface for the "Hickory Tree Identification Example"
#	Widget descriptions developed using VTCL 1.10
#----------------------------------------------------------------------#
global array agv

set agv(bud_scales) unknown
set agv(bud_color) unknown
set agv(terminal_buds) unknown
set agv(outer_scales) unknown
set agv(twigs) unknown

proc set_unknown {} {
	global agv; 

	set agv(bud_scales) unknown
	set agv(bud_color) unknown
	set agv(terminal_buds) unknown
	set agv(outer_scales) unknown
	set agv(twigs) unknown
}

proc {Window} {args} {
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
            if {$exists == "1" && $name != "."} {wm deiconify $name; return}
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

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $base passive
    wm geometry $base 200x200+0+0
    wm maxsize $base 1028 753
    wm minsize $base 104 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt"
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.tree_id {base} {
    if {$base == ""} {
        set base .tree_id
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 300x219+413+56
    wm maxsize $base 1028 753
    wm minsize $base 104 1
    wm overrideredirect $base 0
    wm resizable $base 0 0
    wm deiconify $base
    wm title $base "Hickory Tree Identification"
    label $base.heading \
        -borderwidth 1 -text {Specify observations...} 
    frame $base.bud_scales \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.bud_scales.label \
        -borderwidth 1 -font {{MS Sans Serif} 8 bold} -text {Bud Scales:} 
    radiobutton $base.bud_scales.valvate \
        -text Valvate -value valvate -variable agv(bud_scales) 
    radiobutton $base.bud_scales.imbricate \
        -text Imbricate -value imbricate -variable agv(bud_scales) 
    radiobutton $base.bud_scales.unknown \
        -text Unknown -value unknown -variable agv(bud_scales) 
    frame $base.bud_color \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.bud_color.label \
        -borderwidth 1 -font {{MS Sans Serif} 8 bold} -text {Bud Color:} 
    radiobutton $base.bud_color.yellow \
        -text Yellow -value yellow -variable agv(bud_color) 
    radiobutton $base.bud_color.brownish \
        -text Brownish -value brownish -variable agv(bud_color) 
    radiobutton $base.bud_color.unknown \
        -text Unknown -value unknown -variable agv(bud_color) 
    frame $base.terminal_buds \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.terminal_buds.label \
        -borderwidth 1 -font {{MS Sans Serif} 8 bold} -text {Terminal Buds:} 
    radiobutton $base.terminal_buds.short \
        -text Short -value short -variable agv(terminal_buds) 
    radiobutton $base.terminal_buds.large \
        -text Large -value large -variable agv(terminal_buds) 
    radiobutton $base.terminal_buds.unknown \
        -text Unknown -value unknown -variable agv(terminal_buds) 
    frame $base.outer_scales \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.outer_scales.outer_scales \
        -borderwidth 1 -font {{MS Sans Serif} 8 bold} -text {Outer Scales:} 
    radiobutton $base.outer_scales.persistent \
        -text Persistent -value persistent -variable agv(outer_scales) 
    radiobutton $base.outer_scales.deciduous \
        -text Deciduous -value deciduous -variable agv(outer_scales) 
    radiobutton $base.outer_scales.unknown \
        -text Unknown -value unknown -variable agv(outer_scales) 
    frame $base.twigs \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.twigs.label \
        -borderwidth 1 -font {{MS Sans Serif} 8 bold} -text Twigs: 
    radiobutton $base.twigs.orange_brown \
        -text Orange_brown -value orange_brown -variable agv(twigs) 
    radiobutton $base.twigs.reddish_brown \
        -text Reddish_brown -value reddish_brown -variable agv(twigs) 
    radiobutton $base.twigs.unknown \
        -text Unknown -value unknown -variable agv(twigs) 
    frame $base.buttons \
        -borderwidth 2 -height 75 -relief groove -width 125 
    button $base.buttons.start \
	-font {{MS Sans Serif} 8 bold} \
        -text Reset -command {prolog call identify reset}
    button $base.buttons.identify \
	-font {{MS Sans Serif} 8 bold} -foreground #ff0000 \
        -text Identify -command {prolog call identify identify}
    frame $base.result \
        -borderwidth 2 -height 75 -relief groove -width 125 
    label $base.result.label \
        -borderwidth 1 -text {Tree Name:} 
    label $base.result.answer \
        -background #ffffff -borderwidth 1 -font {{MS Sans Serif} 8 bold} \
        -relief sunken -height 2 -text ? 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.heading \
        -in .tree_id -anchor center -expand 0 -fill none -side top 
    pack $base.bud_scales \
        -in .tree_id -anchor center -expand 0 -fill x -side top 
    pack $base.bud_scales.label \
        -in .tree_id.bud_scales -anchor center -expand 0 -fill none \
        -side left 
    pack $base.bud_scales.valvate \
        -in .tree_id.bud_scales -anchor center -expand 0 -fill none \
        -side left 
    pack $base.bud_scales.imbricate \
        -in .tree_id.bud_scales -anchor center -expand 0 -fill none \
        -side left 
    pack $base.bud_scales.unknown \
        -in .tree_id.bud_scales -anchor center -expand 0 -fill none -side top 
    pack $base.bud_color \
        -in .tree_id -anchor center -expand 0 -fill x -side top 
    pack $base.bud_color.label \
        -in .tree_id.bud_color -anchor center -expand 0 -fill none -side left 
    pack $base.bud_color.yellow \
        -in .tree_id.bud_color -anchor center -expand 0 -fill none -side left 
    pack $base.bud_color.brownish \
        -in .tree_id.bud_color -anchor center -expand 0 -fill none -side left 
    pack $base.bud_color.unknown \
        -in .tree_id.bud_color -anchor center -expand 0 -fill none -side top 
    pack $base.terminal_buds \
        -in .tree_id -anchor center -expand 0 -fill x -side top 
    pack $base.terminal_buds.label \
        -in .tree_id.terminal_buds -anchor center -expand 0 -fill none \
        -side left 
    pack $base.terminal_buds.short \
        -in .tree_id.terminal_buds -anchor center -expand 0 -fill none \
        -side left 
    pack $base.terminal_buds.large \
        -in .tree_id.terminal_buds -anchor center -expand 0 -fill none \
        -side left 
    pack $base.terminal_buds.unknown \
        -in .tree_id.terminal_buds -anchor center -expand 0 -fill none \
        -side top 
    pack $base.outer_scales \
        -in .tree_id -anchor center -expand 0 -fill x -side top 
    pack $base.outer_scales.outer_scales \
        -in .tree_id.outer_scales -anchor center -expand 0 -fill none \
        -side left 
    pack $base.outer_scales.persistent \
        -in .tree_id.outer_scales -anchor center -expand 0 -fill none \
        -side left 
    pack $base.outer_scales.deciduous \
        -in .tree_id.outer_scales -anchor center -expand 0 -fill none \
        -side left 
    pack $base.outer_scales.unknown \
        -in .tree_id.outer_scales -anchor center -expand 0 -fill none \
        -side top 
    pack $base.twigs \
        -in .tree_id -anchor center -expand 0 -fill x -side top 
    pack $base.twigs.label \
        -in .tree_id.twigs -anchor center -expand 0 -fill none -side left 
    pack $base.twigs.orange_brown \
        -in .tree_id.twigs -anchor center -expand 0 -fill none \
        -side left 
    pack $base.twigs.reddish_brown \
        -in .tree_id.twigs -anchor center -expand 0 -fill none \
        -side left 
    pack $base.twigs.unknown \
        -in .tree_id.twigs -anchor center -expand 0 -fill none \
        -side top 
    pack $base.buttons \
        -in .tree_id -anchor center -expand 0 -fill x -side top 
    pack $base.buttons.start \
        -in .tree_id.buttons -anchor center -expand 0 -fill none \
		-side left -padx 15
    pack $base.buttons.identify \
        -in .tree_id.buttons -anchor center -expand 0 -fill none \
		-side right -padx 15
    pack $base.result \
        -in .tree_id -anchor center -expand 0 -fill x -side top 
    pack $base.result.label \
        -in .tree_id.result -anchor center -expand 0 -fill none -side left 
    pack $base.result.answer \
        -in .tree_id.result -anchor center -expand 0 -fill x -side top 

	wm geometry $base ""
}

proc obtain_desc {} {
    global agv
    return [list $agv(bud_scales) $agv(bud_color) $agv(terminal_buds) \
              $agv(outer_scales) $agv(twigs) ]
}

proc report_id { ID } {
	.tree_id.result.answer configure -text $ID
	bell
}

proc clear_report { } {
	.tree_id.result.answer configure -text {}
}

proc set_all_buttons { Color } {
	set TL {\
		{bud_scales {valvate imbricate} } \
		{bud_color  { yellow brownish } } \
		{terminal_buds  { short large } } \
		{outer_scales  { deciduous persistent } } \
		{twigs  { reddish_brown orange_brown } }            }
	foreach X $TL {
		set Func [lindex $X 0]
		set Vals [lindex $X 1]
		foreach Y $Vals {
			set Btn ""
			append Btn .tree_id. $Func . $Y
			$Btn configure -selectcolor $Color
		}
	}
}


Window show .
Window show .tree_id


