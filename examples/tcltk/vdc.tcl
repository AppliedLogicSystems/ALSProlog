#!/usr/local/bin/wish
#############################################################################
# Visual Tcl v1.09 Project
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

#init $argc $argv


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
    toplevel $base -class Toplevel \
        -background #DC00C9E1C9E1 
    wm focusmodel $base passive
    wm geometry $base 286x332+251+303
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "vdc"
    frame $base.vdcfr \
        -background #12641a -borderwidth 1 -height 30 -relief sunken \
        -width 30 
    label $base.vdcfr.inlbl \
        -background #dbb2c9 -borderwidth 2 -font {lucida 12 bold} \
        -foreground #000000000000 -relief groove -text Input: 
    label $base.vdcfr.anslbl \
        -background #dbb0c9 -borderwidth 2 -font {lucida 12 bold} \
        -foreground #000000000000 -relief groove -text Answer: 
    entry $base.vdcfr.indisp \
        -background #cec9c9 -font {lucida 12 bold} -foreground #000000000000 
    entry $base.vdcfr.ansdisp \
        -background #d2c9c9 -font {lucida 12 bold} -foreground #000000000000 
    button $base.vdcfr.btn0 \
        -background #b88cad -command {cbutton 0} -font {lucida 12 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text 0 
    button $base.vdcfr.btn1 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 1 -command {cbutton 1}
    button $base.vdcfr.btn2 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 2 -command {cbutton 2}
    button $base.vdcfr.btn3 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 3 -command {cbutton 3}
    button $base.vdcfr.btn4 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 4 -command {cbutton 4}
    button $base.vdcfr.btn5 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 5 -command {cbutton 5}
    button $base.vdcfr.btn6 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 6 -command {cbutton 6}
    button $base.vdcfr.btn7 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 7 -command {cbutton 7}
    button $base.vdcfr.btn8 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 8 -command {cbutton 8}
    button $base.vdcfr.btn9 \
        -background #b88cad -font {lucida 12 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text 9 -command {cbutton 9}
    button $base.vdcfr.btn_eq \
        -background #db5e16 -command get_answer -font {lucida 24 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text = 
    button $base.vdcfr.btn_plus \
        -background #dbac36 -command {cbutton +} -font {lucida 18 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text + 
    button $base.vdcfr.btn_minus \
        -background #dbac36 -font {lucida 18 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text - -command {cbutton -}
    button $base.vdcfr.btn_times \
        -background #dbac36 -font {lucida 18 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text * -command {cbutton *}
    button $base.vdcfr.btn_div \
        -background #dbac36 -font {lucida 18 bold} -foreground #000000000000 \
        -padx 11 -pady 4 -text / -command {cbutton /}
    button $base.vdcfr.clear_btn \
        -background #d9fb12 -command clear_all -font {lucida 14 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text Clear
    button $base.vdcfr.quitbtn \
        -background #f81616 -command quit_vdc -font {lucida 14 bold} \
        -foreground #000000000000 -padx 11 -pady 4 -text Quit 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.vdcfr \
        -x 0 -y 0 -width 295 -height 335 -anchor nw -bordermode ignore 
    place $base.vdcfr.inlbl \
        -x 15 -y 5 -width 63 -height 22 -anchor nw -bordermode ignore 
    place $base.vdcfr.anslbl \
        -x 5 -y 35 -anchor nw -bordermode ignore 
    place $base.vdcfr.indisp \
        -x 85 -y 5 -width 206 -height 22 -anchor nw -bordermode ignore 
    place $base.vdcfr.ansdisp \
        -x 85 -y 35 -width 206 -height 22 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn0 \
        -x 35 -y 85 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn1 \
        -x 90 -y 85 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn2 \
        -x 150 -y 85 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn3 \
        -x 210 -y 85 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn4 \
        -x 35 -y 130 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn5 \
        -x 90 -y 130 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn6 \
        -x 150 -y 130 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn7 \
        -x 210 -y 130 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn8 \
        -x 90 -y 175 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn9 \
        -x 155 -y 175 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn_eq \
        -x 220 -y 285 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn_plus \
        -x 35 -y 225 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn_minus \
        -x 90 -y 225 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn_times \
        -x 155 -y 225 -anchor nw -bordermode ignore 
    place $base.vdcfr.btn_div \
        -x 210 -y 225 -anchor nw -bordermode ignore 
    place $base.vdcfr.clear_btn \
        -x 120 -y 285 -width 79 -height 28 -anchor nw -bordermode ignore 
    place $base.vdcfr.quitbtn \
        -x 35 -y 285 -anchor nw -bordermode ignore 
}


#####################################################################3

proc cbutton {Arg} {
    .vdc.vdcfr.indisp insert end $Arg
}

proc quit_vdc {} {
	exit
}

proc clear_all {} {
    .vdc.vdcfr.indisp delete 0 end 
    .vdc.vdcfr.ansdisp delete 0 end 
}

proc get_answer {} {
    set input [.vdc.vdcfr.indisp get]
	prolog "desk_calc:vdc($input, Answer)." answer
    .vdc.vdcfr.ansdisp delete 0 end 
    .vdc.vdcfr.ansdisp insert 0 $answer
}

Window show .
Window show .vdc
