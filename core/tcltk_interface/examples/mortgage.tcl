#############################################################################
# Visual Tcl v1.07 Project
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

set argc 0
set argv ""

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
    wm geometry . 1x1+0+0
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

proc vTclWindow.top30 {args} {
    set base .top30
    if {[winfo exists .top30]} {
        wm deiconify .top30; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel .top30 -class Toplevel \
        -background #DC00C9E1C9E1 
    wm focusmodel .top30 passive
    wm geometry .top30 548x367+259+295
    wm maxsize .top30 1265 994
    wm minsize .top30 1 1
    wm overrideredirect .top30 0
    wm resizable .top30 1 1
    wm deiconify .top30
    wm title .top30 "MGraph"
    frame .top30.cpd31 \
        -background #DC00C9E1C9E1 -borderwidth 1 -height 30 -relief raised \
        -width 30 
    label .top30.cpd31.01 \
        -anchor w -background #DC00C9E1C9E1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -relief groove -text {Cmpdng Freq} 
    entry .top30.cpd31.02 \
        -background #DC00C9E1C9E1 -cursor {} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -highlightthickness 0 \
		-textvariable comp_freq 
    frame .top30.cpd32 \
        -background #DC00C9E1C9E1 -borderwidth 1 -height 30 -relief raised \
        -width 30 
    label .top30.cpd32.01 \
        -anchor w -background #DC00C9E1C9E1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -relief groove -text Interest 
    entry .top30.cpd32.02 \
        -background #DC00C9E1C9E1 -cursor {} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -highlightthickness 0  -textvariable interest
    frame .top30.cpd35 \
        -background #DC00C9E1C9E1 -borderwidth 1 -height 30 -relief raised \
        -width 30 
    label .top30.cpd35.01 \
        -anchor w -background #DC00C9E1C9E1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -relief groove -text Principal 
    entry .top30.cpd35.02 \
        -background #DC00C9E1C9E1 -cursor {} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -highlightthickness 0  -textvariable principal
    frame .top30.cpd36 \
        -background #DC00C9E1C9E1 -borderwidth 1 -height 30 -relief raised \
        -width 30 
    label .top30.cpd36.01 \
        -anchor w -background #DC00C9E1C9E1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -relief groove -text Years 
    entry .top30.cpd36.02 \
        -background #DC00C9E1C9E1 -cursor {} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -highlightthickness 0 -textvariable years
    button .top30.but37 \
        -background #DC00C9E1C9E1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -padx 11 -pady 4 -text Update \
		-command vomit_all
    button .top30.but38 \
        -background #DC00C9E1C9E1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -foreground #000000000000 -padx 11 -pady 4 -text Quit \
		-command {prolog "halt."}
    frame .top30.cpd39 \
        -background #DC00C9E1C9E1 -borderwidth 1 -height 30 -relief raised \
        -width 30 
    scrollbar .top30.cpd39.01 \
        -background #DC00C9E1C9E1 -command {.top30.cpd39.03 xview} \
        -orient horiz -width 10 
    scrollbar .top30.cpd39.02 \
        -background #DC00C9E1C9E1 -command {.top30.cpd39.03 yview} \
        -orient vert -width 10 
    canvas .top30.cpd39.03 \
        -background #DC00C9E1C9E1 -borderwidth 2 -height 100 -relief ridge \
        -width 100 -xscrollcommand {.top30.cpd39.01 set} \
        -yscrollcommand {.top30.cpd39.02 set} 
    ###################
    # SETTING GEOMETRY
    ###################
    place .top30.cpd31 \
        -x 345 -y 320 -width 195 -height 25 -anchor nw -bordermode ignore 
    pack .top30.cpd31.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack .top30.cpd31.02 \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
    place .top30.cpd32 \
        -x 345 -y 285 -width 190 -height 25 -anchor nw -bordermode ignore 
    pack .top30.cpd32.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack .top30.cpd32.02 \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
    place .top30.cpd35 \
        -x 125 -y 285 -width 210 -height 25 -anchor nw -bordermode ignore 
    pack .top30.cpd35.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack .top30.cpd35.02 \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
    place .top30.cpd36 \
        -x 125 -y 320 -width 210 -height 25 -anchor nw -bordermode ignore 
    pack .top30.cpd36.01 \
        -anchor center -expand 0 -fill none -padx 2 -pady 2 -side left 
    pack .top30.cpd36.02 \
        -anchor center -expand 1 -fill x -padx 2 -pady 2 -side right 
    place .top30.but37 \
        -x 20 -y 285 -anchor nw -bordermode ignore 
    place .top30.but38 \
        -x 20 -y 320 -anchor nw -bordermode ignore 
    place .top30.cpd39 \
        -x 5 -y 5 -width 528 -height 266 -anchor nw 
    grid columnconf .top30.cpd39 0 -weight 1
    grid rowconf .top30.cpd39 0 -weight 1
    grid .top30.cpd39.01 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid .top30.cpd39.02 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid .top30.cpd39.03 \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
}

set comp_freq 2
set years 5
set principal 1000.0
set interest 0.06

Window show .
Window show .top30

main $argc $argv

bind .top30.cpd31.02 <Return> {prolog "printf('comp freq=%%t\\n',\[2\])." }




bind .top30.cpd32.02 <Return> {prolog "write(interest=$interest),nl."}
bind .top30.cpd35.02 <Return> {prolog "write(principal=$principal),nl."}
bind .top30.cpd36.02 <Return> {prolog "write(years=$years),nl."}

proc vomit_all {} {
	global comp_freq interest principal years

	prolog "write(comp_freq=$comp_freq),nl,\
		write(interest=$interest),nl,\
		write(principal=$principal),nl,\
		write(years=$years),nl."

	prolog "mortgage:payoff($years,$comp_freq,$interest,$principal,List)." list
	puts $list

#    .top30.cpd39.03 create line $list
	eval ".top30.cpd39.03 create line  " $list

}
