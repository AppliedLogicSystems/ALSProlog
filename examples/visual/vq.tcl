
proc chessboard {} {
	global OkWait

    set base .vq
    if {[winfo exists $base]} { destroy $base }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 377x410+236+185
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "vq"
    frame $base.bd \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    frame $base.info \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.info.soln_label \
        -text {Solution # =} 
    label $base.info.soln_num \
        -text {0} -padx 2
    button $base.info.next_btn \
        -text {Next} -padx 2 -command { set OkWait ok} -width 10
    button $base.info.all_btn \
        -text {All} -padx 8 -command find_all_the_rest
							
		

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.bd \
        -anchor center -expand 1 -fill both -side top 
    pack $base.info \
        -anchor s -expand 0 -fill x -side top 
    pack $base.info.soln_label \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.info.soln_num \
        -anchor center -expand 0 -fill none -side left 
    pack $base.info.next_btn \
        -anchor center -expand 0 -fill none -padx 18 -side left 
    pack $base.info.all_btn \
        -anchor center -expand 0 -fill none -padx 10 -side right 

	grid columnconf $base.bd 0 -weight 1
	grid columnconf $base.bd 1 -weight 1
	grid columnconf $base.bd 2 -weight 1
	grid columnconf $base.bd 3 -weight 1
	grid columnconf $base.bd 4 -weight 1
	grid columnconf $base.bd 5 -weight 1
	grid columnconf $base.bd 6 -weight 1
	grid columnconf $base.bd 7 -weight 1

	grid rowconf $base.bd 0 -weight 1
	grid rowconf $base.bd 1 -weight 1
	grid rowconf $base.bd 2 -weight 1
	grid rowconf $base.bd 3 -weight 1
	grid rowconf $base.bd 4 -weight 1
	grid rowconf $base.bd 5 -weight 1
	grid rowconf $base.bd 6 -weight 1
	grid rowconf $base.bd 7 -weight 1

}


proc clear_board {M} {
	for {set i 0} {$i < 8} {incr i} {
		for {set j 0} {$j < 8} {incr j} {
			if {$M=="text"} then {
			.vq.bd.s$i-$j configure -text {}
			} else {
			.vq.bd.s$i-$j configure -image {}
			}
		}
	}
}

proc wait_on_button { } {
	global OkWait

	tkwait variable OkWait
}

proc find_all_the_rest { } {
	global OkWait

	set OkWait ok
	prolog call user find_all_the_rest
}
