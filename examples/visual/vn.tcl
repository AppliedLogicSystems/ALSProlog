proc setup_nim {} {
    set base .vn
    if {[winfo exists $base]} { destroy $base }

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 264x250+100+100
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm deiconify $base
    wm title $base "Nim"

    frame $base.p1 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.p1.b1-1 \
        -activeforeground #000000 -background #0000fe -command {stick 1 1} \
        -height 2 -padx 2 -pady 4 

    frame $base.p2 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.p2.b2-1 \
        -activeforeground #000000 -background #0000fe -command {stick 2 1} \
        -height 2 -padx 2 -pady 4 
    button $base.p2.b2-2 \
        -activeforeground #000000 -background #0000fe -command {stick 2 2} \
        -height 2 -padx 2 -pady 4 
    button $base.p2.b2-3 \
        -activeforeground #000000 -background #0000fe -command {stick 2 3} \
        -height 2 -padx 2 -pady 4 

    frame $base.p3 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.p3.b3-1 \
        -activeforeground #000000 -background #0000fe -command {stick 3 1} \
        -height 2 -padx 2 -pady 4 
    button $base.p3.b3-2 \
        -activeforeground #000000 -background #0000fe -command {stick 3 2} \
        -height 2 -padx 2 -pady 4 
    button $base.p3.b3-3 \
        -activeforeground #000000 -background #0000fe -command {stick 3 3} \
        -height 2 -padx 2 -pady 4 
    button $base.p3.b3-4 \
        -activeforeground #000000 -background #0000fe -command {stick 3 4} \
        -height 2 -padx 2 -pady 4 
    button $base.p3.b3-5 \
        -activeforeground #000000 -background #0000fe -command {stick 3 5} \
        -height 2 -padx 2 -pady 4 

    frame $base.p4 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.p4.b4-1 \
        -activeforeground #000000 -background #0000fe -command {stick 4 1} \
        -height 2 -padx 2 -pady 4 
    button $base.p4.b4-2 \
        -activeforeground #000000 -background #0000fe -command {stick 4 2} \
        -height 2 -padx 2 -pady 4 
    button $base.p4.b4-3 \
        -activeforeground #000000 -background #0000fe -command {stick 4 3} \
        -height 2 -padx 2 -pady 4 
    button $base.p4.b4-4 \
        -activeforeground #000000 -background #0000fe -command {stick 4 4} \
        -height 2 -padx 2 -pady 4 
    button $base.p4.b4-5 \
        -activeforeground #000000 -background #0000fe -command {stick 4 5} \
        -height 2 -padx 2 -pady 4 
    button $base.p4.b4-6 \
        -activeforeground #000000 -background #0000fe -command {stick 4 6} \
        -height 2 -padx 2 -pady 4 
    button $base.p4.b4-7 \
        -activeforeground #000000 -background #0000fe -command {stick 4 7} \
        -height 2 -padx 2 -pady 4 

    frame $base.btns \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.btns.yt \
        -padx 11 -pady 4 -text {Done Picking} -command im_done
    button $base.btns.quit \
        -padx 11 -pady 4 -text {Quit} -command {prolog call user quit}

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.p1 \
        -anchor center -expand 0 -fill none -side top 
    pack $base.p1.b1-1 \
        -anchor center -expand 0 -fill none -padx 4 -side top 
    pack $base.p2 \
        -anchor center -expand 0 -fill none -side top 
    pack $base.p2.b2-1 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p2.b2-2 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p2.b2-3 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3 \
        -anchor center -expand 0 -fill none -side top 
    pack $base.p3.b3-1 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-2 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-3 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-4 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-5 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4 \
        -anchor center -expand 0 -fill none -side top 
    pack $base.p4.b4-1 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-2 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-3 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-4 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-5 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-6 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-7 \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.btns \
        -anchor s -expand 0 -fill x -side top 
    pack $base.btns.quit \
        -anchor center -expand 0 -fill none -padx 8 -side left 
    pack $base.btns.yt \
        -anchor center -expand 0 -fill none -padx 8 -side right 

    wm resizable $base 0 0
}

proc stick {Row Stick} {
#    .vn.p$Row.b$Row-$Stick configure -background #d9d9d9 -state disabled
	prolog call user select_stick -number $Row -number $Stick
}

proc im_done { } {
	prolog call user user_done_picking 
}

#proc im_done { } {
#	global OkWait
#	set OkWait done_picking
#}
#
#proc wait_on_button { } {
#	global OkWait
#	tkwait variable OkWait
#	return $OkWait
#}
#
#proc quit { } {
#	global OkWait
#	set OkWait quit
#}

