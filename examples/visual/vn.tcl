
proc setup_nim {} {
    set base .vn
    if {[winfo exists $base]} { destroy $base }

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel -bg brown
    wm focusmodel $base passive
    wm geometry $base 350x240+100+100
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm deiconify $base
    wm title $base "Nim"

    ttk::style configure [ttk::frame $base.p1 -borderwidth 1 -height 10 -relief sunken -width 1] -padding {1 1 1 1}
    ttk::style configure [ttk::button $base.p1.b1-1 -command {stick 1 1} -width 0 -text |] -background yellow

    ttk::style configure [ttk::frame $base.p2 -borderwidth 1 -height 10 -relief sunken -width 1] -padding {1 1 1 1} -background yellow
    ttk::style configure [ttk::button $base.p2.b2-1 -command {stick 2 1} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p2.b2-2 -command {stick 2 2} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p2.b2-3 -command {stick 2 3} -width 0  -text |] -background yellow

    ttk::style configure [ttk::frame $base.p3 -borderwidth 1 -height 10 -relief sunken -width 1] -padding {1 1 1 1}
    ttk::style configure [ttk::button $base.p3.b3-1 -command {stick 3 1} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p3.b3-2 -command {stick 3 2} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p3.b3-3 -command {stick 3 3} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p3.b3-4 -command {stick 3 4} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p3.b3-5 -command {stick 3 5} -width 0  -text |] -background yellow

    ttk::style configure [ttk::frame $base.p4 -borderwidth 1 -height 10 -relief sunken -width 1] -padding {1 1 1 1}
    ttk::style configure [ttk::button $base.p4.b4-1 -command {stick 4 1} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p4.b4-2 -command {stick 4 2} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p4.b4-3 -command {stick 4 3} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p4.b4-4 -command {stick 4 4} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p4.b4-5 -command {stick 4 5} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p4.b4-6 -command {stick 4 6} -width 0  -text |] -background yellow
    ttk::style configure [ttk::button $base.p4.b4-7 -command {stick 4 7} -width 0  -text |] -background yellow

    ttk::frame $base.btns -borderwidth 1 -height 30 -relief sunken -width 0 
    ttk::button $base.btns.yt -text {Done Picking} -command im_done
    ttk::button $base.btns.quit -text {Quit} -command {prolog call vnim quit}

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.p1 -anchor center -expand 0 -fill none -side top -pady 10
    pack $base.p1.b1-1 -anchor center -expand 0 -fill none -padx 4 -side top 

    pack $base.p2 -anchor center -expand 0 -fill none -side top -pady 10
    pack $base.p2.b2-1 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p2.b2-2 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p2.b2-3 -anchor center -expand 0 -fill none -padx 4 -side left 

    pack $base.p3 -anchor center -expand 0 -fill none -side top -pady 10
    pack $base.p3.b3-1 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-2 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-3 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-4 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p3.b3-5 -anchor center -expand 0 -fill none -padx 4 -side left 

    pack $base.p4 -anchor center -expand 0 -fill none -side top -pady 10
    pack $base.p4.b4-1 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-2 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-3 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-4 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-5 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-6 -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.p4.b4-7 -anchor center -expand 0 -fill none -padx 4 -side left 

    pack $base.btns  -anchor s -expand 0 -fill x -side top -pady 10
    pack $base.btns.quit -anchor center -expand 0 -fill none -padx 8 -side left 
    pack $base.btns.yt -anchor center -expand 0 -fill none -padx 8 -side right 

    wm resizable $base 0 0
}

proc stick {Row Stick} {
#    .vn.p$Row.b$Row-$Stick configure -background #d9d9d9 -state disabled
	prolog call vnim select_stick -number $Row -number $Stick
}

proc im_done { } {
	prolog call vnim user_done_picking 
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

