proc mkdrawing {base} {
    if {$base == ""} {
        set base .drawing
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 413x360+322+99
    wm maxsize $base 1028 753
    wm minsize $base 104 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Drawing"
    frame $base.cnv -borderwidth 1 -relief raised
    scrollbar $base.cnv.xsb \
        -borderwidth 1 -command {.drawing.cnv.canvas xview} -orient horiz 
    scrollbar $base.cnv.ysb \
        -borderwidth 1 -command {.drawing.cnv.canvas yview} -orient vert 
    canvas $base.cnv.canvas \
        -borderwidth 2 -relief ridge \
        -xscrollcommand {.drawing.cnv.xsb set} \
        -yscrollcommand {.drawing.cnv.ysb set} 
    frame $base.buttons -borderwidth 2
    button $base.buttons.dismiss -text Dismiss \
	-command "Window hide $base"
    button $base.buttons.clear -text Clear \
	-command "$base.cnv.canvas delete all"
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.cnv \
        -in .drawing -anchor center -expand 1 -fill both -side top 
    grid columnconf $base.cnv 0 -weight 1
    grid rowconf $base.cnv 0 -weight 1
    grid $base.cnv.xsb -in .drawing.cnv \
	-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.cnv.ysb -in .drawing.cnv \
	-column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $base.cnv.canvas -in .drawing.cnv \
	-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    pack $base.buttons -in .drawing \
	-anchor center -expand 0 -fill x -side top 
    pack $base.buttons.dismiss -in .drawing.buttons \
	-anchor center -expand 0 -fill none -padx 20 -side left 
    pack $base.buttons.clear -in .drawing.buttons \
	-anchor center -expand 0 -fill none -padx 20 -side right 
}

#Window show .
#Window show .drawing
#
#main $argc $argv
