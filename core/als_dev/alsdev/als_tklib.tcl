##=================================================================================
#|              als_tklib.tcl
#|      Copyright (c) 1997 Applied Logic Systems, Inc.
#|
#|      Tcl/Tk procedures for the ALS Prolog GUI library
#|
#|
#|  Author: Ken Bowen
#|  Date:   1997
#|
#|  This file is sourced from init_tk_alslib/1 in tk_alslib.pro.  
#|	It should source any other *.tcl files required by the GUI library
##=================================================================================
 
#puts "SOURCING als_tklib.tcl"

##################################################################################
######### General routines needing defining if alsdev is not loaded:
##################################################################################

if {[info procs Window]==""} then {

proc Window {args} {
global vTcl
	set cmd [lindex $args 0]
	set name [lindex $args 1]
	set newname [lindex $args 2]
	set rest [lrange $args 3 end]
    if {$name == "" || $cmd == ""} {return}
    set exists [winfo exists $name]
    switch $cmd {
        show { eval "vTclWindow$name $name" }
        hide    { if $exists {wm withdraw $name; return} }
        iconify { if $exists {wm iconify $name; return} }
        destroy { if $exists {destroy $name; return} }
    }
}

#            if {[info procs vTclWindow(pre)$name] != ""} {
#                eval "vTclWindow(pre)$name $newname $rest"
#            }
#            if {[info procs vTclWindow$name] != ""} {
#                eval "vTclWindow$name $newname $rest"
#            }
#            if {[info procs vTclWindow(post)$name] != ""} {
#                eval "vTclWindow(post)$name $newname $rest"
#            }




}

if {[info procs vTclWindow. ]==""} then {

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    wm focusmodel $base passive
    wm geometry $base 1x1+0+0
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt.tcl"
}

}

##################################################################################
######### POPUP INPUT
##################################################################################

proc vTclWindow.input_popup {base} {
	global array proenv

    if {$base == ""} {
        set base .input_popup
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################

    toplevel $base -class Toplevel 
    wm focusmodel $base passive
    wm geometry $base 407x130+50+317
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "input_popup"

    frame $base.input_p \
        -borderwidth 1 -relief sunken
    label $base.input_p.input_popup_head \
        -borderwidth 2 \
        -relief groove \
        -text {Type the name of the configuration:} 
    entry $base.input_p.input_popup_entry \
        -foreground #000000000000 
	bind $base.input_p.input_popup_entry <Return> \
		[list fin_input_popup ok $base ]
    frame $base.input_p.btns \
        -borderwidth 1 -relief flat
    button $base.input_p.btns.ok \
        -command "fin_input_popup ok $base" \
        -padx 11 -pady 4 -text OK 
    button $base.input_p.btns.cancel \
        -command "fin_input_popup cancel $base"\
        -padx 11 -pady 4 -text Cancel 
    ###################
    # SETTING GEOMETRY
    ###################

    pack $base.input_p \
		-anchor w -expand 1 -fill both  -side top -padx 4 -pady 4
    pack $base.input_p.input_popup_head \
        -anchor center -expand 0 -fill none -side top -pady 8 
    pack $base.input_p.input_popup_entry \
        -anchor center -expand 0 -fill x -side top -pady 8 -padx 8
    pack $base.input_p.btns \
        -anchor w -expand 1 -fill x -side top -padx 25 
    pack $base.input_p.btns.ok \
        -anchor center -expand 0 -fill none -padx 4 -side left 
    pack $base.input_p.btns.cancel \
        -anchor center -expand 0 -fill none -padx 4 -side right 
}

proc fin_input_popup {Which base} {
	global proenv
	switch $Which {
	ok { set proenv($base) [$base.input_p.input_popup_entry get] }
	cancel { set proenv($base) "" }
	}
	Window destroy $base
}

proc do_popup_input {Prompt Title} {
	global proenv 

	set proenv(.input_popup) ""
	Window show .input_popup
	wm title .input_popup $Title
    .input_popup.input_p.input_popup_head configure -text $Prompt
	tkwait variable proenv(.input_popup)
	return $proenv(.input_popup)
}

##################################################################################
######### POPUP LIST SELECTION  
##################################################################################

proc do_select_items { BaseName Mode Title SourceItemsList } {
	vTclWindow.popup_select_widget $BaseName
    wm title $BaseName $Title

	foreach Item $SourceItemsList {
		$BaseName.clist.listbox insert end $Item
	}
	$BaseName.clist.listbox configure -selectmode $Mode

	append TheVar WaitVar $BaseName
	global $TheVar

	Window show $BaseName
	tkwait variable $TheVar
	set Indicies [$BaseName.clist.listbox curselection]
	set Result ""
	foreach Item $Indicies {
		lappend Result [$BaseName.clist.listbox get $Item]
	}
	destroy $BaseName
	return $Result
}

proc fin_popup_list_box { Which BaseName} {
	append TheVar WaitVar $BaseName
	global $TheVar

	Window hide $BaseName
	switch $Which {
	cancel { set $TheVar "" }
	ok { set $TheVar $BaseName }
	}
}

proc vTclWindow.popup_select_widget {base} {
    if {$base == ""} {
        set base .popup_select_widget
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }

    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 188x382+401+165
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "popup_select_widget"
    frame $base.clist \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.clist.listbox \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -xscrollcommand {.popup_select_widget.clist.02 set} \
        -yscrollcommand {.popup_select_widget.clist.03 set} 
    scrollbar $base.clist.02 \
        -borderwidth 1 -command {.popup_select_widget.clist.listbox xview} \
        -orient horiz -width 10 
    scrollbar $base.clist.03 \
        -borderwidth 1 -command {.popup_select_widget.clist.listbox yview} \
        -orient vert -width 10 
    frame $base.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.buttons.cancel \
        -padx 11 -pady 4 -text Cancel \
		-command "fin_popup_list_box cancel $base"
    button $base.buttons.ok \
        -padx 11 -pady 4 -text OK \
		-command "fin_popup_list_box ok $base"

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.clist \
        -anchor center -expand 1 -fill both -side top 
    grid columnconf $base.clist 0 -weight 1
    grid rowconf $base.clist 0 -weight 1
    grid $base.clist.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.clist.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.clist.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    pack $base.buttons \
        -anchor center -expand 0 -fill x -pady 5 -side bottom 
    pack $base.buttons.cancel \
        -anchor center -expand 0 -fill none -padx 10 -side left 
    pack $base.buttons.ok \
        -anchor center -expand 0 -fill none -padx 10 -side right 
}

