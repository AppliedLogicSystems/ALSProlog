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





proc display_file_image {ImageDir ImageFile ImageBase Win Width Height BorderWidth} {
	set ImagePath [file join $ImageDir $ImageFile]
	toplevel $Win -bd $BorderWidth -relief flat
	wm withdraw $Win
	set screen_width  [winfo screenwidth .]
	set screen_height [winfo screenheight .]
	image create photo $ImageBase -file $ImagePath
	wm overrideredirect $Win 1
	label $Win.label -image $ImageBase -bd 1 -relief flat
	pack $Win.label -side top -expand 1 -fill both
	set X [expr ($screen_width - $Width)/4]
	set Y [expr ($screen_width - $Height)/4]
	wm geometry $Win +$X+$Y
	wm deiconify $Win
	update idletasks
}

proc display_image {ImageName Win Width Height X Y BorderWidth } {
	toplevel $Win -bd $BorderWidth -relief flat
	wm withdraw $Win
	wm overrideredirect $Win 1
	label $Win.label -image $ImageName -bd 1 -relief flat
	pack $Win.label -side top -expand 1 -fill both
	wm geometry $Win +$X+$Y
	wm deiconify $Win
	update idletasks
}

##################################################################################
######### 			TWO-COLUMN CHOICE ARGANGEMENT WIDGET				##########
##################################################################################

proc vTclWindow.columns2_select {base} {
	global array proenv
	global array cols2data

    if {$base == ""} {
        set base .columns2_select
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel 
    wm focusmodel $base passive
    wm geometry $base 440x564+384+076
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Column Name Selection"

    frame $base.cols_fr \
		-borderwidth 1 -relief sunken 
    frame $base.cols_fr.l \
		-borderwidth 1 -relief sunken 

    label $base.cols_fr.l.label \
        -borderwidth 2 \
        -foreground #000000000000 -relief groove -text {Selected Columns} 

    frame $base.cols_fr.l.leftlist \
        -borderwidth 1 -relief raised \
		-width 185 -height 490
    listbox $base.cols_fr.l.leftlist.lstbx \
        -height 23 \
        -yscrollcommand "$base.cols_fr.l.leftlist.vsb set" \
		-selectmode extended
    scrollbar $base.cols_fr.l.leftlist.vsb \
        -command "$base.cols_fr.l.leftlist.lstbx yview" \
        -orient vert 

    frame $base.cols_fr.l.buttons \
        -borderwidth 1 -relief sunken 
    button $base.cols_fr.l.buttons.up \
        -padx 3 -pady 3 -text Up -command "move_leftlist_item $base up" \
		-image up_arrow_gif 
    button $base.cols_fr.l.buttons.down \
        -padx 3 -pady 3 -text Down -command "move_leftlist_item $base down" \
		-image down_arrow_gif 

    frame $base.cols_fr.m \
        -borderwidth 1 -relief sunken 
    button $base.cols_fr.m.move_rt_btn \
        -padx 11 -pady 4 -text button -command "move_col_right $base" \
		-image right_gif 

    button $base.cols_fr.m.mv_left_btn \
        -padx 11 -pady 4 -text button -command "move_col_left $base" \
		-image left_gif 

    frame $base.cols_fr.r \
        -borderwidth 1 -relief sunken 
    label $base.cols_fr.r.label \
        -borderwidth 2 \
        -relief groove -text {Unselected Columns} 
    frame $base.cols_fr.r.rightlist \
        -borderwidth 1 -relief raised 
    listbox $base.cols_fr.r.rightlist.lstbx \
        -yscrollcommand "$base.cols_fr.r.rightlist.vsb set" \
		-selectmode extended
    scrollbar $base.cols_fr.r.rightlist.vsb \
        -command "$base.cols_fr.r.rightlist.lstbx yview" \
        -orient vert 

	frame $base.btns_fr \
        -borderwidth 1 -relief groove 
    button $base.btns_fr.ok_btn \
        -padx 11 -pady 3 -text OK \
		-command "cols_select_done $base ok"

    button $base.btns_fr.cancel_btn \
        -padx 11 -pady 3 -text Cancel \
		-command "cols_select_done $base cancel"

    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.cols_fr \
        -anchor center -expand 1 -fill both -side top 
    pack $base.cols_fr.l \
        -anchor center -expand 1 -fill y -side left 
    pack $base.cols_fr.l.label \
        -anchor center -expand 0 -fill x -side top 

    pack $base.cols_fr.l.leftlist \
        -anchor center -expand 1 -fill y -side top 
    grid columnconf $base.cols_fr.l.leftlist 0 -weight 1
    grid rowconf $base.cols_fr.l.leftlist 0 -weight 1
    grid $base.cols_fr.l.leftlist.lstbx \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.cols_fr.l.leftlist.vsb \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 


	if {[info exists cols2data($base,up_down)]==1} then {
	if {$cols2data($base,up_down)==1} then {
    	pack $base.cols_fr.l.buttons \
        	-anchor center -expand 0 -fill x -side top 
    	pack $base.cols_fr.l.buttons.up \
        	-anchor center -expand 0 -fill none -side left -padx 10 
    	pack $base.cols_fr.l.buttons.down \
        	-anchor center -expand 0 -fill none -side right -padx 10 
	}}

    pack $base.cols_fr.m \
        -anchor center -expand 0 -fill y -side left 
    pack $base.cols_fr.m.move_rt_btn \
        -anchor center -expand 0 -fill none -side top -pady 75
    pack $base.cols_fr.m.mv_left_btn \
        -anchor center -expand 0 -fill none -side top -pady 15

    pack $base.cols_fr.r \
        -anchor center -expand 1 -fill y -side left 
    pack $base.cols_fr.r.label \
        -anchor center -expand 0 -fill x -side top 

    pack $base.cols_fr.r.rightlist \
        -anchor center -expand 1 -fill y -side left 
    grid columnconf $base.cols_fr.r.rightlist 0 -weight 1
    grid rowconf $base.cols_fr.r.rightlist 0 -weight 1
    grid $base.cols_fr.r.rightlist.lstbx \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.cols_fr.r.rightlist.vsb \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 

	pack $base.btns_fr \
        -anchor center -expand 0 -fill x -side top 
    pack $base.btns_fr.ok_btn \
        -anchor center -expand 0 -fill none -side left -padx 20
	if {[info exists cols2data(.spyc,addl_btns)]==1} then {
		set WhichSide left } else { set WhichSide right }
    pack $base.btns_fr.cancel_btn \
        -anchor center -expand 0 -fill none -side $WhichSide -padx 20

}

	#---------------------------
	# choose display columns
	#---------------------------

global array cols2data

proc two_col_display_choose {base LCap RCap Title} {
	global array proenv
	global array cols2data
	vTclWindow.columns2_select $base
	wm title $base $Title
	raise $base
	$base.btns_fr.ok_btn configure -command "cols_select_done $base ok"
	$base.btns_fr.cancel_btn configure -command "cols_select_done $base cancel"
	$base.cols_fr.l.label configure -text $LCap
	$base.cols_fr.r.label configure -text $RCap

	if {[info exists cols2data($base,addl_btns)]==1} then {
		set BtnSpecs $cols2data($base,addl_btns)
		frame $base.btns_fr.addl_btns -borderwidth 1 -relief groove 
		foreach BtnSpec $BtnSpecs {
			puts [concat [lindex $BtnSpec 0] $base.btns_fr.addl_btns.[lindex $BtnSpec 1] [lindex $BtnSpec 2]	 ]

			eval [lindex $BtnSpec 0] $base.btns_fr.addl_btns.[lindex $BtnSpec 1] \
				[lindex $BtnSpec 2]	
		}
    	pack $base.btns_fr.addl_btns \
        	-anchor center -expand 0 -fill none -side right -padx 2
		foreach BtnSpec $BtnSpecs {
    		pack $base.btns_fr.addl_btns.[lindex $BtnSpec 1] \
        		-anchor center -expand 0 -fill none -side left -padx 8
		}
	}

	$base.cols_fr.l.leftlist.lstbx delete 0 end
	$base.cols_fr.r.rightlist.lstbx delete 0 end

	disp_list $cols2data($base,left) $base.cols_fr.l.leftlist.lstbx
	disp_list $cols2data($base,right) $base.cols_fr.r.rightlist.lstbx

	grab set $base
	tkwait variable cols2data($base,list_arrange_done)
	grab release $base
	Window hide $base
}

proc move_col_right {base} {
	set SelIndicies [$base.cols_fr.l.leftlist.lstbx curselection]
	set RevSelIndicies [lsort -decreasing $SelIndicies]
	set LL [$base.cols_fr.r.rightlist.lstbx index end]
	foreach Index $RevSelIndicies {
		set Hold [$base.cols_fr.l.leftlist.lstbx get $Index]
		$base.cols_fr.l.leftlist.lstbx delete $Index
		$base.cols_fr.r.rightlist.lstbx insert $LL $Hold
	}
}


proc move_col_left {base} {
	set SelIndicies [$base.cols_fr.r.rightlist.lstbx curselection]
	set RevSelIndicies [lsort -decreasing $SelIndicies]
	set LL [$base.cols_fr.l.leftlist.lstbx index end]
	foreach Index $RevSelIndicies {
		set Hold [$base.cols_fr.r.rightlist.lstbx get $Index]
		$base.cols_fr.r.rightlist.lstbx delete $Index
		$base.cols_fr.l.leftlist.lstbx insert $LL $Hold
	}
}


#---------------------------------------------------------------
#  disp_list_n {List ListBox N}
#
#	List	- a list of Tcl (sub)lists all of length >= N
#	ListBox	- a Tk listbox
#	N		- an integer
#
#	Displays the list of Nth elements of the items on List
#	in the listbox ListBox
#---------------------------------------------------------------
proc disp_list_n {List ListBox N} {
	$ListBox delete 0 end
	foreach Item $List {
		$ListBox insert end [lindex $Item $N]
	}
}

#---------------------------------------------------------------
#  disp_list {List ListBox}
#
#	List	- a list of Tcl items
#	ListBox	- a Tk listbox
#
#	Displays the items on List in ListBox
#---------------------------------------------------------------
proc disp_list {List ListBox} {
	$ListBox delete 0 end
	foreach Item $List {
		$ListBox insert end $Item
	}
}
proc disp_list_n_full {List ListBox} {
	$ListBox delete 0 end
	foreach Item $List {
		$ListBox insert end $Item
	}
}


proc cols_select_done {base What} {
	global array cols2data
	
	switch $What {
	cancel {
		set cols2data($base,list_arrange_done) cancel
		}
	ok {
		set UnselList ""
		set EndIndex [$base.cols_fr.r.rightlist.lstbx index end]
		for {set J 0} {$J < $EndIndex} {incr J} {
			lappend UnselList [$base.cols_fr.r.rightlist.lstbx get $J]
		}
		set SelctList ""
		set EndIndex [$base.cols_fr.l.leftlist.lstbx index end]
		for {set J 0} {$J < $EndIndex} {incr J} {
			lappend SelctList [$base.cols_fr.l.leftlist.lstbx get $J]
		}
		set cols2data($base,list_arrange_done) ok
		set cols2data($base,right) $UnselList
		set cols2data($base,left)  $SelctList
		}
	}
}

proc move_leftlist_item {base Direction} {

	set SelList [$base.cols_fr.l.leftlist.lstbx curselection]
	if {"$SelList"==""} then {
		bell
		return
	} 
	set FirstIndex [lindex $SelList 0]
	set LastIndex [lindex $SelList end]
	$base.cols_fr.l.leftlist.lstbx selection clear [expr $FirstIndex + 1] end
	if {"$Direction"=="up"} then {
		if {"$FirstIndex"=="0"} then {
			bell
			return
		} else {
			set FirstIndex1 [expr $FirstIndex - 1]
			set Hold [$base.cols_fr.l.leftlist.lstbx get $FirstIndex1]
			$base.cols_fr.l.leftlist.lstbx delete $FirstIndex1
			$base.cols_fr.l.leftlist.lstbx insert $FirstIndex $Hold
			$base.cols_fr.l.leftlist.lstbx selection set $FirstIndex1
		}
	} else {
		set EndIndex [$base.cols_fr.l.leftlist.lstbx index end]
		if {"$FirstIndex"=="$EndIndex"} then {
			bell
			return
		} else {
			set FirstIndex1 [expr $FirstIndex + 1]
			set Hold [$base.cols_fr.l.leftlist.lstbx get $FirstIndex]
			$base.cols_fr.l.leftlist.lstbx delete $FirstIndex
			$base.cols_fr.l.leftlist.lstbx insert $FirstIndex1 $Hold
			$base.cols_fr.l.leftlist.lstbx selection set $FirstIndex1 
		}
	}
}
