##=================================================================================
#|				generic_menu.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl/Tk procedures for generic IDE menu code
#|
#|	Author: Chuck Houpt [original als_menu.tcl]
#|	Date:	January 1998
#|  Generic Mods: Ken Bowen [March 2000]
##=================================================================================

if {$tcl_platform(platform) == "macintosh"} {
	set agv(.document,font)		{Monaco 9 normal}
} elseif {$tcl_platform(platform) == "windows"} {
	set agv(.document,selectbackground) SystemHighlight
} elseif {$tcl_platform(platform) == "unix"} {
	set agv(.document,background) #d9d9d9
	set agv(.document,selectforeground) black
	set agv(.document,selectbackground) #c3c3c3
}
set agv(edit,visible) {}

# Menu accelerator modifier key and elipsis string.

if {$tcl_platform(platform) == "macintosh"} {
	set mod "Cmd"
	set elipsis "…"
} else {
	set mod "Ctrl"
	set elipsis "..."
}

proc add_default_menus {menubar} {
	global tcl_platform
	if {$tcl_platform(platform) == "macintosh"} {
		menu $menubar.apple -tearoff 0
		$menubar.apple add command -label "About ALS Prolog…" -command {display_me About .about}
		$menubar add cascade -menu $menubar.apple
	}
}

proc add_minimal_generic_file_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	
	set TearOff 0
	menu $menubar.file -tearoff $TearOff -title File
	
    $menubar.file add separator

	if {$tcl_platform(platform) == "windows"} {
    	$menubar.file add command -label "Exit" -underline 1 -accelerator "$mod-Q" \
			-command "re exit_app"
    } else {
    	$menubar.file add command -label "Quit" -accelerator "$mod-Q" \
			-command "re exit_app"
	}
	$menubar add cascade -menu $menubar.file -label "File" -underline 0
}

proc add_generic_file_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	
	set TearOff 0
	menu $menubar.file -tearoff $TearOff -title File
	
    $menubar.file add command -label New -underline 0 -accelerator "$mod-N" -command {re document.new}
    $menubar.file add command -label "Open$elipsis" -underline 0 -accelerator "$mod-O" -command {re document.open}
    $menubar.file add command -label Close -underline 0 -accelerator "$mod-W" -command "re {$type.close $window}"
    $menubar.file add separator
    $menubar.file add command -label Save -underline 0 -accelerator "$mod-S" -command "re {$type.save $window}"
    $menubar.file add command -label "Save As$elipsis" -underline 5 -command "re {$type.save_as $window}"

#    $menubar.file add separator
#    $menubar.file add command -label "Page Setup$elipsis" \
#		-command "re {$type.page_setup $window}" -state disabled
#    $menubar.file add command -label "Print$elipsis" -accelerator "$mod-P"\
#		-command "re {$type.print $window}" -state disabled
    $menubar.file add separator

	if {$tcl_platform(platform) == "windows"} {
    	$menubar.file add command -label "Exit" -underline 1 -accelerator "$mod-Q" \
			-command "re exit_app"
    } else {
    	$menubar.file add command -label "Quit" -accelerator "$mod-Q" \
			-command "re exit_app"
	}
	$menubar add cascade -menu $menubar.file -label "File" -underline 0
}

proc add_minimal_generic_edit_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	global agv

	set TearOff 0
	menu $menubar.edit -tearoff $TearOff -title Edit
    $menubar.edit add separator
    $menubar.edit add command \
		-label "Preferences$elipsis" -underline 3 -command "re {fonts_and_colors $window}"

	$menubar add cascade -menu $menubar.edit -label "Edit" -underline 0
}
proc add_generic_edit_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	global agv

	set TearOff 0
	menu $menubar.edit -tearoff $TearOff -title Edit
#    $menubar.edit add command \
#        -label Undo -underline 0 -accelerator "$mod-Z" -command "re {$type.undo $window}" -state disabled
#    $menubar.edit add separator
    $menubar.edit add command -label Cut -underline 2 \
		-accelerator "$mod-X" -command "re {$type.cut $window}"
    $menubar.edit add command \
        -label Copy -underline 0 -accelerator "$mod-C" -command "re {$type.copy $window}"
    $menubar.edit add command \
        -label Paste -underline 0 -accelerator "$mod-V" -command "re {$type.paste $window}"
    $menubar.edit add command \
        -label Delete -underline 2 -command "re {$type.delete $window}"
    $menubar.edit add separator
    $menubar.edit add command \
        -label {Select All} -underline 8 -accelerator "$mod-A" -command "re {$type.select_all $window}"
    $menubar.edit add separator
    $menubar.edit add command \
        -label "Locate$elipsis" -underline 0 -accelerator "$mod-L" -command "re {$type.find $window}" -state disabled
    $menubar.edit add separator
    $menubar.edit add command \
		-label "Preferences$elipsis" -underline 3 -command "re {fonts_and_colors $window}"

	$menubar add cascade -menu $menubar.edit -label "Edit" -underline 0
}

proc add_windows_menu {menubar type window} {
	global tcl_platform
	global mod
	global elipsis
	global agv

	set TearOff 0
	$menubar add cascade -label "Windows" -menu .topals.mmenb.windows -underline 0
}

proc add_help_menu {menubar} {
	global tcl_platform
	global mod
	global elipsis

	if {$tcl_platform(platform) != "macintosh"} {
		$menubar add cascade -label "Help" -underline 0 -menu $menubar.help
		menu $menubar.help -tearoff 0
		$menubar.help add command -label "Commands Overview" \
			-underline 0 -command {commands_overview}
		$menubar.help add command -label "About$elipsis" \
			-underline 0 -command {help_about}
	}
}

proc add_help_menu_sub {menubar HelpCmds} {
	global tcl_platform
	global mod
	global elipsis
	if {$tcl_platform(platform) != "macintosh"} {
		$menubar add cascade -label "Help" -underline 0 \
			-menu $menubar.help 
		menu $menubar.help -tearoff 0
		set LL [llength $HelpCmds]
		for {set i 0} {$i < $LL} {incr i 2} {
		$menubar.help add command -label [lindex $HelpCmds $i] \
			-underline 0 -command [lindex $HelpCmds [expr $i + 1]]
		}
	}
}


		# listener -- edit menu:
proc listener.cut {w} {
#	set w .topals
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
		$w.text delete sel.first sel.last
	}
}

proc listener.copy {w} {
	#set w .topals
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
	}
}

proc listener.paste {w} {
	global agv
	
#	set w .topals
	$w.text insert end [selection get -displayof $w -selection CLIPBOARD]
	set agv($w,dirty) true
	$w.text see end
	$w.text mark set insert end
	focus $w.text
}

proc listener.delete {w} {
#	set w .topals
	global array agv
	catch {$w.text delete sel.first sel.last}
	set agv($w,dirty) true
}

proc listener.select_all {w} {
	$w.text tag add sel 1.0 end
}


proc listener.copy_paste { w } {
	global tcl_platform
	global agv
#	set w .topals

	if  {$tcl_platform(platform) == "unix"} {
		set WhichSel PRIMARY
	} else {
		set WhichSel CLIPBOARD
	}
	set selflag [catch "selection get -displayof $w -selection $WhichSel" data]
	if {$selflag == "0" } then {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
		$w.text insert end [selection get -displayof $w -selection $WhichSel]
		set agv($w,dirty) true
		$w.text see end
		$w.text mark set insert end
		focus $w.text
	}
}

proc listener.find {w} {
#	start_edit_find .topals 
	start_edit_find $w
}


#################################################
#####	Utilities & Environment Settings       ##
#################################################

proc display_me {Title Win} {
	Window show $Win
	post_open_document $Title $Win
}

proc remove_me {Title Win} {
	wm withdraw $Win
	un_post_open_document $Title
}

proc iconify_me {Win} {
	wm iconify $Win
}
	 
proc careful_withdraw {Win} {
	if "[winfo exists $Win]>1" then { wm withdraw $Win }
}

proc choose_background_color {Window} {
	global array agv

	set COLOR [tk_chooseColor \
		-title "Choose Background Color" -initialcolor $agv(main,background)]
	if {$COLOR == ""} then {return}
	.alsdev_settings.background configure -background $COLOR
	$Window.text configure -background $COLOR
}

proc choose_foreground_color {Window} {
	global array agv

	set COLOR [tk_chooseColor \
		-title "Choose Foreground Color" -initialcolor $agv(main,foreground)]
	if {$COLOR == ""} then {return}
	.alsdev_settings.foreground configure -foreground $COLOR
	$Window.text configure -foreground $COLOR
}

proc font_family_choice { Family Window } {
	global array agv

	set PrevFont [$Window.text cget -font]
	set NewFont [list $Family [lindex $PrevFont 1] [lindex $PrevFont 2]]
	$Window.text configure -font $NewFont
}

proc font_size_choice { Size Window } {
	global array agv

	set PrevFont [$Window.text cget -font]
	set NewFont [list [lindex $PrevFont 0] $Size [lindex $PrevFont 2]]
	$Window.text configure -font $NewFont
}

proc font_style_choice { Style Window } {
	global array agv

	set PrevFont [$Window.text cget -font]
	set NewFont [list [lindex $PrevFont 0] [lindex $PrevFont 1] $Style ]
	$Window.text configure -font $NewFont
}

proc text_front_win {} {
	set Kids [winfo children .]
	set FrontWin [lindex $Kids end]
	if {$FrontWin == ".alsdev_settings"} then {
		set FrontWin \
			[lindex $Kids [expr [llength $Kids] - 2]]
	}
	if {$FrontWin == ""} then {
		bell ; return ""
	} else {
		return $FrontWin
	}
}

proc fonts_and_colors { Window } {
	global agv
	set agv(fonts_and_colors) $Window

	set SelectBackground [$Window.text cget -selectbackground ]
	set SelectForeground [$Window.text cget -selectforeground ]
	set Font [$Window.text cget -font ]
	set agv(text,family)  [lindex $Font 0]
	set agv(text,size)  [lindex $Font 1]
	set agv(text,style)  [lindex $Font 2]
	if {$agv(text,style) == ""} then {set agv(text,style) normal}
	set Tabs [$Window.text cget -tabs]

	Window show .alsdev_settings
	raise .alsdev_settings
	.alsdev_settings.buttons.save_settings configure \
		-command "save_fonts_and_colors $Window"
	wm title .alsdev_settings "Fonts&Colors: $Window"
    .alsdev_settings.background configure -background [$Window.text cget -background ]
    .alsdev_settings.foreground configure -foreground [$Window.text cget -foreground ]
	post_open_document Preferences .alsdev_settings

}

proc cancel_fonts_and_colors { } {
	Window hide .alsdev_settings
	un_post_open_document Preferences 
}

proc save_fonts_and_colors { Window } {
	global array agv
	
	if {[string first $agv(toplevel) $Window] == 0} {
		set Grp main 
	} else {
		set Grp .document 
	}
	set Background [$Window.text cget -background ]
	set Foreground [$Window.text cget -foreground ]
	set SelectBackground [$Window.text cget -selectbackground ]
	set SelectForeground [$Window.text cget -selectforeground ]
	set Font [$Window.text cget -font ]
	set Tabs [$Window.text cget -tabs]
	if ![regexp .console $Window] { set Geometry [wm geometry $Window] }

	set agv($Grp,background)         $Background
	set agv($Grp,foreground)         $Foreground
	set agv($Grp,selectbackground)   $SelectBackground
	set agv($Grp,selectbackground)   $SelectForeground
	set agv($Grp,font)               $Font
	set agv($Grp,tabs)               $Tabs
	if { $Grp == ".document" } {set agv(.document,geometry) $Geometry }

	Window hide .alsdev_settings
	un_post_open_document Preferences 
}


proc vTclWindow.alsdev_settings {base} {
	global tcl_platform
	global agv


    if {$base == ""} {
        set base .alsdev_settings
    }
    if {[winfo exists $base]} {
        show_window $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel_patch $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 233x142+199+212
    wm maxsize $base 1137 870
    wm minsize $base 0 0
    wm overrideredirect $base 0
    wm resizable $base 0 0
    wm deiconify $base
    wm title $base "Fonts & Colors"
	wm protocol .alsdev_settings WM_DELETE_WINDOW  {remove_me Preferences .alsdev_settings }

		# Text Font Description:
    label $base.font_label -text {Fonts} 

    label $base.family_label -text {Family:} 
	set FamilyMenuCmd \
		[concat tk_optionMenu $base.familymenu agv(text,family) \
			[concat user system [lsort -ascii [font families]] ] ]

	set FamilyMenu [eval $FamilyMenuCmd]
	set MenuEndNum [$FamilyMenu index end]

	for {set iii 0} {$iii <= $MenuEndNum} {incr iii} {
		$FamilyMenu entryconfigure $iii \
			-command "font_family_choice \"[$FamilyMenu entrycget $iii -label]\" \$agv(fonts_and_colors)"
	}

    label $base.size_label -text {Size:}
    if {$tcl_platform(platform) == "macintosh"} then {
		set SizeMenu [tk_optionMenu $base.sizemenu agv(text,size) \
			9 10 12 14 18 24 36]
	} else {
		set SizeMenu [tk_optionMenu $base.sizemenu agv(text,size) \
			6 8 10 12 14 16 18 20 22 24]
	}
	set MenuEndNum [$SizeMenu index end]
	for {set iii 0} {$iii <= $MenuEndNum} {incr iii} {
		$SizeMenu entryconfigure $iii \
			-command "font_size_choice [$SizeMenu entrycget $iii -label] \$agv(fonts_and_colors)"
	}

    label $base.style_label -text {Style:} 
	set StyleMenu [tk_optionMenu $base.stylemenu agv(text,style) \
		normal bold italic ]
	$StyleMenu entryconfigure 0 -command "font_style_choice normal  \$agv(fonts_and_colors)"
	$StyleMenu entryconfigure 1 -command "font_style_choice bold  \$agv(fonts_and_colors)"
	$StyleMenu entryconfigure 2 -command "font_style_choice italic  \$agv(fonts_and_colors)"


		# Text Color Description:
    label $base.color_label -text {Color: } 
    button $base.background \
		-background $agv(main,background) \
        -command "choose_background_color \$agv(fonts_and_colors)" -padx 11 -pady 4 -text Background 
    button $base.foreground \
		-foreground $agv(main,foreground) \
        -command "choose_foreground_color \$agv(fonts_and_colors)" -padx 11 -pady 4 -text Foreground 

		# Save settings button:
	frame $base.buttons -relief sunken -borderwidth 1
    button $base.buttons.save_settings \
        -command "save_fonts_and_colors .topals" -pady 2 -text {Save as Defaults} 
    button $base.buttons.cancel \
        -command cancel_fonts_and_colors -pady 2 -text {Dismiss} 

    ###################
    # SETTING GEOMETRY
    ###################
    grid columnconf $base 0 -weight 0
    grid rowconf $base 0 -weight 0

    grid $base.font_label \
		-column 0 -row 0 -columnspan 2 -rowspan 1 -sticky ew
    grid $base.family_label \
		-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew
    grid $base.familymenu \
		-column 1 -row 1 -columnspan 1 -rowspan 1 -sticky ew
    grid $base.size_label \
		-column 0 -row 2 -columnspan 1 -rowspan 1 -sticky ew
    grid $base.sizemenu \
		-column 1 -row 2 -columnspan 1 -rowspan 1 -sticky ew
    grid $base.style_label \
		-column 0 -row 3 -columnspan 1 -rowspan 1 -sticky ew
    grid $base.stylemenu \
		-column 1 -row 3 -columnspan 1 -rowspan 1 -sticky ew

    grid $base.color_label \
		-column 2 -row 0 -columnspan 1 -rowspan 1 -sticky ew
    grid $base.background \
		-column 2 -row 1 -columnspan 1 -rowspan 1 -sticky ew
    grid $base.foreground \
		-column 2 -row 2 -columnspan 1 -rowspan 1 -sticky ew

    grid $base.buttons \
		-column 0 -row 4 -columnspan 3 -rowspan 1 -sticky ew

    pack $base.buttons.save_settings \
		 -anchor center -expand 0 -fill none -side right -padx 8
    pack $base.buttons.cancel \
		 -anchor center -expand 0 -fill none -side left -padx 8
}

