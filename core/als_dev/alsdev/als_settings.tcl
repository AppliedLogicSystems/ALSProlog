
proc vTclWindow.alsdev_settings {base} {
	global tcl_platform
	global array proenv

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
	wm protocol .alsdev_settings WM_DELETE_WINDOW  {wm withdraw .alsdev_settings }

		# Text Font Description:
    label $base.font_label -text {Fonts} 

    label $base.family_label -text {Family:} 
	set FamilyMenuCmd \
		[concat tk_optionMenu $base.familymenu proenv(text,family) \
			[concat user system [lsort -ascii [font families]] ] ]

	set FamilyMenu [eval $FamilyMenuCmd]
	set MenuEndNum [$FamilyMenu index end]

	for {set iii 0} {$iii <= $MenuEndNum} {incr iii} {
		$FamilyMenu entryconfigure $iii \
			-command "font_family_choice \"[$FamilyMenu entrycget $iii -label]\" \$proenv(fonts_and_colors)"
	}

    label $base.size_label -text {Size:}
    if {$tcl_platform(platform) == "macintosh"} then {
		set SizeMenu [tk_optionMenu $base.sizemenu proenv(text,size) \
			9 10 12 14 18 24 36]
	} else {
		set SizeMenu [tk_optionMenu $base.sizemenu proenv(text,size) \
			6 8 10 12 14 16 18 20 22 24]
	}
	set MenuEndNum [$SizeMenu index end]
	for {set iii 0} {$iii <= $MenuEndNum} {incr iii} {
		$SizeMenu entryconfigure $iii \
			-command "font_size_choice [$SizeMenu entrycget $iii -label]  \$proenv(fonts_and_colors)"
	}

    label $base.style_label -text {Style:} 
	set StyleMenu [tk_optionMenu $base.stylemenu proenv(text,style) \
		normal bold italic ]
	$StyleMenu entryconfigure 0 -command "font_style_choice normal  \$proenv(fonts_and_colors)"
	$StyleMenu entryconfigure 1 -command "font_style_choice bold  \$proenv(fonts_and_colors)"
	$StyleMenu entryconfigure 2 -command "font_style_choice italic  \$proenv(fonts_and_colors)"

		# Text Color Description:
    label $base.color_label -text {Color: } 
    button $base.background \
		-background $proenv(.topals,background) \
        -command "choose_background_color \$proenv(fonts_and_colors)" -padx 11 -pady 4 -text Background 
    button $base.foreground \
		-foreground $proenv(.topals,foreground) \
        -command "choose_foreground_color \$proenv(fonts_and_colors)" -padx 11 -pady 4 -text Foreground 

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

