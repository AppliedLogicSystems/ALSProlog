
proc vTclWindow.alsdev_settings {base} {
	global array proenv

    if {$base == ""} {
        set base .alsdev_settings
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 535x154+199+212
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "ALS Prolog Development Environment Settings"
	wm protocol .alsdev_settings WM_DELETE_WINDOW {wm withdraw .alsdev_settings}

		# Text Font Description:
    frame $base.font_desc \
        -borderwidth 1 -relief sunken
    label $base.font_desc.family_label -text {Font Family:} 
	set FamilyMenuCmd \
		[concat tk_optionMenu $base.font_desc.familymenu proenv(text,family) [font families]]
	set FamilyMenu [eval $FamilyMenuCmd]
    label $base.font_desc.size_label -text {Size:} 
	set SizeMenu [tk_optionMenu $base.font_desc.sizemenu proenv(text,size) \
		6 8 10 12 14 16 18 20 22 24]
	set SizeUnitsMenu  [tk_optionMenu $base.font_desc.sizeunitsmenu proenv(text,sizeunits) \
		pixels points]
    label $base.font_desc.style_label -text {Style:} 
	set StyleMenu [tk_optionMenu $base.font_desc.stylemenu proenv(text,style) \
		normal bold italic ]
    frame $base.font_desc.spacer1 -borderwidth 1 -relief flat -width 3 -background Black
    button $base.font_desc.install \
        -command install_font -padx 2 -text Install

		# Text Color Description:
    frame $base.color_desc \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.color_desc.color_label \
        -text {Color: } 
    button $base.color_desc.background \
		-background $proenv(win_general,background) \
        -command choose_background_color -padx 11 -pady 4 -text Background 
    button $base.color_desc.foreground \
		-foreground $proenv(win_general,foreground) \
        -command choose_foreground_color -padx 11 -pady 4 -text Foreground 

		# Generated Files Locations:
    frame $base.obp_lcn \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.obp_lcn.glabel \
        -text {Generated Files (*.obp) Location: } 
	set OBPMenu [tk_optionMenu $base.obp_lcn.optmenu proenv(obplcn) \
		{Generated in Current (gic)} {Generated in Source(gis)} \
		{Generated in Arch/Current (giac)} {Generated in Arch/Source(gias)}  ]
	set proenv(obplcn) {Generated in Arch/Source(gias)}


		# Save settings button:
    button $base.save_settings \
        -command save_alsdev_settings -padx 11 -pady 4 \
		-text {Save Settings} 


    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.font_desc \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.font_desc.family_label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.font_desc.familymenu \
        -anchor center -expand 0 -fill none -side left 
    pack $base.font_desc.size_label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.font_desc.sizemenu \
        -anchor center -expand 0 -fill none -side left 
    pack $base.font_desc.sizeunitsmenu \
        -anchor center -expand 0 -fill none -side left 
    pack $base.font_desc.style_label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.font_desc.stylemenu \
        -anchor center -expand 0 -fill none -side left 
    pack $base.font_desc.spacer1 \
        -anchor center -expand 0 -fill y -padx 3 -side left 
    pack $base.font_desc.install \
        -anchor center -expand 0 -fill none -side left 

    pack $base.color_desc \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.color_desc.color_label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.color_desc.background \
        -anchor center -expand 0 -fill none -padx 8 -side left 
    pack $base.color_desc.foreground \
        -anchor center -expand 0 -fill none -padx 8 -side left 

    pack $base.obp_lcn \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.obp_lcn.glabel \
        -anchor center -expand 0 -fill none -side left 
    pack $base.obp_lcn.optmenu \
        -anchor center -expand 0 -fill none -side top 

    pack $base.save_settings \
        -anchor center -expand 0 -fill none -pady 4 -side bottom 
}

