#======================================================================
#		gen_app.tcl
#	Copyright (c) 1997-8 Applied Logic Systems Inc.
#
#		Generic Tcl/Tk tools for applications
#
#	Author: Kenneth A. Bowen, Applied Logic Systems, Inc.
#======================================================================

#--------------------------------------------------------------
# Error handling Procedures
#
# re - Report Errors: Forces the reporting of errors
#--------------------------------------------------------------
proc re {a} {
	if {[catch {uplevel $a} result]} then {
		bgerror $result
		error $result
	}
}

#--------------------------------------------------------------
# Basic try control structure for handling exceptions. 
# Two forms are supported:
#
# try A always B = Always execute B after A, even if A 
#				   fails (ie causes an exception).
# try A fail B = Execute B only when A fails 
#				   (ie causes exeception).
#--------------------------------------------------------------
proc try {a selector b} {
	if {$selector != "always" && $selector != "fail"} then {
		error "bad option \"$selector\": must be always or fail"
	}
	if {[catch {uplevel $a} result] == 0} then {
		if {$selector == "always"} then {uplevel $b}
	} else { 
		uplevel $b 
		error $result 
	}
}

proc show_window {w} {
	raise_patch $w
	wm deiconify $w
}

proc exit_app { } {
	global agv 
	set ans [tk_messageBox -icon warning -parent $agv(toplevel) \
		-title "Exit" -message "Really Exit $agv(title)?" \
		-type yesno -default yes]
	if {$ans == "yes"} then {
		if {$agv(settings_file) != "" } then {
			prolog call app_utils save_settings -atom $agv(settings_file)
		}
		save_settings
		if {[document.close_all]} then {
#			save_window_positions
			exit
			}
	} else {
		return 0
	}
}


proc std_agv_vals { } {
	global agv

	set VV [list \
			agv(.document,foreground) $agv(.document,foreground) \
			agv(.document,background) $agv(.document,background) \
			agv(.document,selectforeground) $agv(.document,selectforeground) \
			agv(.document,selectbackground) $agv(.document,selectbackground) \
			agv(.document,font) $agv(.document,font) \
			agv(.document,tabs) $agv(.document,tabs) \
			agv(main,foreground) $agv(main,foreground) \
			agv(main,background) $agv(main,background) \
			agv(main,selectforeground) $agv(main,selectforeground) \
			agv(main,selectbackground) $agv(main,selectbackground) \
			agv(main,font) $agv(main,font) \
			agv(main,tabs) $agv(main,tabs)  ]
	return $VV	
}

proc save_settings { } {
	global agv

	set File "$agv(app_name)_settings.tcl"
	set SS [open $File w]
			## Standard ones first:
	puts $SS $agv(version_stamp)
	puts $SS "set agv(.document,foreground) $agv(.document,foreground)"
	puts $SS "set agv(.document,background) $agv(.document,background)"
	puts $SS "set agv(.document,selectforeground) $agv(.document,selectforeground)"
	puts $SS "set agv(.document,selectbackground) $agv(.document,selectbackground)"
	puts $SS "set agv(.document,font) \"$agv(.document,font)\""
	puts $SS "set agv(.document,tabs) \"$agv(.document,tabs)\""
	puts $SS "set agv(main,foreground) $agv(main,foreground)"
	puts $SS "set agv(main,background) $agv(main,background)"
	puts $SS "set agv(main,selectforeground) $agv(main,selectforeground)"
	puts $SS "set agv(main,selectbackground) $agv(main,selectbackground)"
	puts $SS "set agv(main,font) \"$agv(main,font)\""
	puts $SS "set agv(main,tabs) \"$agv(main,tabs)\""
	puts $SS ""
	puts $SS "set agv(toplevel,geometry) \"[wm geometry $agv(toplevel)]\""


	save_app_settings $SS $agv(app_settings)

	close $SS
}



proc save_app_settings { Channel AGVExprsList } {
	global agv
	foreach i $AGVExprsList {
		if {([string first ",path)" $i] >= 0) || ([string first "_dir)" $i] >= 0)} {
			set IV [eval concat $$i]
			puts $Channel "set $i [list $IV]  "
		} else {
			puts $Channel "set $i \{[eval concat $$i]\}  "
		}
	}
}

proc set_path {Arr Tag List} {
	upvar #0 $Arr MA
	if {"$List"!=""} then {
		set FF [eval file join $List]
		set MA($Tag) $FF
	} else {
		set MA($Tag) ""
	}
}
	#--------------------------------------------------------------
	#	Effectively calls prolog with:
	#		Mod:send(Obj, Msg)
	# where:
	#		Mod = $agv(dflt_mod)
	#		$agv($Obj) = a number which is an object handle for Obj
	#		Msg is an atom
	#--------------------------------------------------------------
proc send_prolog {Obj Msg} {
	global agv
	prolog call $agv(dflt_mod) send -number $agv($Obj) -atom $Msg
}

	#--------------------------------------------------------------
	#	Effectively calls prolog with:
	#		Mod:send(Obj, Msg)
	# where:
	#		Mod = $agv(dflt_mod)
	#		$agv($Obj) = a number which is an object handle for Obj
	#		Msg is of type $Type 
	#				(normally use this with $Type = "list" )
	#--------------------------------------------------------------
proc send_prolog_t {Obj Msg Type} {
	global agv
	prolog call $agv(dflt_mod) send -number $agv($Obj) -$Type $Msg
}

proc send_prolog_1 {args} {
	global agv
	prolog call $agv(dflt_mod) send_from_tcl1 \
		-number $agv([lindex $args 0]) \
		-atom [lindex $args 1] [lindex $args 2] [lindex $args 3]
}

#proc send_prolog2 {args} {
#	global agv
#	eval prolog call $agv(dflt_mod) send -number $agv([lindex $args 0]) \
#		[lrange $args 1 end]
#}

		######################################
		##### Getting the APP started:
		######################################
proc load_photo_gif {image_name base_name} {
	global tcl_platform APPTCLPATH

	if {$tcl_platform(platform) == "macintosh"} {
		image create photo $image_name -format gif -data [resource read GIFf $base_name]
	} else {
		image create photo $image_name -file [file join $APPTCLPATH images $base_name.gif]
	}
}

proc init_gif_photos {GifList ImgDir} {
	global APPTCLPATH

	set ImgPath [file join $APPTCLPATH $ImgDir]
	foreach GIF $GifList {
		set PhotoName ""
		append PhotoName $GIF "_gif"
 		image create photo $PhotoName -file [file join $ImgPath $GIF.gif]
	}
}

proc setup_font {PropArray} {
	upvar $PropArray AA

	if {"$AA(text,sizeunits)"=="pixels"} then {
		set Size [ expr 0 - $AA(text,size) ]
	} else {
		set Size $AA(text,size)
	}
	set Font [list $AA(text,family) $Size $AA(text,style) ]
	set AA(text,font) $Font
}

proc choose_background_color {PropArray ChoiceWin AppWin} {
	upvar $PropArray AA
	 
	set COLOR [tk_chooseColor \
		-title "Choose Background Color" -initialcolor $AA(text,background)]
	if {"$COLOR" == ""} then {return}
	set AA(text,background) $COLOR
	$ChoiceWin configure -background $AA(text,background)
	$AppWin configure -background $AA(text,background)
}

proc choose_foreground_color {PropArray ChoiceWin AppWin} {
	upvar $PropArray AA
	 
	set COLOR [tk_chooseColor \
		-title "Choose Background Color" -initialcolor $AA(text,foreground)]
	if {"$COLOR" == ""} then {return}
	set AA(text,foreground) $COLOR
	$ChoiceWin configure -foreground $AA(text,foreground)
	$AppWin configure -foreground $AA(text,foreground)
}

proc install_font {PropArray AppWin} {
	upvar $PropArray AA

	setup_font AA
	$AppWin configure -font $AA(text,font)
}

proc source_app_files { FilesList } {
	global APPTCLPATH

	foreach File $FilesList {
		source [ file join $APPTCLPATH $File ]
	}
}

		######################################
		##### APP Info Display
		######################################

proc display_vn {SName VN Win} {
	set  Msg [format "\t\t%s Version %s \n\n" $SName $VN ]
	$Win insert end $Msg
	$Win tag configure bigbold -font {Helvetica 24 bold}
	$Win tag add bigbold 1.0 2.0
}

		######################################
		##### Projects
		######################################

proc select_project_file {PFXL DF projectFile} {
	set FTL [list [list "Project Files" $PFXL] ]
	set projectFile [tk_getOpenFile -filetypes $FTL \
			-title "Project File to Open" \
			-initialdir [pwd] \
			-initialfile $DF]
}

proc load_existing_project {} {
    set projectFile [tk_getOpenFile \
		-filetypes {{"Project Files" {.pjm}}} \
		-title "Project File to Open"]
	prolog call rocker load_project_file -atom $projectFile
}

		######################################
		##### File Path Stuff
		######################################

proc dbl_q_wins_path { Path } {
	set SplitPath [file split $Path]
	set Drive [lindex $SplitPath 0]
	set SplitPathTail [lrange $SplitPath 1 end]
	set DQPath ""
	append DQPath [string range $Drive 0 [expr [string length $Drive] - 2]]
	foreach E $SplitPathTail {
		append DQPath "\\\\" $E
	}
	return $DQPath
}
