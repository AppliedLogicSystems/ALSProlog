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
#		prolog call alsdev possible_save_project
		if {[document.close_all]} then {
#			save_window_positions
			exit
			}
	} else {
		return 0
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

