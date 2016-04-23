##=================================================================================
# 		generic.tcl
# 		Copyright (c) 1998, 2000 Applied Logic Systems, Inc.
#
# 		generic IDE Document windows.
#
# agv indexes:
# -----------
# agv(document_index) - Monotonically increasing index to create unique window names
#
# agv(document_list)  - List of names of currently open documents
#
# agv(document,$file) - Name of window which contains the file $file
#
#
# Document fields:
# ---------------
# agv($window,file)   - Path to the file for this document. Empty string if untitled.
# agv($window,dirty)  - true iff document window is dirty.
#
#	Author: Chuck Houpt [original als_menu.tcl]
#	Date:	January 1998
#	Generic Mods: Ken Bowen [March 2000]
##=================================================================================

## Bindings in class Text to suppress:

proc adjust_Text_bindings {} {
	global mod
	if {$mod == "Ctrl"} then { set MMD Control } else { set MMD $mod }

	bind Text <$MMD-d> ""
	bind Text <$MMD-k> ""
	bind Text <$MMD-o> ""
	bind Text <$MMD-t> ""
	bind Text <$MMD-x> ""
	bind Text <Delete> ""
	bind Text <Delete> {
		if {[%W tag nextrange sel 1.0 end] != ""} {
			%W delete sel.first sel.last
		} elseif [%W compare insert != 1.0] {
			%W delete insert-1c
			%W see insert
		}
	}
}

adjust_Text_bindings

set agv(document_index) 0
set agv(document_list) {}

proc create_document_window {title} {
	global array agv
	global mod
	global tcl_platform

		# Create a unique window name:

	incr agv(document_index)
	set w ".document$agv(document_index)"
	
		# Create window:

	toplevel $w
	wm title $w $title
	if [info exists agv(.document,geometry)] {
#prolog call user write -atom "agv(.document,geometry)=$agv(.document,geometry)"
#prolog call user nl
		wm geometry $w $agv(.document,geometry)
	}
	wm protocol $w WM_DELETE_WINDOW "document.close $w"

		# Setup menus:

	menu $w.menubar -tearoff 0
	add_default_menus $w.menubar
	add_generic_file_menu $w.menubar document $w
	add_generic_edit_menu $w.menubar document $w
#	add_prolog_menu $w.menubar document $w
#	add_tools_menu $w.menubar document $w
#	add_windows_menu $w.menubar document $w
	add_help_menu $w.menubar
		
	$w configure -menu $w.menubar		

		# Setup text and scrollbars
	scrollbar $w.yscrollbar -orient vertical -command "$w.text yview"
	scrollbar $w.xscrollbar -orient horizontal -command "$w.text xview"

	text $w.text -yscrollcommand "$w.yscrollbar set" \
		-xscrollcommand "$w.xscrollbar set"  -wrap none -setgrid true \
		-height 16 -width 60

	grid columnconf $w 0 -weight 1
	grid columnconf $w 1 -weight 0
	grid rowconf $w 0 -weight 1
	grid rowconf $w 1 -weight 0

	grid $w.text -column 0 -row 0 -sticky nesw
	if {$tcl_platform(platform) == "macintosh"} {
		grid $w.yscrollbar -column 1 -row 0 -sticky ns -rowspan 2
	} else {
		grid $w.yscrollbar -column 1 -row 0 -sticky ns
	}
	grid $w.xscrollbar  -column 0 -row 1 -sticky ew

	$w.text configure -highlightthickness 0 \
		-background $agv(.document,background) \
		-foreground $agv(.document,foreground) \
		-selectbackground $agv(.document,selectbackground) \
		-selectforeground $agv(.document,selectforeground) \
		-font $agv(.document,font) \
		-tabs $agv(.document,tabs) 

		## setup linenumber pane and syntax error pane, but
		## don't realize them until they are needed:

    frame $w.error_headers -borderwidth 2 -relief groove \
		-background #ec5648
    button $w.error_headers.open_btn \
        -command "toggle_errors_frame $w" -image open_ptr -padx 11 -pady 4 
    pack $w.error_headers.open_btn \
		-in $w.error_headers -anchor center -expand 0 -fill none -side left 
    label $w.error_headers.lnum \
        -borderwidth 1 -relief raised -text Line# 
    label $w.error_headers.desc \
        -borderwidth 1 -relief raised -text {Syntax Error Description}
    pack $w.error_headers.lnum \
        -in $w.error_headers -anchor center -expand 0 -fill none -padx 15 \
        -side left 
    pack $w.error_headers.desc \
        -in $w.error_headers -anchor center -expand 0 -fill none -side left \
		-padx 30 

	text $w.ltext -yscrollcommand "$w.yscrollbar set" -wrap none -width 5 -setgrid true 

	$w.ltext configure -highlightthickness 0 \
		-background $agv(.document,background) \
		-foreground $agv(.document,foreground) \
		-font $agv(.document,font) 

    listbox $w.listbox \
        -xscrollcommand [list $w.errlist_xsb set] \
        -yscrollcommand [list $w.errlist_ysb set] 
    scrollbar $w.errlist_xsb \
        -borderwidth 1 -command [list $w.listbox xview] -orient horiz 
    scrollbar $w.errlist_ysb \
        -borderwidth 1 -command [list $w.listbox yview] -orient vert 
	$w.listbox configure -highlightthickness 0 \
		-background $agv(.document,background) \
		-foreground $agv(.document,foreground) \
		-font $agv(.document,font) 

	bind $w.listbox <Double-Button-1> [list error_focus_attn $w]

	## Now finish the principal window:

		# accelerators
	bind_accelerators $w $mod document
	wm geometry $w ""

	focus $w.text
	bind $w.text <Key> "dirty_key $w %K"

		# Init document fields
	set agv($w,dirty) false
	lappend agv(document_list) $w
	
	return $w
}

proc bothscrolly { w args } {
	eval $w.text yview $args
	eval $w.ltext yview $args
}

proc add_line_numbers_and_syn_errs { w } {
	global array agv
	set agv($w,error_frame) open

	$w.yscrollbar configure -command ""
	$w.yscrollbar configure -command "bothscrolly $w"

	grid columnconf $w 0 -weight 0
	grid columnconf $w 1 -weight 1
	grid columnconf $w 2 -weight 0
	grid rowconf $w 0 -weight 1
	grid rowconf $w 1 -weight 0
	grid rowconf $w 2 -weight 0
	grid rowconf $w 3 -weight 0
	grid rowconf $w 4 -weight 0

	$w.ltext configure -width 5
	grid $w.ltext  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.text -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $w.yscrollbar -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.xscrollbar  -column 1 -row 1 -columnspan 1 -rowspan 1 -sticky ew

	grid $w.error_headers  -column 0 -row 2 -columnspan 3 -rowspan 1 -sticky ew
	$w.listbox delete 0 end
	grid $w.listbox -column 0 -row 3 -columnspan 2 -rowspan 1 -sticky nesw
	grid $w.errlist_ysb -column 2 -row 3 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.errlist_xsb  -column 0 -row 4 -columnspan 2 -rowspan 1 -sticky ew

	set LastIX [$w.text index end]
	set NL0 [expr [string range $LastIX 0 [expr [string first "." $LastIX] - 1]] - 1]
	$w.ltext delete 1.0 end
	for {set linenum 1} {$linenum < $NL0 } {incr linenum} {
		$w.ltext insert end [format "%s\n" $linenum]
	}
	$w.ltext insert end $NL0
}

proc close_line_numbers_and_syn_errs { w } {
	global array agv

	grid forget $w.ltext  

	grid forget $w.listbox 
	grid forget $w.errlist_ysb 
	grid forget $w.errlist_xsb  
}

proc restore_line_numbers_and_syn_errs { w } {
	global array agv

	$w.ltext configure -width 5
	grid $w.ltext  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.listbox -column 0 -row 3 -columnspan 2 -rowspan 1 -sticky nesw
	grid $w.errlist_ysb -column 2 -row 3 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.errlist_xsb  -column 0 -row 4 -columnspan 2 -rowspan 1 -sticky ew
}

proc add_left_col { w N } {
	global array agv

	$w.yscrollbar configure -command ""
	$w.yscrollbar configure -command "bothscrolly $w"

	grid columnconf $w 0 -weight 0
	grid columnconf $w 1 -weight 1
	grid columnconf $w 2 -weight 0

	$w.ltext configure -width $N 

	grid $w.ltext  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.text -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $w.yscrollbar -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.xscrollbar  -column 1 -row 1 -columnspan 1 -rowspan 1 -sticky ew

	set LastIX [$w.text index end]
	set NL0 [expr [string range $LastIX 0 [expr [string first "." $LastIX] - 1]] - 1]
	$w.ltext delete 1.0 end
	for {set linenum 1} {$linenum < $NL0 } {incr linenum} {
		$w.ltext insert end [format " \n" ]
	}
	$w.ltext insert end $NL0
}

proc toggle_errors_frame { w } {
	global agv

	if { $agv($w,error_frame) == "open" } then {
		set agv($w,error_frame) closed
	   	$w.error_headers.open_btn configure -image closed_ptr
		close_line_numbers_and_syn_errs $w
	} else {
		set agv($w,error_frame) open
    	$w.error_headers.open_btn configure -image open_ptr
		restore_line_numbers_and_syn_errs $w
	}
}

proc dirty_key {w k} {
	global array agv

	if {$k != "Home" && $k != "End" && $k != "Prior" && $k != "Next"
		&& $k != "Left" && $k != "Right" && $k != "Up" && $k != "Down"
		&& $k != "Control_L" && $k != "Control_R"
		&& $k != "Shift_L" && $k != "Shift_R"
		&& $k != "Alt_L" && $k != "Alt_R"
		&& $k != "Meta_L" && $k != "Meta_R"
		&& $k != "Caps_Lock" && $k != "Num_Lock" && $k != "Help"
		} then {
		set agv($w,dirty) true
	}
}

proc bind_accelerators {w mod type} {
	global tcl_platform
	
	if {$tcl_platform(platform) == "macintosh"} return;
	
	if {$mod == "Ctrl"} then { set MMD Control } else { set MMD $mod }
#prolog call user write -atom "bacc: w=$w mod=$mod type=$type"
#prolog call user nl

		# file menu:
	bind $w.text <$MMD-n> "$type.new"
	bind $w.text <$MMD-o> "$type.open"
	bind $w.text <$MMD-w> "$type.close $w"
	bind $w.text <$MMD-s> "$type.save $w"
	bind $w.text <$MMD-q> "exit_app"

		# edit menu:
#	 bind $w.text <$MMD-z> "$type.undo $w"

	 bind $w.text <$MMD-x> "$type.cut $w; break"
	 bind $w.text <$MMD-c> "$type.copy $w; break"
	 bind $w.text <$MMD-v> "$type.paste $w; break"
	 bind $w.text <$MMD-a> "$type.select_all $w; break"
	 bind $w.text <$MMD-f> "$type.find $w; break"

		# prolog menu:
	bind $w.text <$MMD-k> "$type.consult $w"
}

proc clear_src_win_decorations {w line} {
	$w.text tag delete syntax_err_head syntax_err_tail head_tag call_tag
	$w.ltext insert $line.0 " "
	update idletasks
}

proc close_error_annotations {w} {
	grid remove $w.listbox $w.errlist_ysb $w.errlist_xsb  $w.error_headers  $w.ltext  
	update idletasks
}

proc dispose_document_window {w} {
	global array agv
	if {[info exists agv($w,file)] && [info exists agv(document,$agv($w,file))]} then {
		unset agv(document,$agv($w,file))
	}
	if {[info exists agv($w,title)]} then {
		un_post_open_document $agv($w,title)
		unset agv($w,title)
	}
	if {[info exists agv($w,file)]} then {unset agv($w,file)}
	if {[info exists agv($w,dirty)]} then {unset agv($w,dirty)}
	set i [lsearch -exact $agv(document_list) $w]
	set agv(document_list) [lreplace $agv(document_list) $i $i]
	
	destroy $w
}


proc load_text {file text} {
	set s [open $file r]
	try {
		set data [read -nonewline $s]
	} always {
		close $s
	}
	$text insert end $data
}

proc store_text {text file} {
	set s [open $file w]
	try {
		set data [$text get 1.0 end]
		puts -nonewline $s $data
	} always {
		close $s
	}
}

proc load_document {file} {
	global array agv 
	if {[info exists agv(document,$file)]} {
		raise $agv(document,$file)
	} else {
		set file_name [lindex [file split $file] end]
		set w [create_document_window $file_name]		
		try {
			load_text $file $w.text
			set agv($w,file) $file
			set agv($w,title) $file_name
			set agv(document,$file) $w
			post_open_document $file_name $w
		} fail {
			dispose_document_window $w
		}
	}
	return $agv(document,$file)
}

proc close_and_reopen {w} {
	global array agv 
	close_error_annotations $w
	set file $agv($w,file)
	$w.text delete 1.0 end
	try {
		load_text $file $w.text
		set agv(document,$file) $w
	} fail {
		dispose_document_window $w
	}
	return $agv(document,$file)
}


proc post_open_document {Title Win} {
	global agv 
	if {[lsearch -exact $agv(posted_vis) $Title] < 0} then {
		lappend agv(posted_vis) $Title
		$agv(main_mb).windows add command -label $Title -command "show_window $Win"
	}
#		.topals.mmenb.windows add command -label $Title -command "show_window $Win"
}

proc un_post_open_document {Title} {
	global agv 
	set Prev [lsearch -exact $agv(posted_vis) "$Title"] 
	if {$Prev >= 0} then {
		set PrjIdx [$agv(main_mb).windows index $Title]
		$agv(main_mb).windows delete $PrjIdx
		set agv(posted_vis) [lreplace $agv(posted_vis) $Prev $Prev]
	}
#		set PrjIdx [.topals.mmenb.windows index $Title]
#		.topals.mmenb.windows delete $PrjIdx
}

	########################
	# Document methods
	########################

proc main.new {} { document.new }
proc main.open args { document.open $args }
proc main.save {w} { }
proc main.close {} { exit_app }

proc main.cut {w} { document.cut $w }
proc main.copy {w} { document.copy $w }
proc main.paste {w} { document.paste $w }
proc main.delete {w} { document.delete $w }
proc main.select_all {w} { document.select_all $w }
proc main.find {w} { document.find $w }



proc document.new {} {
	global array agv

	incr agv(untitled_counter) 
	set Title "Untitled #$agv(untitled_counter)"
	set w [create_document_window $Title]
	set agv($w,title) $Title
	post_open_document $Title $w
}


proc document.open args {
	global agv tcl_platform
	set file_list $args
	if {$file_list == ""} then {
		if {$tcl_platform(platform) == "macintosh"} {
			set types {{"Text Files" * TEXT} {"Prolog Files" {.pro .pl} TEXT} {"Tcl/Tk Files" {.tcl} TEXT}}
		} else {
			set types [list [list \"$agv(title)\" ".$agv(doc_extension)" ] {{All Files} *} ]
		}
		set file [tk_getOpenFile \
			-title "Open File" \
			-filetypes $types ] 
		if {$file != ""} then {
			set file_list [list $file]
		}
	}
	foreach file $file_list {
		set FT [file tail $file]
		set BaseFile [file rootname [file tail $file]]
		set Ext [file extension $file],

#		send_prolog_t als_ide_mgr [list open_edit_win $file $BaseFile $Ext] list
			## prolog source_handler will call back to do:
			##		load_document $file

		load_document $file
	}
}

proc save_check {w} {
	global array agv
	if {$agv($w,dirty)} then {
		raise $w
		set title [wm title $w]
		set answer [tk_dialog .document_save_dialog "" \
			"Save changes to the document \"$title\" before closing?" \
			{} \
			2 "Don't Save" "Cancel" "Save"]
		if {$answer == 2} then {
			set result [document.save $w]
		} else {
			set result [expr $answer != 1]
		}
	} else {
		set result true
	}
	return $result
}


proc document.close {w} {
	global array agv	
	if {[save_check $w]} then {
#		prolog call $agv(dflt_mod) send -number $agv($w,src_handler) -atom close_edit_win
			## the prolog side used to do this, but moved back here:
		dispose_document_window $w
		return true
	} else {
		return false
	}
}

proc document.close_all {} {
	global array agv	
	foreach w $agv(document_list) {
		if {[document.close $w] == "false"} then {return false}
	}
	return true
}


proc document.save {w} {
	global tcl_platform agv
	if {[info exists agv($w,file)]} then {
		store_text $w.text $agv($w,file)
		set agv($w,dirty) false
		return true
	} else {
		if {$tcl_platform(platform) == "macintosh"} {
			set types {{"Text Files" * TEXT} {"Prolog Files" {.pro .pl} TEXT} {"Tcl/Tk Files" {.tcl} TEXT}}
		} else {
			set types [list [list \"$agv(title)\" ".$agv(doc_extension)" ] {{All Files} *} ]
		}
		set file [tk_getSaveFile -initialfile [wm title $w].$agv(doc_extension) \
			-defaultextension .$agv(doc_extension) -filetypes $types ]
		if {$file != ""} then {
			un_post_open_document $agv($w,title)
			save_as_core $w $file
#			send_prolog_t als_ide_mgr [list save_doc_as $w $file] list
			set file_name [lindex [file split $file] end]
			set agv($w,file) $file
			set agv($w,title) $file_name
			set agv(document,$file) $w
			post_open_document $file_name $w
		} else {
			return false
		}
	}
	if {[info exists agv($w,src_handler)]} then {
#		prolog call alsdev send -number $agv($w,src_handler) -atom clear_errors_display
	}
	return true
}

proc document.save_as {w} {
	global array agv
	
	set file [tk_getSaveFile -initialfile [wm title $w] \
		-defaultextension .pro ]
	if {$file != ""} then {
		save_as_core $w $file
#		send_prolog_t als_ide_mgr [list save_doc_as $w $file] list
		return true
	} else {
		return false
	}
}

proc save_as_core {w file} {
	global array agv

	if {[info exists agv($w,file)]} then {
		unset agv(document,$agv($w,file))
	}
	set agv($w,file) $file
	set agv(document,$file) $w
	wm title $w [lindex [file split $file] end]
	store_text $w.text $file
	set agv($w,dirty) false
	if {[tk windowingsystem] == "aqua"} then {
		file attributes $file -creator ALS4 -type TEXT 
	}
}

proc document.cut {w} {
	global array agv

 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
		$w.text delete sel.first sel.last
		set agv($w,dirty) true
	}
}

proc document.copy {w} {
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
	}
}

proc document.paste {w} {
	global array agv
	global tcl_platform

	catch {$w.text delete sel.first sel.last}
	set clip [selection get -displayof $w -selection CLIPBOARD]
	if {$tcl_platform(platform) == "macintosh"} {
		set lines [split $clip \r]
		set clip [join $lines \n]
	}
	$w.text insert insert $clip
	set agv($w,dirty) true
}

proc document.delete {w} {
	global array agv
	catch {$w.text delete sel.first sel.last}
	set agv($w,dirty) true
}

proc document.select_all {w} {
	$w.text tag add sel 1.0 end
}

proc document.find {w} {
	start_edit_find $w
}

proc document.preferences {w} {

}

proc document.consult {w} {
	global agv

	if [info exists agv($w,file)] then {
		set file $agv($w,file)
	} else {
		set file ""
	}
	set title [wm title $w]
	if {$file == ""} {
		set answer [tk_dialog .document_save_dialog "" \
			"Save document \"$title\" to file in order to consult?" \
			{} \
			1 "Cancel" "Save"]
		if { $answer == 1 } then {
			if {[document.save $w]} then {
				set file $agv($w,file)
			} else { return }
		} else { return }
	}
	if {$agv($w,dirty)} {
		bell
		set answer [tk_dialog .document_save_dialog "" \
			"Save changes to the document \"$title\" in order to consult?" \
			{} \
			1 "Cancel" "Save"]
		if { $answer == 1 } then {
			document.save $w
		} else {
			return
		}
	}
	catch { prolog call alsdev do_reconsult -atom $file }
	insert_prompt  .topals.text "\n?-" 
}

proc document.goto_line {w} {
	global array agv
	
	set N [do_popup_input "Input line number:" "Number?"]

	$w.text tag delete focusline 
	$w.text see $N.0
	$w.text tag add focusline $N.0 $N.end
	$w.text tag configure focusline -background yellow
}


proc create_graph_window {W H title} {
	global array agv
	global mod
	global tcl_platform

prolog call user write -atom "enter create_graph_window $title"
prolog call user nl
update

		# Create a unique window name:

	incr agv(document_index)
	set w ".document$agv(document_index)"
	
		# Create window:

	toplevel $w
	wm title $w $title
	if [info exists agv(.document,geometry)] {
#prolog call user write -atom "agv(.document,geometry)=$agv(.document,geometry)"
#prolog call user nl
		wm geometry $w $agv(.document,geometry)
	}
	wm protocol $w WM_DELETE_WINDOW "graph.close $w"


 	canvas $w.canvas -width $W -height $H 
 	pack $w.canvas  -in $w -expand 1 -fill both
	set agv($w,canvas) $w.canvas
	

	## Now finish the principal window:

		# accelerators
		# bind_accelerators $w $mod graph
	if {$tcl_platform(platform) != "macintosh"} {
		if {$mod == "Ctrl"} then { set MMD Control } else { set MMD $mod }
		bind $w.canvas <$MMD-w> "graph.close $w"
		bind $w.canvas <$MMD-s> "graph.save $w"
	 	bind $w.canvas <$MMD-c> "graph.copy $w; break"
	 	bind $w.canvas <$MMD-a> "graph.select_all $w; break"
	}

	wm geometry $w ""
	focus $w.canvas
		# Init document fields
	set agv($w,dirty) false
	lappend agv(document_list) $w

	return $w

#	bind $w.canvas <Key> "dirty_key $w %K"
}

proc graph.new { W H } {
	global array agv

prolog call user write -atom "enter graph.new $W $H"
prolog call user nl
update

	incr agv(untitled_counter) 
	set Title "Graph #$agv(untitled_counter)"
	set w [create_graph_window $W $H $Title]
	set agv($w,title) $Title
	post_open_document $Title $w
	return $w
}


proc graph.close { w } {
	global array agv
	if {[save_check $w]} then {
			## the prolog side used to do this, but moved back here:
		dispose_document_window $w
		return true
	} else {
		return false
	}
}

proc graph.save { w } {
	global array agv

	if {[info exists agv($w,file)]} then {
		store_graph $w.canvas $agv($w,file)
		set agv($w,dirty) false
		return true
	} else {
		if {$tcl_platform(platform) == "macintosh"} {
			set types {{"Text Files" * TEXT} {"Prolog Files" {.pro .pl} TEXT} {"Tcl/Tk Files" {.tcl} TEXT}}
		} else {
			set types [list [list \"$agv(title)\" ".$agv(doc_extension)" ] {{All Files} *} ]
		}
		set file [tk_getSaveFile -initialfile [wm title $w].$agv(doc_extension) \
			-defaultextension .$agv(doc_extension) -filetypes $types ]
		if {$file != ""} then {
			un_post_open_document $agv($w,title)
			graph_save_as_core $w $file
			set file_name [lindex [file split $file] end]
			set agv($w,file) $file
			set agv($w,title) $file_name
			set agv(document,$file) $w
			post_open_document $file_name $w
		} else {
			return false
		}
	}
	return true
}

proc graph_save_as_core {w file} {
	global array agv

	if {[info exists agv($w,file)]} then {
		unset agv(document,$agv($w,file))
	}
	set agv($w,file) $file
	set agv(document,$file) $w
	wm title $w [lindex [file split $file] end]
	store_canvas $w.canvas $file
	set agv($w,dirty) false
	if {[tk windowingsystem] == "aqua"} then {
		file attributes $file -creator ALS4 -type TEXT 
	}
}

## THESE NEED WORK-- THEY ARE ONLY THE document VERSIONS....
proc graph.copy { w } {
 	if {![catch {set data [$w.canvas get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
	}
	global array agv
}

proc graph.select_all { w } {
	global array agv
	$w.canvas tag add sel 1.0 end
}

