##=================================================================================
# 		als_document.tcl
# 		Copyright (c) 1998 Applied Logic Systems, Inc.
#
# IDE Document windows.
#
# proenv indexes
#
# proenv(document_index) - Monotonically increasing index to create unique window names
#
# proenv(document_list)  - List of names of currently open documents
#
# proenv(document,$file) - Name of window which contains the file $file
#
#
# Document fields
# proenv($window,file) - Path to the file for this document. Empty string if untitled.
# proenv($window,dirty) - true iff document window is dirty.
#
##=================================================================================

## Bindings in class Text to suppress:

proc adjust_Text_bindings {} {
	global mod
	if {"$mod"=="Ctrl"} then { set MMD Control } else { set MMD $mod }

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

set proenv(document_index) 0
set proenv(document_list) {}

proc create_document_window {title} {
	global array proenv
	global mod

		# Create a unique window name:

	incr proenv(document_index)
	set w ".document$proenv(document_index)"
	
		# Create window:

	toplevel $w
	wm title $w $title
	wm protocol $w WM_DELETE_WINDOW "document.close $w"

		# Setup menus:

	menu $w.menubar -tearoff 0
	add_default_menus $w.menubar
	add_file_menu $w.menubar document $w
	add_edit_menu $w.menubar document $w
	add_prolog_menu $w.menubar document $w
	add_tools_menu $w.menubar document $w
	add_help_menu $w.menubar
		
	$w configure -menu $w.menubar		

		# Setup text and scrollbars
	scrollbar $w.yscrollbar -orient vertical -command "$w.text yview"
	scrollbar $w.xscrollbar -orient horizontal -command "$w.text xview"

	text $w.text -yscrollcommand "$w.yscrollbar set" -xscrollcommand "$w.xscrollbar set"  -wrap none -setgrid true

	grid columnconf $w 0 -weight 1
	grid columnconf $w 1 -weight 0
	grid rowconf $w 0 -weight 1
	grid rowconf $w 1 -weight 0

	grid $w.text -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $w.yscrollbar -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	grid $w.xscrollbar  -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew

	$w.text configure -highlightthickness 0 \
		-background $proenv(.document,background) \
		-foreground $proenv(.document,foreground) \
		-selectbackground $proenv(.document,selectbackground) \
		-selectforeground $proenv(.document,selectforeground) \
		-font $proenv(.document,font) \
		-tabs $proenv(.document,tabs) 

		## setup linenumber pane and syntax error pane, but
		## don't realize them until they are needed:
    frame $w.error_headers -borderwidth 2 -relief groove \
		-background #ec5648
    label $w.error_headers.lnum \
        -borderwidth 1 -relief raised -text Line# 
    label $w.error_headers.desc \
        -borderwidth 1 -relief raised -text {Syntax Error Description}
    pack $w.error_headers.lnum \
        -in $w.error_headers -anchor center -expand 0 -fill none -padx 15 \
        -side left 
    pack $w.error_headers.desc \
        -in $w.error_headers -anchor center -expand 0 -fill none -side top 

	text $w.ltext -yscrollcommand "$w.yscrollbar set" -wrap none -width 5 -setgrid true 

	$w.ltext configure -highlightthickness 0 \
		-background $proenv(.document,background) \
		-foreground $proenv(.document,foreground) \
		-font $proenv(.document,font) 

    listbox $w.listbox \
        -xscrollcommand [list $w.errlist_xsb set] \
        -yscrollcommand [list $w.errlist_ysb set] 
    scrollbar $w.errlist_xsb \
        -borderwidth 1 -command [list $w.listbox xview] -orient horiz 
    scrollbar $w.errlist_ysb \
        -borderwidth 1 -command [list $w.listbox yview] -orient vert 
	$w.listbox configure -highlightthickness 0 \
		-background $proenv(.document,background) \
		-foreground $proenv(.document,foreground) \
		-font $proenv(.document,font) 

	bind $w.listbox <Double-Button-1> [list error_focus_attn $w]

	## Now finish the principal window:

		# accelerators
	bind_accelerators $w $mod document

	focus $w.text
	bind $w.text <Key> "dirty_key $w %K"

		# Init document fields
	set proenv($w,dirty) false
	lappend proenv(document_list) $w
	
	return $w
}

proc bothscrolly { w args } {
	eval $w.text yview $args
	eval $w.ltext yview $args
}

proc add_line_numbers_and_syn_errs { w } {
	global array proenv

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

proc add_left_col { w N } {
	global array proenv

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

proc dirty_key {w k} {
	global array proenv

	if {$k!="Home" && $k!="End" && $k!="Prior" && $k!="Next" \
		&& $k!="Left" && $k!="Right" && $k!="Up" && $k!="Down" \
		&& $k!="Control_L" && $k!="Control_R" \
		&& $k!="Shift_L" && $k!="Shift_R" \
		&& $k!="Alt_L" && $k!="Alt_R" \
		&& $k!="Meta_L" && $k!="Meta_R" \
		&& $k!="Caps_Lock" && $k!="Num_Lock" && $k!="Help"
		} {
		set proenv($w,dirty) true
	}
}

proc bind_accelerators {w mod type} {
	global tcl_platform
	
	if {"$tcl_platform(platform)" == "macintosh"} return;
	
	if {"$mod"=="Ctrl"} then { set MMD Control } else { set MMD $mod }

		# file menu:
	bind $w.text <$MMD-n> "$type.new"
	bind $w.text <$MMD-o> "$type.open"
	bind $w.text <$MMD-w> "$type.close $w"
	bind $w.text <$MMD-s> "$type.save $w"
	bind $w.text <$MMD-q> "exit_prolog"

		# edit menu:
#	 bind $w.text <$MMD-z> "$type.undo $w"

	 bind $w.text <$MMD-x> "$type.cut $w; break"
	 bind $w.text <$MMD-c> "$type.copy $w; break"
	 bind $w.text <$MMD-v> "$type.paste $w; break"
	 bind $w.text <$MMD-a> "$type.select_all $w; break"

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
	global array proenv
	if {[info exists proenv($w,file)] && [info exists proenv(document,$proenv($w,file))]} then {
		unset proenv(document,$proenv($w,file))
	}
	if {[info exists proenv($w,title)]} then {
		un_post_open_document $proenv($w,title)
		unset proenv($w,title)
	}
	if {[info exists proenv($w,file)]} then {unset proenv($w,file)}
	if {[info exists proenv($w,dirty)]} then {unset proenv($w,dirty)}
	set i [lsearch -exact $proenv(document_list) $w]
	set proenv(document_list) [lreplace $proenv(document_list) $i $i]
	
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
	global array proenv 
	if {[info exists proenv(document,$file)]} {
		raise $proenv(document,$file)
	} else {
		set file_name [lindex [file split $file] end]
		set w [create_document_window $file_name]		
		try {
			load_text $file $w.text
			set proenv($w,file) $file
			set proenv($w,title) $file_name
			set proenv(document,$file) $w
		} fail {
			dispose_document_window $w
		}
	}
	post_open_document $file_name $w
	return $proenv(document,$file)
}

proc post_open_document {Title Win} {
	global array tcl_platform 
	global mod

	.topals.mmenb.file delete last
	.topals.mmenb.file delete last
	.topals.mmenb.file add command \
		-label $Title -font {Helvetica 10 italic} -command [list bringup $Win]
    .topals.mmenb.file add separator
	if {$tcl_platform(platform) == "windows"} {
    	.topals.mmenb.file add command -label "Exit" -underline 1 -accelerator "$mod-Q" -command {re exit_prolog}
    } else {
    	.topals.mmenb.file add command -label "Quit" -accelerator "$mod-Q" -command {re exit_prolog}
	}
}

proc un_post_open_document {Title} {
	set PrjIdx [.topals.mmenb.file index $Title]
	.topals.mmenb.file delete $PrjIdx
}

	########################
	# Document methods
	########################

proc document.new {} {
	global array proenv

	incr proenv(untitled_counter) 
	set w [create_document_window "Untitled #$proenv(untitled_counter)"]
	send_prolog_t als_ide_mgr [list open_non_file_edit_win $w] list
}

proc document.open args {
	global tcl_platform
	set file_list $args
	if {$file_list == ""} then {
		if {$tcl_platform(platform) == "macintosh"} {
			set types {{"Text Files" * TEXT} {"Prolog Files" {.pro .pl} TEXT} {"Tcl/Tk Files" {.tcl} TEXT}}
		} else {
			set types {{"Prolog Files" {.pro .pl}} {"Tcl/Tk Files" {.tcl}} {{All Files} *}}
		}
		set file [tk_getOpenFile \
			-title "Open File" \
			-filetypes $types ] 
		if { $file != "" } then {
			set file_list [list $file]
		}
	}
	foreach file $file_list {
		set FT [file tail $file]
		set BaseFile [file rootname [file tail $file]],
		set Ext [file extension $file],
		send_prolog_t als_ide_mgr [list open_edit_win $file $BaseFile $Ext] list
			## prolog source_handler will call back to do:
#		load_document $file
	}
}

proc save_check {w} {
	global tcl_platform
	global array proenv
	if {$proenv($w,dirty)} then {
		raise $w
		set title [wm title $w]
		if {$tcl_platform(platform) == "macintosh"} {
			set icon caution
		} else {
			set icon warning
		}
		set answer [tk_dialog .document_save_dialog "" \
			"Save changes to the document \"$title\" before closing?" \
			$icon \
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
	global array proenv	
	if {[save_check $w]} then {
		prolog call $proenv(dflt_mod) send -number $proenv($w,src_handler) -atom close_edit_win
			## the prolog side does this:
			#		dispose_document_window $w
		return true
	} else {
		return false
	}
}

proc document.close_all {} {
	global array proenv	
	foreach w $proenv(document_list) {
		if {[document.close $w]=="false"} then {return false}
	}
	return true
}


proc document.save {w} {
	global array proenv
	if {[info exists proenv($w,file)]} then {
		store_text $w.text $proenv($w,file)
		set proenv($w,dirty) false
		return true
	} else {
		return [document.save_as $w]
	}
}

proc document.save_as {w} {
	global array proenv
	global tcl_platform
	
	set file [tk_getSaveFile -initialfile [wm title $w] \
		-defaultextension .pro ]
	if {$file != ""} then {
		if {[info exists proenv($w,file)]} then {
			unset proenv(document,$proenv($w,file))
		}
		set proenv($w,file) $file
		set proenv(document,$file) $w
		wm title $w [lindex [file split $file] end]
		store_text $w.text $file
		set proenv($w,dirty) false
		if {$tcl_platform(platform) == "macintosh"} then { file attributes $file -creator ALS4 -type TEXT }
		send_prolog_t als_ide_mgr [list save_doc_as $w $file] list
		return true
	} else {
		return false
	}
}

proc document.cut {w} {
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
		$w.text delete sel.first sel.last
		set proenv($w,dirty) true
	}
}

proc document.copy {w} {
 	if {![catch {set data [$w.text get sel.first sel.last]}]} {
	    clipboard clear -displayof $w
	    clipboard append -displayof $w $data
	}
}

proc document.paste {w} {
	catch {$w.text delete sel.first sel.last}
	$w.text insert insert [selection get -displayof $w -selection CLIPBOARD]
	set proenv($w,dirty) true
}

proc document.clear {w} {
	global array proenv
	catch {$w.text delete sel.first sel.last}
	set proenv($w,dirty) true
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
	global array proenv

	set file $proenv($w,file)
	
	if {$file == ""} {
		if {![document.save $w]} {return}
	}
	
	if {$proenv($w,dirty)} {
		bell
		return
	}
	catch { prolog call alsdev do_reconsult -atom $file }
	insert_prompt  .topals.text "\n?-" 
}

proc document.goto_line {w} {
	global array proenv
	
	set N [do_popup_input "Input line number:" "Number?"]

	$w.text tag delete focusline 
	$w.text see $N.0
	$w.text tag add focusline $N.0 $N.end
	$w.text tag configure focusline -background yellow
}
