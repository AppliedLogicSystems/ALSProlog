# getFiles.tcl --
#
#	Derived from the standard tkFDialog/tkfbox
#	Probably can be stripped down further.
#
# ORIGINAL:
# --------
# SCCS: @(#) tkfbox.tcl 1.13 97/10/01 14:51:01
#
# Copyright (c) 1994-1996 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

#----------------------------------------------------------------------
#
#		      M U L T I F I L E   D I A L O G
#
#----------------------------------------------------------------------

proc getFiles {args} {
	eval tkMFDialog $args
}

# tkMFDialog --
#
#	Implements the TK file selection dialog. This dialog is used when
#	the tk_strictMotif flag is set to false. This procedure shouldn't
#	be called directly. Call tk_getOpenFile or tk_getSaveFile instead.
#
proc tkMFDialog {args} {
    global tkPriv
    set w __tk_multifiledialog
    upvar #0 $w data

	set type open

    tkMFDialog_Config $w $type $args

    if {![string compare $data(-parent) .]} {
        set w .$w
    } else {
        set w $data(-parent).$w
    }

    # (re)create the dialog box if necessary
    #
    if {![winfo exists $w]} {
	tkMFDialog_Create $w
    } elseif {[string compare [winfo class $w] TkMFDialog]} {
	destroy $w
	tkMFDialog_Create $w
    } else {
	set data(dirMenuBtn) $w.f1.menu
	set data(dirMenu) $w.f1.menu.menu
	set data(upBtn) $w.f1.up
	set data(icons) $w.icons
	set data(ent) $w.f2.ent
	set data(typeMenuLab) $w.f3.lab
	set data(typeMenuBtn) $w.f3.menu
	set data(typeMenu) $data(typeMenuBtn).m
	set data(okBtn) $w.f2.ok
	set data(okallBtn) $w.f3.okall
	set data(addList) $w.f4.addlist
	set data(removeBtn) $w.f4.remove
	set data(removeallBtn) $w.f4.removeall
	set data(doneBtn) $w.f4.done
	set data(cancelBtn) $w.f4.cancel
    }
    wm transient $w $data(-parent)

    # 5. Initialize the file types menu
    #
    if {$data(-filetypes) != {}} {
	$data(typeMenu) delete 0 end
	foreach type $data(-filetypes) {
	    set title  [lindex $type 0]
	    set filter [lindex $type 1]
	    $data(typeMenu) add command -label $title \
		-command [list tkMFDialog_SetFilter $w $type]
	}
	tkMFDialog_SetFilter $w [lindex $data(-filetypes) 0]
	$data(typeMenuBtn) config -state normal
	$data(typeMenuLab) config -state normal
    } else {
	set data(filter) "*"
	$data(typeMenuBtn) config -state disabled -takefocus 0
	$data(typeMenuLab) config -state disabled
    }
    
    $data(addList) delete 0 end

    tkMFDialog_UpdateWhenIdle $w

    # 6. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set x [expr {[winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]}]
    set y [expr {[winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]}]
    wm geom $w [winfo reqwidth $w]x[winfo reqheight $w]+$x+$y
    wm deiconify $w
    wm title $w $data(-title)

    # 7. Set a grab and claim the focus too.

    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab != ""} {
	set grabStatus [grab status $oldGrab]
    }
    grab $w
    focus $data(ent)
    $data(ent) delete 0 end
    $data(ent) insert 0 $data(selectFile)
    $data(ent) select from 0
    $data(ent) select to   end
    $data(ent) icursor end

    # 8. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    tkwait variable tkPriv(selectFilePathList)
    catch {focus $oldFocus}
    grab release $w
    wm withdraw $w
    if {$oldGrab != ""} {
	if {$grabStatus == "global"} {
	    grab -global $oldGrab
	} else {
	    grab $oldGrab
	}
    }
    return $tkPriv(selectFilePathList)
}

# tkFDialog_Config --
#
#	Configures the TK filedialog according to the argument list
#
proc tkMFDialog_Config {w type argList} {
    upvar #0 $w data

    set data(type) $type

    # 1: the configuration specs
    #
    set specs {
	{-defaultextension "" "" ""}
	{-filetypes "" "" ""}
	{-initialdir "" "" ""}
	{-initialfile "" "" ""}
	{-parent "" "" "."}
	{-title "" "" ""}
    }

    # 2: default values depending on the type of the dialog
    #
    if {![info exists data(selectPath)]} {
	# first time the dialog has been popped up
	set data(selectPath) [pwd]
	set data(selectFile) ""
    }

    # 3: parse the arguments
    #
    tclParseConfigSpec $w $specs "" $argList

    if {![string compare $data(-title) ""]} {
	if {![string compare $type "open"]} {
	    set data(-title) "Add"
	} else {
	    set data(-title) "Save As"
	}
    }

    # 4: set the default directory and selection according to the -initial
    #    settings
    #
    if {[string compare $data(-initialdir) ""]} {
	
	if {[file isdirectory $data(-initialdir)]} {
	    set data(selectPath) [glob $data(-initialdir)]
	} else {
	    set data(selectPath) [pwd]
	}

	# Convert the initialdir to an absolute path name.

	set old [pwd]
	cd $data(selectPath)
	set data(selectPath) [pwd]
	cd $old
    }
    set data(selectFile) $data(-initialfile)

    # 5. Parse the -filetypes option
    #
    set data(-filetypes) [tkFDGetFileTypes $data(-filetypes)]

    if {![winfo exists $data(-parent)]} {
	error "bad window path name \"$data(-parent)\""
    }
}

proc tkMFDialog_Create {w} {
    set dataName [lindex [split $w .] end]
    upvar #0 $dataName data
    global tk_library

    toplevel $w -class TkMFDialog

    # f1: the frame with the directory option menu
    #
    set f1 [frame $w.f1]
    label $f1.lab -text "Directory:" -under 0
    set data(dirMenuBtn) $f1.menu
    set data(dirMenu) [tk_optionMenu $f1.menu [format %s(selectPath) $dataName] ""]
    set data(upBtn) [button $f1.up]
    if {![info exists tkPriv(updirImage)]} {
	set tkPriv(updirImage) [image create bitmap -data {
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};}]
    }
    $data(upBtn) config -image $tkPriv(updirImage)

    $f1.menu config -takefocus 1 -highlightthickness 2
 
    pack $data(upBtn) -side right -padx 4 -fill both
    pack $f1.lab -side left -padx 4 -fill both
    pack $f1.menu -expand yes -fill both -padx 4

    # data(icons): the IconList that list the files and directories.
    #
    set data(icons) [tkIconList $w.icons \
	-browsecmd "tkMFDialog_ListBrowse $w" \
	-command   "tkMFDialog_OkCmd $w"]

    # f2: the frame with the OK button and the "file name" field
    #
    set f2 [frame $w.f2 -bd 0]
    label $f2.lab -text "File name:" -anchor e -width 14 -under 5 -pady 0
    set data(ent) [entry $f2.ent]

    # The font to use for the icons. The default Canvas font on Unix
    # is just deviant.
    global $w.icons
    set $w.icons(font) [$data(ent) cget -font]

    # f3: the frame with the cancel button and the file types field
    #
    set f3 [frame $w.f3 -bd 0]

    # The "File of types:" label needs to be grayed-out when
    # -filetypes are not specified. The label widget does not support
    # grayed-out text on monochrome displays. Therefore, we have to
    # use a button widget to emulate a label widget (by setting its
    # bindtags)

    set data(typeMenuLab) [button $f3.lab -text "Files of type:" \
	-anchor e -width 14 -under 9 \
	-bd [$f2.lab cget -bd] \
	-highlightthickness [$f2.lab cget -highlightthickness] \
	-relief [$f2.lab cget -relief] \
	-padx [$f2.lab cget -padx] \
	-pady [$f2.lab cget -pady]]
    bindtags $data(typeMenuLab) [list $data(typeMenuLab) Label \
	    [winfo toplevel $data(typeMenuLab)] all]

    set data(typeMenuBtn) [menubutton $f3.menu -indicatoron 1 -menu $f3.menu.m]
    set data(typeMenu) [menu $data(typeMenuBtn).m -tearoff 0]
    $data(typeMenuBtn) config -takefocus 1 -highlightthickness 2 \
	-relief raised -bd 2 -anchor w

    # the okBtn is created after the typeMenu so that the keyboard traversal
    # is in the right order
    set data(okBtn)     [button $f2.ok     -text Add     -under 0 -width 10 \
	-default active -pady 3]
    set data(okallBtn) [button $f3.okall -text {Add All} -under 0 -width 10 \
	-default normal -pady 3]

    # f4: the frame with the added list, remove and done buttons
    #
    set f4 [frame $w.f4 -bd 0]

    set data(addList)	[listbox $f4.addlist -selectmode extended]
    $data(addList) config -xscrollcommand "$f4.xscroll set"
    $data(addList) config -yscrollcommand "$f4.yscroll set"
    scrollbar $f4.yscroll -command "$f4.addlist yview"
    scrollbar $f4.xscroll -orient horizontal -command "$f4.addlist xview"
    
    set f5 [frame $w.f5 -bd 0]
    set data(removeBtn)	[button $f5.remove -text Remove -under 0 -width 10 \
    	-default normal -pady 3]
    set data(removeallBtn)	[button $f5.removeall -text {Remove All} -under 0 -width 10 \
    	-default normal -pady 3]
    set data(doneBtn)	[button $f5.done -text Done -under 0 -width 10 \
    	-default normal -pady 3]
    set data(cancelBtn)	[button $f5.cancel -text Cancel -under 0 -width 10 \
    	-default normal -pady 3]
    
    # pack the widgets in f2, f3, f4 and f5
    #
    pack $data(okBtn) -side right -padx 4 -anchor e
    pack $f2.lab -side left -padx 4
    pack $f2.ent -expand yes -fill x -padx 2 -pady 0
    
    pack $data(okallBtn) -side right -padx 4 -anchor w
    pack $data(typeMenuLab) -side left -padx 4
    pack $data(typeMenuBtn) -expand yes -fill x -side right

    grid rowconfig    $f4 0 -weight 1 -minsize 0 -pad 0
    grid columnconfig $f4 0 -weight 1 -minsize 0

    grid $data(addList) -in $f4 -row 0 -column 0 -sticky news
    grid $f4.yscroll -in $f4 -row 0 -column 1 -sticky news
    grid $f4.xscroll -in $f4 -row 1 -column 0 -sticky news
   
    pack $data(removeBtn) -padx 4 -anchor w
    pack $data(removeallBtn) -padx 4 -anchor w
    pack $data(doneBtn) -padx 4 -anchor w
    pack $data(cancelBtn) -padx 4 -anchor w
    
    # Pack all the frames together. We are done with widget construction.
    #
    pack $f1 -fill x -pady 4
    pack $data(icons) -expand yes -fill both -padx 4 -pady 1
    pack $f2 -fill x
    pack $f3 -fill x
    pack $f4 -side left -expand yes -fill both -pady 4 -padx 4
    pack $f5 -fill x

    # Set up the event handlers
    #
    bind $data(ent) <Return>  "tkMFDialog_ActivateEnt $w"
    
    $data(upBtn)     config -command "tkFDialog_UpDirCmd $w"
    $data(okBtn)     config -command "tkMFDialog_OkCmd $w"
    $data(okallBtn) config -command "tkMFDialog_OkAllCmd $w"
    $data(removeBtn) config -command "tkMFDialog_RemoveCmd $w"
    $data(removeallBtn) config -command "tkMFDialog_RemoveAllCmd $w"
    $data(doneBtn)   config -command "tkMFDialog_DoneCmd $w"
    $data(cancelBtn)   config -command "tkMFDialog_CancelCmd $w"
    
    trace variable data(selectPath) w "tkMFDialog_SetPath $w"

    bind $w <Alt-d> "focus $data(dirMenuBtn)"
    bind $w <Alt-t> [format {
	if {"[%s cget -state]" == "normal"} {
	    focus %s
	}
    } $data(typeMenuBtn) $data(typeMenuBtn)]
    bind $w <Alt-n> "focus $data(ent)"
    bind $w <KeyPress-Escape> "tkButtonInvoke $data(cancelBtn)"
    bind $w <Alt-c> "tkButtonInvoke $data(cancelBtn)"
    bind $w <Alt-o> "tkFDialog_InvokeBtn $w Open"
    bind $w <Alt-s> "tkFDialog_InvokeBtn $w Save"

    wm protocol $w WM_DELETE_WINDOW "tkMFDialog_CancelCmd $w"

    # Build the focus group for all the entries
    #
    tkFocusGroup_Create $w
    tkFocusGroup_BindIn $w  $data(ent) "tkMFDialog_EntFocusIn $w"
    tkFocusGroup_BindOut $w $data(ent) "tkMFDialog_EntFocusOut $w"
}
# tkMFDialog_UpdateWhenIdle --
#
#	Creates an idle event handler which updates the dialog in idle
#	time. This is important because loading the directory may take a long
#	time and we don't want to load the same directory for multiple times
#	due to multiple concurrent events.
#
proc tkMFDialog_UpdateWhenIdle {w} {
    upvar #0 [winfo name $w] data

    if {[info exists data(updateId)]} {
	return
    } else {
	set data(updateId) [after idle tkMFDialog_Update $w]
    }
}

# tkMFDialog_Update --
#
#	Loads the files and directories into the IconList widget. Also
#	sets up the directory option menu for quick access to parent
#	directories.
#
proc tkMFDialog_Update {w} {

    # This proc may be called within an idle handler. Make sure that the
    # window has not been destroyed before this proc is called
    if {![winfo exists $w] || [string compare [winfo class $w] TkMFDialog]} {
	return
    }

    set dataName [winfo name $w]
    upvar #0 $dataName data
    global tk_library tkPriv
    catch {unset data(updateId)}

    if {![info exists tkPriv(folderImage)]} {
	set tkPriv(folderImage) [image create photo -data {
R0lGODlhEAAMAKEAAAD//wAAAPD/gAAAACH5BAEAAAAALAAAAAAQAAwAAAIghINhyycvVFsB
QtmS3rjaH1Hg141WaT5ouprt2HHcUgAAOw==}]
	set tkPriv(fileImage)   [image create photo -data {
R0lGODlhDAAMAKEAALLA3AAAAP//8wAAACH5BAEAAAAALAAAAAAMAAwAAAIgRI4Ha+IfWHsO
rSASvJTGhnhcV3EJlo3kh53ltF5nAhQAOw==}]
    }
    set folder $tkPriv(folderImage)
    set file   $tkPriv(fileImage)

    set appPWD [pwd]
    if {[catch {
	cd $data(selectPath)
    }]} {
	# We cannot change directory to $data(selectPath). $data(selectPath)
	# should have been checked before tkMFDialog_Update is called, so
	# we normally won't come to here. Anyways, give an error and abort
	# action.
	tk_messageBox -type ok -parent $data(-parent) -message \
	    "Cannot change to the directory \"$data(selectPath)\".\nPermission denied."\
	    -icon warning
	cd $appPWD
	return
    }

    # Turn on the busy cursor. BUG?? We haven't disabled X events, though,
    # so the user may still click and cause havoc ...
    #
    set entCursor [$data(ent) cget -cursor]
    set dlgCursor [$w         cget -cursor]
    $data(ent) config -cursor watch
    $w         config -cursor watch
    update idletasks
    
    tkIconList_DeleteAll $data(icons)

    # Make the dir list
    #
    foreach f [lsort -dictionary [glob -nocomplain .* *]] {
	if {![string compare $f .]} {
	    continue
	}
	if {![string compare $f ..]} {
	    continue
	}
	if {[file isdir ./$f]} {
	    if {![info exists hasDoneDir($f)]} {
		tkIconList_Add $data(icons) $folder $f
		set hasDoneDir($f) 1
	    }
	}
    }
    # Make the file list
    #
    if {![string compare $data(filter) *]} {
	set files [lsort -dictionary \
	    [glob -nocomplain .* *]]
    } else {
	set files [lsort -dictionary \
	    [eval glob -nocomplain $data(filter)]]
    }

    set top 0
    foreach f $files {
	if {![file isdir ./$f]} {
	    if {![info exists hasDoneFile($f)]} {
		tkIconList_Add $data(icons) $file $f
		set hasDoneFile($f) 1
	    }
	}
    }

    tkIconList_Arrange $data(icons)

    # Update the Directory: option menu
    #
    set list ""
    set dir ""
    foreach subdir [file split $data(selectPath)] {
	set dir [file join $dir $subdir]
	lappend list $dir
    }

    $data(dirMenu) delete 0 end
    set var [format %s(selectPath) $dataName]
    foreach path $list {
	$data(dirMenu) add command -label $path -command [list set $var $path]
    }

    # Restore the PWD to the application's PWD
    #
    cd $appPWD

    # turn off the busy cursor.
    #
    $data(ent) config -cursor $entCursor
    $w         config -cursor $dlgCursor
}

# tkMFDialog_SetPathSilently --
#
# 	Sets data(selectPath) without invoking the trace procedure
#
proc tkMFDialog_SetPathSilently {w path} {
    upvar #0 [winfo name $w] data
    
    trace vdelete  data(selectPath) w "tkMFDialog_SetPath $w"
    set data(selectPath) $path
    trace variable data(selectPath) w "tkMFDialog_SetPath $w"
}


# This proc gets called whenever data(selectPath) is set
#
proc tkMFDialog_SetPath {w name1 name2 op} {
    if {[winfo exists $w]} {
	upvar #0 [winfo name $w] data
	tkMFDialog_UpdateWhenIdle $w
    }
}

# This proc gets called whenever data(filter) is set
#
proc tkMFDialog_SetFilter {w type} {
    upvar #0 [winfo name $w] data
    upvar \#0 $data(icons) icons

    set data(filter) [lindex $type 1]
    $data(typeMenuBtn) config -text [lindex $type 0] -indicatoron 1

    $icons(sbar) set 0.0 0.0
    
    tkMFDialog_UpdateWhenIdle $w
}


# Gets called when the entry box gets keyboard focus. We clear the selection
# from the icon list . This way the user can be certain that the input in the 
# entry box is the selection.
#
proc tkMFDialog_EntFocusIn {w} {
    upvar #0 [winfo name $w] data

    if {[string compare [$data(ent) get] ""]} {
	$data(ent) selection from 0
	$data(ent) selection to   end
	$data(ent) icursor end
    } else {
	$data(ent) selection clear
    }

    tkIconList_Unselect $data(icons)

    if {![string compare $data(type) open]} {
	$data(okBtn) config -text "Add"
    } else {
	$data(okBtn) config -text "Save"
    }
}

proc tkMFDialog_EntFocusOut {w} {
    upvar #0 [winfo name $w] data

    $data(ent) selection clear
}


# Gets called when user presses Return in the "File name" entry.
#
proc tkMFDialog_ActivateEnt {w} {
    upvar #0 [winfo name $w] data

    set text [string trim [$data(ent) get]]
    set list [tkFDialogResolveFile $data(selectPath) $text \
		  $data(-defaultextension)]
    set flag [lindex $list 0]
    set path [lindex $list 1]
    set file [lindex $list 2]

    case $flag {
	OK {
	    if {![string compare $file ""]} {
		# user has entered an existing (sub)directory
		set data(selectPath) $path
		$data(ent) delete 0 end
	    } else {
		tkMFDialog_SetPathSilently $w $path
		set data(selectFile) $file
		tkMFDialog_Add $w
	    }
	}
	PATTERN {
	    set data(selectPath) $path
	    set data(filter) $file
	}
	FILE {
	    if {![string compare $data(type) open]} {
		tk_messageBox -icon warning -type ok -parent $data(-parent) \
		    -message "File \"[file join $path $file]\" does not exist."
		$data(ent) select from 0
		$data(ent) select to   end
		$data(ent) icursor end
	    } else {
		tkMFDialog_SetPathSilently $w $path
		set data(selectFile) $file
		tkMFDialog_Add $w
	    }
	}
	PATH {
	    tk_messageBox -icon warning -type ok -parent $data(-parent) \
		-message "Directory \"$path\" does not exist."
	    $data(ent) select from 0
	    $data(ent) select to   end
	    $data(ent) icursor end
	}
	CHDIR {
	    tk_messageBox -type ok -parent $data(-parent) -message \
	       "Cannot change to the directory \"$path\".\nPermission denied."\
		-icon warning
	    $data(ent) select from 0
	    $data(ent) select to   end
	    $data(ent) icursor end
	}
	ERROR {
	    tk_messageBox -type ok -parent $data(-parent) -message \
	       "Invalid file name \"$path\"."\
		-icon warning
	    $data(ent) select from 0
	    $data(ent) select to   end
	    $data(ent) icursor end
	}
    }
}


# Gets called when user presses the "OK" button
#
proc tkMFDialog_OkCmd {w} {
    upvar #0 [winfo name $w] data

    set text [tkIconList_Get $data(icons)]
    if {[string compare $text ""]} {
	set file [tkFDialog_JoinFile $data(selectPath) $text]
	if {[file isdirectory $file]} {
	    tkMFDialog_ListInvoke $w $text
	    return
	}
    }

    tkMFDialog_ActivateEnt $w
}

proc lbmember {listbox element} {
    set size [$listbox size] 
    for {set i 0} {$i<$size} {incr i} {
	if {![string compare $element [$listbox get $i]]} {return 1}
    }
    return 0
}

proc tkMFDialog_OkAllCmd {w} {
    upvar #0 [winfo name $w] data

    set appPWD [pwd]
    if {[catch {cd $data(selectPath)}]} {
	# We cannot change directory to $data(selectPath). $data(selectPath)
	# should have been checked before tkMFDialog_Update is called, so
	# we normally won't come to here. Anyways, give an error and abort
	# action.
	tk_messageBox -type ok -parent $data(-parent) -message \
	    "Cannot change to the directory \"$data(selectPath)\".\nPermission denied."\
	    -icon warning
	cd $appPWD
	return
    }

    if {![string compare $data(filter) *]} {
	set files [lsort -dictionary \
	    [glob -nocomplain .* *]]
    } else {
	set files [lsort -dictionary \
	    [eval glob -nocomplain $data(filter)]]
    }

    cd $appPWD

    foreach file $files {
    	set path [tkFDialog_JoinFile $data(selectPath) $file]
    	if {![file isdirectory $path]} {
    	    if {![lbmember $data(addList) $path]} then {$data(addList) insert end $path}
    	}
    }
    
    $data(addList) see end
    $data(addList) xview 1000
}

# Gets called when user presses the "Cancel" button
#
proc tkMFDialog_CancelCmd {w} {
    upvar #0 [winfo name $w] data
    global tkPriv

    set tkPriv(selectFilePathList) ""
}

proc tkMFDialog_RemoveCmd {w} {
    upvar #0 [winfo name $w] data
    
    for {set i [$data(addList) size]} {$i >= 0} {incr i -1} {
    	if {[$data(addList) selection includes $i]} {$data(addList) delete $i}
    }
}

proc tkMFDialog_RemoveAllCmd {w} {
    upvar #0 [winfo name $w] data
    
    $data(addList) delete 0 end
}

proc tkMFDialog_DoneCmd {w} {
    tkMFDialog_Done $w
}

# Gets called when user browses the IconList widget (dragging mouse, arrow
# keys, etc)
#
proc tkMFDialog_ListBrowse {w text} {
    upvar #0 [winfo name $w] data

    if {$text == ""} {
	return
    }

    set file [tkFDialog_JoinFile $data(selectPath) $text]
    if {[file isdirectory $file]} {
	$data(okBtn) config -text "Open"
    } else {
	$data(ent) delete 0 end
	$data(ent) insert 0 $text

	if {![string compare $data(type) open]} {
	    $data(okBtn) config -text "Add"
	} else {
	    $data(okBtn) config -text "Save"
	}
    }
}

# Gets called when user invokes the IconList widget (double-click, 
# Return key, etc)
#
proc tkMFDialog_ListInvoke {w text} {
    upvar #0 [winfo name $w] data

    if {$text == ""} {
	return
    }

    set file [tkFDialog_JoinFile $data(selectPath) $text]

    if {[file isdirectory $file]} {
	set appPWD [pwd]
	if {[catch {cd $file}]} {
	    tk_messageBox -type ok -parent $data(-parent) -message \
	       "Cannot change to the directory \"$file\".\nPermission denied."\
		-icon warning
	} else {
	    cd $appPWD
	    set data(selectPath) $file
	}
    } else {
	set data(selectFile) $file
	tkMFDialog_Add $w
    }
}

# tkMFDialog_Done --
#
#	Gets called when user has input a valid filename.  Pops up a
#	dialog box to confirm selection when necessary. Sets the
#	tkPriv(selectFilePath) variable, which will break the "tkwait"
#	loop in tkFDialog and return the selected filename to the
#	script that calls tk_getOpenFile or tk_getSaveFile
#
proc tkMFDialog_Add {w} {
    upvar #0 [winfo name $w] data
    global tkPriv

    set selectFilePath [tkFDialog_JoinFile $data(selectPath) $data(selectFile)]
  
    if {![lbmember $data(addList) $selectFilePath]} {
	    $data(addList) insert end $selectFilePath
	    $data(addList) see end
	    $data(addList) xview 1000
    }
}

proc tkMFDialog_Done {w} {
    upvar #0 [winfo name $w] data
    global tkPriv
    
    set selectFilePathList {}
    for {set i 0} {$i<[$data(addList) size]} {incr i} {
    	lappend selectFilePathList [$data(addList) get $i]
    }
    set tkPriv(selectFilePathList) $selectFilePathList
}

package provide getFiles 1.0