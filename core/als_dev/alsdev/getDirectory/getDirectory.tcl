# getDirectory.tcl --
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
#		      D I R E C T O R Y   D I A L O G
#
#----------------------------------------------------------------------

proc getDirectory {args} {
	eval tkDDialog $args
}

# tkDDialog --
#
#	Implements the TK file selection dialog. This dialog is used when
#	the tk_strictMotif flag is set to false. This procedure shouldn't
#	be called directly. Call tk_getOpenFile or tk_getSaveFile instead.
#
proc tkDDialog {args} {
    global tkPriv
    set w __tk_directorydialog
    upvar #0 $w data

	set type browse

    tkDDialog_Config $w $type $args

    if {![string compare $data(-parent) .]} {
        set w .$w
    } else {
        set w $data(-parent).$w
    }

    # (re)create the dialog box if necessary
    #
    if {![winfo exists $w]} {
		tkDDialog_Create $w
    } elseif {[string compare [winfo class $w] TkFDialog]} {
		destroy $w
		tkDDialog_Create $w
    }
    wm transient $w $data(-parent)

    # 5. Initialize the file types menu
    #

	set data(filter) "*"

	$data(typeMenuBtn) config -state normal
	$data(typeMenuLab) config -state normal

	tkDDialog_UpdateWhenIdle $w

    # 6. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]]
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
    $data(ent) select from 0
    $data(ent) select to   end
    $data(ent) icursor end

    # 8. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    tkwait variable tkPriv(selectFilePath)
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
    return $tkPriv(selectFilePath)
}

# tkDDialog_Config --
#
#	Configures the TK filedialog according to the argument list
#
proc tkDDialog_Config {w type argList} {
    upvar #0 $w data

    set data(type) $type

    # 1: the configuration specs
    #
    set specs {
	{-initialdir "" "" ""}
	{-parent "" "" "."}
	{-title "" "" ""}
    }

    # 2: default values depending on the type of the dialog
    #
    if ![info exists data(selectPath)] {
	# first time the dialog has been popped up
	set data(selectPath) [pwd]
    }

    # 3: parse the arguments
    #
    tclParseConfigSpec $w $specs "" $argList

    if ![string compare $data(-title) ""] {
	    set data(-title) "Choose Directory"
    } 

    # 4: set the default directory and selection according to the -initial
    #    settings
    #
    if [string compare $data(-initialdir) ""] {
	if [file isdirectory $data(-initialdir)] {
	    set data(selectPath) [glob $data(-initialdir)]
	} else {
	    error "\"$data(-initialdir)\" is not a valid directory"
	}
    }
    # 5. Parse the -filetypes option
    #
    if ![winfo exists $data(-parent)] {
	error "bad window path name \"$data(-parent)\""
    }
}

proc tkDDialog_Create {w} {
    set dataName [lindex [split $w .] end]
    upvar #0 $dataName data
    global tk_library

    toplevel $w -class TkFDialog

    # f1: the frame with the directory option menu
    #
    set f1 [frame $w.f1]
    label $f1.lab -text "Directory:" -under 0
    set data(dirMenuBtn) $f1.menu
    set data(dirMenu) [tk_optionMenu $f1.menu [format %s(selectPath) $dataName] ""]
    set data(upBtn) [button $f1.up]
    if ![info exists tkPriv(updirImage)] {
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
	-browsecmd "tkDDialog_ListBrowse $w" \
	-command   "tkDDialog_OkCmd $w"]

#	-command   "tkDDialog_ListInvoke $w"]

    # f2: the frame with the OK button and the "file name" field
    #
    set f2 [frame $w.f2 -bd 0]
	label $f2.lab -text "Directory name:" -anchor e -width 16 -under 5 -pady 0
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

    set data(typeMenuLab) [button $f3.lab -text "Select Directory:" \
	-anchor e -width 14 -under 9 \
	-bd [$f2.lab cget -bd] \
	-highlightthickness [$f2.lab cget -highlightthickness] \
	-relief [$f2.lab cget -relief] \
	-padx [$f2.lab cget -padx] \
	-pady [$f2.lab cget -pady]]
    bindtags $data(typeMenuLab) [list $data(typeMenuLab) Label \
	    [winfo toplevel $data(typeMenuLab)] all]

    set data(typeMenuBtn) [button $f3.menu -text "$data(selectPath)" -under 0 -width 25\
	-default normal -pady 3 -anchor e]

    # the okBtn is created after the typeMenu so that the keyboard traversal
    # is in the right order
    set data(okBtn)     [button $f2.ok     -text OK     -under 0 -width 6 \
	-default active -pady 3]
    set data(cancelBtn) [button $f3.cancel -text Cancel -under 0 -width 6\
	-default normal -pady 3]

    # pack the widgets in f2 and f3
    #
    pack $data(okBtn) -side right -padx 4 -anchor e
    pack $f2.lab -side left -padx 4
    pack $f2.ent -expand yes -fill x -padx 2 -pady 0
    
    pack $data(cancelBtn) -side right -padx 4 -anchor w
    pack $data(typeMenuLab) -side left -padx 4
    pack $data(typeMenuBtn) -expand yes -fill x -side right

    # Pack all the frames together. We are done with widget construction.
    #
    pack $f1 -side top -fill x -pady 4
    pack $f3 -side bottom -fill x
    pack $f2 -side bottom -fill x
    pack $data(icons) -expand yes -fill both -padx 4 -pady 1

    # Set up the event handlers
    #
    bind $data(ent) <Return>  "tkDDialog_ActivateEnt $w"
    
    $data(upBtn)     config -command "tkDDialog_UpDirCmd $w"
    $data(okBtn)     config -command "tkDDialog_OkCmd $w"
    $data(cancelBtn) config -command "tkFDialog_CancelCmd $w"
    $data(typeMenuBtn)   config -command "tkDDialog_SelectCmd $w"

    trace variable data(selectPath) w "tkDDialog_SetPath $w"

    bind $w <Alt-d> "focus $data(dirMenuBtn)"
    bind $w <Alt-t> [format {
	if {"[%s cget -state]" == "normal"} {
	    focus %s
	}
    } $data(typeMenuBtn) $data(typeMenuBtn)]
    bind $w <Alt-n> "focus $data(ent)"
    bind $w <KeyPress-Escape> "tkButtonInvoke $data(cancelBtn)"
    bind $w <Alt-c> "tkButtonInvoke $data(cancelBtn)"

    bind $w <Alt-o> "tkDDialog_InvokeBtn $w Open"
    bind $w <Alt-s> "tkDDialog_InvokeBtn $w Save"

    wm protocol $w WM_DELETE_WINDOW "tkFDialog_CancelCmd $w"

    # Build the focus group for all the entries
    #
    tkFocusGroup_Create $w
    tkFocusGroup_BindIn $w  $data(ent) "tkDDialog_EntFocusIn $w"
    tkFocusGroup_BindOut $w $data(ent) "tkFDialog_EntFocusOut $w"
}

# tkDDialog_UpdateWhenIdle --
#
#	Creates an idle event handler which updates the dialog in idle
#	time. This is important because loading the directory may take a long
#	time and we don't want to load the same directory for multiple times
#	due to multiple concurrent events.
#
proc tkDDialog_UpdateWhenIdle {w} {
    upvar #0 [winfo name $w] data

    if [info exists data(updateId)] {
	return
    } else {
	set data(updateId) [after idle tkDDialog_Update $w]
    }
}

# tkDDialog_Update --
#
#	Loads the files and directories into the IconList widget. Also
#	sets up the directory option menu for quick access to parent
#	directories.
#
proc tkDDialog_Update {w} {
    set dataName [winfo name $w]
    upvar #0 $dataName data
    global tk_library tkPriv

    # This proc may be called within an idle handler. Make sure that the
    # window has not been destroyed before this proc is called
    if {![winfo exists $w] || [string compare [winfo class $w] TkFDialog]} {
	return
    } else {
	catch {unset data(updateId)}
    }

    set TRANSPARENT_GIF_COLOR [$w cget -bg]
    if ![info exists tkPriv(folderImage)] {
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
    if [catch {
	cd $data(selectPath)
    }] {
	# We cannot change directory to $data(selectPath). $data(selectPath)
	# should have been checked before tkDDialog_Update is called, so
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
	if ![string compare $f .] {
	    continue
	}
	if ![string compare $f ..] {
	    continue
	}
	if [file isdir ./$f] {
	    if ![info exists hasDoneDir($f)] {
		tkIconList_Add $data(icons) $folder $f
		set hasDoneDir($f) 1
	    }
	}
    }
    # Make the file list
    #
if [string compare $data(type) browse] {
    if ![string compare $data(filter) *] {
	set files [lsort -dictionary \
	    [glob -nocomplain .* *]]
    } else {
	set files [lsort -dictionary \
	    [eval glob -nocomplain $data(filter)]]
    }

    set top 0
    foreach f $files {
	if ![file isdir ./$f] {
	    if ![info exists hasDoneFile($f)] {
		tkIconList_Add $data(icons) $file $f
		set hasDoneFile($f) 1
	    }
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

# This proc gets called whenever data(selectPath) is set
#
proc tkDDialog_SetPath {w name1 name2 op} {
    upvar #0 [winfo name $w] data
	$data(typeMenuBtn) config -text $data(selectPath) -anchor e
    tkDDialog_UpdateWhenIdle $w
}

# Gets called when the entry box gets keyboard focus. We clear the selection
# from the icon list . This way the user can be certain that the input in the 
# entry box is the selection.
#
proc tkDDialog_EntFocusIn {w} {
    upvar #0 [winfo name $w] data

    if [string compare [$data(ent) get] ""] {
	$data(ent) selection from 0
	$data(ent) selection to   end
	$data(ent) icursor end
    } else {
	$data(ent) selection clear
    }

    tkIconList_Unselect $data(icons)

	$data(okBtn) config -text "Open"

}

# Gets called when user presses Return in the "Directory name" entry.
#
proc tkDDialog_ActivateEnt {w} {
    upvar #0 [winfo name $w] data
    set text [string trim [$data(ent) get]]
    set list [tkFDialogResolveFile $data(selectPath) $text "*"]
    set flag [lindex $list 0]
    set path [lindex $list 1]
    set file [lindex $list 2]
    case $flag {
	OK {
	    if ![string compare $file ""] {
		# user has entered an existing (sub)directory
		set data(selectPath) $path
		$data(ent) delete 0 end
		if ![string compare $data(type) browse] {
			bell
		}
		}


	}
	PATTERN {
	    set data(selectPath) $path
	    set data(filter) $file
	}
	FILE {
		tk_messageBox -icon warning -type ok -parent $data(-parent) \
		    -message "Directory \"[file join $path $file]\" does not exist."
		$data(ent) select from 0
		$data(ent) select to   end
		$data(ent) icursor end
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

# Gets called when user presses the Alt-s or Alt-o keys.
#
proc tkFDialog_InvokeBtn {w key} {
    upvar #0 [winfo name $w] data

    if ![string compare [$data(okBtn) cget -text] $key] {
	tkButtonInvoke $data(okBtn)
    }
}

# Gets called when user presses the "parent directory" button
#
proc tkDDialog_UpDirCmd {w} {
    upvar #0 [winfo name $w] data

    if [string compare $data(selectPath) "/"] {
	set data(selectPath) [file dirname $data(selectPath)]
    }
	$data(typeMenuBtn) config -text $data(selectPath) -anchor e
}

# Join a file name to a path name. The "file join" command will break
# if the filename begins with ~
#
proc tkFDialog_JoinFile {path file} {
    if {[string match {~*} $file] && [file exists $path/$file]} {
	return [file join $path ./$file]
    } else {
	return [file join $path $file]
    }
}

# Gets called when user presses the "OK" button
#
proc tkDDialog_OkCmd {w} {
    upvar #0 [winfo name $w] data
    set text [tkIconList_Get $data(icons)]
    if [string compare $text ""] {
		set file [tkFDialog_JoinFile $data(selectPath) $text]
		if [file isdirectory $file] {
	    	tkDDialog_ListInvoke $w $text
	    	return
		}
    }
    tkDDialog_ActivateEnt $w
}

# Gets called when user presses the "SelectDirectory" button
#
proc tkDDialog_SelectCmd {w} {
    upvar #0 [winfo name $w] data
    set text [tkIconList_Get $data(icons)]
    if [string compare $text ""] {
		set file [tkFDialog_JoinFile $data(selectPath) $text]
		if [file isdirectory $file] {
	    	tkDDialog_ListInvoke $w $text
	    	return
	}
    }
	tkDDialog_Done $w
}

# Gets called when user browses the IconList widget (dragging mouse, arrow
# keys, etc)
#
proc tkDDialog_ListBrowse {w text} {
    upvar #0 [winfo name $w] data

    if {$text == ""} {
	return
    }
    set file [tkFDialog_JoinFile $data(selectPath) $text]
}

# Gets called when user invokes the IconList widget (double-click, 
# Return key, etc)
#
proc tkDDialog_ListInvoke {w text} {
    upvar #0 [winfo name $w] data

    if {$text == ""} {
	return
    }
    set file [tkFDialog_JoinFile $data(selectPath) $text]

    if [file isdirectory $file] {
	set appPWD [pwd]
	if [catch {cd $file}] {
	    tk_messageBox -type ok -parent $data(-parent) -message \
	       "Cannot change to the directory \"$file\".\nPermission denied."\
		-icon warning
	} else {
	    cd $appPWD
	    set data(selectPath) $file
	}
	}
}

# tkDDialog_Done --
#
#	Gets called when user has input a valid filename.  Pops up a
#	dialog box to confirm selection when necessary. Sets the
#	tkPriv(selectFilePath) variable, which will break the "tkwait"
#	loop in tkFDialog and return the selected filename to the
#	script that calls tk_getOpenFile or tk_getSaveFile
#

proc tkDDialog_Done {w {selectFilePath ""}} {
    upvar #0 [winfo name $w] data
    global tkPriv

    if ![string compare $selectFilePath ""] {
	set tkPriv(selectPath)     $data(selectPath)

	if {[file exists $selectFilePath] && 
	    ![string compare $data(type) save]} {

	    set reply [tk_messageBox -icon warning -type yesno -parent $data(-parent) \
	        -message "File \"$selectFilePath\" already exists.\nDo you want to overwrite it?"]
	    if ![string compare $reply "no"] {
		return
	    }
	}
    }
	set tkPriv(selectFilePath) $data(selectPath)
}

package provide getDirectory 1.0
