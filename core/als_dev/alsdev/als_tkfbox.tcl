#----------------------------------------------------------------------
#
#		      F I L E   D I A L O G
#		Canabilizing of original tkFDialog for setting directory...
#----------------------------------------------------------------------

# alstkFDialog --
#
#	Implements the TK file selection dialog. This dialog is used when
#	the tk_strictMotif flag is set to false. This procedure shouldn't
#	be called directly. Call tk_getOpenFile or tk_getSaveFile instead.
#
proc alstkFDialog {args} {
    global tkPriv
    set w __tk_filedialog
    upvar #0 $w data

	set type browse

    alstkFDialog_Config $w $type $args

    if {![string compare $data(-parent) .]} {
        set w .$w
    } else {
        set w $data(-parent).$w
    }

    # (re)create the dialog box if necessary
    #
    if {![winfo exists $w]} {
	tkFDialog_Create $w
    } elseif {[string compare [winfo class $w] TkFDialog]} {
	destroy $w
	tkFDialog_Create $w
    }
    wm transient $w $data(-parent)

    # 5. Initialize the file types menu
    #
	set data(filter) "*"
	$data(typeMenuBtn) config -state disabled -takefocus 0
	$data(typeMenuLab) config -state disabled


    tkFDialog_UpdateWhenIdle $w

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
    $data(ent) insert 0 $data(selectFile)
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

# alstkFDialog_Config --
#
#	Configures the TK filedialog according to the argument list
#
proc alstkFDialog_Config {w type argList} {
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
    if ![info exists data(selectPath)] {
	# first time the dialog has been popped up
	set data(selectPath) [pwd]
	set data(selectFile) ""
    }

    # 3: parse the arguments
    #
    tclParseConfigSpec $w $specs "" $argList
    set data(-title) "Choose Directory"

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
    set data(selectFile) $data(-initialfile)

    # 5. Parse the -filetypes option
    #
    set data(-filetypes) [tkFDGetFileTypes $data(-filetypes)]

    if ![winfo exists $data(-parent)] {
	error "bad window path name \"$data(-parent)\""
    }
}

