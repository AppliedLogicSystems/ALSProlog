####-----------------------------------------------------------####
##			defstr.tcl
##
##		Widget specification and TclTk routines for
##		Prolog DefStruct creation
####-----------------------------------------------------------####

proc vTclWindow.defstr {base} {
    if {$base == ""} {
        set base .defstr
    }
    if {[winfo exists $base]} {
        wm deiconify $base; raise $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 499x365+375+178
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Define Struct"
    frame $base.grp1 \
        -borderwidth 1 -height 26 -relief sunken -width 30 
    frame $base.grp1.name \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.grp1.name.label \
        -padx 5 -text {Base Name:} -width 12 
    entry $base.grp1.name.entry \
        -borderwidth 1 -font {fixed 10 bold}
    frame $base.grp1.make \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.grp1.make.label \
        -padx 5 -text {Make Pred:} -width 12 
    entry $base.grp1.make.entry \
        -borderwidth 1  -font {fixed 10 bold}
    frame $base.grp2 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    frame $base.grp2.access \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.grp2.access.label \
        -padx 5 -text {Access Pred:} -width 12 
    entry $base.grp2.access.entry \
        -borderwidth 1  -font {fixed 10 bold}
    frame $base.grp2.set \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.grp2.set.label \
        -padx 5 -text {Set Pred:} -width 12 
    entry $base.grp2.set.entry \
        -borderwidth 1  -font {fixed 10 bold}
    label $base.elts_label \
        -padx 6 -text Elements: 
    frame $base.list \
        -borderwidth 1 -height 30 -relief raised -width 30 
    listbox $base.list.listbox \
		 -font {fixed 10 bold} \
        -xscrollcommand "$base.list.02 set" \
        -yscrollcommand "$base.list.03 set" 
    scrollbar $base.list.02 \
        -borderwidth 1 -command "$base.list.listbox xview" -orient horiz 
    scrollbar $base.list.03 \
        -borderwidth 1 -command "$base.list.listbox yview" -orient vert 
    frame $base.headings \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    label $base.headings.field \
        -relief groove -text {Field Name (Atom)} -width 25 
    label $base.headings.default \
        -relief groove -text {Default (Term)} -width 16 
    label $base.headings.desc \
        -relief groove -text {/* Description */}
    frame $base.newentry \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    entry $base.newentry.field \
        -width 25 -font {fixed 10 normal}
    entry $base.newentry.default \
        -width 16 -font {fixed 10 normal}
    entry $base.newentry.desc -font {fixed 10 normal}
	bind $base.newentry.desc <Return> \
		[list defstruct_add $base ]
    frame $base.buttons \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button $base.buttons.ok \
        -command "defstruct_ok $base" -padx 11 -pady 4 -text OK 
    button $base.buttons.cancel \
        -command "defstruct_cancel $base" -padx 11 -pady 4 -text Cancel 
    button $base.buttons.add \
        -command "defstruct_add $base" -padx 11 -pady 4 -text Add 
    button $base.buttons.delete \
        -command "defstruct_delete $base" -padx 11 -pady 4 -text Delete 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.grp1 \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.grp1.name \
        -anchor w -expand 0 -fill none -side left 
    pack $base.grp1.name.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.grp1.name.entry \
        -anchor center -expand 0 -fill x -side top 
    pack $base.grp1.make \
        -anchor center -expand 0 -fill none -side right 
    pack $base.grp1.make.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.grp1.make.entry \
        -anchor center -expand 0 -fill x -side top 
    pack $base.grp2 \
        -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $base.grp2.access \
        -anchor center -expand 0 -fill none -side left 
    pack $base.grp2.access.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.grp2.access.entry \
        -anchor center -expand 0 -fill x -side top 
    pack $base.grp2.set \
        -anchor center -expand 0 -fill none -side right 
    pack $base.grp2.set.label \
        -anchor center -expand 0 -fill none -side left 
    pack $base.grp2.set.entry \
        -anchor center -expand 0 -fill x -side top 
    pack $base.elts_label \
        -anchor center -expand 0 -fill none -side top 
    pack $base.list \
        -anchor center -expand 1 -fill both -side top 
    grid columnconf $base.list 0 -weight 1
    grid rowconf $base.list 0 -weight 1
    grid $base.list.listbox \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.list.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $base.list.03 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    pack $base.headings \
        -anchor center -expand 0 -fill x -side top 
    pack $base.headings.field \
        -anchor center -expand 0 -fill none -padx 2 -side left 
    pack $base.headings.default \
        -anchor center -expand 0 -fill none -side left 
    pack $base.headings.desc \
        -anchor center -expand 1 -fill x -side left 
    pack $base.newentry \
        -anchor center -expand 0 -fill x -side top 
    pack $base.newentry.field \
        -anchor center -expand 0 -fill none -side left 
    pack $base.newentry.default \
        -anchor center -expand 0 -fill none -side left 
    pack $base.newentry.desc \
        -anchor center -expand 0 -fill x -side top 
    pack $base.buttons \
        -anchor center -expand 0 -fill x -side top 
    pack $base.buttons.ok \
        -anchor center -expand 0 -fill none -padx 15 -side left 
    pack $base.buttons.cancel \
        -anchor center -expand 0 -fill none -padx 15 -side left 
    pack $base.buttons.add \
        -anchor center -expand 0 -fill none -padx 15 -side right 
    pack $base.buttons.delete \
        -anchor center -expand 0 -fill none -side right 
}


proc new_defstruct {} {
	global proenv
	
	set SName [do_popup_input "Enter the name of the defStruct:" "Input"]

	if {$SName != ""} then {
		display_defstr_win $SName
	} else { bell }
}

proc edit_defstruct {} {
	prolog call alsdev edit_defstruct 
}

proc display_defstr_win {StructName} {
	set ID [string tolower $StructName]

	vTclWindow.defstr .$ID
    .$ID.grp1.name.entry insert end $StructName 
    .$ID.grp1.make.entry insert end make_$StructName
    .$ID.grp2.access.entry insert end access_$StructName
    .$ID.grp2.set.entry insert end set_$StructName
}

proc defstruct_cancel {ID} {
	Window hide $ID
}

proc defstruct_ok {ID} {

	set Basic [list \
    	'[$ID.grp1.name.entry get]' \
    	[$ID.grp1.make.entry get] \
    	[$ID.grp2.access.entry get] \
    	[$ID.grp2.set.entry get] ]

	set Elts [$ID.list.listbox get 0 end] 
	prolog call alsdev install_defstruct -list $Basic -list $Elts
}

proc defstruct_add {ID} {

    set IName [$ID.newentry.field get]
	if {$IName == ""} then {
		bell
		return
	}
    set IDefault [$ID.newentry.default get]
    set Desc [$ID.newentry.desc get]

	regsub -all " " $IName "_" Name
	regsub -all " " $IDefault "_" Default

	set NamePadLen [expr 25 - [string length $Name]]
	if {$NamePadLen > 0} then {
		for {set i 0} {$i < $NamePadLen } {incr i} {
			append NamePad " "
		}
	}
	set DefaultPadLen [expr 16 - [string length $Default]]
	if {$DefaultPadLen > 0} then {
		for {set i 0} {$i < $DefaultPadLen } {incr i} {
			append DefaultPad " "
		}
	}
	append NewEntry $Name $NamePad " / " $Default $DefaultPad "% " $Desc
	$ID.list.listbox insert end $NewEntry
	
    $ID.newentry.field delete 0 end 
    $ID.newentry.default delete 0 end 
    $ID.newentry.desc delete 0 end 

    focus $ID.newentry.field 
}

proc defstruct_delete {ID} {
	set SelectIdx [$ID.list.listbox curselection]
	if {$SelectIdx == ""} then {
		bell
		return
	}
	foreach idx $SelectIdx {
		$ID.list.listbox delete $idx
	}
}


