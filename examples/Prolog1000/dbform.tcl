#!/usr/local/bin/wish
#############################################################################
# Visual Tcl v1.11 Project
#

#################################
# GLOBAL VARIABLES
#
global widget; 
global array app 
   set app(Addl) closed
   set app(self) "<Domain>"
   set app(searchmode) some

#################################
# USER DEFINED PROCEDURES
#


proc {Window} {args} {
global vTcl
    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set newname [lindex $args 2]
    set rest [lrange $args 3 end]
    if {$name == "" || $cmd == ""} {return}
    if {$newname == ""} {
        set newname $name
    }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists == "1" && $name != "."} {wm deiconify $name; return}
            if {[info procs vTclWindow(pre)$name] != ""} {
                eval "vTclWindow(pre)$name $newname $rest"
            }
            if {[info procs vTclWindow$name] != ""} {
                eval "vTclWindow$name $newname $rest"
            }
            if {[info procs vTclWindow(post)$name] != ""} {
                eval "vTclWindow(post)$name $newname $rest"
            }
        }
        hide    { if $exists {wm withdraw $newname; return} }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $base passive
    wm geometry $base 1x1+0+0
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt.tcl"
    ###################
    # SETTING GEOMETRY
    ###################
}

image create photo closed_ptr -file closed_wins.gif
image create photo open_ptr -file open_wins.gif


proc vTclWindow.dbf {base} {
    if {$base == ""} {
        set base .dbf
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 457x450+139+342
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Prolog 1000 Database"
    frame $base.main -borderwidth 2 -height 295 -relief groove 
    frame $base.main.pgm \
        -borderwidth 1 -height 30 -relief raised 
    label $base.main.pgm.01 -anchor w -text Program: 
    entry $base.main.pgm.entry \
        -cursor {} -highlightthickness 0 
    frame $base.main.domain \
        -borderwidth 1 -height 30 -relief raised 
    label $base.main.domain.01 \
        -anchor w -text Domain: 
    entry $base.main.domain.entry \
        -cursor {} -highlightthickness 0 
    label $base.main.purposelabel -borderwidth 1 -text Purpose: 
    text $base.main.purposetext -height 4 
    frame $base.main.devinfo -borderwidth 2 -height 75 -relief groove 
    frame $base.main.devinfo.dvprs \
        -borderwidth 1 -height 30 -relief raised 
    label $base.main.devinfo.dvprs.01 \
        -anchor w -text Developers: 
    entry $base.main.devinfo.dvprs.entry \
        -cursor {} -highlightthickness 0 
    frame $base.main.devinfo.org \
        -borderwidth 1 -height 30 -relief raised 
    label $base.main.devinfo.org.01 \
        -anchor w -text Org: 
    entry $base.main.devinfo.org.entry \
        -cursor {} -highlightthickness 0 
    label $base.main.desclabel \
        -borderwidth 1 -text Description: 
    frame $base.main.desc \
        -borderwidth 2 -height 75 -relief groove 
    text $base.main.desc.text -height 10 \
		-yscrollcommand " $base.main.desc.vsb set"
    scrollbar $base.main.desc.vsb \
        -orient vert -command "$base.main.desc.text yview"
    frame $base.main.control -borderwidth 2 -relief groove 
    button $base.main.control.toggle -relief raised \
		-padx 6 -image closed_ptr -command toggle_addl
    button $base.main.control.prev \
        -padx 6 -text { << } -command {prolog call user show_previous}
    button $base.main.control.next \
        -padx 6 -text { >> } -command {prolog call user show_next}

    button $base.main.control.clear \
        -padx 11 -pady 4 -text Clear -command clear_form
    button $base.main.control.find \
        -padx 11 -pady 4 -text Find -command {Window show .find}
    button $base.main.control.retrieve \
        -padx 11 -pady 4 -text Retrieve -command lookup
    label $base.main.control.num_label \
        -anchor w -text Num: 
    entry $base.main.control.num_entry \
        -cursor {} -highlightthickness 0 -width 5 
    label $base.main.control.total_num \
        -anchor w -text {of ----}
	bind $base.main.control.num_entry <Return> {lookup}

    frame $base.addl -borderwidth 2 -relief groove 

    text $base.addl.text -height 10 \
		-yscrollcommand " $base.addl.vsb set"
    scrollbar $base.addl.vsb \
        -orient vert -command "$base.addl.text yview"

    ###################
    # SETTING GEOMETRY
    ###################
    grid columnconf $base 0 -weight 1
    grid rowconf $base 0 -weight 1
    grid rowconf $base 1 -weight 1

    grid $base.main \
        -in .dbf -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nsew 

    grid columnconf $base.main 0 -weight 1
    grid rowconf $base.main 0 -weight 0
    grid rowconf $base.main 1 -weight 0
    grid rowconf $base.main 2 -weight 0
    grid rowconf $base.main 3 -weight 0
    grid rowconf $base.main 4 -weight 0
    grid rowconf $base.main 5 -weight 0
    grid rowconf $base.main 6 -weight 1
    grid rowconf $base.main 7 -weight 0
    grid $base.main.pgm \
        -in .dbf.main -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky ew 
    pack $base.main.pgm.01 \
        -in .dbf.main.pgm -anchor center -expand 0 -fill none -padx 2 \
        -pady 2 -side left 
    pack $base.main.pgm.entry \
        -in .dbf.main.pgm -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    grid $base.main.domain \
        -in .dbf.main -column 0 -row 1 -columnspan 3 -rowspan 1 -sticky ew 
    pack $base.main.domain.01 \
        -in .dbf.main.domain -anchor center -expand 0 -fill none -padx 2 \
        -pady 2 -side left 
    pack $base.main.domain.entry \
        -in .dbf.main.domain -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    grid $base.main.purposelabel \
        -in .dbf.main -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.main.purposetext \
        -in .dbf.main -column 0 -row 3 -columnspan 3 -rowspan 1 -sticky ew 
    grid $base.main.devinfo \
        -in .dbf.main -column 0 -row 4 -columnspan 3 -rowspan 1 -sticky ew 
    grid columnconf $base.main.devinfo 0 -weight 1
    grid columnconf $base.main.devinfo 1 -weight 1
    grid rowconf $base.main.devinfo 0 -weight 1
    grid $base.main.devinfo.dvprs \
        -in .dbf.main.devinfo -column 0 -row 0 -columnspan 1 -rowspan 1 \
        -sticky ew 
    pack $base.main.devinfo.dvprs.01 \
        -in .dbf.main.devinfo.dvprs -anchor center -expand 0 -fill none \
        -padx 2 -pady 2 -side left 
    pack $base.main.devinfo.dvprs.entry \
        -in .dbf.main.devinfo.dvprs -anchor center -expand 1 -fill x -padx 2 \
        -pady 2 -side right 
    grid $base.main.devinfo.org \
        -in .dbf.main.devinfo -column 1 -row 0 -columnspan 1 -rowspan 1 \
        -sticky ew 
    pack $base.main.devinfo.org.01 \
        -in .dbf.main.devinfo.org -anchor center -expand 0 -fill none \
        -padx 2 -pady 2 -side left 
    pack $base.main.devinfo.org.entry \
        -in .dbf.main.devinfo.org -anchor center -expand 1 -fill x -padx 2 \
        -pady 2 -side right 
    grid $base.main.desclabel \
        -in .dbf.main -column 0 -row 5 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.main.desc \
        -in .dbf.main -column 0 -row 6 -columnspan 3 -rowspan 1 -sticky ewns
    grid columnconf $base.main.desc 0 -weight 1
    grid columnconf $base.main.desc 1 -weight 0
    grid rowconf $base.main.desc 1 -weight 1
    grid $base.main.desc.text \
        -in .dbf.main.desc -column 0 -row 1 -columnspan 1 -rowspan 1 \
        -sticky nesw 
    grid $base.main.desc.vsb \
        -in .dbf.main.desc -column 1 -row 1 -columnspan 1 -rowspan 1 \
        -sticky nes 

    grid $base.main.control \
        -in .dbf.main -column 0 -row 7 -columnspan 3 -rowspan 1 -sticky ew
    pack $base.main.control.toggle \
        -in .dbf.main.control -anchor center -expand 0 -fill none \
        -padx 10 -ipadx 2 -ipady 2 -side left 
    pack $base.main.control.prev \
        -in .dbf.main.control -anchor center -expand 0 -fill none \
        -padx 10 -side left 
    pack $base.main.control.next \
        -in .dbf.main.control -anchor center -expand 0 -fill none \
        -padx 10 -side left 

    pack $base.main.control.clear \
        -in .dbf.main.control -anchor center -expand 0 -fill none \
        -padx 10 -side left 

    pack $base.main.control.find \
        -in .dbf.main.control -anchor center -expand 0 -fill none \
        -padx 15 -side left 

    pack $base.main.control.total_num \
        -in .dbf.main.control -anchor center -expand 0 -fill x -padx 2 -pady 2 \
        -side right 
    pack $base.main.control.num_entry \
        -in .dbf.main.control -anchor center -expand 0 -fill x -padx 2 -pady 2 \
        -side right 
    pack $base.main.control.num_label \
        -in .dbf.main.control -anchor center -expand 0 -fill none -padx 2 \
        -side right 

    pack $base.main.control.retrieve \
        -in .dbf.main.control -anchor center -expand 0 -fill none \
        -side right 


    grid columnconf $base.addl 0 -weight 1
    grid columnconf $base.addl 1 -weight 0
    grid rowconf $base.addl 1 -weight 1
    grid $base.addl.text -in .dbf.addl \
		-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $base.addl.vsb -in .dbf.addl \
		-column 1 -row 1 -columnspan 1 -rowspan 1 -sticky nes 

	wm geometry $base ""
}

proc lookup {} {
	set NEntry [string trim [.dbf.main.control.num_entry get]]
	if {"$NEntry"==""} then {
		bell
	} else {
		prolog call user lookup -number $NEntry
	}
}

proc clear_form {} {
    .dbf.main.pgm.entry delete 0 end
    .dbf.main.control.num_entry delete 0 end
    .dbf.main.domain.entry delete 0 end
    .dbf.main.devinfo.dvprs.entry delete 0 end
    .dbf.main.devinfo.org.entry delete 0 end
	.dbf.main.purposetext delete 1.0 end
	.dbf.main.desc.text delete 1.0 end
}

proc displ_head {Name N} {
    .dbf.main.pgm.entry delete 0 end
    .dbf.main.pgm.entry insert end $Name
    .dbf.main.control.num_entry delete 0 end
    .dbf.main.control.num_entry insert end $N
}

proc displ_rnum {N} {
    .dbf.main.control.num_entry delete 0 end
    .dbf.main.control.num_entry insert end $N
	update
}

proc displ_group2 {Domain Developers Org} {
    .dbf.main.domain.entry delete 0 end
    .dbf.main.domain.entry insert end $Domain
    .dbf.main.devinfo.dvprs.entry delete 0 end
    .dbf.main.devinfo.dvprs.entry insert end $Developers
    .dbf.main.devinfo.org.entry delete 0 end
    .dbf.main.devinfo.org.entry insert end $Org
}

proc toggle_addl { } {
	global array app
	if {$app(Addl) == "closed"} then {
    	.dbf.main.control.toggle configure -image open_ptr 
    	grid .dbf.addl \
        	-in .dbf -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nsew 
		set app(Addl) open
		prolog call user display_addl
	} else {
    	.dbf.main.control.toggle configure -image closed_ptr 
    	grid forget .dbf.addl 
		wm geometry .dbf ""
		set app(Addl) closed
	}
}



proc vTclWindow.find {base} {
	global array app
    if {$base == ""} {
        set base .find
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 328x270+73+366
    wm maxsize $base 1028 753
    wm minsize $base 104 1
    wm overrideredirect $base 0
    wm resizable $base 0 0
    wm deiconify $base
    wm title $base "Search the Prolog 1000 Database"
    frame $base.words \
        -borderwidth 2 -height 75 -width 125 
    label $base.words.label \
        -borderwidth 1 -text Words: 
    entry $base.words.entry
    frame $base.searchmode \
        -borderwidth 2 -height 75 -width 125 
    label $base.searchmode.label \
        -borderwidth 1 -text {must occur in selected field} 
    radiobutton $base.searchmode.all \
        -borderwidth 1 -highlightthickness 0 -pady 0 -text {All words} \
		-variable app(searchmode) -value all
    radiobutton $base.searchmode.some \
        -borderwidth 1 -highlightthickness 0 -padx 3 -pady 0 \
        -text {At least one word } \
		-variable app(searchmode)  -value some

    label $base.sfs_label \
        -borderwidth 1 -text {Search Field:} 

    frame $base.sfs \
        -borderwidth 2 -height 75 -width 125 
    radiobutton $base.sfs.program \
        -text Program -value "<Program>" -variable app(self) 
    radiobutton $base.sfs.domain \
        -text Domain -value "<Domain>" -variable app(self) 
    radiobutton $base.sfs.purpose \
        -text Purpose -value "<Purpose>" -variable app(self) 
    radiobutton $base.sfs.developers \
        -text Developers -value "<Developers>" -variable app(self) 
    radiobutton $base.sfs.organization \
        -text Organization -value "<Organization>" -variable app(self) 
    radiobutton $base.sfs.description \
        -text Description -value "<Description>" -variable app(self) 
    button $base.search \
        -text Search -command do_search
    button $base.search_more \
        -text {Search Further} -command do_search_more
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.words \
        -in .find -anchor center -expand 0 -fill x -pady 10 -side top 
    pack $base.words.label \
        -in .find.words -anchor center -expand 0 -fill none -padx 7 \
        -side left 
    pack $base.words.entry \
        -in .find.words -anchor center -expand 0 -fill x -padx 7 -side top 
    pack $base.searchmode \
        -in .find -anchor center -expand 0 -fill x -side top 
    pack $base.searchmode.label \
        -in .find.searchmode -anchor center -expand 0 -fill none -padx 3 \
        -side right 
    pack $base.searchmode.all \
        -in .find.searchmode -anchor center -expand 0 -fill none -side left 
    pack $base.searchmode.some \
        -in .find.searchmode -anchor center -expand 0 -fill none -side left 

    pack $base.sfs_label \
        -in .find -anchor nw -expand 0 -fill none -padx 4 -pady 4 -side top 
    pack $base.sfs \
        -in .find -anchor center -expand 0 -fill x -pady 4 -side top 

    grid columnconf $base.sfs 0 -weight 0
    grid columnconf $base.sfs 1 -weight 0
    grid columnconf $base.sfs 2 -weight 0
    grid rowconf $base.sfs 0 -weight 0
    grid rowconf $base.sfs 1 -weight 0

    grid $base.sfs.program \
		-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.sfs.domain \
		-column 1 -row 0 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.sfs.purpose \
		-column 2 -row 0 -columnspan 1 -rowspan 1 -sticky w 

    grid $base.sfs.developers \
		-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.sfs.organization \
		-column 1 -row 1 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.sfs.description \
		-column 2 -row 1 -columnspan 1 -rowspan 1 -sticky w 

    pack $base.search \
        -in .find -anchor center -expand 0 -fill none -side left -padx 30 
    pack $base.search_more \
        -in .find -anchor center -expand 0 -fill none -side right -padx 30 

	wm geometry $base ""
}

proc do_search { } {
	global array app

	set Words [.find.words.entry get]
	prolog call user find_record -list $Words -atom $app(searchmode) \
								-atom $app(self) -number 0
}

proc do_search_more { } {
	global array app

	set Words [.find.words.entry get]
	set Cur   [.dbf.main.control.num_entry get]
	prolog call user find_record -list $Words -atom $app(searchmode) \
								-atom $app(self) -number $Cur
}


Window show .
Window show .dbf

