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
 
proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $base passive
    wm geometry $base 200x200+0+0
    wm maxsize $base 1028 753
    wm minsize $base 104 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt"
    ###################
    # SETTING GEOMETRY
    ###################
}




global array agv
    source [file join "$APPTCLPATH" gen_app.tcl]
    source [file join "$APPTCLPATH" generic_menu.tcl]
    source [file join "$APPTCLPATH" generic_document.tcl]
set agv(app_name) genericapp

set agv(version_num) 0

set agv(build_num) 0
set agv(dflt_mod) genericapp
set agv(doc_extension) lts
set agv(main_mb) .genericapp_main.menubar
image create photo about_genericapp -file [file join "$APPTCLPATH" "images/about_genericapp.gif"]

    set agv(posted_vis) {}
    set agv(untitled_counter) 0
    load_photo_gif up_arrow_gif up-arrow-blue
    load_photo_gif down_arrow_gif down-arrow-blue
    load_photo_gif right_gif right-arrow-blue
    load_photo_gif left_gif left-arrow-blue
    switch $tcl_platform(platform) {
    	unix {
    		load_photo_gif closed_ptr closed_unix
    		load_photo_gif open_ptr open_unix
    	}
    	windows {
    		load_photo_gif closed_ptr closed_wins
    		load_photo_gif open_ptr open_wins
    		}
    	macintosh {
    		load_photo_gif closed_ptr closed_mac
    		load_photo_gif open_ptr open_mac
    	}
    	default {
    		load_photo_gif closed_ptr closed_wins
    		load_photo_gif open_ptr open_wins
    	}
    }
set agv(toplevel) .genericapp_main

set agv(title) "Generic Application"

proc genericapp_main { } {
	set base .genericapp_main
   if {[winfo exists $base]} {wm deiconify $base; return}
   toplevel $base -class Toplevel -menu .genericapp_main.menubar
   menu $base.menubar -cursor {} -relief sunken -tearoff 0
   wm focusmodel $base passive
   wm geometry $base 340x260+50+50
   wm maxsize $base 1028 753
   wm minsize $base 104 1
   wm overrideredirect $base 0
   wm resizable $base 1 1
   wm deiconify $base
   wm title $base "Generic Application"

add_generic_file_menu .genericapp_main.menubar listener .genericapp_main.menubar
add_generic_edit_menu .genericapp_main.menubar listener .genericapp_main.menubar
menu .genericapp_main.menubar.windows -tearoff 0 -title Windows
.genericapp_main.menubar add cascade -label "Windows" -menu .genericapp_main.menubar.windows -underline 0
add_help_menu .genericapp_main.menubar 

   frame $base.console -borderwidth 1 -relief raised
   scrollbar $base.console.hsb \
	-borderwidth 1 -command "$base.console.text xview" -orient horiz
   scrollbar $base.console.vsb \
	-borderwidth 1 -command "$base.console.text yview" -orient vert
   text $base.console.text \
	-xscrollcommand "$base.console.hsb set" \
	-yscrollcommand "$base.console.vsb set"

   frame $base.notifier -borderwidth 2 -relief groove
   label $base.notifier.status_label \
	-borderwidth 1 -relief sunken -text Status----

   grid columnconf $base 0 -weight 1
   grid rowconf $base 0 -weight 1
   grid $base.console -sticky nesw  \
	-in $base -column 0 -row 0 -columnspan 1 -rowspan 1
   grid columnconf $base.console 0 -weight 1
   grid rowconf $base.console 0 -weight 1
   grid $base.console.hsb -sticky ew  \
	-in $base.console -column 0 -row 1 -columnspan 1 -rowspan 1
   grid $base.console.vsb -sticky ns  \
	-in $base.console -column 1 -row 0 -columnspan 1 -rowspan 1
   grid $base.console.text -sticky nesw \
	-in $base.console -column 0 -row 0 -columnspan 1 -rowspan 1
   grid $base.notifier -sticky ew \
	-in $base -column 0 -row 1 -columnspan 1 -rowspan 1
   pack $base.notifier.status_label -side left \
	-in $base.notifier -anchor center -expand 0 -fill none
}


proc help_about { } {
    global agv
    toplevel .about_popup -bd 2 -relief raised -background #a4a4a4
    wm overrideredirect .about_popup 1
    label .about_popup.label -image about_genericapp \
        -bd 1 -relief flat -background blue
    frame .about_popup.ff -relief flat -border 0 -background #a4a4a4
    button .about_popup.ff.ok -text OK -bd 1 -relief raised \
	-background black -foreground white  -command {destroy .about_popup}
    label .about_popup.ff.build -text OK -bd 1 -relief flat \
	-text "Build $agv(build_num)" -background #a4a4a4
    label .about_popup.ff.version -text OK -bd 1 -relief flat \
	-text "Version $agv(version_num)" -background #a4a4a4
    pack .about_popup.label -side top -expand 1 -fill both
    pack .about_popup.ff -side top -fill x
    pack .about_popup.ff.version -side left -padx 20
    pack .about_popup.ff.build -side left -padx 20
    pack .about_popup.ff.ok -side right -padx 30
    wm geometry .about_popup ""
    wm deiconify .about_popup
}


source genericapp_gui_funcs.tcl

wm withdraw .

genericapp_main

