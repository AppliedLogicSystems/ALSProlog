#|==================================================================
#|				als_projects.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl support for project management in the 
#|		ALS Development Environment
#|
#|		"$Id: als_projects.tcl,v 1.2 1998/03/30 22:11:38 ken Exp $"
#|==================================================================

proc open_project {} {
    global pmv
	 
	set Projectfile [tk_getOpenFile \
		-filetypes {{"Prolog Project Files" {.ppj}}} \
		-title "Prolog Project File to Open"]
	set File [file tail $Projectfile]
	set Dir [file dirname $Projectfile]
puts "CAlling open_project_file"
	if { "$Projectfile"!="" } then {
		prolog call test open_project_file  \
			-list [file split $Dir] -atom $File

#		prolog call alsdev open_project_file  \
	}
}
