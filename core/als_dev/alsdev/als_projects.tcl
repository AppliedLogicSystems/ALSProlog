#|==================================================================
#|				als_projects.tcl
#|		Copyright (c) 1998 Applied Logic Systems, Inc.
#|
#|		Tcl support for project management in the 
#|		ALS Development Environment
#|
#|		"$Id: als_projects.tcl,v 1.1 1998/03/13 01:45:53 ken Exp $"
#|==================================================================

proc open_project {} {
    global pmv
	 
	set Projectfile [tk_getOpenFile \
		-filetypes {{"Prolog Project Files" {.ppj}}} \
		-title "Prolog Project File to Open"]
	set File [file tail $Projectfile]
	if { "$Projectfile"!="" } then {
		prolog call alsdev open_project_file -atom $Projectfile -atom $File
	}
}
