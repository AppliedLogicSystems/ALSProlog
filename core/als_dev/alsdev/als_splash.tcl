proc center {window args} {
	if {[llength $args]} then {
		set x [winfo x $args]
		set y [winfo y $args]
		set ow [winfo width $args]
		set oh [winfo height $args]
	} else {
		set x 0
		set y 0
		set ow [winfo screenwidth $window]
		set oh [winfo screenheight $window]
	} 
	
	set wratio 0.5
	set hratio 0.3
	
	set w [winfo reqwidth $window]
	set h [winfo reqheight $window]
	
	wm geometry $window \
	+[expr round($ow*$wratio - $w/2)+$x]+[expr round($oh*$hratio - $h/2)+$y] 
}

proc splash {path} {
	global tcl_platform
	
	wm withdraw .
	toplevel .als_splash_screen -bd 2 -relief flat
	wm withdraw .als_splash_screen
	if {$tcl_platform(platform) == "macintosh"} then {
		image create photo als_splash_gif -format gif -data [resource read GIFf turnstile_splash]
	} else {
		image create photo als_splash_gif -file [file join $path .. images turnstile_splash.gif]
	}
	wm overrideredirect .als_splash_screen 1
	label .als_splash_screen.label -image als_splash_gif -bd 1 -relief flat
	pack .als_splash_screen.label -side top -expand 1 -fill both
	update idletask
	center .als_splash_screen
	update idletask
	wm deiconify .als_splash_screen
	update
}
