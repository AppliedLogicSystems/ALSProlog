proc splash {path} {
	global tcl_platform
	
	wm withdraw .
	toplevel .als_splash_screen -bd 2 -relief flat
	wm withdraw .als_splash_screen
	if {$tcl_platform(platform) == "macintosh"} then {
		image create photo als_splash_gif -format gif -data [resource read GIFf turnstile_splash]
	} else {
		image create photo als_splash_gif -file [file join $path turnstile_splash.gif]
	}
	wm overrideredirect .als_splash_screen 1
	label .als_splash_screen.label -image als_splash_gif -bd 1 -relief flat
	pack .als_splash_screen.label -side top -expand 1 -fill both
	wm geometry .als_splash_screen +270+200
	wm deiconify .als_splash_screen
	update idletasks
}
