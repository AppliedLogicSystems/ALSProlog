#======================================================================
#		tables.tcl
#	Copyright (c) 1997-8 Applied Logic Systems Inc.
#
#		Rectangular display tables
#
#		Uses the Tk Table Widget (Version 1.80+)
#
#	Author: Kenneth A. Bowen, Applied Logic Systems, Inc.
#======================================================================


proc load_table_package {DirsList} {
	set Result 1
	if {[string match {} [info commands table]] && \
		[catch {package require Tktable} err]} {
		append TkTFile Tktable [info sharedlibext]
		set NumDirs [llength $DirsList]
		set Flag 0
		set DirNum 0
		set Result 1
		while {[expr $DirNum<$NumDirs] && [expr $Flag == 0]} {
			set Where [eval file join [concat [lindex $DirsList $DirNum] $TkTFile]]
			set Result [catch {load $Where }]
			incr DirNum
		}
	}
	return $Result
}

	## Create a table 

proc build_table {BaseName InfoArrayName OptsList} {
	upvar #0 $InfoArrayName IA 
	global array tcl_platform

	eval [list global array $InfoArrayName]
	set IA(library) Tktable
	append IA(library) [info sharedlibext]

	append IA(tabletop)   "." $BaseName _toplevel
	append IA(table) $IA(tabletop) ".table"
	append DataArrayName $BaseName _array
	set IA(array)   $DataArrayName

	set IA(rows) 6
	set IA(cols) 6
	set IA(font) {Times 9 normal}
	set IA(font) system
	set IA(foreground) black
	set IA(background) #b9b9b9
	set IA(selectmode) extended
	set IA(rowstretch) unset
	set IA(colstretch) unset
	set IA(menu) ""
	set IA(upperpane) ""
	set IA(lowerpane) ""

	set IA(variable) $IA(array)
	append IA(yscrollbar) $IA(tabletop) ".sy"
	append IA(xscrollbar) $IA(tabletop) ".sx"
	append IA(yscrollcommand) $IA(yscrollbar) " set"
	append IA(xscrollcommand) $IA(xscrollbar) " set"

	while {$OptsList != ""} {
		set IA([lindex $OptsList 0]) [lindex $OptsList 1]
		set OptsList [lrange $OptsList 2 end]
	}
	global array $IA(array)
	global array $DataArrayName

	upvar #0 $IA(array) ff
	if {$IA(colheadings) != ""} {
		set ll [llength $IA(colheadings)]
		for {set i 0} { $i < $ll } {incr i} { 
			set ff(-1,$i) [lindex $IA(colheadings) $i] 
		}
	}
	if {$IA(rowheadings) != ""} {
		set ll [llength $IA(rowheadings)]
		for {set i 0} { $i < $ll } {incr i} { 
			set ff($i,-1) [lindex $IA(rowheadings) $i] 
		}
	}
#	table_object $InfoArrayName

#proc table_object {InfoArrayName } {
#	upvar #0 $InfoArrayName IA
#	global array tcl_platform

	eval toplevel $IA(tabletop)
	if {$tcl_platform(platform) == "windows"} {
		focus -force $IA(tabletop)
	}
	table $IA(table) \
		-rows $IA(rows) \
		-height $IA(height) \
		-cols $IA(cols) \
		-width $IA(width) \
		-variable $IA(array) \
		-titlerows $IA(titlerows) \
		-titlecols $IA(titlecols) \
		-font $IA(font) \
		-foreground $IA(foreground) \
		-background $IA(background) \
		-yscrollcommand $IA(yscrollcommand) \
		-xscrollcommand $IA(xscrollcommand) \
		-roworigin $IA(roworigin) \
		-colorigin $IA(colorigin) \
		-selectmode $IA(selectmode) \
		-rowstretch $IA(rowstretch) \
		-colstretch $IA(colstretch) \
		-flashmode $IA(flashmode)

	scrollbar $IA(yscrollbar) -command [list $IA(table) yview]  
	scrollbar $IA(xscrollbar) -command [list $IA(table) xview] -orient horizontal 

	if {$IA(menu) != "" } then {
		set IA(menu) ""
		append IA(menu) $IA(tabletop) ".menu"
    	menu $IA(menu) -relief sunken -tearoff 0
		$IA(tabletop) configure -menu $IA(menu)
	}
	set RowC 0
	set ColC 0
	if {$IA(upperpane) != "" } then {
		set IA(upperpane) ""
		append IA(upperpane) $IA(tabletop) ".upperpane"
		frame $IA(upperpane) -background $IA(background) -relief flat
		grid $IA(upperpane) \
			-column 0 -row $RowC -columnspan 2 -rowspan 1 -sticky nesw
		grid rowconfig $IA(tabletop) $RowC -weight 0
		incr RowC
	}

	grid columnconfig $IA(tabletop) $ColC -weight 1
	grid rowconfig $IA(tabletop) $RowC -weight 1
	grid $IA(table) \
		-column $ColC -row $RowC -columnspan 1 -rowspan 1 -sticky nesw
	grid $IA(yscrollbar) \
		 -column [expr 1 + $ColC] -row $RowC -columnspan 1 -rowspan 1 -sticky ns
	grid $IA(xscrollbar) \
		-column $ColC -row [expr 1 + $RowC] -columnspan 1 -rowspan 1 -sticky ew
	incr RowC
	incr RowC

	if {$IA(lowerpane) != "" } then {
		set IA(lowerpane) ""
		append IA(lowerpane) $IA(tabletop) ".lowerpane"
		frame $IA(lowerpane) -background $IA(background) -relief flat
		grid $IA(lowerpane) \
			-column 0 -row $RowC -columnspan 2 -rowspan 1 -sticky nesw
		grid rowconfig $IA(tabletop) $RowC -weight 0
		incr RowC
	}

	wm title $IA(tabletop) $IA(title)
	update
#	return [list $IA(table) $DataArrayName $InfoArrayName $IA(tabletop)]

	set RetL [list $IA(table) $DataArrayName $InfoArrayName $IA(tabletop) \
			$IA(menu) $IA(upperpane) $IA(lowerpane)	]

	return $RetL
}
}


proc build_table4 {BaseName InfoArrayName OptsList OthersList} {
	upvar #0 $InfoArrayName IA 

	eval [list global array $InfoArrayName]
	set IA(library) Tktable
	append IA(library) [info sharedlibext]

	append IA(tabletop)   "." $BaseName _toplevel
	append IA(table) $IA(tabletop) ".table"
	append DataArrayName $BaseName _array
	set IA(array)   $DataArrayName

	set IA(rows) 6
	set IA(cols) 6
#	set IA(font) {{Times 9 normal}}
	set IA(font) system
	set IA(foreground) black
	set IA(background) #b9b9b9
	set IA(selectmode) extended
	set IA(rowstretch) unset
	set IA(colstretch) unset

	set IA(variable) $IA(array)
	append IA(yscrollbar) $IA(tabletop) ".sy"
	append IA(xscrollbar) $IA(tabletop) ".sx"
	append IA(yscrollcommand) $IA(yscrollbar) " set"
	append IA(xscrollcommand) $IA(xscrollbar) " set"

	set NOL ""
	while {$OptsList != ""} {
		if {[lindex $OptsList 0] != "" } then {
			set IA([lindex $OptsList 0]) [lindex $OptsList 1]
			append NOL " -[lindex $OptsList 0] [lindex $OptsList 1]"
		}
		set OptsList [lrange $OptsList 2 end]
	}
#	append NOL " -font " $IA(font)
tk_dialog .jjj "NOL" "$NOL" "" 0 OK
	global array $IA(array)
	global array $DataArrayName

	while {$OthersList != ""} {
		set IA([lindex $OthersList 0]) [lindex $OthersList 1]
		set OthersList [lrange $OthersList 2 end]
	}

	upvar #0 $IA(array) ff
	if {$IA(colheadings) != ""} {
		set ll [llength $IA(colheadings)]
		for {set i 0} { $i < $ll } {incr i} { 
			set ff(-1,$i) [lindex $IA(colheadings) $i] 
		}
	}
	if {$IA(rowheadings) != ""} {
		set ll [llength $IA(rowheadings)]
		for {set i 0} { $i < $ll } {incr i} { 
			set ff($i,-1) [lindex $IA(rowheadings) $i] 
		}
	}

	table_object2 $InfoArrayName $NOL
	wm title $IA(tabletop) $IA(title)
	update
	return [list $IA(table) $DataArrayName $InfoArrayName $IA(tabletop)]
}

proc table_object2 {InfoArrayName OptsList} {
	upvar #0 $InfoArrayName IA
	global array tcl_platform

	eval toplevel $IA(tabletop)
	if {$tcl_platform(platform) == "windows"} {
		focus -force $IA(tabletop)
	}
	eval table $IA(table) $OptsList

	scrollbar $IA(yscrollbar) -command [list $IA(table) yview]  
	scrollbar $IA(xscrollbar) -command [list $IA(table) xview] -orient horizontal 

	grid columnconfig $IA(tabletop) 0 -weight 1
	grid rowconfig $IA(tabletop) 0 -weight 1
	grid $IA(yscrollbar) \
		 -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns
	grid $IA(table) \
		-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $IA(xscrollbar) \
		-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew
	update
}




proc set_table { ArrayName Row Col Val } {
	upvar #0 $ArrayName TA
	set TA($Row,$Col) $Val
}

proc read_table { ArrayName Row Col} {
	upvar #0 $ArrayName TA
	return $TA($Row,$Col) 
}

proc write_table_row {ArrayName RowN ColSt Lim ValsList} {
	upvar #0 $ArrayName TA

	set Indx $ColSt
	foreach Item $ValsList {
		set TA($RowN,$Indx) $Item
		if { $Indx <= $Lim } then { incr Indx } else { break }
	}
}

proc write_table_col {ArrayName ColN RowSt Lim ValsList} {
	upvar #0 $ArrayName TA

	set Indx $RowSt
	foreach Item $ValsList {
		set TA($Indx,$ColN) $Item
		if { $Indx <= $Lim } then { incr Indx } else { break }
	}
}

proc destroy_table {BaseName InfoArrayName} {
	upvar #0 $InfoArrayName IA 

	destroy $IA(tabletop)   
	unset IA
}




##------------------------------------------------------
##	sweep_file_to_array/2
##	sweep_file_to_array {FilePath TgtArray}
##
##	- FilePath is the path to a tab0-delimited file
##	  of data (1 or more lines);
##	- TgtArray is an array associated with a TkTable;
##
##  Opens the file FilePath, and passes the resulting
## 	stream and TgtArray to sweep_to_array/2, which
##	reads the data from the file line by line, and
##  writes each line into the corresponding line of
##  TgtArray.
##------------------------------------------------------
proc sweep_file_to_array {FilePath TgtArray} {
	set SI [open $FilePath r]
	sweep_to_array $SI $TgtArray -1
	close $SI
}

proc sweep_file_to_array3 {FilePath TgtArray InitColNum} {
	set SI [open $FilePath r]
	sweep_to_array $SI $TgtArray $InitColNum
	close $SI
}

proc sweep_file_to_array_select {FilePath TgtArray InitColNum ColNumList} {
	set SI [open $FilePath r]
	sweep_to_array_select $SI $TgtArray -1 $ColNumList
	close $SI
}

proc sweep_file_to_array_select3 {FilePath TgtArray InitColNum} {
	global SelectedColNums

	set SI [open $FilePath r]
	sweep_to_array_select $SI $TgtArray $InitColNum $SelectedColNums
	close $SI
}


proc sweep_to_array {SI TgtArray InitColNum} {
	upvar #0 $TgtArray TA

	set Row 0
    set CCnt [gets $SI Line]
	while { "$CCnt" >= 1 } {
		set Col $InitColNum
		while {$Line != ""} {
			incr Col
			set Pos [string first "\t" $Line]
			if {$Pos == -1} then { set Pos [ string length $Line ] }
			set Item [ string range $Line 0 [ expr $Pos-1 ] ]
			set TA($Row,$Col) $Item
			set Line [string range $Line [ expr $Pos+1 ] end]
		}
		set CCnt [gets $SI Line]
		incr Row
	}
}

proc sweep_to_array_select {SI TgtArray InitColNum SrcColNumList} {
	global TheLastArray 
	set TheLastArray $TgtArray

#puts "ENTER: sweep_to_array_select"

	upvar #0 $TgtArray TA

	set NumSrcCols [llength $SrcColNumList]
	set Row 0
    set CCnt [gets $SI Line]
#puts "stas: CCnt-1=$CCnt"
	while { "$CCnt" >= 1 } {
#puts "Row=$Row"
		set SaveLine $Line
		set TableCol $InitColNum
		set II 0
		set SCC 0
		while {($Line != "") && ($II<$NumSrcCols)} {
			set SrcColNum [lindex $SrcColNumList $II]
			if {$SCC == $SrcColNum} then {
				set Pos [string first "\t" $Line]
				if {$Pos == -1} then { 
					set Pos [llength $Line]
				}
			} elseif {$SCC < $SrcColNum } then  {
				while {$SCC < $SrcColNum} {
					set Pos [string first "\t" $Line]
					if {$Pos == -1} then { 
						set II $NumSrcCols
						break
					} 
					set Line [string range $Line [ expr $Pos+1 ] end]
					incr SCC
				}
				set Pos [string first "\t" $Line]
				if {$Pos == -1} then { 
					set Pos [llength $Line]
				}
			} else {
				set SCC 0
				set Line $SaveLine
				while {$SCC < $SrcColNum} {
					set Pos [string first "\t" $Line]
					if {$Pos == -1} then { 
						set II $NumSrcCols
						break
					} 
					set Line [string range $Line [ expr $Pos+1 ] end]
					incr SCC
				}
				set Pos [string first "\t" $Line]
				if {$Pos == -1} then { 
					set Pos [llength $Line]
				}
			}
				## Now positioned to read correct data entry & put it
				## into the correct column:
			set Item [ string range $Line 0 [ expr $Pos - 1 ] ]
			incr TableCol
			set TA($Row,$TableCol) $Item
			incr II
		}
		set CCnt [gets $SI Line]
		incr Row
	}
}






