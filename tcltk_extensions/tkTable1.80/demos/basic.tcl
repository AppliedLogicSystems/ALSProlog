#!/bin/sh
# the next line restarts using wish \
	exec wish "$0" ${1+"$@"}

## basic.tcl
##
## This demo shows the basic use of the table widget
##
## jhobbs@cs.uoregon.edu

array set table {
    library	Tktable
    rows	20
    cols	20
    table	.t
    array	t
}
append table(library) [info sharedlibext]

## Ensure that the table library extension is loaded
if {[string match {} [info commands table]] && \
	[catch {package require Tktable} err]} {
    if {[catch {load [file join [pwd] .. $table(library)]} err] && \
	    [catch {load [file join [pwd] $table(library)]} err]} {
	error $err
    }
}

proc fill { array x y } {
    upvar $array f
    for {set i -$x} {$i<$x} {incr i} {
	for {set j -$y} {$j<$y} {incr j} { set f($i,$j) "r:$i,c:$j" }
    }
}

## Test out the use of a procedure to define tags on rows and columns
proc rowProc row { if {$row>0 && $row%2} { return OddRow } }
proc colProc col { if {$col>0 && $col%2} { return OddCol } }

label .label -text "TkTable v1 Example"

fill $table(array) $table(rows) $table(cols)
table $table(table) -rows $table(rows) -cols $table(cols) \
	-variable $table(array) \
	-width 6 -height 6 \
	-titlerows 2 -titlecols 2 \
	-yscrollcommand {.sy set} -xscrollcommand {.sx set} \
	-roworigin -2 -colorigin -2 \
	-rowtagcommand rowProc -coltagcommand colProc \
	-selectmode extended \
	-rowstretch unset -colstretch last \
	-flashmode on

scrollbar .sy -command [list $table(table) yview]
scrollbar .sx -command [list $table(table) xview] -orient horizontal
grid .label - -sticky ew
grid $table(table) .sy -sticky news
grid .sx -sticky ew
grid columnconfig . 0 -weight 1
grid rowconfig . 1 -weight 1

$table(table) tag config OddRow -bg orange -fg purple
$table(table) tag config OddCol -bg brown -fg pink

## Version 1.3 image features
image create photo logo \
	-file [file join [file dirname [info script]] tcllogo.gif]
$table(table) tag config logo -image logo
$table(table) tag cell logo 1,2 2,3 4,1

## Version 1.4 -command feature
proc showrc {r c} { return $r,$c }

update

## This will show the use of the flash mode
after 1000 [list array set $table(array) { 1,0 "Flash Me" 4,2 "And Me" }]

puts [list Table is $table(table) with array [$table(table) cget -var]]

