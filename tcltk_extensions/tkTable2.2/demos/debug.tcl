#!/bin/sh
# the next line restarts using wish \
exec wish "$0" ${1+"$@"}

## version2.tcl
##
## This demo uses most features of the table widget
##
## jeff.hobbs@acm.org

source [file join [file dirname [info script]] loadtable.tcl]

array set table {
    rows	9
    cols	8
    table	.t
    array	t
}

proc fill { array x y } {
    upvar $array f
    for {set i -$x} {$i<$x} {incr i} {
	for {set j -$y} {$j<$y} {incr j} { set f($i,$j) "r$i,c$j" }
    }
}

## Test out the use of a procedure to define tags on rows and columns
proc colProc col { if {$col > 0 && $col % 2} { return OddCol } }

label .label -text "TkTable v2 Example"

fill $table(array) $table(rows) $table(cols)
table $table(table) \
	-rows $table(rows) -cols $table(cols) \
	-variable $table(array) \
	-width 6 -height 8 \
	-titlerows 1 -titlecols 2 \
	-roworigin -1 -colorigin -2 \
	-yscrollcommand {.sy set} \
	-xscrollcommand {.sx set} \
	-coltagcommand colProc \
	-selectmode extended \
	-rowstretch unset \
	-colstretch unset \
	-selecttitles 0 \
	-drawmode single

scrollbar .sy -command [list $table(table) yview]
scrollbar .sx -command [list $table(table) xview] -orient horizontal
button .exit -text "Exit" -command {exit}
grid .label - -sticky ew
grid $table(table) .sy -sticky news
grid .sx -sticky ew
grid .exit -sticky ew -columnspan 2
grid columnconfig . 0 -weight 1
grid rowconfig . 1 -weight 1

$table(table) tag config OddCol -bg brown -fg pink
$table(table) tag config title -bg red -fg blue -relief sunken
$table(table) tag config dis -state disabled

set i -1
set first [$table(table) cget -colorigin]
foreach anchor {n s e w nw ne sw se c} {
    $table(table) tag config $anchor -anchor $anchor
    $table(table) tag row $anchor [incr i]
    $table(table) set $i,$first $anchor
}
font create courier -family Courier -size 10
$table(table) tag config s -font courier -justify center

image create photo logo \
	-file [file join [file dirname [info script]] tcllogo.gif]
$table(table) tag config logo -image logo -showtext 1
$table(table) tag cell logo 1,2 2,3 4,1
$table(table) tag cell dis 2,1 1,-1 3,0
$table(table) width -2 8 -1 9 0 12 4 14

$table(table) set \
	1,1 "multi-line\ntext\nmight be\ninteresting" \
	3,2 "more\nmulti-line\nplaying\n" \
	2,2 "null\0byte"

set i -1
foreach sticky {n s w e nsw nse news} {; # could include: ews new
    set l [label $table(table).$sticky \
	    -text "stick $sticky" -fg black -bg yellow]
    $table(table) window config 5,$i -sticky $sticky -window $l
    if {$i%2} { $table(table) window config 5,$i -relief raised }
    incr i
}
set l [label [winfo parent $table(table)].l -bg orange]
$l config -text $l
$table(table) window config 5,1 -sticky news -window $l

puts [list Table is $table(table) with array [$table(table) cget -var]]

#$table(table) postscript -file out.ps -first origin -last 2,2
#if {[string match {} [info commands tkcon]]} exit
