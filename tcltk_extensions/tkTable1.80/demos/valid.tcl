#!/bin/sh
# the next line restarts using wish \
	exec wish "$0" ${1+"$@"}

## valid.tcl
##
## This demos shows the use of the validation mechanism of the table
## and uses the table's cache (no -command or -variable)
##
## jhobbs@cs.uoregon.edu

array set table {
    library	Tktable
    rows	10
    cols	10
    table	.table
}
append table(library) [info sharedlibext]

if {[string match {} [info commands table]] && \
	[catch {package require Tktable} err]} {
    if {[catch {load [file join [pwd] .. $table(library)]} err] && \
	    [catch {load [file join [pwd] $table(library)]} err]} {
	error $err
    }
}

proc colorize num {
    if {$num>0 && $num%2} { return colored }
}

proc fill_headers {w {r 10} {c 10}} {
    for {set i 1} {$i < $r} {incr i} {
	$w set $i,0 "$i"
    }
    for {set j 1} {$j < $c} {incr j} {
	if {$j%3==1} {
	    $w set 0,$j AlphaNum
	} elseif {$j%2==1} {
	    $w set 0,$j Alpha
	} elseif {$j} {
	    $w set 0,$j Real
	}
    }
}

proc validate {c val} {
    if {$c%3==1} {
	## Alphanum
	set expr {^[A-Za-z0-9 ]*$}
    } elseif {$c%2==1} {
	## Alpha
	set expr {^[A-Za-z ]*$}
    } elseif {$c} {
	## Real
	set expr {^[-+]?[0-9]*\.?[0-9]*([0-9]\.?e[-+]?[0-9]*)?$}
    }
    if {[regexp $expr $val]} {
	return 1
    } else {
	bell
	return 0
    }
}

label .example -text "TkTable v1 Validated Table Example"

set t $table(table)
table $t \
	-rows $table(rows) \
	-cols $table(cols) \
	-cache 1 \
	-titlerows 1 \
	-titlecols 1 \
	-yscrollcommand { .sy set } \
	-xscrollcommand { .sx set } \
	-width 5 -height 5 \
	-coltagcommand colorize \
	-flashmode on \
	-selectmode extended \
	-colstretch unset \
	-rowstretch unset \
	-batchmode 1 \
	-validate yes \
	-vcmd {if {![%W tag includes title %C]} { validate %c %S } }

fill_headers $t
$t tag config colored -bg lightblue
$t tag config title -fg red
$t width 0 3

scrollbar .sy -command [list $t yview]
scrollbar .sx -command [list $t xview] -orient horizontal
grid .example -     -sticky ew
grid $t       .sy   -sticky news
grid .sx            -sticky ew
grid columnconfig . 0 -weight 1
grid rowconfig . 1 -weight 1

puts [list Table is $table(table)]

