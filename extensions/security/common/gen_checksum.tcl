#!/usr/local/bin/tclsh

proc checksum {n} {
	set s [expr $n + 4328]
	
	set d1 [expr ($s % 10)/1]
	set d2 [expr ($s % 100)/10]
	set d3 [expr ($s % 1000)/100]
	set d4 [expr ($s % 10000)/1000]
	set d5 [expr ($s % 100000)/10000]
	
	set s [expr $d3*10000 + $d5*1000 + $d1*100 + $d2*10 + $d4]
	
	set s [expr $s * 5]
	
	set s [expr abs($s - $n * 3) % 100000]
	
	return $s
}

puts $argv
puts [checksum $argv]
