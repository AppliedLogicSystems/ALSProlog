
set date [clock format [clock seconds] -format "%m/%d/%Y"]
set duration 30
set customer_id 1
set demo_serial_number {}

trace variable date w update
trace variable duration w update
trace variable customer_id w update

label .date_label -text "Date"
entry .date -textvariable date
label .duration_label -text "Duration"
entry .duration -textvariable duration
label .customer_id_label -text "Customer ID"
entry .customer_id -textvariable customer_id
label .demo_serial_number_label -text "Demo Serial No."
entry .demo_serial_number -textvariable demo_serial_number

proc update {var args} {
	global date duration customer_id demo_serial_number
	set demo_serial_number [generate $date $duration $customer_id]
}

proc generate {date duration customer_id} {
	#set time [clock scan $date]
	if {[catch {set time [clock scan $date]}]} then {return "Invalid Date" }
	set year [expr [clock format $time -format "%Y"] - 1998]
	
	if {$year < 0 || $year > 99} then {return "Out of Range Year"}
	if {$duration <= 0 || $duration > 99} then {return "Out of Range Duration"}
	if {$customer_id < 0 || $customer_id > 9999} then {return "Out of Range Customer ID"}
	
	# Use scanf to convert month-day to number because expr will interpret
	# the string "0101" as an octal number.
	scan [clock format $time -format "%m%d"] "%d" a
	puts $a
	set b [expr $year*100 + $duration]
	set c $customer_id
		
	set a [rotate $a 4935]
	set b [rotate $b 6723]
	set c [rotate $c 2385] 
	set d [checksum [expr $a ^ $b ^ $c]]
	
	return [format "%04d-%04d-%04d-%04d" $a $b $c $d]
}

proc rotate {n r} {

	set n1 [expr ($n % 10)/1]
	set n2 [expr ($n % 100)/10]
	set n3 [expr ($n % 1000)/100]
	set n4 [expr ($n % 10000)/1000]

	set r1 [expr ($r % 10)/1]
	set r2 [expr ($r % 100)/10]
	set r3 [expr ($r % 1000)/100]
	set r4 [expr ($r % 10000)/1000]

	set n1 [expr ($n1 + $r1) % 10]
	set n2 [expr ($n2 + $r2) % 10]
	set n3 [expr ($n3 + $r3) % 10]
	set n4 [expr ($n4 + $r4) % 10]
	
	return [expr $n1 + $n2*10 + $n3*100 + $n4*1000]
}

proc checksum {n} {
	set s [expr $n + 247]
	
	set d1 [expr ($s % 10)/1]
	set d2 [expr ($s % 100)/10]
	set d3 [expr ($s % 1000)/100]
	set d4 [expr ($s % 10000)/1000]
	
	set s [expr $d3*1000 + $d1*100 + $d2*10 + $d4]
	
	set s [expr $s * 5]
	
	set s [expr abs($s - $n * 3) % 10000]
	
	return $s
}

update foo

pack .date_label .date .duration_label .duration .customer_id_label .customer_id \
	 .demo_serial_number_label .demo_serial_number
	 