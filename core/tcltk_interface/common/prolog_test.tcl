#!/usr/local/bin/tclsh -f run something

require package "ALS Prolog"

proc something {} {
	prolog "consult('prolog test.pro')"
	run_tests
}

proc test {command args} {
	
	catch {eval $command} result

	puts "test " $command "failed."
}

proc run_Tests {} {
run the prolog test to demonstrate trans

	if {prolog true} then {} else {error ""}

test {prolog "true."} 1
test {prolog true} 1
test {prolog "false."} 0
test {prolog false} 0

prolog 

catch prolog "throw exception"

test {prolog "fact(X,Y)."; list X Y} {lemon bitter}
prolog [struct fact [var X] [var Y]]

[functor <struct>] -> fact
[arg 1 <struct>] -> lemon
[arg 2 <struct>] -> bitter
