#!/usr/local/bin/tclsh -f run something

# Testing procedures copied from the file "defs" un Tcl's tests directory.
# Simplified. 

if ![info exists VERBOSE] {
    set VERBOSE 0
}

proc print_problem {name script code expected_code answer expected_answer} {
	puts stdout "\n"
	puts stdout "==== Test name: $name"
	puts stdout "==== Test case:"
	puts stdout "$script"
    if {$code != $expected_code} {
    	if {$code == 0} {
		    puts stdout "==== Test incorrectly generated success:"
		    puts stdout $answer
    	} elseif {$code == 1} {
		    puts stdout "==== Test incorrectly generated error:"
		    puts stdout $answer
		} elseif {$code == 2} {
		    puts stdout "==== Test incorrectly generated return exception:"
		    puts stdout $answer
		} elseif {$code == 3} {
		    puts stdout "==== Test incorrectly generated break exception"
		} elseif {$code == 4} {
		    puts stdout "==== Test incorrectly generated continue exception"
		} else {
		    puts stdout "==== Test incorrectly generated exception $code;  message was:"
		    puts stdout $answer
		}
    } else {
			puts stdout "==== Expected Result was:"
			puts stdout "$expected_answer"
			puts stdout "==== Incorrect Result was:"
			puts stdout "$answer"
    }
}

proc test0 {name script expected_answer expected_code} {
    global VERBOSE
    if $VERBOSE {
        puts stdout "Testing $name"
    }
    set code [catch {uplevel $script} answer]
    if {$code != $expected_code 
        || [string compare $answer $expected_answer] != 0} {
        print_problem $name $script $code $expected_code $answer $expected_answer
    }
}

proc test {name script answer} {
	test0 $name $script $answer 0
}

proc test_error {name script answer} {
	test0 $name $script $answer 1
}

# Load the other half of the test.
#prolog call builtins consult tcl_test.pro

#require package "ALS Prolog"
proc dotest {} {
test prolog-1.0 {prolog read_call true.} 1
test prolog-1.3 {prolog read_call fail.} 0

test simple-success {prolog call builtins true} 1
test simple-fail {prolog call builtins fail} 0
test_error simple_throw {prolog call builtins throw -atom ball} {prolog exception: ball}

# Test basic type conversion

test int  {prolog call user test_int -number 5} 1
test int  {prolog call user test_int -number {5}} 1
test int  {prolog call user test_int -number { 5 }} 1
test int  {prolog call user test_int -number [list 5]} 1

test int  {prolog call user test_int -number 4} 0
test int  {prolog call user test_int -number 5.5} 0
test int  {prolog call user test_int -atom 5} 0
test int  {prolog call user test_int -atom 5abc} 0
test int  {prolog call user test_int -atom abc} 0
test int  {prolog call user test_int -list 5} 0
test int  {prolog call user test_int -list {5 4 3}} 0
test int  {prolog call user test_int -list {a b c}} 0

test fp   {prolog call user test_float -number 5.5} 1
test fp   {prolog call user test_float -number {5.5}} 1
test fp   {prolog call user test_float -number { 5.5 }} 1
test fp   {prolog call user test_float -number [list 5.5]} 1

test fp   {prolog call user test_float -number 4.4} 0
test fp   {prolog call user test_float -number 5} 0
test fp   {prolog call user test_float -atom 5.5} 0
test fp   {prolog call user test_float -atom 5.5abc} 0
test fp   {prolog call user test_float -atom abc} 0
test fp   {prolog call user test_float -list 5.5} 0
test fp   {prolog call user test_float -list {5.5 4.4 3.3}} 0
test fp   {prolog call user test_float -list {a b c}} 0

test atom {prolog call user test_atom -atom {a}} 1
test atom {prolog call user test_atom -atom {a b}} 1
test atom {prolog call user test_atom -atom [list a b]} 1
test atom {prolog call user test_atom -atom {}} 1
test atom {prolog call user test_atom -atom 5} 1
test atom {prolog call user test_atom -atom { 5 }} 1
test atom {prolog call user test_atom -atom 5.5} 1
test atom {prolog call user test_atom -atom { 5.5 }} 1

test atom {prolog call user test_atom -number 5} 0
test atom {prolog call user test_atom -number { 5 }} 0
test atom {prolog call user test_atom -number 5.5} 0
test atom {prolog call user test_atom -number { 5.5 }} 0
test atom {prolog call user test_atom -list a} 0
test atom {prolog call user test_atom -list {a b}} 0
test atom {prolog call user test_atom -list {}} 0

test list {prolog call user test_list -list a} 1
test list {prolog call user test_list -list {a b}} 1
test list {prolog call user test_list -list [list a b]} 1
test list {prolog call user test_list -list 5} 1
test list {prolog call user test_list -list { 5 }} 1
test list {prolog call user test_list -list 5.5} 1
test list {prolog call user test_list -list {}} 1
test list {prolog call user test_list -list [list a b]} 1

test var  {prolog call user test_var -var _} 1

# Test variable binding

test bind-int  {list [prolog call user test_int -var x] $x} {1 5}
test bind-fp   {list [prolog call user test_float -var x] $x} {1 5.5}
test bind-atom {list [prolog call user test_atom -var x] $x} {1 {a b}}
test bind-list {list [prolog call user test_list -var x] $x} {1 {a b}}

#test large_arity {prolog call user large_arity -number 5 -number 5.5 -atom a -atom {a b} -atom 5 -atom { 5 } -atom 5.5 -atom { 5.5 } -atom {} -list {a b} -list {} -list a -list 5 -list { 5 } -list 5.5 -list { 5.5 }} 1
test large_arity {prolog call user large_arity -number 5 -number 5.5 -atom a -atom {a b} -atom 5 -atom { 5 } -atom 5.5 -atom { 5.5 } -atom {} -list {a b} -list {} -list a -list 5} 1

#make this an error
test_error bind-var  {prolog call user test_var -var x} {unset variables: x}

# Error testing
test_error no-options {prolog} {wrong # args: should be "prolog option ?arg ...?"}
test_error bad-option {prolog a} \
	{bad option "a": must be call, or read_call}
test_error no-string-term {prolog read_call} {wrong # args: should be "prolog termString ?varName ...?"}
test_error unused-var {prolog read_call true. b} \
	{unset variables: b}
test_error no-module {prolog call} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}
test_error no-functor {prolog call a} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}

test_error missing-number {prolog call builtins true -number} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}

test_error missing-atom {prolog call builtins true -atom} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}

test_error missing-list {prolog call builtins true -list} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}

test_error missing-var {prolog call builtins true -var} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}

test_error missing-type1 {prolog call builtins true 3} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}
test_error missing-type2 {prolog call builtins true 2 3} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}
test_error missing-type3 {prolog call builtins true 1 2 3} \
	{wrong # args: should be "prolog call module functor ?-type arg ...?"}
}

