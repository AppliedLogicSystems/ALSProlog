#!/bin/sh
# Check for proper function of command line arguments and general
# unix shell functionality.

# Check for correct number of arguments
if test $# -lt 1
then
    echo 'Usage: test_command_line.sh prolog' 1>&2
    echo 'Example:' 1>&2
    echo '    test_command_line.sh ./alspro' 1>&2
    exit 2
fi

echo "Starting Command Line Tests"

prolog=$1

# Set ALS_OPTIONS to the empty string so $prolog will use defaults.
ALS_OPTIONS=
export ALS_OPTIONS

error_count=0


error () {
	command=$1
	expected_result=$2
	result=$3
	echo
    echo "Command Line Test Error:"
    echo "  Command: $command"
    echo "  Expected Result: $expected_result But got: $result"
    echo
    error_count=`expr $error_count + 1`
}

cl_test () {
    command=$1
    expected_result=$2

    echo "testing: $command"
    eval $command
    result=$?
    if test $result -ne $expected_result
    then
        error "$command" "$expected_result" "$result"
    fi
}

opt_test () {
    options=$1
    command=$2
    expected_result=$3

    echo "testing: ALS_OPTIONS=$1 ;  $command"
    ALS_OPTIONS=$1
    eval $command
    result=$?
    if test $result -ne $expected_result
    then
        error "ALS_OPTIONS=$1 ; $command" "$expected_result" "$result"
    fi
}

# Test -q (quiet) argument.

case $(uname) in
	*"_NT"*)	expected_output="?- " ;;
	        *) 	expected_output=""    ;;
esac

command="$prolog -q < /dev/null"
output=`eval $command`

if test "$output" != "$expected_output"
then
	error "$command" "$expected_output" "$output"
fi

# Test -b (batch) argument.

command="$prolog -b"
output=`eval $command`

if test "$output" = ""
then
	error "$command" "ALS Prolog..." "$output"
fi

# Test -q and -b together.

command="$prolog -q -b"
output=`eval $command`

if test "$output" != ""
then
	error "$command" "" "$output"
fi


# test success/fail/exception return values for -b -g options

cl_test "$prolog -q -b -g true" 0
cl_test "$prolog -q -b -g halt" 0
cl_test "$prolog -q -b -g fail" 1
cl_test "$prolog -q -b -g 'throw(foo)'" 2

# test -g syntax error handling
cl_test "$prolog -q -b -g 'foo(bar.'" 2

# test error handling for invalid -heap and -stack options

cl_test "$prolog -b -heap 2> /dev/null" 2
cl_test "$prolog -b -stack 2> /dev/null" 2
cl_test "$prolog -b -heap -stack 2> /dev/null" 2
cl_test "$prolog -b -stack -heap 2> /dev/null" 2
cl_test "$prolog -b -heap xxx 2> /dev/null" 2
cl_test "$prolog -b -stack xxx 2> /dev/null" 2
cl_test "$prolog -b -heap xxx -stack 1000 2> /dev/null" 2
cl_test "$prolog -b -heap 10000 -stack xxx 2> /dev/null" 2
cl_test "$prolog -b -stack 1000 -heap xxx 2> /dev/null" 2
cl_test "$prolog -b -stack xxx -heap 1000 2> /dev/null" 2

# test error handling of invalid and out-of-range -heap and -stack values

bad_values='2000x 2000. 2000.3 2000.0e+3
            -10000000 -1000000 -100000 -10000 -1000 -100 -10 -1 0
            987654321987654321'
# should fail?           1 2 3 4 5 6 7 8 9 10 5000000 10000000

for i in $bad_values
do
    cl_test "$prolog -b -heap $i 2> /dev/null" 2
done


for i in $bad_values
do
    cl_test "$prolog -b -stack $i 2> /dev/null" 2
done


# This is a little too much...
#for i in $bad_values
#do
#    for j in $bad_values
#    do
#        cl_test "$prolog -b -heap $i -stack $j 2> /dev/null" 2
#    done
#done

if [[ ! -z "$LP64_PARTIAL_TEST" ]]
then
echo "TODO: restore test_command_line.sh" >> /dev/stderr
exit
fi

# test the correct functioning of -heap and -stack

cl_test "$prolog -heap 1000 -b -q -g 'statistics([_,_,heap(_,_,_,_,1024000),_])'" 0
cl_test "$prolog -heap 2000 -b -q -g 'statistics([_,_,heap(_,_,_,_,2048000),_])'" 0
cl_test "$prolog -stack 1000 -b -q -g 'statistics([_,stack(_,_,1024000),_,_])'" 0
cl_test "$prolog -stack 2000 -b -q -g 'statistics([_,stack(_,_,2048000),_,_])'" 0
cl_test "$prolog -heap 2000 -stack 1000 -b -q -g 'statistics([_,stack(_,_,1024000),heap(_,_,_,_,2048000),_])'" 0
cl_test "$prolog -heap 1000 -stack 2000 -b -q -g 'statistics([_,stack(_,_,2048000),heap(_,_,_,_,1024000),_])'" 0

# test the correct functioning of duplicate -heap and -stack

cl_test "$prolog -heap 1000 -heap 2000 -b -q -g 'statistics([_,_,heap(_,_,_,_,2048000),_])'" 0
cl_test "$prolog -heap 2000 -heap 1000 -b -q -g 'statistics([_,_,heap(_,_,_,_,1024000),_])'" 0
cl_test "$prolog -stack 2000 -stack 1000 -b -q -g 'statistics([_,stack(_,_,1024000),_,_])'" 0
cl_test "$prolog -stack 1000 -stack 2000 -b -q -g 'statistics([_,stack(_,_,2048000),_,_])'" 0

# Test empty environment

if [[ ! $(uname) =~ '_NT' ]]
then
cl_test "env -i $prolog -q -b -g true" 0
fi

# test error handling of invalid ALS_OPTIONS
# Currently they don't fail, but should:

if false
then
opt_test "xxx" "$prolog -b 2> /dev/null" 2
opt_test "xxx,xxx" "$prolog -b 2> /dev/null" 2
opt_test "heap_size:" "$prolog -b 2> /dev/null" 2
opt_test "stack_size:" "$prolog -b 2> /dev/null" 2
opt_test "heap_size:,stack_size:" "$prolog -b 2> /dev/null" 2
opt_test "stack_size:,heap_size:" "$prolog -b 2> /dev/null" 2
opt_test "heap_size:xxx" "$prolog -b 2> /dev/null" 2
opt_test "stack_size:xxx" "$prolog -b 2> /dev/null" 2
opt_test "xxx,stack_size:1000" "$prolog -b 2> /dev/null" 2
opt_test "heap_size:xxx,stack_size:1000" "$prolog -b 2> /dev/null" 2
opt_test "heap_size:10000,xxx" "$prolog -b 2> /dev/null" 2
opt_test "heap_size:10000,stack_size:xxx" "$prolog -b 2> /dev/null" 2
opt_test "stack_size:1000,xxx" "$prolog -b 2> /dev/null" 2
opt_test "stack_size:1000,heap_size:xxx" "$prolog -b 2> /dev/null" 2
opt_test "xxx,heap_size:10000" "$prolog -b 2> /dev/null" 2
opt_test "stack_size:xxx,heap_size:10000" "$prolog -b 2> /dev/null" 2
opt_test "xxx,heap_size:10000,stack_size:1000" "$prolog -b 2> /dev/null" 2
opt_test "heap_size:10000,stack_size:1000,xxx" "$prolog -b 2> /dev/null" 2
fi

# test error handling of out-of-range ALS_OPTIONS
# Currently they don't fail, but should:

if false
then
for i in $bad_values
do
    opt_test "heap_size:$i" "$prolog -b 2> /dev/null" 2
done


for i in $bad_values
do
    opt_test "stack_size:$i" "$prolog -b 2> /dev/null" 2
done
fi

# Too much...
#for i in $bad_values
#do
#    for j in $bad_values
#    do
#        opt_test "heap_size:$i,stack_size:$j" "$prolog -b 2> /dev/null" 2
#    done
#done

# Check the correct functioning of ALS_OPTIONS 

opt_test "heap_size:1000" "$prolog -b -q -g 'statistics([_,_,heap(_,_,_,_,1024000),_])'" 0
opt_test "heap_size:2000" "$prolog -b -q -g 'statistics([_,_,heap(_,_,_,_,2048000),_])'" 0
opt_test "stack_size:1000" "$prolog -b -q -g 'statistics([_,stack(_,_,1024000),_,_])'" 0
opt_test "stack_size:2000" "$prolog -b -q -g 'statistics([_,stack(_,_,2048000),_,_])'" 0
opt_test "heap_size:2000,stack_size:1000" "$prolog -b -q -g 'statistics([_,stack(_,_,1024000),heap(_,_,_,_,2048000),_])'" 0
opt_test "heap_size:1000,stack_size:2000" "$prolog -b -q -g 'statistics([_,stack(_,_,2048000),heap(_,_,_,_,1024000),_])'" 0

# Spacing causes error:
#opt_test "  heap_size:  1000  ,  stack_size:  2000  " "$prolog -b -q -g 'statistics([_,stack(_,_,2048000),heap(_,_,_,_,1024000),_])'" 0

# Check the correct functioning of duplicate ALS_OPTIONS

opt_test "heap_size:1000,heap_size:2000" "$prolog -b -q -g 'statistics([_,_,heap(_,_,_,_,2048000),_])'" 0
opt_test "heap_size:2000,heap_size:1000" "$prolog -b -q -g 'statistics([_,_,heap(_,_,_,_,1024000),_])'" 0
opt_test "stack_size:1000,stack_size:2000" "$prolog -b -q -g 'statistics([_,stack(_,_,2048000),_,_])'" 0
opt_test "stack_size:2000,stack_size:1000" "$prolog -b -q -g 'statistics([_,stack(_,_,1024000),_,_])'" 0

if test $error_count -ne 0
then
    echo "$error_count Error(s) Found in Command Line Tests"
    exit 1
else
    echo "Finished Command Line Tests"
    exit 0
fi
