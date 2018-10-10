—-
title: 'current_prolog_flag/2'
predicates:
 - 'current_prolog_flag/2' : retrieve value(s) of prolog flag(s)
 - 'set_prolog_flag/2' : set value of a Prolog flag
—-
`current_prolog_flag/2` `—` retrieve value(s) of prolog flag(s)

`set_prolog_flag/2` `—` set value of a Prolog flag


## FORMS

current_prolog_flag(Flag, Value)

set_prolog_flag(Flag, Value)


## DESCRIPTION

current_prolog_flag/2 is re-executable. It unifies Flag and Value with the current instantiations of the flag/value pairs supported by ALS Prolog. If Flag and Value are appropriately instantiated,

set_prolog_flag(Flag, Value)

changes the present value associated with Flag to become Value.

The flags supported by ALS-Prolog are :

ISO Standard Flags

( ISO Standard references are given in parentheses) :

bounded(7.11.1.1)

Values : true, false

Default = true

Changeable : no

max_integer(7.11.1.2)

Default = Value

Changeable : no

min_integer(7.11.1.3)

Default = Value

Changeable : no

integer_rounding_function(7.11.1.4)

Values : down toward_zero

Default = toward_zero

Changeable : no

char_conversion(7.11.2.1)

Values : on off

Default = off

Changeable : yes

debug(7.11.2.2)

Values : off on

Default = off

Changeable : yes

max_arity(7.11.2.3)

Default = Value(= max_integer)

Changeable : no

unknown(7.11.2.4)

Values : error fail warning break

Default = error

Changeable : yes

Describes the course of action to take when an undefined predicate is called. The associated value(action) may be one of the following :

error -

force an existence error when an undefined predicate is called.

fail -

fail when an undefined predicate is called.

warning -

warn the user when an undefined predicate is called.

break -

enter the break handler when an undefined predicate is called.

double_quotes(7.11.2.5)

Values : chars codes atom

Default = codes

Changeable : yes

ALS Extension Flags :

undefined_predicate synonymous with : unknown

windows_system

Values : nowins tcltk

Default = Value

Changeable : no

Takes value &quot; nowins &quot; if no windowing system extension is present; otherwise is the identifier of the windowing extension(at present, only " tcltk ") .

anonymous_solutions(reporting)

Values : true false

Default = false

Changeable : yes

syntax_errors(behavior on syntax errors)

Values : fail error quiet dec10

Default = error

Changeable : yes

obp_location(location of generated obp files)

Values :
gic
gis
giac
gias

Default = gias

Changeable : yes

freeze(whether freeze is available)

Values : true false

Default = Value

Changeable : no

constraints(whether constraints are available)

Values : true false

Default = Value

Changeable : no

iters_max_exceeded(only when constraints = true)

Values : succeed fail warning exception

Default = succeed

Changeable : yes

For
CLP(BNR), the iters_max_exceeded flag controls the behavior when then maximum number of constraint narrowing iterations is exceeded, as follows :


— succeed(leaves network in place)

— fail(quiet; backtracking resets net)

— warning(fails &amp; issues warning; backtracking resets net)

— exception(backtracking resets net)

Settings for prolog flags can be placed in the
startup file(alspro.pro or .alspro) .


## ERRORS

Flag is a variable(set_prolog_flag
only

)

— — -- -- &gt; instantiation_error.

Value is a variable(set_prolog_flag
only

)

— — -- -- &gt; instantiation_error.

Flag is neither a variable nor an atom

— — -- -- &gt; type_error(atom, Flag) .

Value is inappropriate for Flag

— — -- -- &gt; domain_error(flag_value, Flag + Value)


## NOTES

? - current_prolog_flag(unknown, V) .

V = error

? - set_prolog_flag(undefined_predicate, fail) .

