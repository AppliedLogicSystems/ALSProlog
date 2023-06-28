---
title: 'current_prolog_flag/2'
iso: current_prolog_flag
predicates:
- {sig: 'current_prolog_flag/2', desc: 'retrieve value(s) of prolog flag(s)'}
- {sig: 'set_prolog_flag/2', desc: 'set value of a Prolog flag'}
---

## FORMS
```
current_prolog_flag(Flag, Value)

set_prolog_flag(Flag, Value)
```
## DESCRIPTION

`current_prolog_flag/2` is re-executable. It unifies `Flag` and `Value` with the current instantiations of the flag/value pairs supported by ALS Prolog. 

If `Flag` and `Value` are appropriately instantiated,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`set_prolog_flag(Flag, Value)`

changes the present value associated with `Flag` to become `Value`.

The flags supported by ALS-Prolog are :

**ISO Standard Flags**

(ISO Standard references are given in parentheses) :

bounded (7.11.1.1)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: true false
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = true
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

max_integer (7.11.1.2)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = Value
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

min_integer (7.11.1.3)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = Value
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

integer_rounding_function (7.11.1.4)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: down toward_zero
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = toward_zero
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

char_conversion (7.11.2.1)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: on off
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = off
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

debug (7.11.2.2)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: off on
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = off
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

max_arity (7.11.2.3)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = Value (= max_integer)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

unknown (7.11.2.4)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: error fail warning break
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = error
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

Describes the course of action to take when an undefined predicate is called. The associated value (action) may be one of the following:
<br>&nbsp;&nbsp;&nbsp;&nbsp;error - force an existence error when an undefined predicate is called.
<br>&nbsp;&nbsp;&nbsp;&nbsp;fail - fail when an undefined predicate is called.
<br>&nbsp;&nbsp;&nbsp;&nbsp;warning - warn the user when an undefined predicate is called.
<br>&nbsp;&nbsp;&nbsp;&nbsp;break - enter the break handler when an undefined predicate is called.

double_quotes (7.11.2.5)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: chars codes atom
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = codes
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

**ALS Extension Flags**

windows_system
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: nowins tcltk
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = Value
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

Takes value &quot;nowins&quot; if no windowing system extension is present; otherwise is the identifier of the windowing extension (at present, only "tcltk").

anonymous_solutions (reporting)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: true false
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = false
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

syntax_errors (behavior on syntax errors)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: fail error quiet dec10
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = error
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

obp_location (location of generated obp files)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: gic gis giac gias
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = gias
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

The meanings of the obp_location values are:
<br>&nbsp;&nbsp;&nbsp;&nbsp;gic - Generate \*.obp files in current working dir
<br>&nbsp;&nbsp;&nbsp;&nbsp;gis - Generate \*.obp files in same dir as source
<br>&nbsp;&nbsp;&nbsp;&nbsp;giac - Generate \*.obp files in subdir of current working dir 
                    named for architecture (e.g., darwin,win32).
<br>&nbsp;&nbsp;&nbsp;&nbsp;gias - Generate \*.obp files in subdir of source dir named for 
                    architecture (e.g., darwin,win32).


freeze (whether freeze is available)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: true false
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = Value
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

<!--
constraints (whether constraints are available)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: true false
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = Value
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: no

iters_max_exceeded (only when constraints = true)
<br>&nbsp;&nbsp;&nbsp;&nbsp;Values: succeed fail warning exception
<br>&nbsp;&nbsp;&nbsp;&nbsp;Default = succeed
<br>&nbsp;&nbsp;&nbsp;&nbsp;Changeable: yes

For
CLP(BNR), the iters_max_exceeded flag controls the behavior when then maximum number of constraint narrowing iterations is exceeded, as follows:


-- succeed(leaves network in place)

-- fail(quiet; backtracking resets net)

-- warning(fails &amp; issues warning; backtracking resets net)

-- exception(backtracking resets net)
-->

Settings for prolog flags can be placed in the startup file (alspro.pro or .alspro).

## ERRORS

`Flag` is a variable(set_prolog_flag only)

-- -- -- -- &gt; instantiation_error.

`Value` is a variable(set_prolog_flag only)

-- -- -- -- &gt; instantiation_error.

`Flag` is neither a variable nor an atom

-- -- -- -- &gt; type_error(atom, Flag) .

`Value` is inappropriate for Flag

-- -- -- -- &gt; domain_error(flag_value, Flag + Value)

