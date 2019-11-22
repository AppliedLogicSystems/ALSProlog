---
title: 'catch/3'
iso: catch
predicates:
- {sig: 'catch/3', desc: 'execute a goal, specifying an exception handler'}
- {sig: 'throw/1', desc: 'give control to an exception handler'}
---

## FORMS
```
catch(Goal, Pattern, ExceptionGoal)

throw(Reason)
```
## DESCRIPTION

`catch/3` provides a facility for catching errors or exceptions and handling them gracefully. Execution of `catch/3` will cause the goal `Goal` to be executed. If no errors or throws occur during the execution of `Goal`, then `catch/3` behaves just as if [`call/1`](call.html) were called with `Goal`. `Goal` may succeed thus giving control to portions of the program outside the scope of the catch. Failures in these portions of the program may cause re-execution (via backtracking) of goals in the scope of the catch of which `Goal` is the ancestor.

If a goal of the form `throw(Reason)` is encountered during the execution of `Goal` or in any subsequent re-execution of `Goal`, then the goal `ExceptionGoal` of the `catch` with the innermost scope capable of unifying `Reason` and `Pattern` together will be executed with this unification intact. Once started, the execution of the goal `ExceptionGoal` behaves just like the execution of any other goal. The goal `ExceptionGoal` is outside of the scope of the catch which initiated the execution of `ExceptionGoal`, so any throws encountered during the execution of `ExceptionGoal` will be handled by `catch`es which are ancestors of the `catch` which initiated the execution of `ExceptionGoal`. This means that a handler may catch some fairly general pattern, deal with some aspects which it is prepared for, but throw on back to some earlier handler for those aspects for which it is not prepared.

Many of the builtins will cause an error if the types of the arguments to the builtin are wrong. There are other reasons for errors such as exhausting a certain resource. Whatever the cause, an error occurring during the execution of a goal causes `throw/1` to be executed. The effect is as if the goal which caused the error where replaced by a goal `throw(error(ErrorTerm, ErrorInfo))` where `ErrorTerm` and `ErrorInfo` supply information about the error. `ErrorTerm` is mandated by ISO Prolog Standard. `ErrorInfo` is an implementation defined (and therefore specific) term which may or may not provide additional information about the error.

The ISO Prolog Standard specifies that ErrorTerm be one of the following forms:

- `instantiation_error` -- An argument or one of its components is a variable.

- `type_error(ValidType, Culprit)` -- An argument or one of its components is of the incorrect type. `ValidType` may be any one of the following atoms : `atom, body, callable, character, compound, constant, integer, list, number, variable`. `Culprit` is the argument or component which was of the incorrect type.

- `domain_error(ValidDomain, Culprit)` -- The base type of an argument is correct, but the value is outside the domain for which the predicate is defined. The ISO Prolog Standard states that ValidDomain may be any one of the following atoms: `character_code_list, character_list, close_option, flag_value, io_mode, not_less_than_zero, operator_priority, operator_specifier, prolog_flag, read_option, source_sink, stream_or_alias, stream_option, stream_position, write_option`. Moreover, ALS Prolog allows `ValidDomain` to take on these additional values : `depth_computation, line_length, positive_integer`. `Culprit` is the argument which caused the error.

- `existence_error(ObjectType, Culprit)` -- An operation is attempted on a certain type of object specified by `ObjectType` which does not exist. `Culprit` is the nonexistent object on which the operation was attempted. `ObjectType` may take on the following values: `operator, past_end_of_stream, procedure, static_procedure, source_sink, stream`.

- `permission_error(Operation, ObjectType, Culprit)` -- `Operation` is an operation not permitted on object type `ObjectType`. `Culprit` is the object on which the error occurred. `ObjectType` is an atom taking on values as described above for `existence_error`. `Operation` may be one of the following atoms : `access_clause, create, input, modify, open, output, reposition`.

- `representation_error(Flag)` -- The implementation defined limit indicated by `Flag` has been breached. `Flag` may be one of the following atoms : `character, character_code, exceeded_max_arity, flag`.

- `calculation_error(CalcFlag)` -- An arithmetic operation resulted in an exceptional value as indicated by the atom `CalcFlag`. `CalcFlag` may take on the following values : `overflow, underflow, zero_divide, undefined`.

- `resource_error(Resource)` -- There are insufficient resources to complete execution. The type of resource exhausted is indicated by the implementation-defined term `Resource`.

- `syntax_error` -- A sequence of characters being read by [`read_term/4`](read.html) cannot be parsed with the current operator definitions. The reason for the syntax error (in ALS Prolog) is given in the implementation-defined `ErrorInfo` (see below).

- `system_error` -- Other sorts of errors. These will commonly be operating system-related errors such as being unable to complete a write operation due to the disk being full. Additional details about this type of error might be found in the implementation-defined term `ErrorInfo`.

In ALS Prolog, `ErrorInfo` is a list providing additional information where the ISO Prolog Standard mandated term `ErrorTerm` falls short. The terms which may be on this list take the following forms:

- `M:G` -- The predicate in which the error occurred was in module `M` on goal `G`. Due to the compiled nature of ALS Prolog, it is not always possible to obtain all of the arguments to the goal `G`. Those which could not be obtained are indicated as such by the atom ' ? ' . For this reason, the form `M:G` should be used for informational purposes only.

- `errno(ErrNo)` -- This form is used to further elaborate on the reason that a system_error occurred. The `errno/1` form indicates that a system call failed. The value of `ErrNo` is an integer which indicates the nature of the system error. The values that `ErrNo` take on may vary from system to system. ALS is looking at a symbolic way of providing this information.

- `syntax(Context, ErrorMessage, LineNumber, Stream)` -- This form is used to provide additional information about system errors. `Context` is an atom providing some information about the context in which the error occurred. `ErrorMessage` is an atom providing the text of the message for the error. `LineNumber` is the number of the line near which the syntax error occurred. `Stream` is the stream which was being read when the syntax error occurred.


## EXAMPLES

Attempt to open a non-existent file.

```
?- open(wombat,read,S).
Error: Operation attempted on object of type source_sink which does not exist.
Operation attempted on: wombat.
- Goal:          sio:open(wombat,read,_A,?)
- Throw pattern: error(existence_error(source_sink,wombat),
                     [sio:open(wombat,read,_A,?)])
```
Attempt to open a file when the file argument is an uninstantiated variable:
```
?- open(_, read, S).
Error: Instantiation error.
- Goal:          sio:open(_A,read,_B,[type(text)])
- Throw pattern: error(instantiation_error,[sio:open(_A,read,_B,*)])
```
Define a procedure `integer_list/2` to illustrate a use of `throw/1`.  Note that `il/2` builds the list and throws the result back at the appropriate time.
```
?- reconsult(user).
integer_list(N,List)
    :-
    catch(il(N,[]),int_list(List),true),
    !.

il(0,L)
    :-
    throw(int_list(L)).
il(N,L)
    :-
    NN is N-1,
    il(NN,[N|L]).
^D
```

```
?- integer_list(8,L).
L=[1,2,3,4,5,6,7,8]
yes.
```
## ERRORS

`Goal` is a variable

---- &gt; `instantiation_error`.

`Goal` is not a callable term

---- &gt; `type_error(callable, Goal)`. [not yet implemented ]

`Reason` does not unify with `Pattern` in any call of `catch/3`

---- &gt; `system_error`. [not yet implemented ]


## NOTES

In the present implementation of ALS Prolog, `catch/3` leaves a choicepoint which is used to restore the scope of the catch when backtracked into. This choicepoint remains around even for determinate goals which are called from catch. Thus when catch succeeds, you should assume that a choicepoint has been created. If the program should be determinate, a cut should be placed immediately after the catch. It is expected that at some point in the future, this unfortunate aspect of ALS Prolog will be fixed, thus obviating the need for an explicit cut.

If `throw/1` is called with Reason instantiated to a pattern which does not match Pattern in any call of `catch/3`, control will return to the program which started the Prolog environment. This usually means that Prolog silently exits to an operating system shell. When using the development environment, however, the Prolog shell establishes a handler for catching uncaught throws or errors thus avoiding this unceremonious exit from the Prolog system. It is occasionally possible, particularly with resource errors, to end up in this last chance handler only to have another error occur in attempting to handle the error. Since no handler exists to handle this error, control returns to the operating system often with no indication of what went wrong.

