---
title: 'call/1'
iso: call
predicates:
- {sig: 'call/1', desc: 'calls a goal'}
---

## FORMS

```
call(Goal)

Goal
```

## DESCRIPTION

If `Goal` is instantiated to a structured term or atom which would be acceptable as the body of a clause, the goal `call(Goal)` is executed exactly as if that term appeared textually in place of the expression `call(Goal)`.


## EXAMPLES

The following example illustrates the use of `call/1`; first we consult user to input some clauses (note that
a newline must be input after each line, and that ^D indicates typing control-D to close user):

```
?- [user].
jim :- printf("Hello this is Jim Rockford.\n"),
printf("Please leave your name and number,\n"),
printf("and I'll get back to you.\n").
^D
yes.
```

```
?- call(jim).
Hello this is Jim Rockford.
Please leave your name and number,
and I'll get back to you.
yes.
```

The following example shows how an instantiated variable can be used to run a goal:

```
?- Goal=write(Message), Message='WrongWay!', Goal, nl.
WrongWay!

Goal=write('WrongWay!')
Message='WrongWay!'

yes.
```
Note that the last two lines `Goal=...` and `Message=..` are the Prolog interpreter providing answer values for the variables in the successful query.


## ERRORS

If Goal is an uninstantiated variable or a number, `call/1` will fail.

## SEE ALSO

- [`!/0`](cut.html)
- [`:/2`](colon.html)

- {% include book.md id="clocksin81" sec="6.7" %}
- {% include book.md id="bratko86"   sec="7.2" %}
- {% include book.md id="sterling86" sec="10.4" %}

