---
title: 'halt/0'
iso: halt
predicates:
- {sig: 'halt/0', desc: 'exit ALS Prolog'}
---

## FORMS
```
halt
```
## DESCRIPTION

`halt/0` causes the ALS Prolog system to exit, returning control to the operating system shell. It can either be invoked from the top level of the ALS Prolog shell, or from within a running program.

## EXAMPLES

In this example, `halt/0` is called from the Prolog shell on a Unix C shell system on a machine named 'wizard':

```
?- halt.
wizard%
```
## NOTES

On most systems, typing the end of file characters after the `?-` prompt of the Prolog shell will also cause ALS Prolog to exit to the operating system shell. On Unix, the end of file character is entered by typing `Control-D`. On DOS and Win32, the end of file character is `Control-Z`. On the Mac, either `Control-D` or `Control-Z` can be used as the end of file character; in addition, the `Quit` menu can also be used.

## SEE ALSO

- [`abort/0`](abort.html)

## BUGS

`halt/0` does not close any streams nor does it flush their output buffers.

