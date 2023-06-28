---
title: 'command_line/1'
predicates:
- {sig: 'command_line/1', desc: 'provides access to the prolog system start-up command line'}
---

## FORMS
```
command_line(SWITCHES)
```
## DESCRIPTION

When ALS Prolog is started from an operating system shell, the command line can be divided into system-specific and application-specific portions by use of the -p or -P (for Prolog) switch. All command line parameters to the left of the -p switch are treated as ALS Prolog system switches, while those to the right of the -p switch are treated as application switches.

To make the latter available to Prolog applications, when ALS Prolog is initialized a list, `SWITCHES`, of atoms and UIAs representing the items to the right of the -p switch is created, and
```
command_line(SWITCHES)
```
is asserted in module builtins. This assertion is always made, even when -p is not used, in which case the argument of `command_line/1` is the empty list.

The -P switch will force the name of the invoking program to be the argument(usually alspro) .. the switch is useful for developing applications which will eventually be packaged (via [`save_image/2`](save_image.html)). Packaged applications will place the entire command line into `command_line/1`. In particular, the first element in the list obtained from `command_line/1` in a packaged application will be the name of the application.


## EXAMPLES

$ alspro -p -k fast -s initstate foo

ALS-Prolog Version 2.01

Copyright(c) 1987-94 Applied Logic Systems, Inc.


? - command_line(SW) .


SW = [ ' -k ', fast, ' -s ', initstate, foo ]


yes.


$ alspro -P -k fast -s initstate foo

ALS-Prolog Version 2.01

Copyright(c) 1987-94 Applied Logic Systems, Inc.


? - command_line(SW) .


SW = [alspro, ' -k ', fast, ' -s ', initstate, foo ]


yes.

