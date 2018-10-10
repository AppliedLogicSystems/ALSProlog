---
title: 'getenv/2'
predicates:
 - 'getenv/2' : gets the value of the given os environment variable
---
`getenv/2` `--` gets the value of the given os environment variable


## FORMS

getenv(EnvVar, EnvVal)


## DESCRIPTION

EnvVar, which must be instantiated, can be a symbol, an UIA, or a list of ASCII characters denoting an operating system environment or shell variable. The value of this external variable is accessed and the corresponding list of ASCII characters is unified with EnvVal. If EnvVar is not defined in the os environment, getenv/2 fails.


## EXAMPLES

? - getenv(' TERM ', Term) .


Term = xterm


yes.

? - getenv(' PATH ', Path) .


Path = ' . : / usr/local/bin : / usr/bin/X11 : / usr/bin : / bin : / usr/ccs/bin '


yes.

? - getenv(' FOOBAR ', Foobar) .


no.

