—-
title: 'datetime/2(sys)'
predicates:
 - 'datetime/2' : gets the local system date and time
 - 'time/1' : gets the local system time
 - 'date/1' : gets the local date
 - 'gm_datetime/2' : gets the Greenwich mean time(UTC)
—-
`datetime/2` `—` gets the local system date and time

`time/1` `—` gets the local system time

`date/1` `—` gets the local date

`gm_datetime/2` `—` gets the Greenwich mean time(UTC)


## FORMS

datetime(Date, Time)

time(Time)

date(Date)

gm_datetime(Date, Time)


## EXAMPLES

? - time(Time) .


Time =(16 : 51 : 49)

yes.

? - date(Date)

Date = 93/11/5


yes.

