—-
title: 'date_time/2'
predicates:
 - 'date_time/2' : gets current date and time
 - 'date/1' : gets current date
 - 'time/1' : gets the current system time
 - 'gm_date_time/2' : gets current Greenwich mean date and time
—-
`date_time/2` `—` gets current date and time

`date/1` `—` gets current date

`time/1` `—` gets the current system time

`gm_date_time/2` `—` gets current Greenwich mean date and time


## FORMS

date_time(Date, Time)

time(Time)

date(Date)

gm_date_time(Date, Time)


## DESCRIPTION

date_time(Date, Time) obtains the current Date and Time from the same call to the OS/system clock. gm_date_time(Date, Time) obtains the current Geenwich UTC Date and Time from the same call to the OS/system clock. date/1 and time/1 are effectively defined by :

date(Date) :- date_time(Date, _) .

time(Time) :- date_time(_, Time) .


## EXAMPLES

? - time(Time) .


Time =(16 : 51 : 49)

yes.

? - date(Date)

Date = 93/11/5


yes.

