---
title: 'datetime/2'
group: Input Output
predicates:
- {sig: 'datetime/2', desc: 'gets the local system date and time'}
- {sig: 'time/1', desc: 'gets the local system time'}
- {sig: 'date/1', desc: 'gets the local date'}
- {sig: 'gm_datetime/2', desc: 'gets the Greenwich mean time(UTC)'}
- {sig: 'date_less/2', desc: 'compares two dates'}
- {sig: 'date_pattern/4', desc: 'describes the pattern term for date display'}
- {sig: 'set_date_pattern/1', desc: 'changes the pattern term for date display'}
- {sig: 'valid_date/[1,3]', desc: 'determines if a date pattern represents a valid date'}
- {sig: 'time_less/2', desc: 'compares two times'}
- {sig: 'datetime_less/2', desc: 'compares two datetimes'}
---

## FORMS
```
datetime(Date, Time)

time(Time)

date(Date)

gm_datetime(Date, Time)

date_less(Date0, Date1)

date_pattern(YY,MM,DD,YY/MM/DD)

set_date_pattern(AA/BB/CC)

valid_date(Date)

valid_date(YY,MM,DD)

time_less(Time0, Time1)

datetime_less(DateTime0, DateTime1)

```
## DESCRIPTION

`date_time(Date, Time)` obtains the current system local `Date` and `Time` from the same call to the OS/system clock. 

`gm_date_time(Date, Time)` obtains the current Greenwich UTC `Date` and `Time` from the same call to the OS/system clock. 

`date/1` and `time/1` are effectively defined by:
```
date(Date) :- date_time(Date, _).

time(Time) :- date_time(_, Time).
```
`date_less(Date0, Date1)` holds if and only if Date0 and Date1 are date terms of the form YY/MM/DD 
and Date0 represents a date earlier than Date1.

`date_pattern/4` is a single fact which establishes the pattern term to be used for displaying dates.  The default value is:
```
date_pattern(YY,MM,DD,YY/MM/DD).
```
`set_date_pattern/1` is used to change the pattern term to be used for displaying dates, that is, to change the fact defining `date_pattern/4`.  It should be passed a term AA/BB/CC representing the desired date display expression, where the values of AA,BB,CC are atoms beginning with `y`, `m`, and `d` in the desired order. [Note that 
`set_date_pattern(m/d/y), set_date_pattern(mm/dd/yy)`, and `set_date_pattern(month/day/year)` would all set the same date pattern display.]

`valid_date(Date)` is true if and only if Date represents a meaningful date, that is, whether the numerical values of `YY,MM,DD` (whatever the date_pattern) are in the correct ranges.  If PP is a pattern made out of Y,M,D and '-' matches the current date_pattern [e.g., PP is M-D-Y], then PP is an acceptable input to `valid_date/1`.

`valid_date/1` is defined in terms of `valid_date/3` by:
```
valid_date(Date)
        :-
        date_pattern(YY,MM,DD,Date),
        valid_date(YY,MM,DD).
```
`time_less(Time0, Time1)` holds if and only if `Time0` and `Time1` are time terms of the form `HH:MM:SS` and  `Time0` represents a time earlier than `Time1`.

`datetime_less(DateTime0, DateTime1)` holds if and only if `DateTime0` and `DateTime1` are time terms of the forms
```
(Date0,Time0), (Date1,Time1)
```
and `DateTime0` precedes or equals `DateTime1`.

## EXAMPLES
```
?- time(T).

T=12:47:57 

yes.
?- date(D).

D=2018/10/20 

yes.
?- datetime(D,T).

D=2018/10/20 
T=12:48:16 

yes.
?- gm_datetime(D,T).

D=2018/10/20 
T=18:48:26 

yes.

?- date_pattern(YY,MM,DD,YY/MM/DD).

YY=YY 
MM=MM 
DD=DD 

yes.
?- set_date_pattern(a/b/c).

no.
?- set_date_pattern(y/m/d).

yes.
?- set_date_pattern(yy/mm/dd).

yes.
?- date_pattern(YY,MM,DD,P).

YY=YY 
MM=MM 
DD=DD 
P=YY/MM/DD 

yes.
?- set_date_pattern(month/day/year).

yes.
?- date_pattern(YY,MM,DD,P).

YY=YY 
MM=MM 
DD=DD 
P=MM/DD/YY 

yes.

?- valid_date(2/29/1916).

yes.

?- valid_date(1-15-2001).

yes.

?- valid_date(2/29/1917).

no.

?- set_date_pattern(year/month/day).

yes.
?- date_pattern(YY,MM,DD,P).

YY=YY 
MM=MM 
DD=DD 
P=YY/MM/DD 

?- time_less(9:22:01, 13:48:53).

yes.

?- time_less(13:48:53, 12:12:14).

no.

?- datetime_less((2000/3/14,9:22:01), (2000/3/14,13:48:53)).

yes.
?- datetime_less((2000/1/14,9:22:01), (2000/3/14,1:48:53)).

yes.

```

