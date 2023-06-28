/*============================================================*
 |		julian.pro
 |	Copyright (c) 1999-2019 Applied Logic Systems, Inc.
 |		
 |	Author: Ken Bowen
 |
 |	Includes:	
 |	Basic Julian calendar arithmetic is adapted from: 
 |		Julian.py (from the Python home site):
 |		Julian Dates, Date Arithmetic, and Time Zones
 |  	by Ted Horst <ted_horst@il.us.swissbank.com>
 |  	README file assembled from email by:
 |		Keith Waclena <k-waclena@uchicago.edu>
 |		http://www.lib.uchicago.edu/keith/
 |
 | EPOCH = 2440587L
 | SECSADAY = 86400L
 | HOURSADAY = 24L
 | MINUTESADAY = 1440L
 | SECSAHOUR = 3600L
 | SECSAMINUTE = 60L
 | MINUTESAHOUR = 60L
 |
 *============================================================*/

module builtins.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% JULIAN CALENDAR
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export julianDay/4.
julianDay( Year, Month, Day, JD)
	:-
	B = 0,
	(Month > 12 ->
		Year1 is Year + Month // 12,
		Month1 is Month rem 12
		;
		(Month < 1 ->
			Month1x is - Month,
			Year1 is  Year - (Month // 12) - 1,
			Month1 is 12 - Month rem 12
		;
		Month1 = Month, Year1 = Year
		)
	),
	( Year1 > 0 -> YearCorr = 0 ; YearCorr = 3 ),
	( Month1 < 3 ->
		Year2 is Year1 - 1,
		Month2 is Month1 + 12
		; 
		Month2 = Month1, Year2 = Year1
	),
	( (Year2*10000 + Month2*100 + Day) > 15821014 ->
		B1 is 2 - Year2 // 100 + Year2 // 400
		;
		B1 = B
	),
	JD is (1461 * Year2 -  YearCorr)//4  + 
			306001 *(Month2 + 1 )//10000 + Day + 1720994 + B1.

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export calendarDayForJulian/4.
calendarDayForJulian(Julian, Year, Month, Day)
	:-
	( Julian < 2299160 ->
		B is Julian + 1525
		;
		Alpha is (4 * Julian - 7468861)//146097,
		B is Julian + 1526 + Alpha - Alpha//4
	),
	C is (20 * B - 2442)//7305,
	D is 1461 * C // 4,
	E is 10000 * (B - D) // 306001,
	Day is floor(B - D - 306001 * E // 10000),
	( E < 14 ->
		Month is floor(E - 1)
		;
		Month is floor(E - 13)
	),
	(Month > 2 ->
		Year is  C - 4716
		;
		Year is  C - 4715
	).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export leap_year/1.
leap_year(YY)
	:-
		% if {([expr $YY % 4] == 0) } {
	0 is YY rem 4,
	!,
	((0 is YY rem 100 -> 
		(0 is YY rem 400) ->
			true
			;
			fail
		)
		;
		true
	).

month_days_base(0, 0).
month_days_base(1, 31).
month_days_base(2, 28).
month_days_base(3, 31).
month_days_base(4, 30).
month_days_base(5, 31).
month_days_base(6, 30).
month_days_base(7, 31).
month_days_base(8, 31).
month_days_base(9, 30).
month_days_base(10, 31).
month_days_base(11, 30).
month_days_base(12, 31).

month_days(YY, 2, 29)
	:-
	leap_year(YY),
	!.
month_days(_, MM, MDD)
	:-
	month_days_base(MM, MDD).

	% returns the number of the day in a year:
num_day_in_year(YY, MM, DD, N)
	:-
	MM0 is MM - 1,
	sum_month_days(MM0, YY, SMM0),
	N is SMM0 + DD.

calc_sum_month_days(0, _, 0).
calc_sum_month_days(MM, YY, SMM)
	:-
	MM1 is MM-1,
	calc_sum_month_days(MM1, YY, SMM1),
	month_days(YY, MM, MMD),
	SMM is SMM1 + MMD,
write_clause(sum_month_days(MM,SMM)).

	%% Non-leap year:
sum_month_days(1,31).
sum_month_days(2,59).
sum_month_days(3,90).
sum_month_days(4,120).
sum_month_days(5,151).
sum_month_days(6,181).
sum_month_days(7,212).
sum_month_days(8,243).
sum_month_days(9,273).
sum_month_days(10,304).
sum_month_days(11,334).
sum_month_days(12,365).

sum_month_days(MM, YY, SMM)
	:-
	leap_year(YY),
	!,
	sum_month_days(MM, SMMNL),
	(MM >= 2 -> SMM is SMMNL + 1 ; SMM = 31).

sum_month_days(MM, YY, SMM)
	:-
	sum_month_days(MM, SMM).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% EPOCH-SECONDS ARITHMETIC
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% EPOCH = 2440587 in julian
	% SecsPerDay = 86400
/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export today_epoch_secs/1.
export epoch_secs/4.
export epoch_secs/5.
export epoch_secs/7.
export epoch_secs/8.
export epoch_secs/9.

today_epoch_secs(ESS)
	:-
	date(YY/MM/DD),
	epoch_secs(YY, MM, DD, ESS).


epoch_secs(YY, MM, DD, ESS)
	:-
	julianDay( YY, MM, DD, JD),
	ESS is (JD - 2440587) * 86400.

epoch_secs(YY, MM, DD, TZC, ESS)
	:-
	julianDay( YY, MM, DD, JD),
	ESS is (JD-2440587)*86400 - TZC*3600.

epoch_secs(YY, MM, DD, HH, MnMn, SS, ESS)
	:-
	epoch_secs(YY, MM, DD, ESS0),
	ESS is floor(ESS0 + HH*3600 + MnMn*60 + SS).

epoch_secs(YY, MM, DD, HH, MnMn, SS, TZC, ESS)
	:-
	epoch_secs(YY, MM, DD, ESS0),
	ESS is floor(ESS0 + (HH-TZC)*3600 + MnMn*60 + SS).

epoch_secs(YY, MM, DD, HH, MnMn, SS, TZC, ESS0, ESS)
	:-
	epoch_secs(YY, MM, DD, ESS0),
	ESS is floor(ESS0 + (HH-TZC)*3600 + MnMn*60 + SS).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
	%% convenience:
export ess2dt/1.
export ess2dt/2.
ess2dt(ESS)
	:-
	ess_to_datetime(ESS,YY,MM,DD,HH,MnMn,SS),
	printf('%t/%t/%t   %t:%t:%t \n',[MM,DD,YY,HH,MnMn,SS]).

ess2dt(ESS, TZC)
	:-
	ess_to_datetime(ESS,YY,MM,DD,HH,MnMn,SS,TZC),
	printf('%t/%t/%t   %t:%t:%t \n',[MM,DD,YY,HH,MnMn,SS]).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export ess_to_datetime/7.
ess_to_datetime(ESS,YY,MM,DD,HH,MnMn,SS)
	:-
	TZC=0,
	ess_to_datetime(ESS,YY,MM,DD,HH,MnMn,SS,TZC).

export ess_to_datetime/8.
ess_to_datetime(ESS,YY,MM,DD,HH,MnMn,SS,TZC)
	:-
		%	ESS is (JD - 2440587) * 86400.
	ESSAdj is ESS + TZC*3600,
	JD is 2440587 + (ESSAdj//86400),
	calendarDayForJulian(JD, YY, MM, DD),
	Rem is ESSAdj rem 86400,
	HH is (Rem//3600),
	Rem2 is Rem rem 3600,
	MnMn is Rem2//60,
	SS is Rem2 rem 60.


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DAY OF WEEK CALCULATION
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
/*
	Days: enum (sun,mon,....sat) = (0,l,2,...,6)
 */
export day_of_week/4.
day_of_week(YY,MM,DD, WD)
	:-
		% set M [expr 1 + (($MM - 2) %12)]
	M is 1 + (MM- 3) mod 12,
		% set C [expr int($YY / 100)]
	C is floor(YY // 100),
		% set Z [expr $YY - $C*100 ]
	Z is (YY - C*100),
		% set L [leap_year $YY]
	(leap_year(YY) -> L = 1 ; L = 0),

		% set day [expr ($DD + int(2.6*$M -0.2) + $Z + int($Z/4) \
		% 	+ int($C/4) - 2*$C - (1+$L)*int($M/11)) % 7]

	WD is (DD + floor(2.6*M - 0.2) + Z + floor(Z/4) 
		+ floor(C/4) - 2*C - (1+L)*floor(M/11) ) mod 7.

export ess_day_of_week/2.
ess_day_of_week(StDtESS, WD)
	:-
	JD is 2440587 + (StDtESS//86400),
	calendarDayForJulian(JD, YY, MM, DD),
	day_of_week(YY,MM,DD, WD).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export next_business_day_ess/2.
next_business_day_ess(StDtESS, NxtStDtESS)
	:-
	ess_day_of_week(StDtESS, WD),
	fin_next_business_day_ess(WD, StDtESS, NxtStDtESS).

	% Friday --> Monday
fin_next_business_day_ess(5, StDtESS, NxtStDtESS)
	:-!,
	NxtStDtESS is StDtESS + 3*86400.
	% Saturday --> Monday
fin_next_business_day_ess(6, StDtESS, NxtStDtESS)
	:-!,
	NxtStDtESS is StDtESS + 2*86400.
	% All others:
fin_next_business_day_ess(_, StDtESS, NxtStDtESS)
	:-!,
	NxtStDtESS is StDtESS + 86400.


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DAYLIGHT SAVINGS INFO
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*--------------------------------------------------------------------------*
 	DST info for the U.S. used to determine the changeover dates 
	back to 1990 -- could be used (together with details about how the 
	dates went during the oil embargoes) back into the 60's.  
	At present, IN GENERAL, in the US, in those places that observe DST,
	DST begins at 2am on the first Sunday in April, and DST ends at 2am 
	on the last Sunday in October.

		http://www.webexhibits.org/daylightsaving/e.html

	The Uniform Time Act of 1966 (15 U.S. Code Section 260a) which was 
	signed into Public Law 89-387 on April 13, 1966, by President
	Lyndon Johnson, created Daylight Saving Time to begin on the last 
	Sunday of April and to end on the last Sunday of October.

	Any State that wanted to be exempt from Daylight Saving Time could do 
	so by passing a State law. 

	The Uniform Time Act of 1966 established a system of uniform (within 
	each time zone) Daylight Saving Time throughout the U.S. and its 
	possessions, exempting only those states in which the legislatures 
	voted to keep the entire state on standard time.  

	In 1972, Congress revised the law to provide that, if a State was in two 
	or more time zones, the State could exempt the part of the State that was 
	in one time zone while providing that the part of the State in a different 
	time zone would observe Daylight Saving Time.  

	The Federal law was amended in 1986 to begin Daylight Saving Time on the 
	first Sunday in April.  

	Under legislation enacted in 1986, Daylight Saving Time in the USA
		- begins at 2 a.m.  on the first Sunday of April 
			and 
		- ends at 2 a.m.  on the last Sunday of October 

	In most of the countries of western Europe, including the countries 
	that are members of the EEC, Daylight Saving Time, DST:
		- begins at 1 a.m.  GMT on the last Sunday of March 
			and 
		- ends at 1 a.m.  GMT on the last Sunday of October 

	Observance of Daylight Saving Time elsewhere in the world is highly 
	variable.  

	I used the above with the perpetual calendar at

			http://www.harold.com/toys/cal.cgi?year=2000

	to cacluate the following changeovers.

	=======================================
	Not yet incorporated (from Wikipedia:)
	https://en.wikipedia.org/wiki/Daylight_saving_time_in_the_United_States#2005–2009:_Second_extension

	By the Energy Policy Act of 2005, daylight saving time (DST) was extended in the United States 
	beginning in 2007.[10] As from that year, DST begins on the second Sunday of March and ends on 
	the first Sunday of November. 
	=======================================
 *--------------------------------------------------------------------------*/


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DAYLIGHT SAVINGS DATES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dst_ahead(2025,3,9).
dst_back(2025,11,2).

dst_ahead(2024,3,10).
dst_back(2024,11,3).

dst_ahead(2023,3,12).
dst_back(2023,11,5).

dst_ahead(2022,3,13).
dst_back(2022,11,6).

dst_ahead(2021,3,14).
dst_back(2021,11,7).

dst_ahead(2020,3,8).
dst_back(2020,11,1).

dst_ahead(2019,3,10).
dst_back(2019,11,3).

dst_ahead(2018,3,11).
dst_back(2018,11,4).

dst_ahead(2017,3,12).
dst_back(2017,11,5).

dst_ahead(2016,3,13).
dst_back(2016,11,6).

dst_ahead(2015,3,8).
dst_back(2015,11,1).

dst_ahead(2014,3,9).
dst_back(2014,11,2).

dst_ahead(2013,3,10).
dst_back(2013,11,3).

dst_ahead(2012,3,11).
dst_back(2012,11,4).

dst_ahead(2011,3,13).
dst_back(2011,11,6).

dst_ahead(2010,3,14).
dst_back(2010,11,7).

dst_ahead(2009,3,8).
dst_back(2009,11,1).

dst_ahead(2008,3,9).
dst_back(2008,11,2).

dst_ahead(2007,3,11).
dst_back(2007,11,4).

dst_ahead(2006,4,2).
dst_back(2006,10,29).

dst_ahead(2005,4,3).
dst_back(2005,10,30).

dst_ahead(2004,4,4).
dst_back(2004,10,31).

dst_ahead(2003,4,6).
dst_back(2003,10,26).

dst_ahead(2002,4,7).
dst_back(2002,10,27).

dst_ahead(2001,4,1).
dst_back(2001,10,28).

dst_ahead(2000,4,2).
dst_back(2000,10,29).

dst_ahead(1999,4,4).
dst_back(1999,10,31).

dst_ahead(1998,4,5).
dst_back(1998,10,25).

dst_ahead(1997,4,6).
dst_back(1997,10,26).

dst_ahead(1996,4,7).
dst_back(1996,10,27).

dst_ahead(1995,4,2).
dst_back(1995,10,29).

dst_ahead(1994,4,3).
dst_back(1994,10,23).

dst_ahead(1993,4,4).
dst_back(1993,10,24).

dst_ahead(1992,4,5).
dst_back(1992,10,25).

dst_ahead(1991,4,7).
dst_back(1991,10,28).

dst_ahead(1990,4,1).
dst_back(1990,10,28).

export calc_dst_type/2.
calc_dst_type(ESS, DSTTp)
	:-
	ess_to_datetime(ESS,YY,MM,DD,_,_,_,0),
	datetime:dst_ahead(YY,MA,DA),
	datetime:dst_back(YY,MB,DB),
	dst_type(MM,MA,MB,DD,DA,DB,DSTTp).

export calc_end_of_day/2.
calc_end_of_day(ESS, EndESS)
	:-
	ess_to_datetime(ESS,YY,MM,DD,_,_,_,0),
	datetime:dst_ahead(YY,MA,DA),
	datetime:dst_back(YY,MB,DB),
	dst_type(MM,MA,MB,DD,DA,DB,DSTTp),
	(DSTTp = st ->
		epoch_secs(YY, MM, DD, 9, 30, 0, EndESS)
		;
		epoch_secs(YY, MM, DD, 10, 30, 0, EndESS)
	).

dst_type(MM,MA,MB,_,_,_,st)
	:-
	MM < MA, !.
dst_type(MM,MA,MB,_,_,_,st)
	:-
	MM > MB, !.

	%% Know MA =< MM =< MB
dst_type(MA,MA,_,DD,DA,_,st)
	:-
	DD < DA, !.
dst_type(MA,MA,_,DD,DA,_,dst)
	:-!.

	%% Know MA < MM =< MB
dst_type(MB,_,MB,DD,_,DB,st)
	:-
	DD > DB, !.

dst_type(_,_,_,_,_,_,dst).

endmod.





