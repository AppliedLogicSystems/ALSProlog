/*==============================================================*
 |      freetim2.pro
 |  Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |      A variant of the free_time example of the
 |      V. 3.0 BNR Prolog User Guide (pp. 142 ff)
 |
 |  Author: Ken Bowen
 *===============================================================*/

executives(['Bob','Carol','Ted','Alice',room]).

    %% free(ExecutiveName, PeriodsList)  where
    %% PeriodsList contains items of the form
    %%      period(Day, StartTime, EndTime)
    %%
    %%  Day : ['Mon', 'Tues', 'Wed', 'Thurs', 'Fri']
    %%
    %%  Times are of the form:  Hours  or Hours:Mins,
    %%  where Hours are integers in the range 0..23
    %%  and Mins are integers in the range 0..59

free('Bob',
    [   period('Mon',   9, 10),
        period('Mon',   13:30, 14:15),
        period('Tues',  11:15, 11:45),
        period('Tues',  15, 17),
        period('Wed',   8, 8:15),
        period('Thurs', 12, 12:30),
        period(Fri,     10, 11:15)
         ] ).

free('Carol',
    [   period('Mon',   10, 10:30),
        period('Mon',   12:30, 13:15),
        period('Tues',  12:15, 13:45),
        period('Tues',  15, 17),
        period('Wed',   9, 10:15),
        period('Thurs', 8, 8:30),
        period('Fri',   10, 11:15)  
          ] ).

free('Ted',
    [   period('Mon',   9, 11:30),
        period('Mon',   12:30, 13:00),
        period('Tues',  9, 11),
        period('Tues',  12, 18),
        period('Wed',   10, 11:15),
        period('Thurs', 8, 10),
        period('Thurs', 20, 21:30),
        period('Fri',   9, 13:15)  
          ] ).

free('Alice',
    [   period('Mon',   10:30, 15),
        period('Tues',  8, 10),
        period('Tues',  14, 16:15),
        period('Wed',   8, 11),
        period('Wed',   14, 15:45),
        period('Thurs', 9:45, 11:15),
        period('Fri',   8, 11:15)  
          ] ).

free(room,
    [   period('Mon',   8, 12),
        period('Tues',  14, 16:15),
        period('Wed',   8, 9),
        period('Thurs', 12, 14),
        period('Fri',   8, 9:30),
        period('Fri',   10:30, 12)  
          ] ).

member(X, [X | _]).
member(X, [_ | T])
    :-
    member(X, T).

    %%
    %% Find a single (= the first) day and time
    %%
find_one_time(CommonDay, CommonTime)
    :-
    execs_free(ListOfFreeTimes),
    CommonTime::real,

    find_common(ListOfFreeTimes, CommonDay, CommonTime),
write(day=CommonDay),write(' -- '),
show_variable(CommonTime),nl,flush_output,
    {0.75 =< delta(CommonTime) }.

    %%
    %% Find and output a (the first) day & time
    %%
find_a_time
    :-
    find_one_time(CommonDay, CommonTime),
    output_time(CommonDay, CommonTime).

    %%
    %% Find and output all days and times
    %%
find_times
    :-
    find_a_time,
    fail.
find_times.

    %%
    %% Return the list of sublists of free periods for each
    %% executive, and the room
    %%
execs_free(L)
    :-
    executives(EL),
    execs_free(EL, L).

    %%
    %% Map the list of executives (& the room) into
    %% the corresponding list of sublists of free periods
    %%
execs_free([], []).
execs_free([E |Es], [FL | FLs])
    :-
    free(E, FL),
    execs_free(Es, FLs).

    /*-----------------------------------------------------------
     | find_common/3
     | find_common(ListOfFreeTimes, CommonDay, CommonTime)
     | find_common(+, -, +)
     |
     | + ListOfFreeTimes is a list of sublists, where each
     |   sublist contains period(Day,Start,End) terms;
     | + CommonDay is an atom (normally uninstantiated)
     | + CommonTime is a real interval, normally with 
     |   indefinite (i.e., maximal) endpoints;
     |
     |  This holds if there exists a choice list
     |      [....,period(CommonDay, Si, Ei), ....]
     |  from ListOfFreeTimes (one choice from each sublist)
     |  such that for each i, CommonTime is inside read(DSi,DEi),
     |  and DSi (resp., DEi) are the decimal forms of times
     |  Si (resp., Ei).
     *----------------------------------------------------------*/

find_common([], CommonDay, CommonTime).

find_common([Frees | ListOfFreeTimes], CommonDay, CommonTime)
    :-
    member(period(CommonDay, Begin, End), Frees),
    time2dec(Begin, DecBegin),
    time2dec(End, DecEnd),
    CommonTime :: real(DecBegin, DecEnd),
    find_common(ListOfFreeTimes, CommonDay, CommonTime).

output_time(CommonDay, CommonTime)
    :-
    CommonTime :: real(Start, End),
    dec2time(Start, StartTime),
    dec2time(End, EndTime),
    nl, 
    write(CommonDay, ' - ', StartTime, ' to ', EndTime), nl.

    %%
    %% Conversions between hh:mm times and decimal times
    %%
time2dec(Hr:Mins, DecTime)
    :-!,
    DecTime is Hr + (Mins / 60).

time2dec(Time, Time).

dec2time(DecTime, (Hrs:Mins))
    :-
    Hrs is floor(DecTime),
    Mins is floor((DecTime - Hrs)*60).
