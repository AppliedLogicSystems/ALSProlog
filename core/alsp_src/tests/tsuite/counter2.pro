bench :-
    Start is cputime,
    count(0,800000),
    Delta is cputime-Start,
    write('Time'=Delta),nl.

count(I,Limit) :- I<Limit, !, INext is I+1, count(INext,Limit).
count(_,_).
