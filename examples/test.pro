test :-
	consult(['als/bench']),
	bench_test,
	
	.

bench_test :-
	solve(y=m*x+b, x, Answer1),
	Answer1 = (x=(-y+b)/ -m),
	solve(((2^(cos(x)^2)*2^(sin(x)^2))^sin(x))^cos(x)=2^(1/4), x, Answer2),
	Answer2 = (x = (n0*180+ -1*arcsin(log(2,(2 ^ (1/4)) ^ (1/2^ -1))))/2).

:- test.