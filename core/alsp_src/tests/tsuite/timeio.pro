%% Following op declaration is needed to read par4.pro
:- op(990,xfy,&), op(990,xfy,&&).


time_parser :-
	time_parser('testmath.pro'),
	time_parser('par4.pro'),
	time_parser('inpf1'),
	time_parser('inpf2').

time_parser(Name) :-
	open(Name,read,S),
	!,
	Start is cputime,
	read(S,Term),
	time_parser(Term,S),
	Delta is cputime-Start,
	close(S),
	printf("Parsing time for file %s is %g\n",[Name,Delta]).
time_parser(Name) :-
	write('Nope'),nl.

time_parser(Term,_) :-
	nonvar(Term),
	Term = end_of_file,
	!.
time_parser(Term,S) :-
	read(S,NextTerm),
	time_parser(NextTerm,S).

time_parser_rt :-
	time_parser_rt('testmath.pro'),
	time_parser_rt('par4.pro'),
	time_parser_rt('inpf1'),
	time_parser_rt('inpf2').

time_parser_rt(Name) :-
	open(Name,read,S),
	!,
	Start is cputime,
	rt_read(S,Term),
	time_parser_rt(Term,S),
	Delta is cputime-Start,
	close(S),
	printf("Parsing time for file %s is %g\n",[Name,Delta]).
time_parser_rt(Name) :-
	write('Nope'),nl.

time_parser_rt(Term,_) :-
	nonvar(Term),
	Term = end_of_file,
	!.
time_parser_rt(Term,S) :-
	rt_read(S,NextTerm),
	time_parser_rt(NextTerm,S).


time_lexan_t :- 
	time_lexan_t('testmath.pro'),
	time_lexan_t('par4.pro'),
	time_lexan_t('inpf1'),
	time_lexan_t('inpf2').

time_lexan_t(Name) :-
	open(Name,read,S),
	!,
	Start is cputime,
	get_token(S,T,V),
	time_lexan_t(T,V,S),
	Delta is cputime-Start,
	close(S),
	printf("Lexan time for file %s is %g\n",[Name,Delta]).
time_lexan_t(Name) :-
	write('Nope'),nl.

time_lexan_t(T,_,_) :-
	nonvar(T),
	T = end_of_file,
	!.
time_lexan_t(T,_,S) :-
	get_token(S,NT,NV),
	time_lexan_t(NT,NV,S).
	
time_lexan_l :- 
	time_lexan_l('testmath.pro'),
	time_lexan_l('par4.pro'),
	time_lexan_l(inpf1),
	time_lexan_l(inpf2).

time_lexan_l(Name) :-
	open(Name,read,S),
	!,
	Start is cputime,
	get_token_list(S,L),
	time_lexan_l(L,S),
	Delta is cputime-Start,
	close(S),
	printf("Lexan (list) time for file %s is %g\n",[Name,Delta]).
time_lexan_l(Name) :-
	write('Nope'),nl.

time_lexan_l([end_of_file(_,_,_)],_) :-
	!.
time_lexan_l(L,S) :-
	get_token_list(S,NL),
	time_lexan_l(NL,S).
	

time_parser_old :- 
	time_parser_old('testmath.pro'),
	time_parser_old('par4.pro'),
	time_parser_old(inpf1),
	time_parser_old(inpf2).

time_parser_old(Name) :-
	pbi_seeing(What),
	pbi_see(Name),
	Start is cputime,
	pbi_read(X),
	time_parser_old0(X),
	Delta is cputime-Start,
	pbi_seen,
	pbi_see(What),
	printf("Parsing time for file %s is %g\n",[Name,Delta]).

time_parser_old0(Done) :-
	nonvar(Done),
	Done=end_of_file,
	!.
time_parser_old0(Term) :-
	pbi_read(X),
	time_parser_old0(X).

