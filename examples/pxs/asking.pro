/*------------------------------------------------*
 |		asking.pro
 |		Copyright (c) 1986-89 Applied Logic Systems, Inc.
 |
 |		Author: Kenneth A. Bowen
 |		Asking predicates for the solve shell
 *------------------------------------------------*/

ask_about(Predicate)
	:-
	ask_info(Predicate, Question, InfoVar),
	var(InfoVar),!,			% get value for InfoVar from user
	write_list(Question),
	read(Answer),
	check_response(Predicate, Answer, InfoVar, Reason).

ask_about(Predicate)
	:-
	ask_confirm(Predicate, Question, InfoVar),
	nonvar(InfoVar),!,		% ask user question about (value of) InfoVar
	write_list(Question),
	read(Answer),
	act_on_yes_no(Answer).

/*------------------------------------------------*
	Predicate: act_on_yes_no/1:
		act_on_yes_no(Answer)

	If Answer is an acceptable version of 'yes' -- succeeds
	If Answer is an acceptable version of 'no' -- fails
	Otherwise, asks user to retype response, and 
		acts appropriately on the new response.
 *------------------------------------------------*/
act_on_yes_no(Answer)
	:-
	var(Answer), !,
	nl,
	write('You have typed an expression which I interpret as a Prolog variable.'),
	nl,
	write('This is probably because you began a word with an uppercase letter.'),
	nl, write('The list of acceptable positive responses is '),
	bagof(YesWord, affirmative(YesWord),  Positives),
	write(Positives), 
	nl, write('The list of acceptable negative responses is '),
	bagof(NoWord, negative(NoWord),  Negatives),
	write(Negatives),
	nl, write('Please type a new (correct) response: '),
	read(NewAnswer),
	!,
	act_on_yes_no(NewAnswer).
act_on_yes_no(Answer) :-  affirmative(Answer), !.
act_on_yes_no(Answer) :-  negative(Answer), !, fail.
act_on_yes_no(Answer)
	:-
	nl, write('I can''t understand your response.'),
	nl, write('The responses I can understand are these:'),
	bagof(YesWord, affirmative(YesWord),  Positives),
	bagof(NoWord, negative(NoWord),  Negatives),
	nl, write('Affirmative: '), write(Positives),
	nl, write('Negative: '), write(Negatives),
	nl, write('Please type a new (correct) response: '),
	read(NewAnswer),
	!,
	act_on_yes_no(NewAnswer).

affirmative(yes).
affirmative(y).
affirmative(yea).
affirmative(si).
affirmative(tak).
affirmative(hai).

negative(no).
negative(n).
negative(ochi).
negative(nein).

/*------------------------------------------------*
	Predicate: check_response/3:
		check_response(PREDICATE, ANSWER, INFOVAR)
	Used for checking the acceptability of user's
	answers to requests for information (e.g.,
	What symptoms are bothering you?).  
	-- PREDICATE is the name of the predicate immediately
	requiring the requested information;
	-- INFOVAR is the variable in the call to PREDICATE
	which will be bound to the (ultimate) information;
	-- ANSWER is the user's response to the request.
	> If ANSWER is a variable, user is so informed and is
	requested to retype response;
	> If there is a recorded TEST for acceptablity for
	PREDICATE which ANSWER passes, succeeds 
	with INFROVAR = ANSWER;
	> If ANSWER is recorded as a synonym for a 
	presumably correct VALUE, succeeds with INFOVAR = VALUE;
	> Otherwise, tells user the response can't be 
	understood and asks for retyping.
	NOTE:  In order for any responses to be accepted,
	the program must contain clauses for either or
	both of the following:

	ask_acceptability(PREDICATE, ITEM, TEST).
	   (e.g., ask_acceptability(complains_of, Item, relieves(_, Item)). )
	   TEST is a call on a predicate which should be defined in the
	   program.  ITEM should occur in the call TEST.

	synonym_for(PREDICATE, INPUT,  TARGET), 
	  (e.g., synonym_for(_, stuffy_nose, nasal_congestion).	
		synonym_for(complains_of, zip, cough).	)
 *------------------------------------------------*/
check_response(Predicate, Answer, InfoVar, Reason)
	:-
	var(Answer), !,		% suppress other clauses if Answer is a variable
	nl, write('Your answer is a Prolog variable,  most likely because '),
	nl, write('you typed an identifier beginning with an uppercase letter.'),
	nl, write('Please try again, avoiding uppercase letters: '),
	read(NewAnswer),
	check_response(Predicate, NewAnswer, InfoVar, Reason).
		%  passing the original variable InfoVar to this recursive call
		%  on check_response means that the original call to check_response
		%  will get the ultimate result of checking the NewAnswer 

check_response(Predicate, Answer,  InfoVar, [userinfo-Predicate])
	:- 	% can assume Answer is non-variable
		% because of cut in first clause
	ask_acceptability(Predicate, Answer, Test),
	Test, !,				% Is it acceptable?
	InfoVar = Answer.			% Yes - return it.	
					

check_response(Predicate, Answer,  InfoVar, [synonymous(Answer, InfoVar), userinfo-Predicate])
	:-					% can assume Answer is non-variable
	synonymous(Predicate, Answer, InfoVar),	% and than Answer is not a basic acceptable
	! .					% response;  suppress backtracking
						% to final (error) clause if synonym is
						% found.

check_response(Predicate, Answer,  InfoVar, Reason)
	:-
	nl, write('I can''t understand what you typed.'),
	nl, write('Please try again: '),
	read(NewAnswer),
	check_response(Predicate, NewAnswer,  InfoVar, Reason).

synonymous(Predicate, Expression, Target)
	:-
	synonym_for(Predicate, Expression,  FirstHit), 
	!,					% at least one synonym exists - avoid error case
	check_ambiguity(Predicate, Expression,  Target).

%synonymous(Expression,  Target)
%	:-
%	........	% other more complex cases of synonymity may be possible

synonym_for(_, stuffy_nose, nasal_congestion).	
synonym_for(complains_of(_,_), zip, cough).	
synonym_for(complains_of(_,_), zip, nasal_congestion).	
synonym_for(_, pain, moderate_pain).	
synonym_for(_, pain, severe_pain).	
%......

check_ambiguity(Predicate, Expression,  FinalSynonym)
	:-
	setof(Hit,  synonym_for(Predicate, Expression, Hit), Synonyms),
	decide_ambiguity(Synonyms, FinalSynonym).
	
decide_ambiguity([FinalSynonym], FinalSynonym) 
	:-!.			% Only one synonym -- return it.
decide_ambiguity(Synonyms, FinalSynonym)
	:-
	select_one(
	   [nl, 'Your response is ambiguous', nl,
	    'Please select a synonym - type corresponding number',nl],
	   Synonyms,
	   FinalSynonym).

select_one(Message,  ChoiceList,  Choice)
	:-
	write_list(Message),
	get_choice(ChoiceList, Choice).

get_choice(ChoiceList, Choice)
	:-
	write_numbered_list(ChoiceList,  1,  Length),
	nl, write( 'Choice = '),
	read(Number),
	( ( integer(Number), 1 =< Number, Number =< Length)  ->
		nth_element(ChoiceList, Number, Choice)  ;
		nl, write('Improper entry or number out of range...please try again'), nl,
		get_choice(ChoiceList,  Choice)  ).

write_list([]).
write_list([nl | Items ])
	:-!,
	nl, write_list(Items).
write_list([Item | Items ])
	:-
	write(Item),
	write_list(Items).

write_numbered_list([], Length,  Length) :- !.
write_numbered_list([ Item | Items ],  CurCount,  Length)
	:-
	write(CurCount), write(':  '), write(Item), nl,
	NextCount is CurCount + 1,
	write_numbered_list(Items,  NextCount,  Length).

nth_element([ Choice | _ ], 1, Choice) :- !.
nth_element([ _ | Tail ], CurCountDown, Choice)
	:-
	CurCountDown > 1,
	NextCountDown is CurCountDown - 1,
	nth_element(Tail, NextCountDown,  Choice).

write_nl_list([]).
write_nl_list([Item | Items])
	:-
	print(Item),nl,
	write_nl_list(Items).

print((Conclusion if true))
	:-!,
	print(Conclusion).
print((Conclusion if Premisses))
	:-!,
	print(Conclusion),write('if '),
	nl,tab(3),
	print(Premisses).
print(asked(What))
	:-!,
	print(What),write(' -- asked').
print((A,B))
	:-!,
	print(A), write(' & '),
	nl,tab(3), 
	print(B).
	
print(Item)
	:-
	write(Item).

obtain_conf(C) 
	:-
	write('What is your confidence in that answer?'),
	read(C).

member(Item, [Item | _]).
member(Item, [_ | Tail])
	:-
	member(Item, Tail).

append([], Right, Right).
append([Head | Tail], Right, [Head | ResultTail])
	:-
	append(Tail, Right, ResultTail).
