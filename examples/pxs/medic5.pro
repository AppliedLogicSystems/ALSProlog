/*============================*
 *	medic5.pro
 *	(c) 1986-90 Applied Logic Systems,  Inc.
 *	Author:  Kenneth  A. Bowen 
 *
 *	Elementary "medical" advice-giving program
 *	based on drugs.pro:
 *	A database  of  over-the-counter  U.S.
 *	medications, their  uses  and limitations.
 *============================*/

/*	Predicate: relieves/2:         
	relieves(DRUG,  SYMPTOM).        */

relieves(aspirin, headache).
relieves(aspirin, moderate_pain). 
relieves(aspirin, moderate_arthritis).
relieves(aspirin_codeine_combination, severe_pain).
relieves(robitussin_dm, cough). 
relieves(darvon, severe_pain). 
relieves(kaopectate, diarrhea).
relieves(novahistine, cough).
relieves(novahistine, nasal_congestion).
relieves(penicillin, pneumonia).
relieves(pepto_bismol, diarrhea).
relieves(pepto_bismol, nausea).
relieves(tylenol, headache).
relieves(tylenol, moderate_pain).
relieves(penicillin, pneumonia).
relieves(triaminic, nasal_congestion). 

/*	Predicate: aggravates/2:
		aggravates(DRUG, CONDITION)

	CONDITION is a single disease process, or a collection of
	disease processes, sometimes called a syndrome.  */

aggravates(aspirin, asthma).
aggravates(aspirin, peptic_ulcer).
aggravates(kaopectate, fever). 
aggravates(novahistine, high_blood_pressure). 
aggravates(novahistine, heart_disease).
aggravates(novahistine, diabetes). 
aggravates(novahistine, glaucoma). 
aggravates(penicillin, asthma).
aggravates(pepto_bismol, diabetes).
aggravates(pepto_bismol, gout).
aggravates(pepto_bismol, fever).

/*------------------------------------------------*
	Predicate: run/0:
		run
	New top-level entry to advice-giver.
 *------------------------------------------------*/
run
	:-
	write('Name ='), 
	read(Person),
	should_take(Person, Drug, Reason),
	write_list(['Recommend taking ', Drug, nl]),
	write('Explanation:'),nl,
	write_nl_list(Reason).
run
	:-
	write('I don''t know how to help you...sorry.'),nl.
/*------------------------------------------------*
	Predicate: should_take/2:
			should_take(PERSON, DRUG)

	Top level (entry) to the advice-giving
	program;  Interacts with PERSON to collect
	information on SYMPTOM, and CONDITION(S)
	suffered, and makes recommendation of DRUG
	for PERSON to take to relieve SYMPTOM without
	aggravating any CONDITION(S) suffered.
 *------------------------------------------------*/
should_take(Person, Drug, Reason) 
	:-
	complains_of(Person, Symptom, ComplainsReason),
	suppresses(Drug, Symptom, SuppressesReason),
	not_unsuitable_for(Person, Drug, UnsuitReason),
	append(SuppressesReason, UnsuitReason, InterReason),
	append(ComplainsReason, InterReason, Reason).

suppresses(Drug,  Symptom, [relieves(Drug,  Symptom)] )
	:-  
	relieves(Drug,  Symptom).

not_unsuitable_for(Person, Drug, _) 
	:-
	aggravates(Drug, Condition),
	suffers_from(Person, Condition,_),
	!, fail.

not_unsuitable_for(Person, Drug, [no_aggravated_conditions]).

complains_of(Person,  Symptom, ComplainsReason)
	:-
	ask_about(complains_of(Person,Symptom), Symptom, ComplainsReason).

ask_info(complains_of(Person,Symptom), 
			[ 'What symptom are you bothered by?' ], Symptom).
ask_confirm(complains_of(Person,Symptom), 
			['Are you bothered by ', Symptom, ' (yes/no) ?'], Symptom).
ask_acceptability(complains_of(Person,Answer), Answer, relieves(_, Answer)).

suffers_from(Person,  Condition, Reason)
	:-
	ask_about(suffers_from(Person, Condition), Condition, Reason).

ask_info(suffers_from(Person, Condition), 
			['What condition do you suffer from?'], Condition).
ask_confirm(suffers_from(Person, Condition), 
			['Do you suffer from ', Condition, ' (yes/no) ?'], Condition).
ask_acceptability(suffers_from(Person, Answer), Answer, aggravates(_, Answer)).

/*===================================================
	General Advice Program support predicates:
	Primary:  asking the user questions, and
	checking the responses for acceptability.
 *===================================================*/
ask_about(Predicate, InfoVar, Reason)
	:-
	var(InfoVar),!,                     % get value for InfoVar from user
	ask_info(Predicate, Question, InfoVar),
	write_list(Question),
	read(Answer),
	check_response(Predicate, Answer, InfoVar, Reason).

ask_about(Predicate, InfoVar, [Predicate])
	:-
	nonvar(InfoVar),!,                       % ask user question about (value of) InfoVar
	ask_confirm(Predicate, Question, InfoVar),
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
	var(Answer), !,          % suppress other clauses if Answer is a variable
	nl, write('Your answer is a Prolog variable,  most likely because '),
	nl, write('you typed an identifier beginning with an uppercase letter.'),
	nl, write('Please try again, avoiding uppercase letters: '),
	read(NewAnswer),
	check_response(Predicate, NewAnswer, InfoVar, Reason).
		%  passing the original variable InfoVar to this recursive call
		%  on check_response means that the original call to check_response
		%  will get the ultimate result of checking the NewAnswer 

check_response(Predicate, Answer,  InfoVar, [Predicate])
	:- 		% can assume Answer is non-variable
			% because of cut in first clause
	ask_acceptability(Predicate, Answer, Test),
	Test, !,		% Is it acceptable?
	InfoVar = Answer.	% Yes - return it.


check_response(Predicate, Answer,  InfoVar, [synonymous(Answer, InfoVar), Predicate])
	:-		% can assume Answer is non-variable
	synonymous(Predicate, Answer, InfoVar),
	! .	% and that Answer is not a basic acceptable
		% response;  suppress backtracking
		% to final (error) clause if synonym is found.

check_response(Predicate, Answer,  InfoVar, Reason)
	:-
	nl, write('I can''t understand what you typed.'),
	nl, write('Please try again: '),
	read(NewAnswer),
	check_response(Predicate, NewAnswer,  InfoVar, Reason).

synonymous(Predicate, Expression, Target)
	:-
	synonym_for(Predicate, Expression,  FirstHit), 
	!,		% at least one synonym exists - avoid error case
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
	write(Item),nl,
	write_nl_list(Items).

aggravates(Drug, Condition)
	:-
	compound(Drug, Components),
	member(OtherDrug, Components),
	aggravates(OtherDrug, Condition).

compound(aspirin_codeine_combination, [codeine, aspirin]).

member(Item, [Item | _]).
member(Item, [_ | Tail])
	:-
	member(Item, Tail).
