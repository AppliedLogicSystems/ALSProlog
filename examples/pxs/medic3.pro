/*============================*
 *	medic3.pro
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

/*========================================================*
	Predicate: should/2
		should_take(Person, Drug)
	Top level of the simple advice-giving system.
 *========================================================*/
should_take(Person, Drug) 
	:-
	complains_of(Person, Symptom),
	suppresses(Drug, Symptom),
	not( unsuitable_for(Person, Drug) ).

/*========================================================*
	Predicate: complains_of/2
		complains_of(Person, Symptom)
	Gets initial symptom complaint from user and
	checks corrects, synonyms, etc.
 *========================================================*/
complains_of(Person,  Symptom)
	:-
	write('What symptom are you bothered by?'),
	read(Answer),	% for most systems
%	newread(Answer),	% for Macintosh
	filter_symptom(Answer, Symptom).

filter_symptom(Answer,  Symptom)
	:-
	var(Answer), !,         % suppress other clauses if Answer is a variable
	nl, write('Your answer is a Prolog variable,  most likely because '),
	nl, write('you typed an identifier beginning with an uppercase letter.'),
	nl, write('Please try again, avoiding uppercase letters: '),
	read(NewAnswer),	% for most systems
%	newread(NewAnswer),	% for Macintosh
	filter_symptom(NewAnswer, Symptom).
		%  passing the original variable Symptom to this recursive call
		%  on filter_symptom means that the original call on filter_symptom
		%  will get the result of filtering the NewAnswer 

filter_symptom(Answer,  Symptom)
	:- 		% can assume Answer is non-variable
			% because of cut in first clause
	relieves(Anything, Answer), !,		% Is it a recorded Symptom?
	Symptom = Answer.		% Yes - return it.	


filter_symptom(Answer,  Symptom)
	:-		% can assume Answer is non-variable
	synonymous(Answer, Symptom),             % and than Answer is not a recorded
	! .		% symptom;  suppress backtracking
			% to last (error) clause if synonym is
			% found.

filter_symptom(Answer, Symptom)
	:-
	nl, write('I can''t understand what you typed.'),
	nl, write('Please try again: '),
	read(NewAnswer),	% for most systems
%	newread(NewAnswer),	% for Macintosh
	filter_symptom(NewAnswer,  Symptom).

synonymous(Expression, Target)
	:-
	synonym_for(Expression,  FirstHit), 
	!,		                  % simple case holds - ignore other cases
	check_ambiguity(Expression,  Target).

%synonymous(Expression,  Target)
%	:-
%........	% other more complex cases of synonymity may be possible

synonym_for(stuffy_nose, nasal_congestion).	
synonym_for(zip, cough).	
synonym_for(zip, nasal_congestion).	
%......

check_ambiguity(Expression,  FinalSynonym)
	:-
	setof(Hit,  synonym_for(Expression, Hit), Synonyms),
	decide_ambiguity(Synonyms, FinalSynonym).

decide_ambiguity([FinalSynonym], FinalSynonym) 
	:-!.		                 % Only one synonym -- return it.
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
	read(Number),	% for most systems
%	newread(Number),	% for Macintosh
	( ( integer(Number), 1 =< Number, Number =< Length)  ->
		nth_element(ChoiceList, Number, Choice)  ;
		nl, write('Improper entry or number out of range...please try again'), nl,
		get_choice(ChoiceList,  Choice)  ).

write_list([]).
write_list( [nl | Items ] )
	:-!,
	nl, write_list(Items).
write_list( [Item | Items ] )
	:-
	write( Item ),
	write_list( Items ).

write_numbered_list( [], Length,  Length) :- !.
write_numbered_list( [ Item | Items ],  CurCount,  Length)
	:-
	write(CurCount), write(':  '), write(Item), nl,
	NextCount is CurCount + 1,
	write_numbered_list( Items,  NextCount,  Length).

nth_element( [ Choice | _ ], 1, Choice) :- !.
nth_element( [ _ | Tail ], CurCountDown, Choice)
	:-
	CurCountDown > 1,
	NextCountDown is CurCountDown - 1,
	nth_element( Tail, NextCountDown,  Choice).

/*========================================================*
	Predicate: suppresses/2
		suppresses(Drug, Symptom)
	Simple version: just checks database to see if
	Drug is directly recorded as relieving Symptom
 *========================================================*/
suppresses(Drug,  Symptom)  
	:-  
	relieves(Drug,  Symptom).

/*========================================================*
	Predicate: unsuitable_for/2
		unsuitable_for(Person, Drug)
	Drug is bad for a Person if it aggravates a
	Condition which the Person suffers from.
 *========================================================*/
unsuitable_for(Person, Drug) 
	:-
        aggravates(Drug, Condition),
        suffers_from(Person, Condition).

/*========================================================*
	Predicate: suffers_from/2
		suffers_from(Person, Condition)
	Prompts user with yes/no questions about suffering
	from Condition, and checks soundness of answer
 *========================================================*/
suffers_from(  Person,  Condition)
	:-
	write('Do you suffer from '), write(Condition), write(' (yes/no) ?' ),
	read(Answer),	% for most systems
%	newread(Answer),	% for Macintosh
	act_on_yes_no(Answer).

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
	read(NewAnswer),	% for most systems
%	newread(NewAnswer),	% for Macintosh
	act_on(NewAnswer).
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
	read(NewAnswer),	% for most systems
%	newread(NewAnswer),	% for Macintosh
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

