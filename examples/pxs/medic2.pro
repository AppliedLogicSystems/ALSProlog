/*============================*
 *	medic2.pro
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


should_take(Person, Drug) 
	:-
	complains_of(Person, Symptom),
	suppresses(Drug, Symptom),
	not( unsuitable_for(Person, Drug) ).


suppresses(Drug,  Symptom)  
	:-  
	relieves(Drug,  Symptom).

unsuitable_for(Person, Drug) 
	:-
        aggravates(Drug, Condition),
        suffers_from(Person, Condition).

complains_of(Person,  Symptom)
	:-
	write('What symptom are you bothered by?'),
	read(Symptom).	% for most systems
%	newread(Symptom).

suffers_from(  Person,  Condition)
	:-
	write('Do you suffer from '), write(Condition), write(' (yes/no) ?' ),
	read(Symptom),	% for most systems
%	newread(Symptom),
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
	read(NewAnswer),
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
	read(NewAnswer),
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

