/*============================*
 *	medic1.pro
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
	read(Symptom).

suffers_from(  Person,  Condition)
	:-
	write('Do you suffer from '), 
	write(Condition), 
	write(' (yes/no) ?' ),
	read(Symptom),
	affirmative(Answer).

affirmative(yes).
affirmative(y).
affirmative(yea).
affirmative(si).
affirmative(tak).
affirmative(hai).

