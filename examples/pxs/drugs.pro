/*------------------------------------------------------*
 *		drugs.pro
 *           (c) 1986-90  Applied Logic Systems,  Inc.
 *     Author:      Kenneth  A. Bowen 
 *
 *     A database  of  over-the-counter  U.S. 
 *    medications, their  uses  and limitations
*------------------------------------------------------*/ 

/*	Predicate: relieves/2:         
		relieves(DRUG,  SYMPTOM).        

	SYMPTOM is a manifestation of an underlying disease process,
	but is distinct from the disease process itself.
*/

relieves(aspirin,    headache).
relieves(aspirin, moderate_pain).
relieves(aspirin, moderate_arthritis).
relieves(aspirin_codeine_combination,  severe_pain).
relieves(robitussin_dm, cough).
relieves(darvon,  severe_pain).
relieves(kaopectate, diarrhea).
relieves(novahistine, cough).
relieves(novahistine, nasal_congestion).
relieves(penicillin, pneumonia).
relieves(pepto-bismol,  diarrhea).
relieves(pepto-bismol,  nausea).
relieves(tylenol, headache).
relieves(tylenol, moderate_pain). 
relieves(triaminic, nasal_congestion).
relieves(penicillin, pneumonia).

/*	Predicate: aggravates/2:
		aggravates(DRUG, CONDITION)

	CONDITION is a single disease process, or a collection of
	disease processes, sometimes called a syndrome.
*/

aggravates(aspirin,  asthma).
aggravates(aspirin, peptic_ulcer).
aggravates(kaopectate, fever).
aggravates(novahistine,  high_blood_pressure).
aggravates(novahistine,  heart_disease).
aggravates(novahistine, diabetes).
aggravates(novahistine, glaucoma).
aggravates(pepto-bismol, diabetes).
aggravates(pepto-bismol,  gout).
aggravates(pepto-bismol, fever). 

