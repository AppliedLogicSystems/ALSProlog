sentence(sent(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

noun_phrase(np(ADJ, NOUN)) --> adjective(ADJ), noun(NOUN).

noun_phrase(np(DET,NP)) --> determiner(DET), noun_phrase(NP).

verb_phrase(vp(ADV,VERB,NP)) --> adverb(ADV), verb(VERB), noun_phrase(NP).

determiner(det(W)) --> [W], 
		{member(W, [the,an,a])}.

adjective(adj(W)) --> [W], 
		{member(W, [new,efficient,red,yellow,hard,soft])}.

noun(noun(W)) --> [W],
		{member(W, [student,program,computer,car,boat,chair,building])}.

adverb(adv(W)) -->
		{member(W, [expertly,slowly,quickly,poorly])}.

verb(verb(W)) -->
		{member(W, [designed,programmed,ran,fell,flew])}.
