/*-----------------------------------------------------------------------------* 
 |      	animals.pro                                          
 |	Copyright 2015 Applied Logic Systems, Inc.
 |
 |      Program to figure out the type of an animal by       
 |                 asking questions.                                    
 |                                                                     
 |      Books:                                                          
 |      [CCP] = How to Solve it in Prolog,                              
 |                      Coelho, Cotta, Pereira, Lisbon, 1980            
 |
 |      Modified slightly by Keith Hughes to show better programming style.
 |	Added dynamic assertions 11/8/15 - KAB
 *-----------------------------------------------------------------------------*/

/*
 *      Predicate:      animal.
 *      Normal usage:   animal.
 *
 *      Function:       Query user to discover the type of an animal.
 *                     
 *      Reference:      [CCP] - Problem 50, Page 47.
 */

/* Knowledge base */

:- dynamic(rule/0).
:- dynamic(fact/2).

rule(1,animal,mammal,[c1]).
rule(2,animal,mammal,[c2]).
rule(3,animal,bird,[c3]).
rule(4,animal,bird,[c4,c5]).
rule(5,mammal,carnivore,[c6]).
rule(6,mammal,carnivore,[c7,c8,c9]).
rule(7,mammal,ungulate,[c10]).
rule(8,mammal,ungulate_toed,[c11]).
rule(9,carnivore,cheetah,[c12,c13]).
rule(10,carnivore,tiger,[c12,c14]).
rule(11,ungulate,giraffe,[c15,c16,c12,c13]).
rule(12,ungulate,zebra,[c17,c14]).
rule(13,bird,orstrich,[c18,c15,c16,c19]).
rule(14,bird,penguin,[c18,c20,c19]).
rule(15,bird,albatross,[c21]).

% Question database.

question(c1,'Has it hair').
question(c2,'Does it give milk').
question(c3,'Has it feathers').
question(c4,'Does it fly').
question(c5,'Does it lay eggs').
question(c6,'Does it eat meat').
question(c7,'Has it pointed teeth').
question(c8,'Has it claws').
question(c9,'Has it eyes pointing forward').
question(c10,'Has it hoofs').
question(c11,'Does it chew cud').
question(c12,'Has it a tawny color').
question(c13,'Has it dark spots').
question(c14,'Has it black stripes').
question(c15,'Has it long legs').
question(c16,'Has it a long neck').
question(c17,'Has it a white color').
question(c18,'Does it not fly').
question(c19,'Is it black and white').
question(c20,'Does it swim').
question(c21,'Is it a good flyer').

animal :-
   nl,write('Simple shell for animal identification via questions'),nl,
   write('From: How to Solve it in Prolog,'),nl,
   write('        by Coelho, Cotta, Pereira, Lisbon, 1980'),nl,nl,
   abolish(fact/2),
   write('Please describe the animal.'),nl,nl,
   recognition(_),
   nl,nl,
   write('Run again? '),
   read(Ans),
   animal(Ans).

animal(yes) :- !,
   animal.
animal(_).

/* Recognition process : discover animal's name */

recognition(X) :- 
	rule(N,X,Y,Z), 
	discover(Z),
	found(rule),
        conclusion(X,Y,N), 
	recognition(Y),
        abolish(fact,2).
recognition(_) :- retract(rule),write('Done.'),nl.
recognition(_) :- write('Don''t know this animal.'),nl.

found(X) :- X, !.
found(X) :- assert(X).

/* Discovering process */

discover([]).
discover([X|Y]) :- ask(X), discover(Y).

ask(X) :- fact(X,yes), !.   % Has it been answered yet?
ask(X) :- fact(X,no),!,fail.
ask(QuestionTag) :-
   question(QuestionTag,Question),!,
   write(Question), 
   complete(QuestionTag).

complete(X) :- write('? '),read(Y),assert(fact(X,Y)), Y=yes.

/* Conclusion of the recognition process */

conclusion(X,Y,N) :- nl,tab(4),write('---the '),write(X),
                     write(' is a '),write(Y),write(' by rule '),
                     write(N),nl,nl.

/* Description process:   discover animal's properties */

description(X) :- rule(N,Y,X,Z), description(Y,L,[]),
                  conclusion1(X,L,Y,Z,N).
description(_) :- nl,write('Don''t know this animal.'),nl.

description1(Y,L,Ls) :- rule(_,X,Y,_),description(X,L,[X|Ls]).
description1(_,L,L).

/* Conclusions of the description process */

conclusions1(X,L,Y,Z,N) :- nl,write(' a '),write(X),
                           write(' is an '),
                           output(L),write(Y),
                           write('satisfying conditions:  '),nl,
                           output(Z),nl,write('by rule '),write(N),
                           write('.').

output([]).
output([A|B]) :- write(A),tab(1),output(B).
 
