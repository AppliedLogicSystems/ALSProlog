identify 
	:-
	obtain_description,
	entertain_hypothesis(Identification),	% these 2 goals are a generate
	validate(Identification),	% and test loop 
	report(Identification).


obtain_description 
	:-
	write('Observation: '),
	read(Observer),			% for most systems
%	newread(Observer),		% for Macintosh
	dispatch(Observer).

dispatch(done).	% if the user wants to quit,  
dispatch(quit) :- abort.	% let her or him, otherwise: 
dispatch(Observation)
	:-
	asserta(observed(Observation)),	% record the observation & 
	obtain_description. 	% get another one


entertain_hypothesis(Identification) 
	:-
	observed(Characteristic),
	trait(Characteristic, Identification).

entertain_hypothesis(Identification) 
	:-
	not(observed(_)),
	trait(_, Identification).

entertain_hypothesis(Identification) 
	:-
	observed(Characteristic),
	trait(Characteristic, Identification).

entertain_hypothesis(_) 
	:-
	write('I have no item in my database with those characteristics.'),nl,
	abolish(observed,1),
	abolish(does_not_hold,1),!,fail.


validate(Identification) 
	:-
	bagof(Attribute, trait(Attribute,Identification), Characterization),
	verify(Characterization),!.

verify([ ]).
verify([Attribute | Rest_of_Attributes]) 
	:-
	check(Attribute),
	verify(Rest_of_Attributes).


check(Attribute) 
	:-
	observed(Attribute),!.

check(Attribute) 
	:-
	does_not_hold(Attribute),!,fail.

check(Attribute) 
	:-
	ask_about(Attribute), !.

ask_about(Attribute) 
	:-
	write('Is it true that '), write(Attribute), write('?'),
	read(Answer),			% for most systems
%	newread(Answer),		% for Macintosh
	act_on(Answer, Attribute).

act_on(yes, Attribute) 
	:-
	asserta(observed(Attribute)),!.

act_on(Other, Attribute) 
	:-
	asserta(does_not_hold(Attribute)),!,fail.

report(Identification) 
	:-
	write('The item appears to be a '),
	write(Identification), nl,
	bagof(Characteristic, trait(Characteristic, Identification), Traits),
	write('The traits were: '),
	write(Traits), nl.


trait(bud_scales(valvate),bitternut_hickory).  
trait(buds(yellow),bitternut_hickory).
trait(bud_scales(valvate),pecan_hickory).  
trait(buds(brownish),pecan_hickory).
trait(bud_scales(imbricate),pignut_hickory).  
trait(terminal_buds(short),pignut_hickory).
trait(bud_scales(imbricate),mockernut_hickory).  
trait(terminal_buds(large),mockernut_hickory).  
trait(outer_scales(deciduous),mockernut_hickory).
trait(bud_scales(imbricate),shellbark_hickory).  
trait(terminal_buds(large),shellbark_hickory).  
trait(outer_scales(persistent),shellbark_hickory).
trait(twigs(orange_brown),shellbark_hickory).
trait(bud_scales(imbricate),shagbark_hickory).  
trait(terminal_buds(large),shagbark_hickory).  
trait(outer_scales(persistent),shagbark_hickory).  
trait(twigs(reddish_brown),shagbark_hickory).


observed(outer_scales(X)) 
	:-
	opposite(X,Y),
	does_not_hold(outer_scales(Y)).

observed(terminal_buds(X)) 
	:-
	opposite(X,Y),
	does_not_hold(terminal_buds(Y)).

observed(bud_scales(X)) 
	:-
	opposite(X,Y),
	does_not_hold(bud_scales(Y)).

opposite(short,large).  
opposite(large,short).  
opposite(deciduous,persistent).  
opposite(persistent,deciduous).  
opposite(valvate,imbricate).  
opposite(imbricate,valvate).


observed(outer_scales(X)) 
	:-
	synonymous(X,Y),
	observed(outer_scales(Y)).

observed(terminal_buds(X)) 
	:-
	synonymous(X,Y),
	observed(terminal_buds(Y)).

observed(bud_scales(X)) 
	:-
	synonymous(X,Y),
	observed(bud_scales(Y)).

synonymous(valvate,non-overlapping).  
synonymous(imbricate,overlapping).  
synonymous(short,stout).

