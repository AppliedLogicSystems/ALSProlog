identify 
	:-
	obtain_description,
	collect(Hypotheses),                                   % get all consistent identifications 
	filter(Hypotheses, Identification),               % & filter out failures 
	report(Identification).

obtain_description 
	:-
	write('Observation: '),
	read(Observer),
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

collect(Hypotheses)
	:-		% gets the RreasonableS candidates 
	setof(H, entertain_hypothesis(H), Hypotheses).


filter(Hypotheses, Identification)
	:-     
	setof(A, observed(A), Observations),     
	filter_consistency(Hypotheses, Observations, Possible_Ids),
	validate_filter(Possible_Ids,  Identification).

filter_consistency([], _, []).

filter_consistency([Hyp | Hyps], Observations, [Hyp | Ids])     
	:-     consistent(Hyp, Observations), !,     
	filter_consistency(Hyps, Observations, Ids).

filter_consistency([_ | Hyps], Observations, Ids)     
	:-     
	filter_consistency(Hyps, Observations, Ids).


consistent(Hyp, []).

consistent(Hyp, [Observation | Observations])     
	:-
	not(inconsistent(Hyp, Observation)),     
	consistent(Hyp, Ovservations).

inconsistent(Hypothesis, Observation)    
	 :-     
	Hypothesis =.. [Predicate, Hypothesis_Argument],     
	Observation =.. [Predicate, Observation_Argument],
	opposite(Hypothesis_Argument, Observation_Argument).


validate_filter([], []) :- !.

validate_filter([Identification], [Identification]) :- !.

validate_filter([Hyp | Hyps], [Hyp | Ids] )     
	:-     
	validate(Hyp), !,     
	validate_filter(Hyps, Ids).

validate_filter([ _ | Hyps], Ids)
	:-     
	validate_filter(Hyps, Ids).


