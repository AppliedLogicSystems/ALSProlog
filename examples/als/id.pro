/* ---------------------------------------------------------------------*
 | 		id.pro  
 | 	Copyright (c) 1986-2015 by Applied Logic Systems
 |
 |	An identification and classification shell
 |
 | Author:  Kenneth A. Bowen
 |
 |	Description:
 |
 |	This is a generic identification by properties shell.
 | 	When combined with the file 
 |		hickory.pro
 |	this becomes part of the Hickory Tree Identification Expert
 |
 |	This is the identification portion of the "expert".  It
 |	must be coupled with a database of "trait" assertions,
 | 	such as are found in the file hickory.pro.  The general form
 |	of the database of trait assertions is:
 |
 |		trait(<characteristic>, <tree identity>).
 |		e.g., trait(buds(yellow), bitternut_hickory).
 |
 |	The system first allows the user to freely input some
 |	observations;  these must be entries from the first
 |	argument of the trait predicate.  The user indicates
 |	the end of this phase by entering 'done'.  The system then
 |	does its diagnosis via a hypothsize and test loop.  When it
 |	is trying to verify a conjecture, it will ask the user
 |	yes-no questions concerning further observations.
 |
 |	Note that hickory.pro contains a load directive 
 |		:- consult(id).
 |	So don't add a load directive for hickory.pro in this file
 *---------------------------------------------------------------------*/

module identify.

export identify/0.

:- dynamic(user_observed/1).
:- dynamic(does_not_hold/1).
:- dynamic(trait/2).
 
 identify :- 
	header,
	identify0.

header :- nl,
 	write('Generic Identification by Properties shell'),nl,
	write('   applied to Hickory Trees'),nl,
	write('       by Ken Bowen'),nl,nl,
	write('Enter properties characteristics of hicory trees (see hickory.pro)'),nl,
	write('Type ''done.'' to finish inputting initial observations'),nl,nl,
	write('Type ''exit.'' to exit the shell.'),nl,nl.

 identify0 :-
        obtain_description,
        entertain_hypothesis(Identification),  	% these 2 goals are a generate
        validate(Identification),              	% and test loop
        report(Identification),
        abolish(user_observed,1),              	% clear the database of recorded
        abolish(does_not_hold,1),          	% observations
	loop_or_quit.

loop_or_quit
	:-
	nl, write('Identify another (yes. or no. ):'),
	read(Ans),
	loop_or_quit(Ans).

loop_or_quit(yes)
	:-
	identify0.
loop_or_quit(_) 
	:- 
	write('Bye.'),nl.

 obtain_description :-
        write('Observation: '),			% requires a term which occurs
        read(Observer),				% as the first argument of a trait
        dispatch(Observer).			% predicate; e.g., buds(yellow).

 dispatch(done) :-!.                    % if the user wants to quit,
 dispatch(quit) :- abort.               % let her or him, otherwise
 dispatch(Observation) :-               % record the observation & get
        asserta(user_observed(Observation)), % another one
        obtain_description.

 entertain_hypothesis(Identification) :-     % hypothesizes that the tree is
        user_observed(Characteristic),       % one which has at least one of
        trait(Characteristic, Identification).   % the observed traits

 entertain_hypothesis(Identification) :-     % if no observations are recorded,
        \+(user_observed(_)),                    % guess wildly
        trait(_, Identification).

 entertain_hypothesis(Identification) :-     % duplicates 1st clause -- later
        user_observed(Characteristic),       % user interaction may
        trait(Characteristic, Identification).   % cause an observation to be
                                                 % recorded, the wild guess
                                             % above may be wrong, and the
                                             % generate-&-test loop return to
                                             % here
 entertain_hypothesis(_) :-
        write('I have no tree in my database with those characteristics.'),nl,
        abolish(user_observed,1),                 % the user has provided an
        abolish(does_not_hold,1),!,fail.     % impossible set of descriptors
                                                 % -- all possibilities have
                                                 % been tried

observed(X) :-
   user_observed(X).

/*-----------------------------------------------------------
 | Validation of a hypothesis here means verification that all the  |
 | descriptors attached to the hypothesis by the database have or   |
 | can be observed about the presented twig.                        |
 -------------------------------------------------------------------*/
 validate(Identification) :-
        bagof(Attribute, trait(Attribute,Identification),
                                           Characterization),
        verify(Characterization),!.

 verify([]).                                    % Verification : simply check
 verify([Attribute | Rest_of_Attributes]) :-    % that each descriptor required
        check(Attribute),                       % is actually present
        verify(Rest_of_Attributes).

 check(Attribute) :-                   % You can check off a descriptor if it
        observed(Attribute),!.         % has been observed

 check(Attribute) :-                        % You fail to check off a descriptor
        does_not_hold(Attribute),!,fail.    % if it has been recorded as absent

 check(Attribute) :-                 % If nothing has been recorded about the
        ask_about(Attribute),!.      % descriptor, ask the user about it

% The cuts are used in check since we want exactly one of the clauses to be
% executed exactly once.

 ask_about(Attribute) :-
        write('Is it true that '), write(Attribute), write('? '),
        read(Answer),
        act_on(Answer, Attribute).

 act_on(yes, Attribute) :-
        asserta(user_observed(Attribute)),!.             % See below re: asserta
 act_on(y, Attribute) :-
        asserta(user_observed(Attribute)),!.             % See below re: asserta
 act_on(Other, Attribute) :-
        asserta(does_not_hold(Attribute)),!,fail.

% As with check, we use cuts here since we want exactly one of the clauses
% for act_on to be executed exactly once.

 report(Identification) :-
        nl, write('The tree appears to be a '),
        write(Identification), nl,
        bagof(Characteristic, trait(Characteristic, Identification), Traits),
        write('The traits were: '),
        write(Traits), nl.

/*---------------------------------------------------------------------------*
 | Some traits, e.g., short vs. large, are opposites;  if the user           |
 | tells us that the terminal buds are not large, then we should             |
 | be able to conclude that they are short.  The following clauses, called   |
 | by check, do shallow reasoning to accomplish this.  These rules function  |
 | as defaults for observed; hence the use of asserta in dispatch and        |
 | act_on, which places the new information above these defaults.            |
 ----------------------------------------------------------------------------*/
 observed(outer_scales(X)) :-
        opposite(X,Y),
        does_not_hold(outer_scales(Y)).

 observed(terminal_buds(X)) :-
        opposite(X,Y),
        does_not_hold(terminal_buds(Y)).

 observed(bud_scales(X)) :-
        opposite(X,Y),
        does_not_hold(bud_scales(Y)).


 opposite(short,large).
 opposite(large,short).
 opposite(deciduous,persistent).
 opposite(persistent,deciduous).
 opposite(valvate,imbricate).
 opposite(imbricate,valvate).

/*--------------------------------------------------------------------------
 | The following clauses for observed, similar to those above, do shallow  |
 | reasoning to allow users to make use of synonyms for the descriptors    |
 | entered in the basic database.                                          |
 --------------------------------------------------------------------------*/
 observed(outer_scales(X)) :-
        synonymous(X,Y),
        observed(outer_scales(Y)).

 observed(terminal_buds(X)) :-
        synonymous(X,Y),
        observed(terminal_buds(Y)).

 observed(bud_scales(X)) :-
        synonymous(X,Y),
        observed(bud_scales(Y)).

 synonymous(valvate,non-overlapping).
 synonymous(imbricate,overlapping).
 synonymous(short,stout).

endmod.
