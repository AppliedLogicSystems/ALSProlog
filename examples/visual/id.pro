/*============================================================*
 | 		id.pro  
 | Copyright (c) 1986, 1988 by Applied Logic Systems
 |	Copying per "Copying ALS"
 |
 |	An identification and classification shell
 |
 | Author:  Kenneth A. Bowen
 *============================================================*/

/*----------------------------------------------------------*
 |	Description:
 |
 |	This is part of the Hickory Tree Identification Expert
 |
 |	This is the identification portion of the "expert".  It
 |	must be coupled with a database of "trait" assertions,
 | 	such as are found in the file hickory.pro.  The general form
 |	of the database of trait assertions is:
 |
 |		trait(<characteristic>, <tree identity>).
 |		e.g., trait(bud_color(yellow), bitternut_hickory).
 |
 |	The system first allows the user to freely input some
 |	observations;  these must be entries from the first
 |	argument of the trait predicate.  The user indicates
 |	the end of this phase by entering 'done'.  The system then
 |	does its diagnosis via a hypothsize and test loop.  When it
 |	is trying to verify a conjecture, it will ask the user
 |	yes-no questions concerning further observations.
 *----------------------------------------------------------*/

module identify.
use tcltk.
use tk_alslib.

export start_id/0.
start_id
	:- 
	init_tk_alslib,
	tcl_call(tcli, [source,'twigs_id.tcl'], X).

export reset/0.
reset :-
       abolish(user_observed,1),              	% clear the database of recorded
       abolish(does_not_hold,1),          	% observations
	tcl_call(tcli,[set_unknown],_),
	set_all_buttons(white),
	tcl_call(tcli,[clear_report],_).

export identify/0.
identify :-
	set_prolog_flag(unknown, fail),
       obtain_description,
	!,
       entertain_hypothesis(Identification),  	% these 2 goals are a generate
       validate(Identification),              	% and test loop
       report(Identification),
       abolish(user_observed,1),              	% clear the database of recorded
       abolish(does_not_hold,1).          	% observations

obtain_description :-
	tcl_call(tcli,[obtain_desc],DescArgs),
	DescPreds = [bud_scales, bud_color, terminal_buds,
			outer_scales, twigs],
	map_desc(DescPreds, DescArgs).

map_desc([], []).
map_desc([DP | DescPreds], [DA | DescArgs])
	:-
	check_assert(DA, DP),
	map_desc(DescPreds, DescArgs).

check_assert(unknown, DP) :-!.
check_assert(DA, DP)
	:-
	AA =.. [DP, DA],
	assert(user_observed(AA)),
	assert(orig_observe(AA)).

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
	tcl_call(tcli, [report_id,'Can\'t identify tree'], _),
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
       sprintf(atom(Q),'Is it true that %t?', [Attribute]),
       yes_no_dialog(Q, 'Query', Answer),
       act_on(Answer, Attribute).

act_on(yes, Attribute) :-
       asserta(user_observed(Attribute)),!.             % See below re: asserta
act_on('Yes', Attribute) :-
       asserta(user_observed(Attribute)),!.             % See below re: asserta
act_on(y, Attribute) :-
       asserta(user_observed(Attribute)),!.             % See below re: asserta
act_on(Other, Attribute) :-
       asserta(does_not_hold(Attribute)),!,fail.

% As with check, we use cuts here since we want exactly one of the clauses
% for act_on to be executed exactly once.

report(Identification) :-
	tcl_call(tcli, [report_id,Identification], _),
       bagof(Characteristic, 
		(trait(Characteristic, Identification), 
			not orig_observe(Characteristic) ),
		Traits),
	set_button_color(Traits, red).

/*----------------------------------------------------------------------
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


set_button_color([], Color).
set_button_color([Desc | Traits], Color)
	:-
	Desc =.. [Func , Value],
	catenate(['.tree_id.',Func,'.',Value], Button),
	tcl_call(tcli,[Button,configure,'-selectcolor',Color],_),
	set_button_color(Traits, Color).

set_all_buttons(Color)
	:-
	tcl_call(tcli,[set_all_buttons,Color],_).

endmod.
