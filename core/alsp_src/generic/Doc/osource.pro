%
% This is the old source code extracter (decompiler).
% It has been saved in case anyone wants to see the basic idea if and
% when the system can take advantage of indexing.
%
% Written by Keith Hughes 12/88
%
% Applied Logic Systems
%

%
% The s/1 interrupt is used as the entry point in the interrupt code for
% the source code extracter. It used used for entering a clause for the
% first time.
%

% Going into a clause for the first time.
$int(s(ForReal),_,Goal) :-
	$source1(ForReal,[],Goal).
% Catch a failure that means $source is failing all the way out.
$int(s(_),_,_) :-
		% See if ultimate failure has been requested.
	getMagic(fail(OldMagic)),
		% Set back to old value so can work the next time.
	setMagic(OldMagic),
		% And do it.
	!,fail.
% $source has backtracked and we need to see if there is another matching
% clause in the procedure.
$int(s(ForReal),_,_) :- !,
		% Set interrupt for entry into a clause.
	setMagic(s(ForReal)),
		% And fail into it.
	ocall(fail).

%
% For s0/2 interrupt, we are in the middle of source extracting a clause.
%

$int(s0(ForReal,Goals),_,Goal) :- !,
	$source1(ForReal,Goals,Goal).

% When the source extracter was called, a variable was given which is used to
% identify the end of extraction. This variable is ForReal, and the $endSource
% atom will have this variable as its value when the extracter is brought
% to an end. Here we check it. This is done so that we can extract the
% extracter.
$source1(ForReal,_,$endSource(Check)) :-
	Check == ForReal,!.
% Not at the end.
$source1(ForReal,Goals,Goal) :-
		% Set s0/2 interrupt with the latest goal tacked on.
	setMagic(s0(ForReal,[Goal|Goals])),
		% Since the continuation pointer points into the clause
		% that is being extracted, all we have to do is return from
		% this one without starting up the sub-goal we have just
		% extracted, after setting up the next interrupt.
	resetint,
	ouch.

%
% The s/3 interrupt is used as the entry point in the interrupt code for
% the source code extracter with DBRefs. It used used for entering a clause for
% the first time.
%

$int(s(ForReal,Current,Next),_,Goal) :- !,
	$source2(ForReal,Current,Next,Goal).

%
% For s0/3 interrupt, we are in the middle of source extracting a clause.
%

$int(s0(ForReal,Goals,DBRef),_,Goal) :- !,
	$source1(ForReal,Goals,Goal,DBRef).

$source2(ForReal,Current,_,Goal) :-
	$source1(ForReal,[],Goal,Current).
$source2(_,_,0,_) :- !,
	fail.
$source2(ForReal,_,Next,_) :-
	$next_clause(Next,After),!,
	setMagic(s(ForReal,Next,After)),
		% And fail into it.
	ocall(fail).
$source2(ForReal,_,Next,_) :-
	setMagic(s(ForReal,Next,0)),
		% And fail into it.
	ocall(fail).

% When the source extracter was called, a variable was given which is used to
% identify the end of extraction. This variable is ForReal, and the $endSource
% atom will have this variable as its value when the decompiler is brought
% to an end. Here we check it. This is done so that we can extract the
% extracter.
$source1(ForReal,_,$endSource(Check),_) :-
	Check == ForReal,!.
% Not at the end.
$source1(ForReal,Goals,Goal,DBRef) :-
		% Set s/5 interrupt with the latest goal tacked on.
	setMagic(s0(ForReal,[Goal|Goals],DBRef)),
		% Since the continuation pointer points into the clause
		% that is being extracted, all we have to do is return from
		% this one without starting up the sub-goal we have just
		% extracted, after setting up the next interrupt.
	resetint,
	ouch.

%
% The source extracter (I though decompiler was no longer appropriate) when
% DBRef's are not required. This means we can take advantage of indexing.
%

export $source/3.

$source(Module,Head,Clause) :-
	getMagic(Magic),
		% Source of the ForReal variable used to mark the end
		% of extraction.
	$source0(ForReal,Module,Head,Clause,Magic).

$source0(ForReal,Module,Head,Clause,OldMagic) :- 
		% Set the s/2 interrupt.
	setMagic(s(ForReal)),
		% Start extracting.
	ocall(Module,Head),
		% This goal is never run, but is used to stop the extracter.
	$endSource(ForReal),
	getMagic(S),
		% Make the clause to be returned.
	fixBody(S,Head,Clause),
		% Reset old magic value.
	setMagic(OldMagic).
% $source has been failed all the way. Have to cleanup s/2 interrupt pending
% from the last clause of the extracted procedure.
$source0(ForReal,_,_,_,OldMagic) :-
			% Stop the extracter. This goal is never run.
	$endSource(ForReal),
			% Got to clean up choice point in s/2 interrupt.
	setMagic(fail(OldMagic)),
	fail.
	
fixBody(s(_),Head,Head) :- !.
fixBody(s0(_,[First|Rest]),Head,(Head :- Body)) :-
	xformGoal(First,XFirst),
	fixBody0(Rest,XFirst,Body).

fixBody0([],Last,Last) :- !.
fixBody0([Current|Rest],SoFar,Body) :-
	xformGoal(Current,XCurrent),
	fixBody0(Rest,(XCurrent,SoFar),Body).

%
% Get rid of cut closures.
%

xformGoal('!'(A),!) :- !.
xformGoal(call(A,_),call(A)) :- !.
xformGoal(';'(A,S,_),(A;S)) :- !.
xformGoal(','(A,S,_),(A,S)) :- !.
xformGoal(A,A).

%
% The source extracter when DBRefs are required. This means we can't take
% advantage of indexing.
%

export $source/4.

$source(Module,Head,Clause,DBRef) :-
	getMagic(Magic),
		% Make fake head (with only variables) and get the
		% DBRef for the first clause in the procedure.
	functor(Head,F,A),
	functor(Fake,F,A),
	$first_clause(Module,F,A,First),
		% Source of the ForReal variable used to mark the end
		% of extraction.
	$source0(ForReal,First,Module,Fake,Clause,DBRef),
		% Make sure the head matches.
	Head = Fake,
		% Reset old magic value.
	setMagic(Magic). 

$source0(ForReal,First,Module,Head,Clause,DBRef) :- 
	$next_clause(First,Next),!,
	$source0(ForReal,First,Next,Module,Head,Clause,DBRef).
$source0(ForReal,First,Module,Head,Clause,DBRef) :- 
	$source0(ForReal,First,0,Module,Head,Clause,DBRef). 

$source0(ForReal,First,Next,Module,Head,Clause,DBRef) :- 
		% Set the s/3 interrupt.
	setMagic(s(ForReal,First,Next)),
		% Start extracting.
	ocall(Module,Head),
		% This goal is never run, but is used to stop the extracter.
	$endSource(ForReal),
		% Get the extracted source.
	getMagic(S),
		% Make the clause to be returned.
	fixBody(S,Head,Clause,DBRef).

fixBody(s(_,DBRef,_),Head,Head,DBRef) :- !.
fixBody(s0(_,[First|Rest],DBRef),Head,(Head :- Body),DBRef) :-
	xformGoal(First,XFirst),
	fixBody0(Rest,XFirst,Body).

%
% Clause, and that ilk.
%

:- $icode(-24,clause,2,clause,0).

export clause/3.

clause(Module,Head,Body) :-
	$source(Module,Head,Clause),
	clause0(Clause,Body).

export clause/4.

clause(Module,Head,Body,DBRef) :-
	$source(Module,Head,Clause,DBRef),
	clause0(Clause,Body).

clause0((Head :- Body),Body) :- !.
clause0(Head,true).

%
% retract and family.
%

% Do dat module closure thing.
:- $icode(-24,abolish,2,abolish,0).
:- $icode(-24,retract,1,retract,0).

export retract/2.

retract(Module,Clause) :-
	retract0(Clause,Head),
	$source(Module,Head,Clause,DBRef),
	$erase(DBRef).

retract0((Head :- _), Head) :- !.
retract0(Head,Head).
