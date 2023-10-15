/*============================================================================*
                                load_csv.pro


 *============================================================================*/

:-['sqlite3_intf.pro'].

module csv.

export input_csv_line/3.

input_csv_line('', Delim, [])
	:-!.

	%% A line or line tail starting with double quote (")
	%% Suck in field up to matching " at end; may have
	%% embedded doubled double quotes ("")
input_csv_line(LineAtom, Delim, [LeftField | RestFieldList])
	:-
	sub_atom(LineAtom, 0, 1, _, '"'),
	!,
	sub_atom(LineAtom, 1, _, 0, LineAtomTail),
	input_quoted_field(LineAtomTail, Delim, FieldSubFieldList, LineTail),
	catenate(FieldSubFieldList, LeftField),
	input_csv_line(LineTail, Delim, RestFieldList).

	%% A line or line tail NOT starting with double quote (")
input_csv_line(LineAtom, Delim, [LeftField | RestFieldList])
	:-
		%% split at the next <Delim> field boundary
	atom_split(LineAtom, Delim, LeftField, RightTail),
	!,
	input_csv_line(RightTail, Delim, RestFieldList).

	%% None of above apply, so LineAtom is just an atom not 
	%% contatining Delim or double quote ("); so it's
	%% the right-most field in the input line:
input_csv_line(LineAtom, Delim, [LineAtom])
	:-!.

	%% LineAtom is a segment of a line following an 
	%% initial double quote ("); look for the 
	%% next double quote ("):
input_quoted_field(LineAtom, Delim, FieldSubFieldList, LineTail)
	:-
	atom_split(LineAtom, '"', LeftField, RestLineAtom),
	!,
	cont_input_quoted_field(LeftField, Delim, RestLineAtom, FieldSubFieldList, LineTail).

	%% Something really wrong - should be a double quote (") inside 
	%% the field, or at the very end
input_quoted_field(LineAtom, Delim, _, _)
	:-
	printf('Something wrong: Missing double quote " in %t\n', [Delim, LineAtom]).

	%% Did we split on a final wrapping double quote ("), (in which case it should 
	%% be followed by a Delim, or the line is empty after it) or on an embedded 
	%% one (in which case it should have been doubled), so there should be 
	%% another double quote (") right after it:

	%% Case: the double quote (") which was split on is at the right end of
	%% the line; after it is empty:
cont_input_quoted_field(LeftField, Delim, '', FieldSubFieldList, '')
	:-!,
	FieldSubFieldList = [LeftField].

	%% Case: the char right after the split " is also ":
	%% So LeftField here is a subfield of the larger leftmost field
	%% we are currently collecting
cont_input_quoted_field(LeftField, Delim, RestLineAtom, FieldSubFieldList, LineTail)
	:-
	sub_atom(RestLineAtom, 0, 1, _, '"'),
	!,
	sub_atom(RestLineAtom, 1, _, 0, RestLineAtomTail),
	cont_input_quoted_field(RestLineAtomTail, Delim, [LeftField | FieldSubFieldList], LineTail).

	%% Case: the char right after the split " is NOT ";
	%% There must be chars after the split (clause #1 of cont_input_quoted_field above);
	%% So the split " above is the terminal of a "-wrapped field,
	%% The char right after it must be Delim:
	%% so LeftField is the last subfield of a Delim-wrapped field:
cont_input_quoted_field(LeftField, Delim, RestLineAtom, [LeftField], LineTail)
	:-
	sub_atom(RestLineAtom, 0, 1, _, Delim),
	sub_atom(RestLineAtom, 1, _, 0, LineTail).
	

%%-----
u :- Delim = ',', 
%	LineAtom = California,CA,2016,F,"Some college credit, but not a degree",4,47094,28.4,3239.1
	LineAtom = '"College credit, not degree",4,47094,28.4,3239.1',
	input_csv_line(LineAtom, Delim, FieldList),
	write(fL=FieldList),nl.

u1:- Delim = ',', 
	LineAtom = 'foo,"College credit, not degree",4,47094,28.4,3239.1',
	input_csv_line(LineAtom, Delim, FieldList),
	write(fL=FieldList),nl.
%%-----




endmod.
