/*============================================================ *
 |		template_write.pro
 |	Copyright (c) 2004 Applied Logic Systems, Inc.
 |
 |	- Simple template processing using named variables
 |
 | Views any file as a collection of lines which are templates.
 | Variables are text sequences of the form
 |		%BLAHBLAH%
 | where BLAHBLAH is any text sequence not containing the % character.
 | Because of the use of the % character as a variable delimiter,
 | if the % is to occur in text, it must be doubled:  %% .
 |
 | Given one or more files containing such variables, they lead
 | to 'template instantiations' if one conjoins them with an
 | environmental file.  This is a file of equations:
 |		A = B
 | The left side of the equation is an expression which is the
 | name of a variable occurring in one or more of the files to
 | be instantiated; the right side is the value which is subsituted
 | for this variable.
 |
 | Whitespace on either side of the equals sign is ignored, 
 | as is whitespace on the far left or the far right.  The first
 | equation is distinguished; it's left side must the be 
 | expression
 |		output_ext
 | The right side of this equation is taken to be the extension
 | used for all of the files being processed (e.g., 'java').
 |
 | Every variable occuring in any template file must have a
 | value assigned by the environment variable; absence of any
 | such value will terminate processing.
 |
 | The only ill-formed templates are those:
 | a) which contain an odd number of non-double percent (%) characters;
 | b) which contain the subexpression %%%%.
 | The occurrence of an ill-formed template will terminate processing.
 *============================================================ */

:-[strings].

module template_write.

export template_process_lines/3.
export process_template_files/2.
export template_write_files/3.
export template_write_file/3.
export template_write_lines/3.
export template_rewrite/3.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
template_process_lines([], Env, []).
template_process_lines([SchemeLine | SchemeLines], Env, DoneLines)
    :-
    template_rewrite(SchemeLine, Env, ListOfRewrittenLines),
    append(ListOfRewrittenLines, RestDoneLines, DoneLines),
    template_process_lines(SchemeLines, Env, RestDoneLines).

/*!----------------------------------------------------------*
 | process_template_files/2.
 | process_template_files(Files, EnvFile)
 | process_template_files(+, +)
 | 
 | - process a list of template files with an environment file
 |
 | Files is a list of source template files, and EnvFile is
 | an environment file, which is used to process the 
 | template files.
 *-----------------------------------------------------------*/
process_template_files(Files, EnvFile)
	:-
	read_templates_env(EnvFile, Env, Ext),
	template_write_files(Files, Env, Ext).

/*!----------------------------------------------------------*
 | template_write_files/3.
 | template_write_files(Files, Env, Ext).
 | template_write_files(+, +, +).
 |
 | - process a list of template files with an environment and an atom
 |
 | Files is a list of source template files, Env is an
 | environment, which is a list of terms of the form
 | A = B, where A and B are atoms, and Ext is an atom.
 | Each source file F.W on the list files is processed to the
 | file F.Ext by replacing all occurrences of A by B for
 | all equations A = B on Env.
 *-----------------------------------------------------------*/
template_write_files([], _, _).
template_write_files([File | Files], Env, Ext)
	:-
	template_write_file(File, Ext, Env),
	template_write_files(Files, Env, Ext).

/*!----------------------------------------------------------*
 | template_write_file/3.
 | template_write_file(File, OutExt, Env)
 | template_write_file(+, +, +)
 |
 | - process a single template file using an atom and an environment
 |
 | File is the name of a single source template files; Env is an
 | environment, which is a list of terms of the form
 | A = B, where A and B are atoms, and OutExt is an atom.
 | If File is F.W, File is processed to the
 | file F.OutExt by replacing all occurrences of A by B for
 | all equations A = B on Env.
 *-----------------------------------------------------------*/
template_write_file(File, OutExt, Env)
	:-
	exists_file(File),
	!,
	file_extension(File, FileName, _),
	file_extension(OutFile, FileName, OutExt),
	grab_lines(File, SourceLines),
	open(OutFile, write, OS),
	unwind_protect( template_write_lines(SourceLines, Env, OS), close(OS) ).

template_write_file(File, Ext, Env)
	:-
	printf(user_output, 'File %t does not exist ... skipping.\n', [File]).

/*----------------------------------------------------------*
 | template_write_lines/3
 | template_write_lines(SourceLines, Env, OS).
 | template_write_lines(+, +, +).
 |
 | - process a list of source template lines 
 | 
 | SourceLines = a list of source lines (atoms);
 | Env = an environment;
 | OS = an output stream to which the resulting lines are written.
 *-----------------------------------------------------------*/
template_write_lines([], _, _).
template_write_lines([Line | SourceLines], Env, OS)
	:-
	template_rewrite(Line, Env, ListOfRewrittenLineSegments),
%	write_atoms_lists(OS, ListOfRewrittenLineSegments),
	write_lines(OS, ListOfRewrittenLineSegments),
	template_write_lines(SourceLines, Env, OS).

/*----------------------------------------------------------*
 *-----------------------------------------------------------*/
dispatch_catch(template_write_error(Error, TailSegment), Line, Start, Env)
	:-!,
	printf(user_output, 'Error: %t\n', [Error]),
	printf(user_output, 'In line:\n', []),
	printf(user_output, '%t\n', [Line]),
	printf(user_output, 'Occurred before final segment:', []),
	printf(user_output, '%t\n\n', [TailSegment]),
	abort.

dispatch_catch(Ball, Line, Start, Env)
	:-
	throw(Ball).
	
/*----------------------------------------------------------*
 | template_rewrite/3
 | template_rewrite(Line, Env, RewrittenLineSegments)
 | template_rewrite(+, +, -)
 |
 | - process a template line into a list of lists of segments
 |
 | RewrittenLineSegments is the result of breaking Line into
 | segments delineated by the start/end variable delimiters,
 | deleting the delimiters, and replacing each variable
 | name by its value; in the case that Line is exactly
 | a template (%.....%  -- no extra chars outside the %...%),
 | it may possibly be expanded in a list of lists of segments
 | (i.e., it expands into multiple lines).
 *-----------------------------------------------------------*/
template_rewrite('', _, [''])
	:-!.
  %% check for recursive line expansion:
template_rewrite(Line, Env, ListOfRewrittenLineSegments)
	:-
	     %% is line precisely a template var?
	sub_atom(Line, 0, 1, _, '%'), % starts with %
	sub_atom(Line, _, 1, 0, '%'), % ends with %
	sub_atom(Line, 1,_,0, Tail1),
	sub_atom(Tail1, Z, 1, _, '%'),
	atom_length(Tail1, LT1),
	Z is LT1 - 1,                 % no % between first and last
	sub_atom(Tail1, 0, _, 1, VarName),  % get the variable
	dmember(VarName=Expr, Env),   % these is an expr to evaluate 
	!,
	(var(Expr) ->
		sprintf(atom(Error), 'Missing variable value: %t', [VarName]),
		Ball =.. [template_write_error, Error, Line],
	       	dispatch_catch(Ball, Line, 0, Env)
		;
		catch( 
	       		template_list_execute(Expr, Env, ListOfRewrittenLineSegments),
	       		Ball,
	       		dispatch_catch(Ball, Line, 0, Env)  )
	).

    %% Single line (maybe multiple template vars) case:
template_rewrite(Line, Env, [RewrittenLine])
	:-
	template_rewrite_line(Line, Env, RewrittenLineSegments),
	catenate(RewrittenLineSegments, RewrittenLine).


    % if no '%' occurs in Line, 
    % the entire line is the single output segment:
template_rewrite(Line, _, [Line]).


template_rewrite_line('', _, ['']).
template_rewrite_line(Line, Env, RewrittenLineSegments)
	:-
		% find the first occurrence of '%', if one exists:
	sub_atom(Line, Start, 1, _, '%'),
	StartPost is Start + 1,
	sub_atom(Line, StartPost, 1, _, CharAtom),
	catch( 
	       check_continue_rewrite(CharAtom, Start, Line, Env, RewrittenLineSegments),
	       Ball,
	       dispatch_catch(Ball, Line, Start, Env)  ).
template_rewrite_line(Line, _, [Line]).
/*----------------------------------------------------------*
   two cases, according as the % found was doubled or not:
 *-----------------------------------------------------------*/
check_continue_rewrite('%', Start, Line, Env, [InitSeg | RestRewrittenLineSegments])
	:-!,
	StartLen is Start + 1,
	sub_atom(Line, 0, StartLen, _, InitSeg),
	StartLen1 is StartLen + 1,
	sub_atom(Line, StartLen1, _, 0, FinalSeg),
	template_rewrite_line(FinalSeg, Env, RestRewrittenLineSegments).

check_continue_rewrite(_, Start, Line, Env, [InitSeg | RestRewrittenLineSegments])
	:-
	sub_atom(Line, 0, Start, _, InitSeg),
	StartPost is Start + 1,
	sub_atom(Line, StartPost, _, 0, TailSeg),
	find_var(TailSeg, Env, RestRewrittenLineSegments).

/*----------------------------------------------------------*
  try to extract the variable name begun by the % just crossed:
 *-----------------------------------------------------------*/
find_var(Line, Env, RewrittenLineSegments)
	:-
	sub_atom(Line, PcntLcn, 1, _, '%'),
	!,
	sub_atom(Line, 0, PcntLcn, _, VarName),
	PcntLcn1 is PcntLcn + 1,
	sub_atom(Line, PcntLcn1, _, 0, TailSegment),
	finish_find_var(VarName, Env, TailSegment, RewrittenLineSegments).

find_var(Line, Env, RewrittenLineSegments)
	:-
	Error = 'Missing variable closing %',
	Ball = template_write_error(0, 0),
	Ball =.. [template_write_error, Error, Line],
	throw( Ball ).

/*----------------------------------------------------------*
  try to find a value for the variable name just extracted:
 *-----------------------------------------------------------*/
finish_find_var(VarName, Env, TailSegment, [FinalValue | RestRewrittenLineSegments])
	:-
	dmember(VarName=Value, Env),
	nonvar(Value),
	template_execute(Value, Env, FinalValue),
	!,
	template_rewrite_line(TailSegment, Env, RestRewrittenLineSegments).

finish_find_var(VarName, Env, TailSegment, RewrittenLineSegments)
	:-
	sprintf(atom(Error), 'Missing variable value: %t', [VarName]),
	Ball =.. [template_write_error, Error, TailSegment],
	throw( Ball ).


/*----------------------------------------------------------*
 *-----------------------------------------------------------*/
template_execute(Value, Env, Value)
	:-
	atomic(Value),
	!.

template_execute(VV^Expr, Env, FinalValue)
	:-
	call(Expr),
	FinalValue = VV.

%% need error case...

/*----------------------------------------------------------*
 *-----------------------------------------------------------*/

template_list_execute(VV^Expr, Env, VV)
	:-!,
	call(Expr).

template_list_execute(Item, Env, Item).

/*----------------------------------------------------------*
  Given the environment file EnvFile, read the environment Env
  of equations which it contains, and use the first to determine 
  the extension Ext to use for processed files:
 *-----------------------------------------------------------*/
read_templates_env/3.
read_templates_env(EnvFile, Env, Ext)
	:-
	exists_file(EnvFile),
	!,
	grab_eqns(EnvFile, Env),
	get_ext(Env, EnvFile, Ext).

read_templates_env(EnvFile, Env, Ext)
	:-
	printf(user_output, 'File %t does not exist ... quitting.\n', [EnvFile]),
	abort.

/*----------------------------------------------------------*
  Try to extract the file extension Ext from an environment Env:
 *-----------------------------------------------------------*/
get_ext(Env, EnvFile, Ext)
	:-
	dmember(output_ext=Ext, Env),
	!.
get_ext(Env, EnvFile, Ext)
	:-
	printf(user_output, 'Can\'t find output_ext=<value> in %t ... quitting.\n', [EnvFile]),
	abort.

/*----------------------------------------------------------*
 | write_atoms_list/2
 | write_atoms_list(OS, Atoms).
 | write_atoms_list(+, +).
 |
 | - write out a list of atoms
 |
 | Writes the list Atoms of prolog atoms out to the output
 | stream OS with no spacing between the atoms.
 *-----------------------------------------------------------*/
write_atoms_list(OS, []) :- nl(OS).
write_atoms_list(OS, [Atom | Atoms])
	:-
	write(OS, Atom),
	write_atoms_list(OS, Atoms).

write_atoms_lists(OS, []).
write_atoms_lists(OS, [List | Lists])
    :-
    write_atoms_list(OS, List),
    write_atoms_lists(OS, Lists).

/*----------------------------------------------------------*
 *-----------------------------------------------------------*/
grab_eqns(EnvFile, Env)
	:-
	grab_lines(EnvFile, EnvFileLines),
	lines_to_eqns(EnvFileLines, Env).

/*----------------------------------------------------------*
 *-----------------------------------------------------------*/
lines_to_eqns([], []).
lines_to_eqns([Line | EnvFileLines], [Eqn | Env])
	:-
	line_to_eqn(Line, Eqn),
	lines_to_eqns(EnvFileLines, Env).

/*----------------------------------------------------------*
 *-----------------------------------------------------------*/
line_to_eqn(Line, Eqn)
	:-
	sub_atom(Line, EqLcn, 1, _, '='),
	!,
	sub_atom(Line, 0, EqLcn, _, VarName0),
	strip_both_white_atom(VarName0, VarName),
	EqLcn1 is EqLcn + 1,
	sub_atom(Line, EqLcn1, _, 0, VarValue0),
	strip_both_white_atom(VarValue0, VarValue),
	Eqn = (VarName = VarValue).

line_to_eqn(Line, Eqn)
	:-
	(report_bad_eqns ->
		printf(user_output, 'Bad equation line: %t\n', [Line])
		;
		true
	).

endmod.
