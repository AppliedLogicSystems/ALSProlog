/*======================================================================
 |			blt_msg.pro
 |		Copyright (c) 1992-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		System error messages and handler
 |
 |	Authors: Ken Bowen
 |	Original Creation Date: April/1992
 *====================================================================*/

module builtins.

export prolog_system_error/2.

%%
%% New rt_ reader
%%
prolog_system_error(syntax(Context,ErrorMessage,LineNumber,Stream), Args) :-
	sio:is_stream(Stream,Stream0),
	sio:is_input_stream(Stream0),
	!,
	EType = 'Syntax error',
	sio:stream_type(Stream0,StreamType),
	sio:stream_name(Stream0,StreamName),
	(StreamType = file ->
		pathPlusFile(_,File,StreamName)
	;	File = StreamName
	),
	printf(error_stream,'\n%s\n%t: %t, line %d: %s\n',
			[Context,EType,File,LineNumber,ErrorMessage]),
	flush_output(error_stream).

prolog_system_error(s(EWCode,Stream), Args) :-
	(error_code(EWCode, Pattern),!,EType='Error' ;
		warning_code(EWCode, Pattern), EType='Warning'),
	!,
	sio:stream_type(Stream,StreamType),
	sio_linenumber(Stream,LineNumber),
	sio:stream_name(Stream,StreamName),
	printf(error_stream, '\n%t: %t stream %t,line %t:\n     ',
			[EType,StreamType,StreamName,LineNumber]),
	printf(error_stream,Pattern, Args),
	flush_output(error_stream).

prolog_system_error(qc_failed(ErrTag,Name,LineNumber),Args) :-
	(error_code(ErrTag, Pattern),!,EType='Error' ;
		warning_code(ErrTag, Pattern), EType='Warning'),
	!,
	printf(error_stream, '\n%t: %t,line %t: ',[EType,Name,LineNumber]),
	printf(error_stream,Pattern, Args),
	flush_output(error_stream).

prolog_system_error(error(W,L),_) :-
	decode_error(W,L,Pattern,Args),
	!,
	printf(error_stream, '\nError: ',[]),
	printf(error_stream, Pattern, Args, [quoted(true),maxdepth(6)]),
	print_error_goal_attributes(L),
	printf(error_stream,'- %-14s %t\n',
		['Throw pattern:',error(W,L)],
		[quoted(true),maxdepth(4),indent(17)]),
	flush_output(error_stream).
	
prolog_system_error(ErrorCode, Args) :-
	error_code(ErrorCode, Pattern),
	!,
	printf(error_stream, '\nError: ',[]),
	printf(error_stream,Pattern, Args,[quoted(true), maxdepth(9)]),
	flush_output(error_stream).

prolog_system_error(WarningCode, Args) :-
	warning_code(WarningCode, Pattern),
	printf(warning_output, '\nWarning: ',[]),
	printf(warning_output,Pattern, Args,[quoted(true), maxdepth(9)]),
	flush_output(error_stream).

error_code(as_is(Mess),	Mess) :-!.

error_code(bad_qnt,	'Improper quantifier type: %t\n').
error_code(no_pckg_name,'pckgName must have a value\n'). 
error_code(no_shell,	'No os shell available.\n'). 
error_code(no_handler,	'No handler for Event: %t\nGoal: %t\n').
error_code(stack_over,	'Execution aborted due to stack overflow.\n'). 
error_code(heap_over,	'Execution aborted due to heap overflow.\n').
error_code(bad_goal,	'Improper Goal: %t\n').
error_code(abort,	'Execution aborted.\n').
error_code(uncaught,	'Uncaught throw...\n%t\nExecution aborted.\n').

error_code(xconsult_er, 'Error (x)consulting file %t.\n').
error_code(qf, 		'Query failed.\n').
error_code(cf, 		'Command failed.\n').
error_code(rdef_comma,	'Attempt to redefine comma - ignored.\n').
error_code(rdef_semi,	'Attempt to redefine semicolon - ignored.\n').
error_code(rdef_arrow,	'Attempt to redefine arrow - ignored.\n').
error_code(forload_er, 	'Loading foreign file %t.\n').
error_code(proload_er, 	'Loading Prolog file %t.\n').
error_code(ld_src_err, 	'Internal error in load_source for file %t\n').
error_code(ld_fail, 	'Unable to load %t in either source or object format!\n').
error_code(no_prev_lev,	'No previous level!!\n').
error_code(no_open_wrt,	'Unable to open file %t for writing!\n').
error_code(no_open_upd,	'Unable to open file %t for update!\n').
error_code(endmods,		'Too many endmods.\n').

error_code(bad_pack_opt,'Bad packaging option: %t.\n').

error_code(datax,	'Data areas exhausted! Package=%t\n').
error_code(xsyms_2big,	'Too many external symbols! Package=%t\n').


warning_code(mcflct1,
	'Proc %t/%t in mod %t exported by mod %t (on use list of %t)\n').
warning_code(mcflct2,	
	'Proc %t/%t exported by mods %t & %t\n--both on use list of %t\n').
warning_code(no_file,	'File: %t -- not found\n').
warning_code(old_obp, 	'%t in old or unrecognized .obp format.\n').
warning_code(atmpt_cmp,	'Attempting to compile %t.\n').
warning_code(no_open_fil, 'Unable to open or create %t.\n').
warning_code(no_obp,	'Loading %t without creating object file.\n').
warning_code(obp_removed,	
			'%t contained syntax errors.\n\t%t removed.\n').
warning_code(obp_not_removed,
	'%t contained syntax errors.\n\tUnable to remove %t.\n').
warning_code(abort_ctlc,'Aborting from Control-C or Control-Break.\n').
warning_code(exit_ctlc,	'Exiting Prolog from Control-C or Control-Break.\n').
warning_code(mods_open,
		'At end of file %t: following modules are open:\n\t%t\n').



warning_code(nyi, '%t not yet implemented on %t.\n').


%%
%% decode_error(W,L,Pattern,Args)
%%

decode_error(instantiation_error,_,'Instantiation error.\n',[]).
decode_error(type_error(Type,Culprit),_,'Argument of type %s expected instead of %t.\n',
		[Type, Culprit]).
decode_error(domain_error(character_code_list,Culprit),_,
		'Character code list expected instead of %t.\n',[Culprit]).
decode_error(domain_error(character_list,Culprit),_,
		'Character list expected instead of %t.\n',[Culprit]).
decode_error(domain_error(close_option,Culprit),_,
		'Close option expected instead of %t.\n',[Culprit]).
decode_error(domain_error(flag_value,Culprit),_,
		'Flag value expected instead of %t.\n',[Culprit]).
decode_error(domain_error(io_mode,Culprit),_,
		'I/O mode (read, write, etc) expected instead of %t.\n',[Culprit]).
decode_error(domain_error(not_less_than_zero,Culprit),_,
		'Non-negative value expected instead of %t.\n',[Culprit]).
decode_error(domain_error(operator_priority,Culprit),_,
		'Invalid operator priority: %t.\n',[Culprit]).
decode_error(domain_error(operator_specifier,Culprit),_,
		'Invalid operator specifier: %t.\n',[Culprit]).
decode_error(domain_error(prolog_flag,Culprit),_,
		'Prolog flag expected instead of %t.\n',[Culprit]).
decode_error(domain_error(read_option,Culprit),_,
		'Read option expected instead of %t.\n',[Culprit]).
decode_error(domain_error(source_sink,Culprit),_,
		'Filename or other source/sink expected instead of %t.\n',
		[Culprit]).
decode_error(domain_error(stream_or_alias,Culprit),_,
		'Stream or alias expected instead of %t.\n',[Culprit]).
decode_error(domain_error(stream_option,Culprit),_,
		'Stream (open) option expected instead of %t.\n',[Culprit]).
decode_error(domain_error(stream_position,Culprit),_,
		'Stream position expected instead of %t.\n',[Culprit]).
decode_error(domain_error(write_option,Culprit),_,
		'Write option expected instead of %t.\n',[Culprit]).
decode_error(domain_error(Other,Culprit),_,
		'Value in domain %t expected instead of %t\n',[Other,Culprit]).
decode_error(existence_error(past_end_of_stream,Culprit),_,
		'Attempt made to read past end of stream.\n',[]).
decode_error(existence_error(stream_not_ready, Culprit),_,
		'Stream not ready.\n',[]).
decode_error(existence_error(Type,Culprit), _,
	'Operation attempted on object of type %t which does not exist.\nOperation attempted on: %t.\n',
	[Type,Culprit]).
decode_error(permission_error(open,source_sink,alias(A)),_,
	'Attempt to open stream with alias \`%t\' which is already in use.\n',
	[A]).
decode_error(permission_error(open,source_sink,reposition(true)),_,
	'Attempt to open non-repositionable stream with option reposition(true).\n',
	[]).
decode_error(permission_error(Operation,Type,Culprit),_,
		'The %s operation is not permitted on the %s object \`%t\'.\n',
		[Operation,Type,Culprit]).
decode_error(representation_error(Rep),_,
		'Implementation defined limit exceeded for object of type %t.\n',
		[Rep]).
decode_error(calculation_error(overflow),_,
		'Overflow detected.\n',[]).
decode_error(calculation_error(underflow),_,
		'Underflow detected.\n',[]).
decode_error(calculation_error(zero_divide),_,
		'Division by zero attempted.\n',[]).
%% FIXME:  message for calculation_error(undefined)
decode_error(calculation_error(undefined),_,
		'Undefined quantity found during calculation. (?)\n', []).
decode_error(resource_error(Resource),_,
		'Insufficient resource %t to continue execution.\n',[Resource]).
%% FIXME:  Extend syntax error handling here
decode_error(syntax_error, _, 'Syntax error.\n',[]).
decode_error(system_error, _, 'System error.\n',[]).

print_error_goal_attributes([]) :- !.
print_error_goal_attributes([H|T]) :-
	print_error_goal_attribute(H),
	print_error_goal_attributes(T).

print_error_goal_attribute(M:G) :-
	!,
	printf(error_stream,'- %-14s %t\n',
		['Goal:',M:G],[quoted(true),maxdepth(5),indent(17)]),
	flush_output(error_stream).

print_error_goal_attribute(Huh) :-
	functor(Huh,AttribName0,1),
	!,
	arg(1,Huh,AttribValue),
	atom_concat(AttribName0,':',AttribName),
	printf(error_stream,'- %-14s %t\n',
			[AttribName,AttribValue], 
			[quoted(true),maxdepth(5),indent(17)]),
	flush_output(error_stream).

print_error_goal_attribute(Other) :-
	printf(error_stream,'- %-14s %t\n',
			['Error Attribute:', Other],
			[quoted(true),maxdepth(10),indent(17)]),
	flush_output(error_stream).


/*
 * Print all advisory and warning messages to
 * warning_output stream.
 */

export als_advise/1.
als_advise(FormatString) 
	:-
	als_advise(FormatString, []),
	flush_output.

export als_advise/2.
als_advise(FormatString, Args) 
	:-
	printf(warning_output, FormatString, Args),
	flush_output(warning_output).

/*
 * Print the given consult message if the "consultmessage" flag is on.
 */

consultmessage(Message, ParameterList) 
	:-
	consultmessage,
	!,
	als_advise(Message, ParameterList).
consultmessage(_,_).

endmod.
