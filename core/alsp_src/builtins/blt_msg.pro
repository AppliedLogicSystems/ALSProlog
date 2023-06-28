/*======================================================================
 |			blt_msg.pro
 |		Copyright (c) 1992-96 Applied Logic Systems, Inc.
 |
 |		System error messages and handler
 |
 |	Authors: Ken Bowen
 |	Original Creation Date: April/1992
 *====================================================================*/

module builtins.

:-dynamic(alsdev_running/0).

export prolog_system_error/2.
prolog_system_error(ErrorCode, Args)
	:-
	alsdev_running,
	!,
	prolog_system_error(ErrorCode, alsdev, Args).

prolog_system_error(ErrorCode, Args)
	:-
	prolog_system_error(ErrorCode, alsshell, Args).

export prolog_system_warning/2.
prolog_system_warning(ErrorCode, Args) 
	:-
	alsdev_running,
	!,
	prolog_system_warning(ErrorCode, alsdev, Args).

prolog_system_warning(ErrorCode, Args) 
	:-
	prolog_system_warning(ErrorCode, alsshell, Args).

export expand_code/3.
expand_code(EWCode, Pattern, '\nError: ')
	:-
	error_code(EWCode, Pattern),
	!.

expand_code(EWCode, Pattern, '\nWarning: ')
	:-
	warning_code(EWCode, Pattern).

expand_code(EWCode, Pattern, '')
	:-
	info_code(EWCode, Pattern).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ERROR OUTPUT PREDICATES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export prolog_system_error/3.
prolog_system_error(ErrorCode, Env, Args)
	:-
	prolog_system_error(ErrorCode, Env, Args, error_stream).

export prolog_system_error/4.

	%% For new rt_ reader:
prolog_system_error(
	error(syntax_error,[_,syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream)]),
	Env, [], OutS )
	:-
	prolog_system_error(
		syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream), 
		Env, [], OutS).

prolog_system_error(syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream), 
					Env, _, OutS) 
	:-
	sio:is_stream(Stream,Stream0),
	sio:is_input_stream(Stream0),
	!,
	EType = 'Syntax error ',
	sio:stream_type(Stream0,StreamType),
	sio:stream_name(Stream0,StreamName),
	(StreamType = file ->
		path_directory_tail(StreamName, _, File)
		;	
		File = StreamName
	),
	fin_prolog_syntax_error(Env,Context,File,LineNumber,
							ErrorMessage,EType,OutPattern, OutArgs, OutS).

fin_prolog_syntax_error(_,Context,File,LineNumber,ErrorMessage,EType,
							OutPattern, OutArgs, OutS)
	:-
	printf(error_stream,'\n%s',[Context]),
	OutPattern = '\n\t%t, line %d: %s\n',
	OutArgs = [File,LineNumber,ErrorMessage],
	pse_out(error_stream, EType, OutPattern, OutArgs),
	flush_output(error_stream).

prolog_system_error(s(ErrorCode,Stream), Env, Args, OutS) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	!,
	sio:stream_type(Stream,StreamType),
	sio:stream_name(Stream,StreamName),
	Args = [LineNumber],

	OutPattern = '%t stream: %t : line %d:\n     ',
	OutArgs = [StreamType,StreamName,LineNumber],
	pse_out(OutS, EType, OutPattern, OutArgs),
	printf(OutS,Pattern, Args),
	flush_output(OutS).

prolog_system_error(qc_failed(ErrorCode,Name,LineNumber), Env, Args, OutS) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	!,
	catenate('%t,line %t: ', Pattern, OutPattern),
	OutArgs = [Name,LineNumber | Args],
	pse_out(OutS, EType, OutPattern, OutArgs),
	flush_output(OutS).

prolog_system_error(error(W,L),Env, _, OutS) 
	:-
	decode_error(W, L, Pattern, Args),
	!,
	pse_out(OutS, 'Error: ', Pattern, Args),
	print_error_goal_attributes(L, OutS),
	printf(OutS,'- %-14s %t\n',
		['Throw pattern:',error(W,L)],
		[quoted(true),maxdepth(4),indent(17)]),
	flush_output(OutS).
	
prolog_system_error(ErrorCode, Env, Args, OutS) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	!,
	pse_out(OutS, EType, Pattern, Args),
	flush_output(OutS).

prolog_system_error(ErrorCode, Env, Args, OutS)
	:-
	decode_error(ErrorCode, Args, Pattern, MArgs),
	pse_out(OutS, '', Pattern, MArgs),
	flush_output(OutS).

expand_code(EWCode, Pattern, '\nError: ')
	:-
	error_code(EWCode, Pattern),
	!.

expand_code(EWCode, Pattern, '\nWarning: ')
	:-
	warning_code(EWCode, Pattern).

expand_code(EWCode, Pattern, '')
	:-
	info_code(EWCode, Pattern).

pse_out(Stream, EType, Pattern, Args)
	:-
	printf(Stream, '%t',[EType]),
	printf(Stream, Pattern, Args, [quoted(true), maxdepth(6)]).

print_error_goal_attributes([], OutS) :- !.
print_error_goal_attributes([H|T], OutS) 
	:-
	print_error_goal_attribute(H, OutS),
	print_error_goal_attributes(T, OutS).
print_error_goal_attributes(Other, OutS) 
	:-
	print_error_goal_attribute(Other, OutS).

print_error_goal_attribute(M:G, OutS) 
	:-!,
	printf(OutS,'- %-14s %t\n',
		['Goal:',M:G],[quoted(true),maxdepth(5),indent(17)]),
	flush_output(OutS).

print_error_goal_attribute(Huh, OutS) 
	:-
	functor(Huh,AttribName0,1),
	!,
	arg(1,Huh,AttribValue),
	atom_concat(AttribName0,':',AttribName),
	printf(OutS,'- %-14s %t\n',
			[AttribName,AttribValue], 
			[quoted(true),maxdepth(5),indent(17)]),
	flush_output(OutS).

print_error_goal_attribute(Other, OutS) 
	:-
	printf(OutS,'- %-14s %t\n',
			['Error Attribute:', Other],
			[quoted(true),maxdepth(10),indent(17)]),
	flush_output(OutS).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% WARNING OUTPUT PREDICATES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 	%% Print all advisory and warning messages to
 	%% warning_output stream.

export prolog_system_warning/3.
prolog_system_warning(ErrorCode, Env, Args)
	:-
	prolog_system_warning(ErrorCode, Env, Args, error_stream).

export prolog_system_warning/4.
prolog_system_warning(error(W,L), Env, _, OutS) 
	:-
	decode_error(W, L, Pattern, Args),
	!,
	pse_out(OutS, 'Warning: ', Pattern, Args),
	print_error_goal_attributes(L, OutS),
	flush_output(OutS).

prolog_system_warning(ErrorCode, Env, Args, OutS) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	printf(OutS, '%t',[EType]),
	printf(OutS, Pattern, Args, [quoted(true), maxdepth(9)]),
	flush_output(OutS).


export als_advise/1.
als_advise(FormatString) 
	:-
	als_advise(warning_output, FormatString, []).

export als_advise/2.
als_advise(FormatString, Args) 
	:-
	als_advise(warning_output, FormatString, Args).

export als_advise/3.
als_advise(Stream, FormatString, Args) 
	:-
	printf(Stream, FormatString, Args),
	flush_output(Stream).

%%%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%
%%%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%
%%%
%%%			OUTPUT STRING PATTERNS
%%%
%%%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%
%%%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ERROR OUTPUT STRING PATTERNS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% error_code(as_is(Mess),	Mess) :-!.

error_code(bad_qnt,		'Improper quantifier type: %t\n').
error_code(no_shell,	'No os shell available.\n'). 
error_code(no_handler,	'No handler for Event: %t\nGoal: %t\n').
error_code(stack_over,	'Execution aborted due to stack overflow.\n'). 
error_code(heap_over,	'Execution aborted due to heap overflow.\n').
error_code(bad_goal,	'Improper Goal: %t\n').
error_code(abort,		'Execution aborted.\n').
error_code(uncaught,	'Uncaught throw...\n%t\nExecution aborted.\n').

error_code(xconsult_er, 'Error (x)consulting file %t.\n').
error_code(qf, 			'Query failed: %t\n').
error_code(cf, 			'Command failed.\n').
error_code(rdef_comma,	'Attempt to redefine comma - ignored.\n').
error_code(rdef_semi,	'Attempt to redefine semicolon - ignored.\n').
error_code(rdef_arrow,	'Attempt to redefine arrow - ignored.\n').
error_code(rdef_cut,	'Attempt to redefine cut - ignored.\n').
error_code(forload_er, 	'Loading foreign file %t.\n').
error_code(obpload_er, 	'Loading Prolog obp file %t.\n').
error_code(ld_src_err, 	'Internal error in load_source for file %t\n').
error_code(ld_fail, 	'Unable to load %t in either source or object format!\n').
error_code(no_prev_lev,	'No previous level!!\n').
error_code(no_open_wrt,	'Unable to open file %t for writing!\n').
error_code(no_open_upd,	'Unable to open file %t for update!\n').
error_code(endmods,		'Too many endmods.\n').

error_code(no_pckg_name,'pckgName must have a value\n'). 
error_code(bad_pack_opt,'Bad packaging option: %t.\n').
error_code(datax,		'Data areas exhausted! Package=%t\n').
error_code(xsyms_2big,	'Too many external symbols! Package=%t\n').


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% COMPLEX ERROR RETURNS:
	%% decode_error(W,L,Pattern,Args)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


export decode_error/4.
	%%-------------------------
	%% Instantiation Error:
	%%-------------------------
decode_error(instantiation_error,_,'Instantiation error.\n',[]).

	%%-------------------------
	%% Type Error:
	%%-------------------------
decode_error(type_error(Type,Culprit),_,'Argument of type %s expected instead of %t.\n',
		[Type, Culprit]).

	%%-------------------------
	%% Domain Error:
	%%-------------------------
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

decode_error(domain_error(stream_option_url,Culprit),_,
		'Stream generic (open) option expected instead of %t.\n''%t'' appears to be a Curl option.\nIt should be in the options list in url(__,[<curl options>]).\n\n', [Culprit,Culprit]).

decode_error(domain_error(http_rest_verb,Culprit),_,
		'http rest verb expected instead of %t.\n',[Culprit]).

decode_error(domain_error(curl_option,Culprit),_,
		'curl option expected instead of %t.\n',[Culprit]).

decode_error(domain_error(stream_position,Culprit),_,
		'Stream position expected instead of %t.\n',[Culprit]).

decode_error(domain_error(write_option,Culprit),_,
		'Write option expected instead of %t.\n',[Culprit]).

decode_error(domain_error(Other,Culprit),_,
		'Value in domain %t expected instead of %t\n',[Other,Culprit]).

	%%-------------------------
	%% Existence Error:
	%%-------------------------
decode_error(existence_error(lib_procedure,lib(Module:P/A,FileName)),[Goal],
	'Call %t attempted failed library load of %t:%t/%t from %t\n',
	[Goal,Module,P,A,FileName]).

decode_error(existence_error(procedure,(M:P/A)),[Goal],
	'Undefined procedure %s:%s/%d called.\n',[M,P,A]).

decode_error(existence_error(file,(F)),[Goal],
	'File does not exist: %s.\n',[F]).

decode_error(existence_error(past_end_of_stream,Culprit),_,
		'Attempt made to read past end of stream.\n',[]).

decode_error(existence_error(stream_not_ready, Culprit),_,
		'Stream not ready.\n',[]).

decode_error(existence_error(Type,Culprit), _,
	'Operation attempted on object of type %t which does not exist.\nOperation attempted on: %t.\n',
	[Type,Culprit]).

	%%-------------------------
	%% Permission Error:
	%%-------------------------
decode_error(permission_error(open,source_sink,alias(A)),_,
	'Attempt to open stream with alias \`%t\' which is already in use.\n',
	[A]).

decode_error(permission_error(open,source_sink,reposition(true)),_,
	'Attempt to open non-repositionable stream with option reposition(true).\n',
	[]).

decode_error(permission_error(Operation,Type,Culprit),_,
		'The %s operation is not permitted on the %s object \`%t\'.\n',
		[Operation,Type,Culprit]).

	%%-------------------------
	%% Representation Error:
	%%-------------------------
decode_error(representation_error(Rep),_,
		'Implementation defined limit exceeded for object of type %t.\n',
		[Rep]).

	%%-------------------------
	%% Calculation Error:
	%%-------------------------
decode_error(calculation_error(overflow),_,
		'Overflow detected.\n',[]).

decode_error(calculation_error(underflow),_,
		'Underflow detected.\n',[]).

decode_error(calculation_error(zero_divide),_,
		'Division by zero attempted.\n',[]).

%% FIXME:  message for calculation_error(undefined)
decode_error(calculation_error(undefined),_,
		'Undefined quantity found during calculation. (?)\n', []).

	%%-------------------------
	%% Resource Error:
	%%-------------------------
decode_error(resource_error(Resource),_,
		'Insufficient resource %t to continue execution.\n',[Resource]).
%% FIXME:  Extend syntax error handling here

	%%-------------------------
	%% Syntax Error:
	%%-------------------------
decode_error(syntax_error, _, 'Syntax error.\n',[]).

	%%-------------------------
	%% System Error:
	%%-------------------------
decode_error(system_error, [], 'System error.\n',[]) :-!.
decode_error(system_error, [A | As], 'System error: %t\n', [A | As]).
decode_error(system_error, A, 'System error: %t\n', [A]).

	%%-------------------------
	%% Other Error:
	%%-------------------------

decode_error(_, _, 'Unknown Error.\n', []).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% WARNING OUTPUT STRING PATTERNS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

warning_code(mcflct1,
		'Proc %t/%t in mod %t exported by mod %t (on use list of %t)\n').

warning_code(mcflct2,	
		'Proc %t/%t exported by mods %t & %t\n--both on use list of %t\n').

warning_code(no_file,		'File: %t -- not found\n').

warning_code(old_obp, 		'%t in old or unrecognized .obp format.\n').

warning_code(atmpt_cmp,		'Attempting to compile %t.\n').

warning_code(no_open_fil, 	'Unable to open or create %t.\n').

warning_code(no_obp,		'Loading %t without creating object file.\n').

warning_code(obp_removed,	'%t contained syntax errors.\n\t%t removed.\n').

warning_code(obp_not_removed,
		'%t contained syntax errors.\n\tUnable to remove %t.\n').

warning_code(bad_consult_opt,	'Bad consult options: %t\n').

warning_code(abort_ctlc,	'Aborting from Control-C or Control-Break.\n').

warning_code(exit_ctlc,		'Exiting Prolog from Control-C or Control-Break.\n').


warning_code(mods_open,
		'At end of file %t: following modules are open:\n\t%t\n').

warning_code(lib_act,		'Error activating library: %t %t %t-%t\n').
warning_code(lib_pth,		'Non-existent library path: %t\n').
warning_code(loc_alslib, 	'Can\'t locate ALS library in ALSDIR: %t\n').


info_code(start_consult,	'Consulting %t ... ').
info_code(end_consult,	'done.\n').

info_code(start_shared_load,	'Attemping to load shared object %t ...').
info_code(end_shared_load,	'shared object %t loaded\n').

endmod.
