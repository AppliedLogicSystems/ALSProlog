/*=================================================================*
 |			pml.pro
 |	Copyright (c) 1999 Applied Logic Systems, Inc.
 |
 |	Operational shell for driving pxml --> html output conversions
 |
 | Authors: Ken Bowen & Chuck Houpt
 *=================================================================*/

module mpml.
use pxml.
:-dynamic(macro/2).
endmod.

module pml.
use pxml.
use mpml.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Outer shell for processing PXML source files
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export xpml/0.
export pml/1.
export pml/2.

xpml :-
	catch( 
	  ( pbi_get_command_line(RawCL),
		pml_cl(RawCL, File, IncludeDirs),
		pml(File, IncludeDirs)       ), 	
	  Ball,
	  ( pml_err_rpt(Ball),
	    halt(2)
      )             ).

pml_err_rpt(pml_error_info(Ball))
	:-!,
	xtr_pml_err_info(Ball,ErrInfoList),
	write(error_stream,'Error:'),nl(error_stream),
	display_errs(ErrInfoList).

pml_err_rpt(Ball)
	:-
	throw(Ball).


xtr_pml_err_info(pml_error_info(Ball), ErrInfoList)
	:-!,
	xtr_pml_err_info(Ball, ErrInfoList).
xtr_pml_err_info(Ball,[Ball])
	:-
	atomic(Ball),
	!.
xtr_pml_err_info(Ball,[Msg | ErrInfoList])
	:-
	functor(Ball, Tag, Arity),
	(Arity = 0 -> 
		Msg = Tag,
		ErrInfoList = []
		;
		arg(1,Ball,A1),
		Msg = (Tag=A1),
		(Arity = 1 ->
			ErrInfoList = []
			;
			arg(2,Ball,A2),
			xtr_pml_err_info(A2,ErrInfoList)
		)
	).

display_errs(ErrInfoList)
	:-
	display_errs(ErrInfoList,error_stream).

display_errs([],_).
display_errs([Msg | ErrInfoList],Stream)
	:-
	write(Stream,Msg),
	nl(Stream),
	display_errs(ErrInfoList,Stream).


	%% Skip over ALS Prolog shell stuff on the command line (go to -p):
pml_cl(['-p' | RestCL], File, IncludeDirs) 
	:-!,
	xpml_cl(RestCL, File, IncludeDirs).
pml_cl([_ | RestCL], File, IncludeDirs) 
	:- 
	pml_cl(RestCL, File, IncludeDirs).

xpml_cl([], File, IncludeDirs)
	:- 
	throw(cl_err(no_args)).
xpml_cl(['-inc'], File, IncludeDirs)
	:- 
	throw(cl_err(no_args)).
xpml_cl(RawCL, File, IncludeDirs) 
	:-
	x0pml_cl(RawCL, Files, IncludeDirs),
	(Files = [] ->
		throw(cl_error(no_file))
		;
		(Files = [File] -> true ; throw(cl_error(too_many_files)))).

x0pml_cl([], [], []).

x0pml_cl(['-inc', Path | RawCL], Files, [Path | IDs])
	:-!,
	x0pml_cl(RawCL, Files, IDs).

x0pml_cl(['-mm', CLMacroAtom | RawCL], Files, IDs)
	:-!,
	atomread(CLMacroAtom, CLMacro),
	mpml:assert(CLMacro),
	x0pml_cl(RawCL, Files, IDs).

x0pml_cl([File | RawCL], [File | Files], IDs)
	:-
	x0pml_cl(RawCL, Files, IDs).


pml(File)
	:-
	pml(File, []).

pml(File, IncludeDirs)
	:-
	pml_process_file(File, IncludeDirs, DataOut),
	record_dependencies(DataOut).
	
pml_process_file(File, IncludeDirs, DataOut)
	:-
	pml_process_file(File, [includes=IncludeDirs],OptsOut, [],DataOut).

pml_process_file(File, OptsIn,OptsOut, DataIn,DataOut)
	:-
	(Path = File ; 
		dmember(includes=SearchDirs, OptsIn),
		find_include_file(SearchDirs, File, Path)
	),
	exists_file(Path),
	!,
	open(Path, read, InS, []),
	catch(unwind_protect(
	   		  pml_process_stream(InS, OptsIn,OptsOut, [include=Path | DataIn], DataOut),
	   		  close(InS) ),
		   Ball,
		   (copy_term( pml_error_info(file(File, Ball)), PMLErr),
		    throw(PMLErr) )
		   ).

pml_process_stream(InS, OptsIn, OptsOut, DataIn, DataOut)
	:-
	read(InS, Term),
	((nonvar(Term), Term = end_of_file) ->
		OptsIn = OptsOut, DataIn=DataOut
		;
%printf(user_output,'--processing: %t\n', [Term], [maxdepth(3)]),
		pml_process_term(Term, InS, OptsIn,OptsInter, DataIn,DataInter),
	    pml_process_stream(InS, OptsInter,OptsOut, DataInter,DataOut)
	).

pml_process_term(include(File), InS, OptsIn,OptsOut, DataIn,DataOut)
	:-!,
	pml_process_term(include=File, InS, OptsIn,OptsOut, DataIn,DataOut).

pml_process_term(include=File, InS, OptsIn,OptsIn, DataIn,DataIn)
	:-
	dmember(include=File, DataIn), !.

pml_process_term(include=File, InS, OptsIn, OptsIn, DataIn,DataIn)
	:-
	include_file_loaded(File),
	!.

pml_process_term(include=File, InS, OptsIn, OptsOut, DataIn,DataOut)
	:-!,
	pml_process_file(File, OptsIn,OptsOut, DataIn,DataOut),
	assert(include_file_loaded(File)).
		    

pml_process_term(target(Target,Term), InS, 
			OptsIn,OptsIn, DataIn,[DEP | DataIn])
	:-
	open_target(Target, OutS),
	catch( unwind_protect(mpml2xml([Term], OutS), close(OutS)),
		   Ball,
		   (copy_term( pml_error_info(target(Target, Ball)), PMLErr),
		    throw(PMLErr) )
		   ),
	!,
	findall(IF, member(include=IF, DataIn), CurIFs),
	DEP = (deps=[target=Target | CurIFs]).
pml_process_term(target(Target,Term), InS, 
			OptsIn,OptsIn, DataIn,[DEP | DataIn])
	:-
	copy_term( pml_error_info(target(Target, failure_processing(Target))), PMLErr),
	throw(PMLErr).

pml_process_term((module mpml), InS, OptsIn,OptsIn, DataIn,DataIn) :-!.
pml_process_term(endmod, InS, OptsIn,OptsIn, DataIn,DataIn) :-!.
pml_process_term(Term, InS, OptsIn,OptsIn, DataIn,DataIn)
	:-
	mpml:assert(Term).
/*
	(check_unique_macro(Term) ->
		mpml:assert(Term)
		;
		true
	).
*/

find_include_file([], File, FilePath)
	:-
	copy_term( pml_error_info(file(File, 'can\'t locate'(File))), PMLErr),
	throw(PMLErr).
find_include_file([Dir | SearchDirs], File, FilePath)
	:-
	join_path([Dir, File], FilePath).

open_target(stream(S), S) :-!.
open_target(file(File), OutS)
	:-
	open(File, write, OutS, []).
open_target(File, OutS)
	:-
	atom(File),
	open(File, write, OutS, []).

check_unique_macro((macro(In, _) :- _))
	:-
	mpml:clause(macro(In, _), _, Ref),
	!,
	fail.
%	throw(pml_error(macro_exists(In))).
check_unique_macro(macro(In, _))
	:-
	mpml:clause(macro(In, _), _, Ref),
	!,
	fail.
%	throw(pml_error(macro_exists(In))).
check_unique_macro(_).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Dependencies
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

record_dependencies(DataOut)
	:-
	findall(DL, member(deps=DL, DataOut), DepsLists),
	!,
	record_d_files(DepsLists).
record_dependencies(_).

record_d_files([]).
record_d_files([Deps | DepsLists])
	:-
	record_d(Deps),
	record_d_files(DepsLists).

record_d(Deps)
	:-
	dmember(target=Tgt, Deps),
	expand_deps(Deps, [], XDeps),
	write_d_file(Tgt, XDeps).

expand_deps([], XDeps, XDeps).
expand_deps([DepF | Deps], CurDeps, XDeps)
	:-
	x_dep(DepF, CurDeps, InterDeps),
	expand_deps(Deps, InterDeps, XDeps).

x_dep(target=_, CurDeps, CurDeps)
	:-!.
x_dep(DepF, CurDeps, CurDeps)
	:-
	dmember(DepF, CurDeps),
	!.
x_dep(DepF, CurDeps, [DepF | CurDeps]).

write_d_file(file(Tgt), Deps)
	:-!,
	write_d_file(Tgt, Deps).

write_d_file(Tgt, Deps)
	:-
	atom(Tgt),
	file_extension(Tgt, Base, Ext),
	!,
	file_extension(DTgt, Base, d),
	write_d_file(DTgt, Tgt, Deps).
write_d_file(file(Tgt), Deps)
	:-
	atom(Tgt),
	!,
	file_extension(DTgt, Tgt, d),
	write_d_file(DTgt, Tgt, Deps).
write_d_file(_, _).

write_d_file(DTgt, Tgt, Deps)
	:-
	open(DTgt, write, OS, []),
	printf(OS, '%t: ', [Tgt]),
	write_d_deps(Deps, OS),
	close(OS).

write_d_deps([], OS) 
	:-
	nl(OS).
write_d_deps([Dep | Deps], OS)
	:-
	printf(OS, '%t ', [Dep]),
	write_d_deps(Deps, OS).

endmod.
