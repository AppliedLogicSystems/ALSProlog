/*
 * atrans.pro		-- assembly language translator
 *	Copyright(c) 1990-1994 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Created: 4/3/90
 */


:- [avl,aparse,atoken,aexp,alabel].

atrans :-
	command_line(L),
	parse_command_line(L, InName, OutName),
	write('Reading from '), write(InName), nl,
	tell(OutName),
	read_file(InName),
	told,
	write(OutName), write(' written to.'),nl,nl.

parse_command_line(L, InName, OutName) :-
	pcl(L, InName, OutName),
	(var(InName) -> printf('Error: no input file specified\n',[]), halt
	             ; true),
	(var(OutName) -> printf('Error: no output file specified\n',[]), halt
	              ; true).

pcl([], _, _) :-
	!.
pcl(['-o', Name | More], InName, OutName) :-
	(nonvar(OutName) -> printf('Error: multiple -o options\n',[]), halt
	                 ;  true),
	!,
	Name = OutName,
	pcl(More, InName, OutName).
pcl([IncOpt | More], InName, OutName) :-
	atom_concat('-I',IncDir,IncOpt),
	!,
	parser:assertz(incdir(IncDir)),
	pcl(More, InName, OutName).
pcl([Name | More], InName, OutName) :-
	(nonvar(InName) -> printf('Error: more than one input file specified\n',[]), halt
	                ;  true),
	!,
	Name = InName,
	pcl(More, InName, OutName).
