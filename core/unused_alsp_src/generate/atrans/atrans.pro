/*=================================================================*
 |		atrans.pro		
 |	Copyright(c) 1990-1995 Applied Logic Systems, Inc.
 |
 |		-- assembly language translator
 |
 | Author: K.A. Buettner
 | Created: 4/3/90
 | Various modifications by P.Raman; I.Cicekli
 *=================================================================*/

:- [avl,aparse,atoken,aexp,alabel].

atrans 
	:-
	command_line(L),
	parse_command_line(L, InName, OutName),
	atrans_msg('Read from file %s\n',[InName]),
%	write('Reading from '), write(InName), nl,
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




atrans_msg(Format,Args)
	:-
	printf(Format, Args).
