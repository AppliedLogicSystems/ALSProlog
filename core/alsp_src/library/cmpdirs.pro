/*===========================================================================*
 |		cmpdirs.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	-- compares two directory trees, producing a printed report
 |
 |  If OldDir and NewDir are (absolute) paths to two directories, and if
 |	OutFile is (a path to) an output file, the call
 |
 |		cmpdirs(OldDir, NewDir, OutFile)
 |
 |	recursively traverses the two trees, producing a node-by-node
 |	comparison; each pair of dirs visiting is noted, and:
 |		i)	It lists items in the old dir, but not in the corresponding new;
 |		ii)	It lists items in the new dir, but not in the old;
 |		iii)If there are no differences, no further comment is made;
 |
 |	For the call
 |		cmpdirs('/mailbox3/alsp_src','/mailbox3/nk',nkexamine)
 |
 |	the report appears as follows:
 |
 |		Dirs:  alsp_src [old]  nk [new]
 |		 -Old {not new} items: [mailshar,powerpc,README,sharbld,mips]
 |		 -New {not old} items: [bld-llib,tools]
 |		    Dirs:  vax [old]  vax [new]
 |		        Dirs:  vms [old]  vms [new]
 |		    Dirs:  hppa [old]  hppa [new]
 |		    Dirs:  rs6000 [old]  rs6000 [new]
 |		    Dirs:  utils [old]  utils [new]
 |		    Dirs:  doc [old]  doc [new]
 |		    Dirs:  cinterf [old]  cinterf [new]
 |		     -New {not old} items: [ARCHIVE]
 |		        Dirs:  Doc [old]  Doc [new]
 |		        Dirs:  c2pro [old]  c2pro [new]
 |		            Dirs:  Examples [old]  Examples [new]
 |		                Dirs:  CVS [old]  CVS [new]
 |		        Dirs:  deps [old]  deps [new]
 |		        Dirs:  pro2intf [old]  pro2intf [new]
 |		         -Old {not new} items: [p2i.obp]
 |		    Dirs:  i386 [old]  i386 [new]
 |		     -Old {not new} items: [planning]
 |
 |	Author: Ken Bowen
 |	Date:	28 Aug 94
 *=================================================================================*/

module builtins.

export cmpdirs/3.
/*!-----------------------------------------------------------------------
 |	cmpdirs/3
 |	cmpdirs(Old, New, TgtFile)
 |	cmpdirs(+, +, +)
 |
 |	- recursively compare directories, writing report to TgtFile
 |
 |	Recursively traverses the two directory trees, producing a node-by-node
 |	comparison; each pair of dirs visiting is noted, and:
 |		i)	It lists items in the old dir, but not in the corresponding new;
 |		ii)	It lists items in the new dir, but not in the old;
 |		iii)If there are no differences, no further comment is made;
 |
 |	A partial example of the report appears as follows:
 |
 |		Dirs:  alsp_src [old]  nk [new]
 |		 -Old {not new} items: [mailshar,powerpc,README,sharbld,mips]
 |		 -New {not old} items: [bld-llib,tools]
 |		    Dirs:  vax [old]  vax [new]
 |		        Dirs:  vms [old]  vms [new]
 *-----------------------------------------------------------------------*/
cmpdirs(OldDesc, NewDesc, TgtFile)
	:-
	open(TgtFile,write,TgtS,[]),
	rootPathFile(_,OldPath,Old,OldDesc),
	rootPathFile(_,NewPath,New,NewDesc),
	cmpdirs([Old-New], TgtS, '',OldPath,NewPath),
	close(TgtS).

cmpdirs([], TgtS, _,_,_).

	%% Note Old,New will usually be the same name; but they live in different
	%% directory trees:
cmpdirs([Old-New | Stack], TgtS, CurInd,OldPath,NewPath)
	:-!,
	rootPathFile('',OldPath,Old,CompOld),
	file_status(CompOld, OldStatus),

	rootPathFile('',NewPath,New,CompNew),
	file_status(CompNew, NewStatus),
	cont_cmpdirs(OldStatus, NewStatus, Old, New, TgtS, CurInd, OldPath, NewPath),
	!,
	cmpdirs(Stack, TgtS, CurInd,OldPath,NewPath).

cmpdirs([CmnName | Stack], TgtS, CurInd,OldPath,NewPath)
	:-
	cmpdirs([CmnName-CmnName | Stack], TgtS, CurInd,OldPath,NewPath).


cont_cmpdirs(Status,Status,Old,New,TgtS,CurInd,OldPath,NewPath)
	:-
	dmember(type=regular, Status),
	!.

cont_cmpdirs(Status,Status,Old,New,TgtS,CurInd,OldPath,NewPath)
	:-!,
	dmember(type=directory, Status),
	proceed_dirs(Old,New,TgtS,CurInd,OldPath,NewPath).

cont_cmpdirs(OldStatus,NewStatus,Old,New,TgtS,CurInd,OldPath,NewPath)
	:-
	dmember(type=directory, OldStatus),
	dmember(type=directory, NewStatus),
	!,
	proceed_dirs(Old,New,TgtS,CurInd,OldPath,NewPath).

cont_cmpdirs(OldStatus,NewStatus,Old,New,TgtS,CurInd,OldPath,NewPath)
	:-
	dmember(type=regular, OldStatus),
	dmember(type=regular, NewStatus),
	!.

cont_cmpdirs(OldStatus,NewStatus,Old,New,TgtS,CurInd,OldPath,NewPath)
	:-
	printf(TgtS,'%t!!!TYPE MISMATCH: %t [old]  %t [new] !!!\n',[CurInd,Old,New]).

proceed_dirs(Old,New,TgtS,CurInd,OldPath,NewPath)
	:-
	append(OldPath,[Old], XOldPath),
	rootPathFile('',XOldPath,'*',OldPattern),
	directory(OldPattern,[regular,directory], InitOldDirMems),
	list_diff(InitOldDirMems, ['.','..'], OldDirMems),

	append(NewPath,[New], XNewPath),
	rootPathFile('',XNewPath,'*',NewPattern),
	directory(NewPattern,[regular,directory], InitNewDirMems),
	list_diff(InitNewDirMems, ['.','..'], NewDirMems),

	cmp_dir_lists(OldDirMems, NewDirMems, Common, OldNotNew, NewNotOld),
	printf(TgtS, '%tDirs:  %t [old]  %t [new]\n',[CurInd,Old,New]),
	rpt_dirs(OldNotNew, NewNotOld, TgtS, CurInd),
	indent_step(IS),
	catenate(CurInd,IS, NextInd),
	cmpdirs(Common, TgtS, NextInd,XOldPath,XNewPath).

cmp_dir_lists(OldDirMems, NewDirMems, Common, OldNotNew, NewNotOld)
	:-
	int_diff(OldDirMems,  NewDirMems, Common, OldNotNew),
	list_diff(NewDirMems, Common, NewNotOld).

indent_step('    ').

rpt_dirs([], [], TgtS, CurInd)
	:-!.

rpt_dirs(OldNotNew, NewNotOld, TgtS, CurInd)
	:-
	(OldNotNew \= [] ->
		printf(TgtS,'%t -Old {not new} items: %t\n',[CurInd,OldNotNew]) ; true ),

	(NewNotOld \= [] ->
		printf(TgtS,'%t -New {not old} items: %t\n',[CurInd,NewNotOld]) ; true ).



endmod.
