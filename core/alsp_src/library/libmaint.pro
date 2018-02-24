/*==================================================================
 |		libmaint.pro
 |	Copyright (c) 1991-93 Applied Logic Systems, Inc.
 |
 |   Tools for maintaining a library, including the ALS Library
 |
 |	Date: October, 1991
 |	Author: Ken Bowen
 |	Revised: May 1993
 *==================================================================*/

module builtins.

export install_lib/0.

/*!-----------------------------------------------------------------
 | install_lib/0
 | install_lib
 |
 |	- installs a library file(s) specified on the command line
 |
 *!----------------------------------------------------------------*/
install_lib
   :-
   builtins:command_line(CmdLine),
   find_file_names(CmdLine,FileNames),
	builtins:sys_searchdir(SD),
	name(SD,SDCs),
	dappend(SDCs,"builtins/blt_lib.pro",BLCs),
	name(BLPath,BLCs),
	file_status(BLPath,BLStatus),
	dmember(mod_time=BLTime,BLStatus),
	dappend(SDCs,"library",LPCs),
	name(LibPath,LPCs),
   install_libs(FileNames, BLTime, LibPath, BLPath),
   als_advise('Library file(s) %t.pro installed in ALS Library\n',[FileNames]).

	%% Need this here for booting the library:
find_file_names([],[]).
find_file_names([One, Two | CmdLine], FileNames)
	:-
	sub_atom(One, 0, 1, _, '-'),
	!,
	find_file_names(CmdLine,FileNames).
find_file_names([File | CmdLine], [File FileNames])
	:-
	find_file_names(CmdLine,FileNames).

install_libs([],_,_,_).
install_libs([File | FileNames],BLTime,LibPath,BLPath)
	:-
	install_lib_file(File,BLTime,LibPath,BLPath),
	install_libs(FileNames,BLTime,LibPath,BLPath).

install_lib_file(File,BLTime,LibPath,BLPath)
	:-
	file_status(File,FileStatus),
	dmember(mod_time=FileTime,FileStatus),
	install_lib_file(FileTime,File,BLTime,LibPath,BLPath).

install_lib_file(FileTime,File,BLTime,LibPath,BLPath)
	:-
	FileTime=< BLTime,
	!,
   als_advise('Library file %t.pro earlier than %t .. no action\n',[File,BLPath]).
install_lib_file(FileTime,File,BLTime,LibPath,BLPath)
	:-
    als_advise('Processing library file %t.pro\n',[File]),
	install_lib_file(File, LibPath, BLPath).
		










/*!-----------------------------------------------------------------
 |  install_lib/1
 |  install_lib(FileName)
 |  install_lib(+)
 |
 |	- installs the specified library file(s).
 |
 |  If the argument is a list, recursively calls install_lib/1 on
 |  each element of the list.
 |
 |  If the argument is a file name (no ext, path allowed),
 |  performs all necessary work to incorporate the predicates defined
 |  in FileName.pro as part of the ALS Library, including:
 |  1.  Adding FileName to the master list in file Lib_Files;
 |  2.  Reads and analyzes FileName.pro and does the following:
 |      a.  Installs information about the exported predicates, the
 |          module, etc., in a file FileName.lib;  this file is
 |          in Prolog-readable format as a list of file names;
 |      b.  Installs information about the predicates in FileName.pro
 |          in a file manual_info.FileName, suitable as a starting
 |          point for building FileName.tex, the (top level) of the
 |          manual Library pages for this part of the library;
 |      c.  Appends the appropriate "lib_load"-style definitions
 |          of the exported predicates from FileName.pro in the
 |          builtins file blt_lib.pro;
 |      d.  Installs copies of FileName.pro and FileName.obp
 |          in the library subdirectory under alsdir.
 *!----------------------------------------------------------------*/
/*
install_lib([]) :-!.
install_lib([InputFileName | Files])
    :-!,
	install_lib(InputFileName),
	install_lib(Files).

install_lib(InputFileName)
    :-
	lib_location(Disk, ALSDIR_Path, LibPath),
    full_source_file_path(InputFileName,LibPath,Disk,FileName,
						  SourceFile, BltLibFile),
	sys_lib_man_path(LibPath,Disk, FileName, LibManFile),
	sys_lib_info_path(LibPath,Disk,FileName,LibInfoFile),
	install_lib(FileName,Disk,LibPath,SourceFile,
				LibManFile,LibInfoFile,BltLibFile).
*/
install_lib_file(File, LibPath, BLPath)
	:-
	(file_extension(FileName, _, File) ->
		true
		;
		FileName = File
	),
	file_extension(FileName, man, ManFileName),
%	extendPath(LibPath, man, ManPath),
%	pathPlusFile(ManPath, ManFileName, LibManFile),
	split_Path(LibPath, LibPathElts),
	dappend(LibPathElts, [man,ManFileName], MFElts),
	join_path(MFElts, LibManFile).

install_lib(FileName,Disk,LibPath,SourceFile,LibManFile,LibInfoFile,BltLibFile)
	:-
	create_lib_info(SourceFile, FileName,LibInfo),

			%% create the library "man" file:
	create_lib_man_file(SourceFile, LibManFile, LibInfo, GroupsInfo),

			%% record the source file name for the librarian:
	add_to_libfiles(FileName, LibInfoFile),

			%% get path to blt_lib.pro:
	file_extension(blt_lib,pro,BltLibFileName),
	rootPathFile(Disk,ALSDIR_Path,BltLibFileName,BltLibFile),

			%% Save all prev info in blt_lib.pro:
%	collect_lib_load_info(BltLibFile, PrevLibInfo),

			%% create the new blt_lib.pro:
	add_to_blt_lib(BltLibFile, FileName, Disk, ALSDIR_Path, ExportList, 
	               ModuleClosures, Module, LibraryKey, PrevLibInfo).

install_lib(InputFileName)
	:-
	lib_error(unknown(InputFileName)).

/*!-----------------------------------------------------------------
 |  install_lib/2
 |  install_lib(FileList,SourceDir)
 |  install_lib(+,+)
	 |
 |	- installs the specified library file(s) located in SourceDir
 |
 |	If FileList is a list of files found in SourceDir, installs
 |	each of the files in the library.
 *!----------------------------------------------------------------*/
install_lib(InputFiles,SourceDir)
	:-
	rootPlusPath(Disk,Path,SourceDir),
	install_lib0(InputFiles,Disk,Path).

install_lib0([],_,_).
install_lib0([InputFileName | RestInputFiles],Disk,Path)
	:-
	rootPathFile(Disk,Path,InputFileName, SourceFile),
	install_lib(SourceFile),
	install_lib0(RestInputFiles,Disk,Path).

export bootLib/0.
bootLib
	:-
	install_lib([listutl1, listutl2, listutl3, misc_io, strings, 
				 lib_ctl, filemisc, filepath],
				 '/elvis/prolog/Utils').

special_alsdir("/elvis/prolog/Merge3/Builtins").

/*-----------------------------------------------------------------
 |  install_lib_dir/1
 |  install_lib_dir(SourceDir)
 |  install_lib_dir(+)
 |
 |	If SourceDir is a directory, installs all the *.pro file found
 |	in SourceDir into the library.
 *----------------------------------------------------------------*/
export install_lib_dir/1.
install_lib_dir(SourceDir)
	:-
	files(SourceDir,'*.pro', FileList),
	install_lib(FileList, SourceDir).


/*!------------------------------------------------
 |	lib_location/3
 |	lib_location(Disk,ALSDIR_Path,LibPath)
 |	lib_location(+,+,+)
 |
 |	- returns location of the ALS Library
 |
 |	Disk - the disk on which the library resides
 |	ALSDIR_Path - the (abstract) path to ALSDIR
 |	LibPath - the (abstract) path to the library (below ALSDIR)
 *!-----------------------------------------------*/
lib_location(Disk,ALSDIR_Path,LibPath,BltLibFile)
	:-
	(builtins:'$getenv'("ALSDIR",DirPathCs) ->
		true;
		(special_alsdir(DirPathCs) ->
			true;
			write('Can''t find an ALSDIR environment variable!'),nl,
			write('Please type the complete ALSDIR path (in single quotes):'),nl,
			read(ALSDIR_atom),
			name(ALSDIR_atom, DirPathCs)
		)
	),
	builtins:rootPlusPath0(DiskCs,PathCs,DirPathCs),
	name(Disk,DiskCs),
	builtins:subPath1(ALSDIR_Path,PathCs),
	dappend(ALSDIR_Path,['Library'],LibPath),
			%% get path to blt_lib.pro:
	file_extension(blt_lib,pro,BltLibFileName),
	rootPathFile(Disk,ALSDIR_Path,BltLibFileName,BltLibFile).


/*------------------------------------------
 *-----------------------------------------*/
full_source_file_path(InputFileName,LibPath,Disk,FileName,
					  SourceFile,BltLibFile)
    :-
	lib_location(Disk, ALSDIR_Path, LibPath,BltLibFile),
	(rootPathFile(ISDisk,ISPath,SFile,InputFileName) ->
		true;
		pathPlusFile(ISPath,SFile,InputFileName)
	),
	(file_extension(FileName,FileExt,SFile) ->
		true
		;
		FileName = SFile, FileExt = pro
	),
	file_extension(FileName,FileExt,TheFile),
			%% Analyze the path name of the source file:
	(var(ISDisk) ->
		get_current_drive(CurrentDrive),
		SDisk = CurrentDrive;
		SDisk = ISDisk
	),
	(ISPath = [] ->
		get_cwd(CurrentPath),
		subPath(SPath, CurrentPath);
		SPath = ISPath
	),
	rootPathFile(SDisk,SPath,TheFile,SourceFile),

	(exists_file(SourceFile) ->
        true;
        printf('Source file %t does not exist.\n',[SourceFile]),
        fail
    ).
/*

		%% Link or copy the real source file into
		%% the current directory (TargetFile)
	((SPath = []; SPath = ['.']; 
	 (same_path(SPath, LibPath), same_disk(SDisk, Disk)))
		->
		true;
		rootPathFile(Disk,LibPath,[FileName,pro],TargetFile),
    	(make_links_to_source ->
        	name(SourceFile, SFCs),
			rootPlusPath(Disk,LibPath,FullLibPath),
			name(FullLibPath,FLPCs),
        	dappend("ln -s ", SFCs, Tmp0cs),
        	dappend(Tmp0cs, [0' | FLPCs], LnkCmd),
	    	(system(LnkCmd) ->
				printf('Installed Link from Library to %t.\n',[SourceFile]);
				printf('Can't install link from Library to %t.\n',[SourceFile]),
            	printf('Copying %t to %t\n',[SourceFile,TargetFile]),
		    	copy_file(SourceFile,TargetFile)
        	)
        	;
        	printf('Copying %t to %t\n',[SourceFile,TargetFile]),
			copy_file(SourceFile,TargetFile)
		)
    ).
*/


/*------------------------------------------
 *-----------------------------------------*/
collect_lib_load_info(BltLibFile, PrevLibInfo)
	:-
	see(BltLibFile),
	collect_lib_load(PrevLibInfo),
    !,
	seen.
collect_lib_load_info(_, [])
	:-
	seen.

collect_lib_load(PrevLibInfo)
	:-
	read(Item),
	disp_collect_lib_load(Item, PrevLibInfo).

disp_collect_lib_load(end_of_file, []) :-!.
disp_collect_lib_load(Item, [Item | RestPrevLibInfo])
	:-
	(functor(Item, lib_load_info, _) ;
	 functor(Item, (:-), 1)
	),
	!,
	collect_lib_load(RestPrevLibInfo).
disp_collect_lib_load(Item, PrevLibInfo)
	:-
	collect_lib_load(PrevLibInfo).

/*!------------------------------------------
 |	add_to_libfiles/3
 |	add_to_libfiles(FileName, Disk, LibInfoDirPath)
 |	add_to_libfiles(+, +, +)
 |	
 |	- adds the new lib file name to the list of library files
 |	
 |	The librarian uses the file [<LibDir>,info,'libfiles.pro']
 |	to save a list of names of all files which have been installed
 |	as library files.  The format of libfiles.pro is
 |		[file1,file2,....].
 |	where the filei are the filenames, without extension. This predicate
 |	simply reads the list from this file (creates the file if it
 |	doesn't exist), appends the new file at the end, and overwrites
 |	the existing file, writing the extended list into it.
 *!-----------------------------------------*/
sys_lib_info_path(LibPath,Disk,FileName,LibInfoFile)
	:-
	file_extension(libfiles,pro,ListFile),
	dappend(LibPath, [info],LibInfoPath),
	rootPathFile(Disk, LibInfoPath, ListFile, LibInfoFile),
	(exists_file(LibInfoFile) ->
   		true;
		open(LibInfoFile,write,NewLLFileStream,[]),
		write_clause(NewLLFileStream,[],[]),
		close(NewLLFileStream)
	).

add_to_libfiles(FileName, LibInfoFile)
   :-
	open(LibInfoFile,read,LLFileReadStream,[]),
	read_term(LLFileReadStream,OldLibFileList,[]),
	close(LLFileReadStream),
	sort([FileName | OldLibFileList], NewLibFileList),
	open(LibInfoFile,write,LLFileWriteStream,[]),
	write_clause(LLFileWriteStream,NewLibFileList),
	close(LLFileWriteStream).

/*------------------------------------------
 *-----------------------------------------*/
create_lib_info(SourceFile, FileName, 
				[
				 file_name=FileName,
				 exports=ExportList,
				 module=Module,
				 module_closures=ModuleClosures,
				 lib_key=LibraryKey
			    ])
   :-
   see(SourceFile),
   lib_analyze(ExportList, ModuleClosures,Module, LibraryKey),
   seen,
   (var(Module) ->
      Module = user; true),
   (var(LibraryKey) ->
      LibraryKey = FileName; true
	).

/*------------------------------------------
 *-----------------------------------------*/
lib_analyze(ExportList, ModuleClosures,Module, LibraryKey)
   :-
   read(Item),
   disp_lib_analyze(Item,ExportList, ModuleClosures,Module, LibraryKey).

disp_lib_analyze(end_of_file,[], [], Module, LibraryKey) :-!.

disp_lib_analyze((export What),[What | ExportList], 
                 ModuleClosures,Module, LibraryKey)
   :-!,
   lib_analyze(ExportList, ModuleClosures,Module, LibraryKey).

disp_lib_analyze((module What),ExportList, 
                 ModuleClosures,Module, LibraryKey)
   :-!,
   (Module = What ->
      lib_analyze(ExportList, ModuleClosures,Module, LibraryKey);
	  cur_lib_file(CurLibFile),
      lib_error(multi_module(CurLibFile)) ).

disp_lib_analyze(library_key(What),ExportList, 
                 ModuleClosures,Module, LibraryKey)
   :-!,
   (LibraryKey = What ->
      lib_analyze(ExportList, ModuleClosures,Module, LibraryKey);
	  cur_lib_file(CurLibFile),
      lib_error(multi_key(CurLibFile))  ).

disp_lib_analyze((:- Command),ExportList, 
                 ModuleClosures,Module, LibraryKey)
   :-!,
   extract_module_closures(Command, ModuleClosures, RestModuleClosures),
   lib_analyze(ExportList, RestModuleClosures, Module, LibraryKey).

disp_lib_analyze(_,ExportList, ModuleClosures,Module, LibraryKey)
   :-
   lib_analyze(ExportList, ModuleClosures,Module, LibraryKey).

/*------------------------------------------
 *-----------------------------------------*/
extract_module_closures((A,B), ModuleClosures, RestModuleClosures)
	:-!,
	extract_module_closures(A, ModuleClosures, InterModuleClosures),
	extract_module_closures(B, InterModuleClosures, RestModuleClosures).
extract_module_closures(Command, 
						[Command | RestModuleClosures], 
						RestModuleClosures)
	:-
	functor(Command, module_closure, _),
	!.
extract_module_closures(_, ModuleClosures, ModuleClosures).

/*------------------------------------------
 *-----------------------------------------*/
note_exports([],_).
note_exports([Pred | ExportList],LibraryKey)
	:-
	write_clause(exports(LibraryKey, Pred)),
	Pred = PredName/Arity,
	write_clause(lib_member(PredName, Arity, LibraryKey)),
	note_exports(ExportList,LibraryKey).

/*------------------------------------------
 *-----------------------------------------*/
sys_lib_man_path(LibPath,Disk,FileName,LibManFile)
	:-
	dappend(LibPath,[man], LibManDirPath),
	file_extension(FileName,man,LibManFileName),
	rootPathFile(Disk, LibManDirPath, LibManFileName, LibManFile).

create_lib_man_file(SourceFile, LibManFile, LibInfo, GroupsInfo)
	:-
	dmember(file_name=FileName, LibInfo),
	dmember(exports=ExportList, LibInfo),
	dmember(module=Module, LibInfo),
	dmember(lib_key=LibraryKey, LibInfo),

	see(SourceFile),
	open(LibManFile,write,LibManFileStream,[alias(LibManFile)]),

	lib_man_info(LibManFileStream, FileName, 
				 LibraryKey, ExportList, Module,
				 DocumentedPredsList, GroupsInfo),
	!,
	seen, close(LibManFileStream),
	check_exports_docs(ExportList, DocumentedPredsList).

create_lib_man_file(_, _, _, _)
   :-
   told,seen.

/*------------------------------------------
 *-----------------------------------------*/
lib_man_info(Stream, FileName, LibraryKey, ExportList, Module,
			 DocumentedPredsList, GroupsInfo)
	:-
	write_clause(Stream,key=LibraryKey),nl(Stream),
	sort(ExportList, SortedExportList),
	assemble_documentation(NamesList, FormsList,  DescriptionsList, 
	                       DocumentedPredsList, GroupsInfo),
	write_documentation(Stream, NamesList, FormsList, 
                    DescriptionsList, DocumentedPredsList).

/*------------------------------------------
 *-----------------------------------------*/
lib_names([]).
lib_names([P | SortedExportList])
	:-
	write_clause(P-''),
	lib_names(SortedExportList).

/*------------------------------------------
 *-----------------------------------------*/
check_exports_docs(ExportList, DocumentedPredsList)
	:-
	list_diffs(ExportList,DocumentedPredsList,ExportSide,DocSide),
	report_exports_docs(ExportSide, DocSide).

report_exports_docs(ExportSide, DocSide)
	:-
	report_exp_nodoc(ExportSide),
	report_doc_noexp(DocSide).

report_exp_nodoc([])
	:-!,
	printf('All exported predicates are documented.\n').
report_exp_nodoc(ExportSide)
	:-
	printf('The following are exported, but not documented:\n\t%t\n',
		   [ExportSide]).

report_doc_noexp([])
	:-!,
	printf('All documented predicates are exported.\n').
report_doc_noexp(DocSide)
	:-
	printf('The following are documented, but not exported:\n\t%t\n',
		   [DocSide]).

/*------------------------------------------
 *-----------------------------------------*/
add_to_blt_lib(BltLibFile, FileName, Disk, ALSDIR_Path, ExportList, 
               ModuleClosures, Module, LibraryKey, PrevLibInfo)
   :-
   tell(BltLibFile),
   write_prev_items_list(PrevLibInfo, LibraryKey),

		%% Don't need any module enclosure, since the file (blt_lib.pro)
		%% is consulted from inside builtins.pro, so the builtins module
		%% is open when that happens, and the lib_load_info/4 fact will
		%% end up in module builtins.
   name(FileName,FileNameCs),
   write_lib_file_info(FileNameCs,LibraryKey,ExportList,
   		               ModuleClosures, Module),

   told.
add_to_blt_lib(_, _, _, _, _, _)
   :-
   seen,told.

/*------------------------------------------
 *-----------------------------------------*/
write_lib_file_info(FileNameCs,LibraryKey,ExportList,
		               ModuleClosures, Module)
   :-
   		%% Change the arity of the exported module closures:
   mod_close_revise(ExportList, ModuleClosures, DefnList),

   		%% Note: pass DefnList for the "retractions" argument
		%% of lib_load_info, because it is the mod_close-expanded
		%% predicate which will actually be executed; it must
		%% be what is "lib_load-defined" and it is this defn
		%% which must be retracted.
   write_clause(lib_load_info(LibraryKey, FileNameCs, DefnList, Module)),

   		%% Don't declare module if Module = builtins, since all
		%% this is happening inside module builtins:
   (Module \= builtins ->
   		write('module '),write(Module),put(0'.),nl;
		true
   ),

   write_mod_closures(ModuleClosures,LibraryKey),
   write_exports_list(ExportList),nl,

   		%% Note: pass DefnList to write_lib_loads, since it is the 
		%% mod_close-expanded predicate which will actually be executed; 
		%% it must be what is "lib_load-defined" to cause loading of
		%% the library file:
   write_lib_loads(DefnList, LibraryKey),

   (Module \= builtins ->
   		write('endmod.'),nl,nl;
		true
   ).

/*------------------------------------------
 *-----------------------------------------*/
mod_close_revise([], _, []).
mod_close_revise([P/A | RestExportList], ModuleClosures, 
                 [P0/A0 | RestDefnList])
	:-
	rev_mod_close(P,A,ModuleClosures,P0,A0),
	mod_close_revise(RestExportList, ModuleClosures, RestDefnList).

rev_mod_close(P,A,ModuleClosures,P0,A0)
	:-
	dmember(module_closure(P,A,P0),ModuleClosures), !,
	A0 is A + 1.

rev_mod_close(P,A,ModuleClosures,P,A0)
	:-
	dmember(module_closure(P,A),ModuleClosures), !,
	A0 is A + 1.

	%% default - no change:
rev_mod_close(P,A,_,P,A).

/*------------------------------------------
 *-----------------------------------------*/
write_mod_closures([],_) :-!. 
write_mod_closures(ModuleClosures,LibraryKey)
	:-
	write(':-'), 
	write(LibraryKey=LibraryKey),put(0',),
	write_list_no_brackets(ModuleClosures),
	nl,nl.

write_list_no_brackets([]).
write_list_no_brackets([Item])
	:-!,
	write(Item),put(0'.),nl.
write_list_no_brackets([Item | Tail])
	:-
	write(Item),put(0',),nl,
	write_list_no_brackets(Tail).

/*------------------------------------------
 *-----------------------------------------*/
write_prev_items_list([], _).
write_prev_items_list([Info | RestPrevLibInfo], LibraryKey)
	:-
	Info = lib_load_info(ThisLibraryKey,FileNameCs,ExportList,Module),
	!,
	(ThisLibraryKey = LibraryKey ->
		true;
		write_lib_file_info(FileNameCs,ThisLibraryKey,ExportList, [], Module)
	),
	write_prev_items_list(RestPrevLibInfo, LibraryKey).
write_prev_items_list([Info | RestPrevLibInfo], LibraryKey)
	:-
	Info = (:- (ThisLibraryKey=ThisLibraryKey, _)),
	(LibraryKey = ThisLibraryKey ->
		true;
		write_clause(Info)
	),
	write_prev_items_list(RestPrevLibInfo, LibraryKey).

/*------------------------------------------
 *-----------------------------------------*/
write_exports_list([]).
write_exports_list([Pred | ExportList])
	:-
	write(' export '),write(Pred),put(0'.),nl,
	write_exports_list(ExportList).


/*------------------------------------------
 *-----------------------------------------*/
write_lib_loads([], _).
write_lib_loads([P/A | DefnList], LibraryKey)
	:-
	functor(Head, P, A),
	write_clause( (Head :- lib_load(Head,LibraryKey) ) ),
	write_lib_loads(DefnList, LibraryKey).

%% goes to misc_io.pro
copy_file(FileName,TargetFile)
	:-
	see(FileName),
	tell(TargetFile),
	pump_chars,
	!,
	told, seen.
copy_file(FileName,TargetFile)
	:-
	told, seen.

pump_chars
	:-
	get0(C),
	disp_pump_chars(C).

disp_pump_chars(C)
	:-
	eof_char(C),!.
disp_pump_chars(C)
	:-
	put(C),
	pump_chars.

eof_char(-1).

%% goes to misc_io.pro:
readline(Result)
	:-
	get0(C),
	(eof(C) ->
		Result = end_of_file;
		disp_readline_tail(C, Result)
	).

readline_tail(CurTail)
	:-
	get0(C),
	disp_readline_tail(C, CurTail).

disp_readline_tail(C, CurTail)
	:-
	eof_char(C),!, CurTail = end_of_file.

disp_readline_tail(C, CurTail)
	:-
	eoln_char(C),!, CurTail = [].

disp_readline_tail(C, [C | NextTail])
	:-
	readline_tail(NextTail).

eof_char(-1).
eoln_char(13).
eoln_char(10).

		/*-----------------------------------------
		 |    LIBRARY DOCUMENTATION PROCESSING
		 *----------------------------------------*/
/*------------------------------------------
 *-----------------------------------------*/
search_lib_comment(FirstLine)
   :-
   search_lib_comment_init,
   skip_to_next(NextLine),
   strip_leader(NextLine, PotentialFirstLine, [0' ,9,0'|,0'*]),
   (PotentialFirstLine = [0'! | _] ->
		FirstLine = end_of_comment;
		FirstLine = PotentialFirstLine
	).

skip_to_next(NextLine)
	:-
	readline(VeryNextLine),
	disp_skip_to_next(VeryNextLine,NextLine).

disp_skip_to_next(end_of_file,end_of_file) :-!.
disp_skip_to_next([],NextLine)
	:-!,
	readLine(NextLine).
disp_skip_to_next(NextLine,NextLine).

/*------------------------------------------
 *-----------------------------------------*/
search_lib_comment_init
	:-
	readline(Line),
	disp_search_lib_comment_init(Line).

disp_search_lib_comment_init(end_of_file) :-!.
disp_search_lib_comment_init([0'/, 0'*, 0'! | _]) :-!.
disp_search_lib_comment_init(_)
	:-
	search_lib_comment_init.


/*------------------------------------------
 *-----------------------------------------*/
strip_leader([],[],_).
strip_leader([C | RestL],Tail,Skips)
	:-
	dmember(C,Skips),!,
	strip_leader(RestL,Tail,Skips).
strip_leader(Tail,Tail,_).

/*------------------------------------------
 *-----------------------------------------*/
get_doc(Doc)
	:-
	search_lib_comment(FirstLine),
	(dmember(FirstLine, [end_of_file,end_of_comment]) ->
		Doc = empty_doc;
		get_pred_doc(FirstLine, Doc)
	).

/*------------------------------------------
 *-----------------------------------------*/
get_pred_doc(FirstLine, Doc)
	:-
	bufread(FirstLine, ReadResult),
	((ReadResult = [ErrMsg | Pos],integer(Pos)) ->
		PredDesc = '??'/'?',
		cur_lib_file(CurLibFile),
		lib_error(parse(CurLibFile, FirstLine, ErrMsg, Pos));
		ReadResult = [PredDesc | Vars]
	),
	get_header_doc(HeaderDoc),
	(dmember(end_of_comment, HeaderDoc) ->
		BriefDescrip = end_of_comment;
		get_brief_descrip(BriefDescrip)
	),
	(BriefDescrip = end_of_comment ->
		ExtendedDescrip = end_of_comment;
		get_extended_descrip(ExtendedDescrip)
	),
	(HeaderDoc = [TextPredDesc |  CallingForm_IOPatterns] ->
		true;
		TextPredDesc = '??',
		CallingForm_IOPatterns = []
	),
	!,
	Doc = doc(PredDesc, (TextPredDesc, BriefDescrip), 
	          CallingForm_IOPatterns, [TextPredDesc | ExtendedDescrip]).

/*------------------------------------------
 *-----------------------------------------*/
get_header_doc(HeaderDoc)
	:-
	readline(NextLine),
	strip_leader(NextLine, StrippedNextLine, [0' ,9,0'|,0'*]),
	disp_get_header_doc(StrippedNextLine, HeaderDoc).

disp_get_header_doc([], []) :-!.
disp_get_header_doc([0'! | _], [end_of_comment]) :-!.
disp_get_header_doc(StrippedNextLine, 
					[StrippedNextLine | RestHeaderDoc])
	:-
	get_header_doc(RestHeaderDoc).

/*------------------------------------------
 *-----------------------------------------*/
get_brief_descrip(BriefDescrip)
	:-
	readline(NextLine),
	strip_leader(NextLine, StrippedNextLine, [0' ,9,0'|,0'*, 0'-]),
	disp_get_brief_descrip(StrippedNextLine, BriefDescrip).

disp_get_brief_descrip([], BriefDescrip)
	:-!,
	get_brief_descrip(BriefDescrip).
disp_get_brief_descrip(BriefDescrip, BriefDescrip).

/*------------------------------------------
 *-----------------------------------------*/
get_extended_descrip(ExtendedDescrip)
	:-
	readline(NextLine),
	strip_leader(NextLine, StrippedNextLine, [0' ,9,0'|,0'*]),
	disp_get_extended_descrip(StrippedNextLine,ExtendedDescrip).

disp_get_extended_descrip(StrippedNextLine,[])
	:-
	StrippedNextLine = [0'! | _], !.

disp_get_extended_descrip(StrippedNextLine,
                          [StrippedNextLine | RestExtendedDescrip])
	:-
	get_extended_descrip(RestExtendedDescrip).

/*------------------------------------------
 *-----------------------------------------*/
assemble_documentation(NamesList, FormsList, 
                       DescriptionsList, DocumentedPredsList, GroupsInfo)
	:-
	get_all_doc(FirstDocList, GroupsInfo),
	sort(FirstDocList,SortedDocList),
	combine_docs(SortedDocList,
	             NamesList, FormsList, DescriptionsList,
				 DocumentedPredsList).
	
/*------------------------------------------
 *-----------------------------------------*/
get_all_doc([Doc | RestDoc], GroupsInfo)
	:-
	get_groups(GroupsInfo),
	get_doc(Doc),
	(Doc = empty_doc ->
		RestDoc = [];
		get_all_doc0(RestDoc)
	).

get_all_doc0(Docs)
	:-
	get_doc(Doc),
	!,
	disp_get_all_doc0(Doc,Docs).

disp_get_all_doc0(empty_doc,[]).
disp_get_all_doc0(Doc,[Doc | RestDoc])
	:-
	get_all_doc0(RestDoc).


%get_groups(GroupsInfo).
get_groups([]).

/*------------------------------------------
 *-----------------------------------------*/
combine_docs([],[], [], [],[]).
combine_docs([Doc | RestSortedDocList],
			 [NameEntry | RestNamesList], 
			 FormsList, 
			 [ExtendedDescrip | RestDescriptionsList],
			 [PredDesc | RestDocumentedPredsList])
	:-
	Doc = doc(PredDesc, NameEntry,
	          CallingForm_IOPatterns, ExtendedDescrip),
	dappend(CallingForm_IOPatterns, RestFormsList, FormsList),
	combine_docs(RestSortedDocList,
	             RestNamesList, 
				 RestFormsList, 
				 RestDescriptionsList,
			     RestDocumentedPredsList).

combine_docs([_ | RestSortedDocList], NamesList, FormsList, 
				 DescriptionsList, DocumentedPredsList)
	:-
	combine_docs(RestSortedDocList, NamesList, FormsList, 
				 DescriptionsList, DocumentedPredsList).

/*------------------------------------------
 *-----------------------------------------*/
write_documentation(Stm, NamesList, FormsList, 
                    DescriptionsList, DocumentedPredsList)
	:-
	printf(Stm,'\n@-----Name/Brief Description-----\n\n',[]),
	write_names_list(NamesList,Stm),

	printf(Stm,'\n@-----Forms & I/O Patterns-----\n\n',[]),
	write_string_list(FormsList,Stm),

	printf(Stm,'\n@-----Description-----\n\n',[]),
	write_descrips_list(DescriptionsList,Stm).

/*------------------------------------------
 *-----------------------------------------*/
write_names_list([],_).
write_names_list([(PredDesc,BriefDescrip) | RestNamesList],Stm)
	:-
    PredDesc \= end_of_comment,
	!,
	printf(Stm,'%t -- %t\n',[PredDesc,BriefDescrip]),
	write_names_list(RestNamesList,Stm).
write_names_list([_ | RestNamesList],Stm)
	:-
	write_names_list(RestNamesList,Stm).

/*------------------------------------------
 *-----------------------------------------*/
write_descrips_list([],_).
write_descrips_list([Descrip | RestDescriptionsList],Stm)
	:-
    Descrip \= [end_of_comment | _],
	!,
	write_string_list(Descrip,Stm),nl(Stm),nl(Stm),
	write_descrips_list(RestDescriptionsList,Stm).
write_descrips_list([_ | RestDescriptionsList],Stm)
	:-
	write_descrips_list(RestDescriptionsList,Stm).

write_string_list([],_).
write_string_list([String | Strings],Stm)
	:-
	printf(Stm,'%t\n',[String]),
	write_string_list(Strings,Stm).

lib_error(multi_module(CurLibFile))
	:-
	error_message('Error: Multiple modules declared in library file %t\n',
				  [CurLibFile]).
lib_error(multi_key(CurLibFile))
	:-
	error_message('Error: Multiple library keys declared in library file %t\n',
				  [CurLibFile]).
lib_error(parse(CurLibFile, Line, ErrMsg, Pos))
	:-
	error_message('Error: Bad parse in library file %t: \n%s\nPos=%t: %s\n',
				  [CurLibFile,Line,Pos,ErrMsg]).

lib_error(unknown(InputFileName))
	:-
	error_message('Error in processing library file: %t\n',[InputFileName]).

lib_error(bad_mod_close(TagSortMCs))
	:-
	MessageList = [
		'Mismatch on module closures:\n'-[],
		common_pattern('%t\n',TagSortMCs)  ],
	error_messages(MessageList).

lib_error(bad_exports(CommonExports,UncommonExports))
	:-
	MessageList = [
		'Mismatch on predicate exports--\n'-[],
		'Common exports are:\n%t\n=======\n'-[CommonExports],
		'Differences are:\n'-[],
		common_pattern('%t\n-----\n',UncommonExports)   ],
	error_messages(MessageList).

error_message(Pattern, Values)
	:-
    telling(Where), tell(user),
	printf(Pattern, Values),
    tell(Where).

error_messages([]) :-!.
error_messages(MessageList)
	:-
	telling(Where), tell(user),
	error_messages0(MessageList),
	tell(Where).

error_messages0([]).
error_messages0([Pattern-Data | MessageList])
	:-!,
	error_message(Pattern,Data),
	error_messages0(MessageList).
error_messages0([common_pattern(Pattern,DataList) | MessageList])
	:-!,
	error_messages1(DataList,Pattern),
	error_messages0(MessageList).

error_messages1([],_).
error_messages1([Datum | DataList],Pattern)
	:-
	error_message(Pattern, Datum),
	error_messages1(DataList,Pattern).




err_messages([],_).
err_messages([Datum | Data],Pattern)
	:-
	error_message(Pattern,Datum),
	err_messages(Data,Pattern).

export tc/0.
tc :-
install_lib([unix,fsunix,dos386,fsdos386], '/u/prolog/Utils/Develop',
		    LoadConditon, ReturnVar).

export install_lib/4.
install_lib(CondFilesList, SourceDir, LoadConditon, ReturnVar)
    :-
	lib_analyze_all(CondFilesList, SourceDir, Exports, 
					ModuleCloseups, Modules, LibraryKeys),

	consis_analyze(Exports, ModuleCloseups, Modules, LibraryKeys,
					TheExports, TheModuleClosures, Module, LibraryKey),

	lib_location(Disk, ALSDIR_Path, LibPath),
	dappend(LibPath,[info], LibInfoDirPath),

			%% get path to blt_lib.pro:
	rootPathFile(Disk,ALSDIR_Path,[blt_lib,pro],BltLibFile),

			%% Save all prev info in blt_lib.pro:
	collect_lib_load_info(BltLibFile, PrevLibInfo).


install_lib(CondFilesList, SourceDir, LoadConditon, ReturnVar)
    :-
	lib_error(unknown(CondFilesList)).

lib_analyze_all([], _, [], [],[], []).
lib_analyze_all([Item,InputFileName | RestCondFilesList], SourceDir,
			    Exports, ModuleCloseups,Modules, LibraryKeys)
	:-
	(file_extension(_,_,InputFileName) ->
		FileName = InputFileName;
		file_extension(InputFileName,pro,FileName)
	),
    pathPlusFile(SourceDir,FileName,SourceFile),
	(exists_file(SourceFile) ->
        true;
        printf('Source file %t does not exist.\n',[SourceFile]),
        fail
    ),

	see(SourceFile),
	lib_analyze(ExportList, ModuleClosures,Module, LibraryKey),
	seen,
	!,

   	(var(Module) ->
      	Module = user; true),

   	(var(LibraryKey) ->
      	LibraryKey = FileName; true),

	Exports = [Item-ExportList | RestExports],
	ModuleCloseups = [Item-ModuleClosures | RestModuleCloseups],
	Modules = [Item-Module | RestModules],
	LibraryKeys = [Item-LibraryKey | RestLibraryKeys],
	lib_analyze_all(RestCondFilesList, SourceDir, RestExports, 
					RestModuleCloseups, RestModules, RestLibraryKeys).

lib_analyze_all([_,InputFileName | RestCondFilesList], SourceDir,
			    Exports, ModuleCloseups,Modules, LibraryKeys)
	:-
	lib_error(unknown(InputFileName)),
	lib_analyze_all(RestCondFilesList, SourceDir, Exports, 
					ModuleCloseups, Modules, LibraryKeys).

consis_analyze(TagExports, TagModuleClosures, TagModules, TagLibraryKeys,
				TheExports, TheModuleClosures, Module, LibraryKey)
	:-
	remove_item_tags(TagModules, Modules),
	sort(Modules, ModulesResidue),
	(ModulesResidue = [Module] ->
		true;
		lib_error(multi_module(Modules)), fail
	),

	remove_item_tags(TagLibraryKeys, LibraryKeys),
	sort(LibraryKeys, LibraryKeysResidue),
	(LibraryKeysResidue = [LibraryKey] ->
		true;
		lib_error(multi_key(LibraryKeys)), fail
	),

	remove_item_tags(TagModuleClosures, ModuleClosures),
	sort_each(ModuleClosures, SortedModuleClosures),
	sort(SortedModuleClosures, ModuleClosuresResidue),
	(ModuleClosuresResidue = [TheModuleClosures] ->
		true;
		retag(TagModuleClosures,SortedModuleClosures,TagSortMCs),
		lib_error(bad_mod_close(TagSortMCs)), fail
	),

	remove_item_tags(TagExports, Exports),
	sort_each(Exports, SortedExports),
	sort(SortedExports, ExportsResidue),
	(ExportsResidue = [TheExports] ->
		true;
		intersect(ExportsResidue, CommonExports),
		all_list_diffs(TagExports, CommonExports, UncommonExports),
		lib_error(bad_exports(CommonExports,UncommonExports)), 
		fail
	).



remove_item_tags([], []).
remove_item_tags([_-Item | TagItems], [Item | Items])
	:-
	remove_item_tags(TagItems, Items).

sort_each([], []).
sort_each([List | Lists], [SortedList | SortedLists])
	:-
	sort(List, SortedList),
	sort_each(Lists, SortedLists).

retag([],[],[]).
retag([Tag-_ | RestTagged],[New | RestNew],[[Tag-New] | RestTaggedNew])
	:-
	retag(RestTagged,RestNew,RestTaggedNew).

all_list_diffs([], _, []).
all_list_diffs([Tag-ThisExports | TagExports], CommonExports, 
			   [[Tag-ThisDiff] | UncommonExports])
	:-
	list_diff(ThisExports, CommonExports, ThisDiff),
	all_list_diffs(TagExports, CommonExports, UncommonExports).

endmod.



tt :- install_lib('/elvis/prolog/Utils/misc_db.pro').

