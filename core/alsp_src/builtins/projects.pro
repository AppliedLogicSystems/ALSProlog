/*=============================================================*
 |				projects.pro
 |		Copyright (c) 1998 Applied Logic Systems, Inc.
 |
 |			Prolog Project Management
 |			
 *=============================================================*/

module alsdev.
use tk_alslib.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 		NEW PROJECT			%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%% Called from "New Project" button:

als_ide_mgrAction(start_new_project, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,PrevProject),
	start_new_project(PrevProject, ALSIDEObject).

start_new_project('', ALSIDEObject)
	:-!,
	proceed_start_new_project(ALSIDEObject).

start_new_project(nil, ALSIDEObject)
	:-!,
	proceed_start_new_project(ALSIDEObject).

start_new_project(PrevProject, ALSIDEObject)
	:-
	accessObjStruct(title,PrevProject,ProjName),
	sprintf(atom(Msg), 'Close project %t ?', [ProjName]),
	yes_no_dialog(shl_tcli, Msg, 'Close Project', Answer),
	close_old_start_new_project(Answer, PrevProject, ALSIDEObject).

close_old_start_new_project('No', PrevProject, ALSIDEObject)
	:-!.
close_old_start_new_project(Answer, PrevProject, ALSIDEObject)
	:-
	als_ide_mgrAction(close_project, ALSIDEObject),
	proceed_start_new_project(ALSIDEObject).

proceed_start_new_project(ALSIDEObject)
	:-
	create_object(
		[instanceOf=project_mgr,
		 handle=true ], 
		ProjectMgr),
	setObjStruct(cur_project,ALSIDEObject,ProjectMgr),
	send(ProjectMgr, init_gui).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%  Project Manager ObjectPro CLASS DEFINITIONS  %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% Note: Only one project can be open at a time;
		%% The manager for the project is read in from
		%% the project file when the (existing) project is
		%% opened, and is written back out to the project
		%% file when the project is closed.

:- defineClass(
	[   name=gen_project_mgr,
		subClassOf=genericObjects,
		addl_slots=
			[
				internal_name,
				title,
				project_file,
				primary_project_dir,	% normally where project_file is
				list_of_files_slots,
				list_slots,
				text_slots,
				search_dirs,
%				search_trees,
				gui_spec,
				slot_names
			], 
		defaults=
			[
				title = '',
				project_file = '',
				list_of_files_slots = [],
				list_slots = [],
				text_slots = [],
				search_dirs = [],
%				search_trees = [],
				slot_names = []
			] 
	]).

		%% The manager for a prolog/tcl/c project:
:- defineClass(
	[   name=project_mgr,
		subClassOf=gen_project_mgr,
		addl_slots=
			[
%				production_goal,
%				debug_goal,
				executable_name,
				prolog_files,
%				library_files,
%				tcltk_files,
%				tcltk_interpreters,
%				c_files,
				file_types,
				default_dirs,
				project_loaded			%% true/fail
			], 
		defaults=
			[
				project_loaded = fail,
				list_of_files_slots = [
					prolog_files
%					,library_files,
%					tcltk_files,
%					c_files
					],
				list_slots = [ 
%					tcltk_interpreters 
					],
				text_slots = [
%					production_goal,
%					debug_goal
					],
				production_goal = start_,
				debug_goal = debug_start_,
				prolog_files = [],
%				library_files = [],
%				tcltk_files = [],
%				tcltk_interpreters = [tcli],
%				c_files = [],
				file_types =  [ 
					[prolog_files, ['.pro', '.pl'] ]
%					,[prolog_library_files, ['.pro'] ],
%					[tcltk_files, ['.tcl'] ],
%					[c_files, ['.c'] ] 
				],
				default_dirs = [],
				slot_names = [
					[production_goal,	'Startup Goal:'],
					[debug_goal, 		'Debug Goal:'],
					[executable_name, 	'Image Name:'],
					[prolog_files, 		'Prolog Files:']
%					,[library_files, 	'Library Files:'],
%					[tcltk_files, 		'Tcl/Tk Files:'],
%					[tcltk_interpreters,'Tcl/Tk Interps:'],
%					[c_files, 			'C Files:']
				]
			]
	]).

:-dynamic(project_mgrAction/2).

gen_project_mgrAction(init_gui, State)
	:-
	gensym(project,X),
	sub_atom(X,1,_,0,InternalName),
	gen_project_mgrAction(init_gui(InternalName), State).

gen_project_mgrAction(init_gui(InternalName), State)
	:-
	setObjStruct(internal_name, State, InternalName),
	accessObjStruct(list_of_files_slots, State, ListOfFilesSlots),
	accessObjStruct(list_slots, State, ListSlots),
	accessObjStruct(text_slots, State, TextSlots),
	accessObjStruct(slot_names, State, SlotNames),
	accessObjStruct(file_types, State, FileTypes),
	accessObjStruct(default_dirs, State, DfltDirs),
	catenate('.', InternalName, GuiPath),
	setObjStruct(gui_spec, State, GuiPath),
	tcl_call(shl_tcli, 
		[init_prj_spec, GuiPath, 
			TextSlots, ListOfFilesSlots, ListSlots, 
			SlotNames, FileTypes, XDfltDirs], _),
	send(State, display_state).


gen_project_mgrAction(display_state, State)
	:-
	accessObjStruct(gui_spec, State, GuiPath),
	accessObjStruct(list_of_files_slots, State,  ListOfFilesSlots),
	accessObjStruct(list_slots, State, ListSlots),
	accessObjStruct(text_slots, State, TextSlots),
	display_text_slots([title, project_file | TextSlots], GuiPath, State),
%	display_list_slots( [search_dirs, search_trees | ListOfFilesSlots], GuiPath, State),
	display_list_slots( [search_dirs | ListOfFilesSlots], GuiPath, State),
	display_list_slots(ListSlots, GuiPath, State).

display_text_slots([TextSlot | TextSlots], GuiPath, State)
	:-
	accessObjStruct(TextSlot, State, SlotValue),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,TextSlot,SlotValue], _),
	display_text_slots(TextSlots, GuiPath, State).
display_text_slots([_ | TextSlots], GuiPath, State)
	:-!,
	display_text_slots(TextSlots, GuiPath, State).
display_text_slots(_, GuiPath, State).

display_list_slots([ListSlot | ListSlots], GuiPath, State)
	:-
	accessObjStruct(ListSlot, State, SlotValueList),
	accessObjStruct(myHandle, State, PrjMgrHandle),
	tcl_call(shl_tcli, [show_list_slot,GuiPath,ListSlot,SlotValueList,PrjMgrHandle], _),
	display_list_slots(ListSlots, GuiPath, State).
display_list_slots([_ | ListSlots], GuiPath, State)
	:-!,
	display_list_slots(ListSlots, GuiPath, State).
display_list_slots(X, GuiPath, State).

gen_project_mgrAction(read_gui_spec, State)
	:-
	accessObjStruct(gui_spec, State, GuiPath),
	accessObjStruct(list_of_files_slots, State, ListOfFilesSlots),
	accessObjStruct(list_slots, State, ListSlots),
	accessObjStruct(text_slots, State, TextSlots),
	tcl_call(shl_tcli, 
		[rd_prj_spec, GuiPath, TextSlots, ListOfFilesSlots, ListSlots],
		InitResult),
	InitResult = [OutTextSlots, OutListSlots],
	findall(T=V, member([T, V], OutTextSlots), CleanTextSlots),
	forbidden_slots(Forbidden),
	weak_all_setObjStruct(CleanTextSlots, Forbidden, State),
	findall(Tag=CleanValue,
			(member([Tag,Value], OutListSlots), 
				cleanup_value(Value, CleanValue) ),
			CleanListSlots),
	weak_all_setObjStruct(CleanListSlots, Forbidden, State).

cleanup_value('', []) :-!.
cleanup_value(Value, CleanValue)
	:-
	atom_codes(Value, VCs),
	asplit0_all(VCs, 0' , Chunks),
	all_to_atoms(Chunks, CleanValue).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 		CLOSE PROJECT		%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction(close_project, ALSIDEObject)
	:-
	yes_no_dialog(shl_tcli, 'Save Project First?', 'Save??', 'Yes', 'No', Answer),
	(Answer = 'Yes' ->
		als_ide_mgrAction(save_project(SaveFlag), ALSIDEObject)
		;
		true
	),
	(SaveFlag = ok ->
		accessObjStruct(cur_project,ALSIDEObject,CurProject),
		send(CurProject, close_project),
		setObjStruct(cur_project,ALSIDEObject,nil),
		accessObjStruct(initial_dir, ALSIDEObject, InitialDir),
		change_cwd(InitialDir)
		;
		true
	).

gen_project_mgrAction(close_project, State)
	:-
	accessObjStruct(gui_spec, State, GuiPath),
	accessObjStruct(title, State, ProjTitle),
	tcl_call(shl_tcli, [destroy, GuiPath], _),
	tcl_call(shl_tcli, [unpost_open_project, ProjTitle], _).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 		SAVE PROJECT		%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
als_ide_mgrAction(save_project, ALSIDEObject)
	:-
	als_ide_mgrAction(save_project(_), ALSIDEObject).

als_ide_mgrAction(save_project(Flag), ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,CurProject),
	send(CurProject, update_check_complete(Flag)),
	(Flag = ok ->
		send(CurProject, save_to_file)
		;
		true
	).

gen_project_mgrAction(update_check_complete(ok), State)
	:-
	gen_project_mgrAction(read_gui_spec, State),
	accessObjStruct(title, State, ProjTitle),
	ProjTitle \= '', ProjTitle \= nil,
	accessObjStruct(project_file, State, InitFilePath),
	InitFilePath \= '', InitFilePath \= nil,
	!,
	accessObjStruct(gui_spec, State, GuiPath),
	tcl_call(shl_tcli, [post_open_project,ProjTitle, GuiPath], _).


gen_project_mgrAction(update_check_complete(fail), State)
	:-
	Title = 'Incomplete Project',
	Msg = 'Project must have title and file name',
	info_dialog(shl_tcli, Msg, Title).

gen_project_mgrAction(save_to_file, State)
	:-
%	gen_project_mgrAction(read_gui_spec, State),
	check_ppj(State, FilePath),
	forbidden_slots(Forbidden),
	dump_object(State, Forbidden, Eqns),
	open(FilePath, write, OS, []),
	write_clauses(OS, Eqns, [quoted(true)]),
	close(OS).

dump_object(State, Forbidden, Eqns)
	:-
	functor(State, Class, _),
	arg(2, State, Module),
	findall(SlotName=Value,
			  ( Module:slot_num(Class, SlotName, Offset),
				arg(Offset, State, Value),
				not(dmember(SlotName, Forbidden)) ),
			Eqns).




check_ppj(State, FilePath)
	:-
	accessObjStruct(project_file, State, InitFilePath),
	check_ppj(InitFilePath, FilePath, State).

check_ppj(nil, 'default.ppj', State)
	:-!.

check_ppj('', 'default.ppj', State)
	:-!.

check_ppj(FilePath, FilePath, State)
	:-
	file_extension(_, ppj, FilePath),
	!.

check_ppj(InitFilePath, FilePath, State)
	:-
	file_extension(InitFilePath, ppj, FilePath),
	setObjStruct(project_file, State, FilePath).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 		OPEN PROJECT		%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction(open_project, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,PrevProject),
	open_project(PrevProject, ALSIDEObject).

open_project(nil, ALSIDEObject)
	:-!,
	proceed_open_another_project(ALSIDEObject).

open_project(PrevProject, ALSIDEObject)
	:-
	accessObjStruct(title,PrevProject,ProjName),
	sprintf(atom(Msg), 'Close project %t ?', [ProjName]),
	yes_no_dialog(shl_tcli, Msg, 'Close Project', Answer),
	close_old_open_another_project(Answer, PrevProject, ALSIDEObject).

close_old_open_another_project('No', PrevProject, ALSIDEObject)
	:-!.

close_old_open_another_project(Answer, PrevProject, ALSIDEObject)
	:-
	send(PrevProject, close_project),
	proceed_open_another_project(ALSIDEObject).

proceed_open_another_project(ALSIDEObject)
	:-
	tcl_call(shl_tcli, [select_project_file], FileSpec),
	cont_open_another_project(FileSpec, ALSIDEObject, _).


als_ide_mgrAction(open_project_file(FilePath), ALSIDEObject)
	:-
	split_path(FilePath, PathList),
	dreverse(PathList, [File | RDirL]),
	dreverse(RDirL, DirList),
	cont_open_another_project([File, DirList], ALSIDEObject, _).

cont_open_another_project('', ALSIDEObject, _)
	:-!.
cont_open_another_project(nil, ALSIDEObject, _)
	:-!.

cont_open_another_project([FileName, PathListIn], ALSIDEObject, ProjectMgr)
	:-
	sys_env(OS,MinorOS,Proc),
	(OS = mswin32 ->
		PathListIn = [PathHead | RestPL],
		repair_path(PathHead, RepairedPathHead),
		PathList = [RepairedPathHead | RestPL]
		;
		PathList = PathListIn
	),
	append(PathList, [FileName], FileList),
	join_path(FileList, File),
	grab_terms(File, Eqns),
	alsdev:create_object(
		[instanceOf=project_mgr,
		 handle=true ], 
		ProjectMgr),
	accessObjStruct(myHandle, ProjectMgr, MyHandle),
	forbidden_slots(Forbidden),
	weak_all_setObjStruct(Eqns, Forbidden, ProjectMgr),

	setObjStruct(project_loaded, ProjectMgr, fail),
	setObjStruct(cur_project,ALSIDEObject,ProjectMgr),
	join_path(PathList, ProjectDir),
	setObjStruct(primary_project_dir,ProjectMgr,ProjectDir),
	send(ProjectMgr, init_gui),

	accessObjStruct(title,ProjectMgr,ProjectTitle),
	accessObjStruct(gui_spec, ProjectMgr, GuiPath),
	tcl_call(shl_tcli, [post_open_project,ProjectTitle, GuiPath], _).

forbidden_slots([myHandle,internal_name,gui_spec,project_loaded]).

weak_all_setObjStruct([], Forbidden, State).
weak_all_setObjStruct([Tag = Value | Eqns], Forbidden, State)
	:-
	not(dmember(Tag, Forbidden)),
	setObjStruct(Tag, State, Value),
	!,
	weak_all_setObjStruct(Eqns, Forbidden, State).
weak_all_setObjStruct([_ | Eqns], Forbidden, State)
	:-
	weak_all_setObjStruct(Eqns, Forbidden, State).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 		LOAD PROJECT		%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction(load_project, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,PrevProject),
	load_project(PrevProject, ALSIDEObject).

load_project(nil, ALSIDEObject)
	:-!,
	proceed_load_project(ALSIDEObject).

load_project(PrevProject, ALSIDEObject)
	:-
	accessObjStruct(title,PrevProject,ProjName),
	sprintf(atom(Msg), 'Close project %t ?', [ProjName]),
	yes_no_dialog(shl_tcli, Msg, 'Close Project', Answer),
	close_old_load_another_project(Answer, PrevProject, ALSIDEObject).

close_old_load_another_project('No', PrevProject, ALSIDEObject)
	:-!.

close_old_load_another_project('Yes', PrevProject, ALSIDEObject)
	:-
	send(PrevProject, close_project),
	proceed_load_project(ALSIDEObject).

proceed_load_project(ALSIDEObject)
	:-
	tcl_call(shl_tcli, [select_project_file], FileName),
	cont_load_project(FileName, ALSIDEObject).


cont_load_project('', ALSIDEObject)
	:-!.

cont_load_project([FileName, PathList], ALSIDEObject)
	:-
	cont_open_another_project([FileName, PathList], ALSIDEObject, ProjectObj),
	gen_project_mgrAction(load_project, ProjectObj).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	LOAD THIS PROJECT		%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction(load_this_project, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,ThisProject),
	gen_project_mgrAction(load_project, ThisProject).

gen_project_mgrAction(load_project, ProjectMgr)
	:-
	accessObjStruct(primary_project_dir,ProjectMgr,ProjectDir),
	change_cwd(ProjectDir),
	tcl_call(shl_tcli, [show_dir_on_main,ProjectDir], _),
	load_the_project(ProjectMgr),
	setObjStruct(project_loaded, ProjectMgr, true),
	listener_prompt.

check_root([], '.') :-!.
check_root([Head | Elts], ProjectDirList)
	:-
	sys_env(OS,MinorOS,Proc),
	(OS = mswin32 ->
		repair_path(Head, FixedHead)
		;
		Head = FixedHead
	),
	ProjectDirList = [FixedHead | Elts].

load_the_project(ProjectMgr)
	:-
	accessObjStruct(prolog_files, ProjectMgr, PrologFiles),
	accessObjStruct(search_dirs, ProjectMgr, SearchDirs),
	consult(PrologFiles, [search_path(SearchDirs)]),
	setObjStruct(project_loaded, ProjectMgr, true).

add_mult_files(PrevFiles, ListBoxWin)
	:-
	files('*.pro', ProFiles),
	files('*.pl',  PLFiles),
	list_diff(ProFiles, PrevFiles, NewProFiles),
	list_diff(PLFiles, PrevFiles, NewPLFiles),
	append(NewProFiles, NewPLFiles, ChoiceFiles),
	Options = [mode=multiple, title='Choose Files'],
	popup_select_items(shl_tcli, ChoiceFiles, Options, ChoiceList),
	tcl_call(shl_tcli, [merge_into_listbox, ChoiceList, ListBoxWin], _).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% OPEN FILE FOR EDITING AFTER DOUBLE-CLICK %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_project_mgrAction([prj_slot_focus, prolog_files, RootFileName], State)
	:-
	accessObjStruct(search_dirs, State, SearchList),
%write(pro_prj_slot_focus(RootFileName,SearchList)),nl,flush_output,
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, open_edit_win_by_root(RootFileName,SearchList)).

gen_project_mgrAction([prj_slot_focus, _, Item], State)
	:-
	true.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	BUILD THE PROJECT		%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export build_project/0.
build_project
	:-
	tcl_call(shl_tcli, ['.ppj_spec.filename.entry', get], ProjFile),
	grab_terms(ProjFile, ProjectDesc),
	sys_env(OS,MinorOS,Proc),
	temp_file_name(OS,BldFile),
	open(BldFile, write, BSt, []),

	dmember(package_name = PackageName, ProjectDesc),
	catenate(xx_bld_, PackageName, BldPredName),
	printf(BSt, '%t :- \n\t', [BldPredName], [quoted(true)]),
	dmember(search_dirs = SearchDirs, ProjectDesc),
	add_search_dirs_cl(SearchDirs, OS, BSt),

	dmember(prolog_files = PrologFiles, ProjectDesc),
	printf(BSt, '\t%t,\n', [PrologFiles], [quoted(true)]),

	dmember(package_start_goal = PStG, ProjectDesc),
	dmember(lib_files = ProjLibFiles, ProjectDesc),
	StdLibFiles = [miscterm,msc_ioin,strctutl,strings,tcl_sppt,tk_alslib],
	append(ProjLibFiles,StdLibFiles,LF0),
		%% sort to remove duplicates:
	sort(LF0, LibFiles),
	SaveOptions = [start_goal(PStG),select_lib(library,LibFiles)],

	printf(BSt, '\tsave_image(%t,%t).\n', [PackageName,SaveOptions], [quoted(true)]),

	close(BSt),

	bld_skel(OS, BldPattern),
	open(atom(Cmd), write, CS, []),
	printf(CS, BldPattern, [BldFile, BldPredName]),
	close(CS),
	printf('Calling: %t \n', [Cmd]),
	system(Cmd),
	listener_prompt.




	%% bld_skel(unix, Pattern)
	%% printf(Cmd,   Pattern, [<BldFile>, BldPredName>])
	%%


bld_skel(OS, Skel)
	:-
	CmdTail = ' -b -q -giac  -no_dot_alspro %t -g %t ',
	builtins:sys_searchdir(SSD),
	path_elements(SSD,SSDEs),
	dreverse(SSDEs, RSSDEs),
	(RSSDEs = [alsdir | RestRSSDEs] ->
		true 
		; 
		RestRSSDEs = RSSDEs
	),
	bld_goal(OS, XG),
	dreverse([XG | RestRSSDEs], XEs),
	join_path(OS, XEs, XPath),
	catenate(XPath, CmdTail, Skel).



bld_goal(mswin32, 'ALS Prolog Base.exe').
bld_goal(unix, 'alspro_b').
bld_goal(macos, 'ALS Prolog Base').



add_search_dirs_cl([], OS, BSt).
add_search_dirs_cl([DirList | SearchDirs], OS, BSt)
	:-
	join_path(OS, DirList, Path),
	printf(BSt, '\tbuiltins:assert(searchdir(%t)),\n', [Path], [quoted(true)]),
	add_search_dirs_cl(SearchDirs, OS, BSt).

temp_file_name(_, 'A19ztmp').

export choose_mult_files/3.
choose_mult_files(FileTypes, Descriptor, DfltDir, ChoiceList)
	:-
	(DefltDir = '' -> true
		;
		change_cwd(DfltDir)
	),
	patterns_from_types(FileTypes, FilePatterns),
	directory(FilePatterns, regular, FilesList),
	Options = [mode = multiple, title = Descriptor],
	popup_select_items(shl_tcli, FilesList, Options, ChoiceList).

patterns_from_types([], []).
patterns_from_types([FType | FileTypes], [Pat | FilePatterns])
	:-
	catenate('*', FType, Pat),
	patterns_from_types(FileTypes, FilePatterns).

endmod.
