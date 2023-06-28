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
	accessObjStruct(gui_spec, PrevProject, GuiPath),
	tcl_call(shl_tcli, [prj_perf_isdirtycheck, GuiPath], InitRes),
	%% InitRes == false iff project is not dirty, or user said ok to close:
	(InitRes == true  -> 
		send(PrevProject, update_check_complete(Flag)),
		(Flag = ok ->
			send(PrevProject, save_to_file)
			;
			true
		),
		accessObjStruct(title,PrevProject,ProjName),
		sprintf(atom(Msg), 'Save project %t ?', [ProjName]),
		yes_no_dialog(shl_tcli, Msg, 'Close Project', Answer)
		;
		    %% InitRes == false (so close project):
		Answer = 'Yes'
	),
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
	get_cwd(CurDir),
	setObjStruct(primary_project_dir,ProjectMgr,CurDir),
	setObjStruct(cur_project,ALSIDEObject,ProjectMgr),
	send(ProjectMgr, init_gui).

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
	accessObjStruct(addl_text_slots, State, AddlTextSlots),
	accessObjStruct(slot_names, State, SlotNames),
	accessObjStruct(file_types, State, FileTypes),
	accessObjStruct(default_dirs, State, DfltDirs),
	accessObjStruct(library_files, State, LibFiles),
	getValuesFor(AddlTextSlots, State, AddlTextSlotsValues),
	catenate('.', InternalName, GuiPath),
	setObjStruct(gui_spec, State, GuiPath),

	tcl_call(shl_tcli, 
		[init_prj_spec, GuiPath, 
			TextSlots, ListOfFilesSlots, ListSlots, 
			SlotNames, FileTypes, XDfltDirs, 
			AddlTextSlots, AddlTextSlotsValues, LibFiles], _),
	send(State, display_state).


getValuesFor([], _, []).
getValuesFor([Slot | AddlTextSlots], State, [[Slot, SlotValue] | AddlTextSlotsValues])
	:-
	accessObjStruct(Slot, State, SlotValue),
	!,
	getValuesFor(AddlTextSlots, State, AddlTextSlotsValues).
getValuesFor([_ | AddlTextSlots], State, AddlTextSlotsValues)
	:-
	getValuesFor(AddlTextSlots, State, AddlTextSlotsValues).

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
	!,
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
	!,
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
	accessObjStruct(addl_text_slots, State, AddlTextSlots),
	tcl_call(shl_tcli, 
		[rd_prj_spec, GuiPath, TextSlots, ListOfFilesSlots, ListSlots, AddlTextSlots],
		InitResult),
	forbidden_slots(project,Forbidden),
 	findall(T=V, (member(L, InitResult), member([T, V], L)), Eqns),
	weak_all_setObjStruct(Eqns, Forbidden, State).

%% For dev/debug: prints an Eqns list:
dpbil([]).
dpbil([X | T]) :- pbi_write(X), pbi_nl, dpbil(T).

cleanup_value('', []) :-!.
cleanup_value(Value, CleanValue)
	:-
	atom_codes(Value, VCs),
	asplit0_all(VCs, 0' , Chunks),
	all_to_atoms(Chunks, CleanValue).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%      CLOSE PROJECT 	    %%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction(close_project, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,PrevProject),
	accessObjStruct(gui_spec, PrevProject, GuiPath),
	tcl_call(shl_tcli, [prj_perf_isdirtycheck, GuiPath], InitRes),
	%% InitRes == false iff project is not dirty, or user said ok to close:
	(InitRes == true  -> 
		send(PrevProject, update_check_complete(Flag)),
		(Flag = ok ->
			send(PrevProject, save_to_file)
			;
			true
		),
		accessObjStruct(title,PrevProject,ProjName),
		sprintf(atom(Msg), 'Save project %t ?', [ProjName]),
		yes_no_dialog(shl_tcli, Msg, 'Close Project', Answer)
		;
		Answer = 'Yes'
	),
	close_old_project(Answer, PrevProject, FilePath, ALSIDEObject).


close_old_project('No', PrevProject, FilePath, ALSIDEObject)
	:-!,
	send(ALSIDEObject, shutdown_project).

close_old_project(Answer, PrevProject, FilePath, ALSIDEObject)
	:-
	als_ide_mgrAction(save_project(SaveFlag), ALSIDEObject),
	send(ALSIDEObject, shutdown_project).

als_ide_mgrAction(shutdown_project, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,CurProject),
	send(CurProject, shutdown_project),
	setObjStruct(cur_project,ALSIDEObject,nil).

gen_project_mgrAction(shutdown_project, State)
	:-
	accessObjStruct(gui_spec, State, GuiPath),
	accessObjStruct(title, State, ProjTitle),
	tcl_call(shl_tcli, [destroy, GuiPath], _),
	tcl_call(shl_tcli, [unpost_open_project, ProjTitle], _).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	SAVE PROJECT	    %%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
als_ide_mgrAction(save_project, ALSIDEObject)
	:-
	als_ide_mgrAction(save_project(_), ALSIDEObject).

als_ide_mgrAction(save_project(Flag), ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,CurProject),
	send(CurProject, update_check_complete(UpdateCCFlag)),
	(UpdateCCFlag = ok ->
		send(CurProject, save_to_file)
,info_dialog(shl_tcli, 'Project Saved', 'Info')
		;
		true
	).

possible_save_project
	:-
	builtins:get_primary_manager(ALSIDEObject),
	accessObjStruct(cur_project,ALSIDEObject,CurProject),
	CurProject \= nil,
	!,
	accessObjStruct(gui_spec, CurProject, GuiPath),
	tcl_call(shl_tcli, [prj_perf_isdirtycheck, GuiPath], InitRes),
	%% InitRes == false iff project is not dirty, or user said ok to close:
	(InitRes  -> 
	yes_no_dialog(shl_tcli, 'Save Project First?', 'Save??', 'Yes', 'No', Answer),
	(Answer = 'Yes' ->
		als_ide_mgrAction(save_project(SaveFlag), ALSIDEObject)
		;
		true
	)
	;
	true).

possible_save_project.

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
	check_filepath(ppj, State, FilePath),
	forbidden_slots(project,Forbidden),
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

check_filepath(DistExt, State, FilePath)
	:-
	(DistExt==crf ->
		Slot = suite_file
		;
		Slot = project_file
	),
	accessObjStruct(Slot, State, InitFilePath),
	check_valid_path(InitFilePath, DistExt, FilePath, State, Slot).

check_valid_path(nil, DistExt, FilePath, State, Slot)
	:-!,
	file_extension(FilePath, default, DistExt).

check_valid_path('', DistExt, FilePath, State, Slot)
	:-!,
	file_extension(FilePath, default, DistExt).

check_valid_path("", DistExt, FilePath, State, Slot)
	:-!,
	file_extension(FilePath, default, DistExt).

check_valid_path(FilePath, DistExt, FilePath, State, Slot)
	:-
	file_extension(FilePath,_, DistExt),
	!.

check_valid_path(InitFilePath, DistExt, FilePath, State, Slot)
	:-
	file_extension(FilePath,InitFilePath, DistExt),
	setObjStruct(Slot, State, FilePath).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 		OPEN PROJECT		%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export launch_project/1.
launch_project(File)
	:-
	canon_path(File, CanonSrcPath),
	exists_file(CanonSrcPath),
	builtins:get_primary_manager(ALSIDEMGR),
	send(ALSIDEMGR, open_this_project(CanonSrcPath)).

als_ide_mgrAction(open_this_project(FilePath), ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,PrevProject),
	open_project(PrevProject, FilePath, ALSIDEObject).

als_ide_mgrAction(open_project, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,PrevProject),
	open_project(PrevProject, nil, ALSIDEObject).

open_project(nil, FilePath, ALSIDEObject)
	:-!,
	proceed_open_another_project(FilePath, ALSIDEObject).

open_project(PrevProject, FilePath, ALSIDEObject)
	:-
	accessObjStruct(gui_spec, PrevProject, GuiPath),
	tcl_call(shl_tcli, [prj_perf_isdirtycheck, GuiPath], InitRes),
	%% InitRes == false iff project is not dirty, or user said ok to close:
	(InitRes == true  -> 
		send(PrevProject, update_check_complete(Flag)),
		(Flag = ok ->
			send(PrevProject, save_to_file)
			;
			true
		),
		accessObjStruct(title,PrevProject,ProjName),
		sprintf(atom(Msg), 'Save project %t ?', [ProjName]),
		yes_no_dialog(shl_tcli, Msg, 'Close Project', Answer)
		;
		Answer = 'Yes'
	),
	close_old_open_another_project(Answer, PrevProject, FilePath, ALSIDEObject).

close_old_open_another_project('No', PrevProject, FilePath, ALSIDEObject)
	:-!.

close_old_open_another_project(Answer, PrevProject, FilePath, ALSIDEObject)
	:-
	send(PrevProject, shutdown_project),
	proceed_open_another_project(FilePath, ALSIDEObject).

proceed_open_another_project(nil, ALSIDEObject)
	:-!,
	tcl_call(shl_tcli, [select_project_file], FileSpec),
	cont_open_another_project(FileSpec, ALSIDEObject, _).

proceed_open_another_project(FilePath, ALSIDEObject)
	:-
	split_path(FilePath, PathList),
	dreverse(PathList, [File | RDirL]),
	dreverse(RDirL, DirList),
	cont_open_another_project([File, DirList], ALSIDEObject, _).

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
	exists_file(File),
	!,
	fin_open_proj(File, PathList, ALSIDEObject, ProjectMgr).

cont_open_another_project([FileName, PathListIn], ALSIDEObject, ProjectMgr)
	:-
	append(PathList, [FileName], FileList),
	join_path(FileList, File),
	sprintf(atom(Msg), 'Can\'t find project file %t!', [FileName]),
	info_dialog(shl_tcli, Msg, 'Missing File').

fin_open_proj(File, PathList, ALSIDEObject, ProjectMgr)
	:-
	grab_terms(File, Eqns),
	alsdev:create_object(
		[instanceOf=project_mgr,
		 handle=true ], 
		ProjectMgr),
	accessObjStruct(myHandle, ProjectMgr, MyHandle),
	forbidden_slots(project,Forbidden),
	weak_all_setObjStruct(Eqns, Forbidden, ProjectMgr),
	setObjStruct(project_loaded, ProjectMgr, fail),
	setObjStruct(cur_project,ALSIDEObject,ProjectMgr),
	join_path(PathList, ProjectDir),
	setObjStruct(primary_project_dir,ProjectMgr,ProjectDir),
	change_cwd(ProjectDir),
	send(ProjectMgr, init_gui),

	accessObjStruct(title,ProjectMgr,ProjectTitle),
	accessObjStruct(gui_spec, ProjectMgr, GuiPath),
	tcl_call(shl_tcli, [post_open_project,ProjectTitle, GuiPath], _).

forbidden_slots(project,[myHandle,internal_name,gui_spec,project_loaded]).

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
	accessObjStruct(gui_spec, PrevProject, GuiPath),
	tcl_call(shl_tcli, [prj_perf_isdirtycheck, GuiPath], InitRes),
	%% InitRes == false iff project is not dirty, or user said ok to close:
	(InitRes == true  -> 
		send(PrevProject, update_check_complete(Flag)),
		(Flag = ok ->
			send(PrevProject, save_to_file)
			;
			true
		),
		accessObjStruct(title,PrevProject,ProjName),
		sprintf(atom(Msg), 'Save project %t ?', [ProjName]),
		yes_no_dialog(shl_tcli, Msg, 'Close Project', Answer)
		;
		Answer = 'Yes'
	),
	close_old_load_another_project(Answer, PrevProject, ALSIDEObject).

close_old_load_another_project('No', PrevProject, ALSIDEObject)
	:-!.

close_old_load_another_project('Yes', PrevProject, ALSIDEObject)
	:-
	send(PrevProject, shutdown_project),
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
	accessObjStruct(prolog_files, ProjectMgr, ProjectFiles),
	filter_prolog_files(ProjectFiles, PrologFiles),
	accessObjStruct(search_dirs, ProjectMgr, SearchDirs),
	consult(PrologFiles, [search_path(SearchDirs)]),
	setObjStruct(project_loaded, ProjectMgr, true).

filter_prolog_files([], []).

filter_prolog_files([ProjFile | ProjectFiles], [ProjFile | PrologFiles])
	:-
	file_extension(ProjFile,_, Ext),
	dmember(Ext, [pro, pl, psl]),
	!,
	filter_prolog_files(ProjectFiles, PrologFiles).

filter_prolog_files([_ | ProjectFiles], PrologFiles)
	:-
	filter_prolog_files(ProjectFiles, PrologFiles).

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
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, open_edit_win_by_root(RootFileName,SearchList)).

gen_project_mgrAction([prj_slot_focus, _, Item], State)
	:-
	true.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	PROJECT LIBRARY FILES  %%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction(add_lib_file, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,CurProject),
	send(CurProject, add_lib_file).

gen_project_mgrAction(add_lib_file, State)
	:-
	corrected_sys_searchdir(SSDIR),
	join_path([alsdir,library], Tail),
	path_directory_tail(LibPath, SSDIR, Tail),
	tcl_call(shl_tcli, [select_library_file, LibPath], PathAndFile),
	builtins:pathPlusFile(_, File, PathAndFile),
	accessObjStruct(library_files,State,PrevLibFiles),
	finish_add_lib_file(File, PrevLibFiles, State).

	%% Previously added:
finish_add_lib_file(File, PrevLibFiles, State)
	:-
	dmember(File, PrevLibFiles),
	!.

	%% Not yet added:
finish_add_lib_file(File, PrevLibFiles, State)
	:-
	NowLibFiles = [File | PrevLibFiles],
	setObjStruct(library_files,State,NowLibFiles),
	accessObjStruct(gui_spec, State, GuiPath),
	tcl_call(shl_tcli, [addto_libfiles_disp, File, GuiPath], _).
	
gen_project_mgrAction(add_lib_files_list(FilesList), State)
	:-
	accessObjStruct(library_files,State,PrevLibFiles),
	list_diff(FilesList, PrevLibFiles, Files_Not_Prev),
	(Files_Not_Prev == [] ->
		true;
		append(Files_Not_Prev, PrevLibFiles, NowLibFiles),
		setObjStruct(library_files,State,NowLibFiles),
		accessObjStruct(gui_spec, State, GuiPath),
		tcl_call(shl_tcli, [addto_libfiles_disp_list, Files_Not_Prev, GuiPath], _)
	).
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	BUILD THE PROJECT	%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export build_project/0.
build_project
	:-
	tcl_call(shl_tcli, ['.ppj_spec.filename.entry', get], ProjFile),
	grab_terms(ProjFile, ProjectDesc),
	sys_env(OS,MinorOS,Proc),
	temp_file_name(OS,BldFile),
	open(BldFile, write, BSt, []),


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	BUILD THE PROJECT	%%%%%%%%%
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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	RUN CREF ON THE PROJECT		%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*  Cref object:
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
			search_trees,
			gui_spec,
			slot_names
			], 
*/

als_ide_mgrAction(run_cref_on_prj, ALSIDEObject)
	:-
	accessObjStruct(cur_project,ALSIDEObject,ThisProject),
	gen_project_mgrAction(run_cref_on_prj, ThisProject).

gen_project_mgrAction(run_cref_on_prj, ThisProject)
	:-
	cref_present,
	accessObjStruct(project_file, ThisProject, ProjectFile),
	accessObjStruct(primary_project_dir, ThisProject, Primary_project_dir),
	accessObjStruct(search_dirs, ThisProject, SearchList),
	SearchList = [FirstSearchDir | _],
	accessObjStruct(prolog_files, ThisProject, Prolog_files),
	file_extension(ProjectFile, BaseName, ppj),
	file_extension(CrfFile, BaseName, crf),
	file_extension(Tgt, BaseName, xrf),

	pathPlusFile(Primary_project_dir, CrfFile, CrfPathAndFile),
	pathPlusFile(Primary_project_dir, Tgt, XrfPathAndFile),
	
	open(CrfPathAndFile, write, CrfOut, []),

	printf(CrfOut, '%% Suite spec for project %t.\n\n', [ProjectFile]),
	printf(CrfOut, 'dir = \'%t\'.\n\n', [FirstSearchDir]),
	printf(CrfOut, 'files = [', []),
	writeWithQuotes(Prolog_files, CrfOut),
	printf(CrfOut, '].\n\n', []),
	printf(CrfOut, 'tgt = \'%t\'.\n\n', [XrfPathAndFile]),

	close(CrfOut),

	cref:cref(CrfFile),
		% cref:lib_files_used(myTestProj, 
		%	[lf(interleave,3,app_utils,'library/cmn_utils') | ...])
	cref:lib_files_used(BaseName, LibFileEntries),
	libFilesOnly(LibFileEntries, LibFiles),

	gen_project_mgrAction(add_lib_files_list(LibFiles), ThisProject).


writeWithQuotes([], _)
	:-!.
writeWithQuotes([File], CrfOut)
	:-!,
	printf(CrfOut, '\'%t\'', [File]).
writeWithQuotes([File | Files], CrfOut)
	:-
	printf(CrfOut, '\'%t\',', [File]),
	writeWithQuotes(Files, CrfOut).

libFilesOnly([], []).
libFilesOnly([ lf(P,N,Mod,FullLibFile) | RestLibFileEntries], [LibFilePro | RestLibFiles])
	:-
	path_directory_tail(FullLibFile, Directory, Tail),
	file_extension(LibFilePro, Tail, pro),
	libFilesOnly(RestLibFileEntries, RestLibFiles).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% 	CREF PANEL	%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction(run_cref_on_suite, ALSIDEObject)
	:-
	accessObjStruct(cur_cref_mgr,ALSIDEObject,ThisSuiteMgr),
	gen_cref_mgrAction(run_cref_on_suite, ThisSuiteMgr).

gen_cref_mgrAction(run_cref_on_suite, SuiteMgr)
	:-
	accessObjStruct(suite_file, SuiteMgr, BaseFile),
	accessObjStruct(suite_dir, SuiteMgr, DirPath),
	pathPlusFile(DirPath, BaseFile, CrfPathAndFile),
	cref:cref(CrfPathAndFile).

cref_present 
	:-
	cref:clause(cref(_), _),
	!.
cref_present 
	:-
	consult(cref).

export new_cref/0.
new_cref 
	:-
	builtins:get_primary_manager(ALS_IDE_Mgr),
	cref_present,
	create_object(
		[instanceOf=cref_panel_mgr,
		 handle=true ], 
		SuiteMgr),
	setObjStruct(cur_cref_mgr, ALS_IDE_Mgr, SuiteMgr),
	gensym(cref,X),
	sub_atom(X,1,_,0,InternalName),
	catenate('.', InternalName, GuiPath),

	new_suite_mgr(InternalName, GuiPath, SuiteMgr),
	tcl_call(shl_tcli, [cref_panel, GuiPath ], _),

	accessObjStruct(suite_dir, SuiteMgr, DirPath),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,suite_dir,DirPath], _),

	accessObjStruct(list_of_files, SuiteMgr, ListOfFiles),
	accessObjStruct(myHandle, SuiteMgr, SuiteMgrHandle),
	tcl_call(shl_tcli, [show_list_slot,GuiPath,prolog_files,ListOfFiles,SuiteMgrHandle], _),

	accessObjStruct(src_dir, SuiteMgr, SrcDir),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,src_dir,SrcDir], _),

	tcl_call(shl_tcli, [disable_cref_btns], _).

export open_cref/0.
open_cref
	:-
	builtins:get_primary_manager(ALS_IDE_Mgr),
	cref_present,
	create_object(
		[instanceOf=cref_panel_mgr,
		 handle=true ], 
		SuiteMgr),
	setObjStruct(cur_cref_mgr, ALS_IDE_Mgr, SuiteMgr),

	tcl_call(shl_tcli, [select_cref_suite], FileSpec),

	gensym(cref,X),
	sub_atom(X,1,_,0,InternalName),
	catenate('.', InternalName, GuiPath),

	setup_suite_mgr(FileSpec, InternalName, GuiPath, SuiteMgr),
	tcl_call(shl_tcli, [cref_panel, GuiPath ], _),

	accessObjStruct(title, SuiteMgr, SuiteName),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,title,SuiteName], _),

	accessObjStruct(suite_file, SuiteMgr, BaseFile),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,suite_file,BaseFile], _),

	accessObjStruct(suite_dir, SuiteMgr, DirPath),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,suite_dir,DirPath], _),

	accessObjStruct(list_of_files, SuiteMgr, ListOfFiles),
	accessObjStruct(myHandle, SuiteMgr, SuiteMgrHandle),
	tcl_call(shl_tcli, [show_list_slot,GuiPath,prolog_files,ListOfFiles,SuiteMgrHandle], _),

	accessObjStruct(src_dir, SuiteMgr, SrcDir),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,src_dir,SrcDir], _),

	accessObjStruct(target, SuiteMgr, TargetXRFFile),
	file_extension(TargetXRFFile, Name, _),
	file_extension(Target2Files, Name, '[xrf,html]'),
	tcl_call(shl_tcli, [show_text_slot,GuiPath,target,Target2Files], _),

	tcl_call(shl_tcli, [disable_cref_btns], _).

/**************************** Cref Object slots: ***************
	[   name=cref_panel_mgr, subClassOf=genericObjects,
		addl_slots=
		[ internal_name,
			title,
			suite_file,	% *.crf
			suite_dir,	% where suite_file is
			list_of_files,
			src_dir,	% (internal) dir where files reside
			target,
			gui_spec ], 
 ****************************/

new_suite_mgr(InternalName, GuiPath, SuiteMgr)
	:-
	setObjStruct(internal_name, SuiteMgr, InternalName),
	setObjStruct(gui_spec, SuiteMgr, GuiPath),

	setObjStruct(suite_dir, SuiteMgr, './'),
	setObjStruct(list_of_files, SuiteMgr, []),
	setObjStruct(src_dir, SuiteMgr, './').

setup_suite_mgr(FileSpec, InternalName, GuiPath, SuiteMgr)
	:-
	setObjStruct(internal_name, SuiteMgr, InternalName),
	setObjStruct(gui_spec, SuiteMgr, GuiPath),

	FileSpec = [BaseFile , DirParts],
	setObjStruct(suite_file, SuiteMgr, BaseFile),
	join_path(DirParts, DirPath),
	setObjStruct(suite_dir, SuiteMgr, DirPath),

	path_directory_tail(FullPath, DirPath, BaseFile),
	cref:read_crf_file(FullPath,Directory,FilesList,TargetFile, ConfigInfo, SuiteName),

	setObjStruct(title, SuiteMgr, SuiteName),
	setObjStruct(list_of_files, SuiteMgr, FilesList),
	setObjStruct(src_dir, SuiteMgr, Directory),
	setObjStruct(target, SuiteMgr, TargetFile).

forbidden_slots(cref,[myHandle,internal_name,gui_spec]).
	
als_ide_mgrAction(show_html_report, ALSIDEObject)
	:-
	accessObjStruct(cur_cref_mgr,ALSIDEObject,ThisSuiteMgr),
	gen_cref_mgrAction(show_report, html, ThisSuiteMgr).

als_ide_mgrAction(show_xrf_report, ALSIDEObject)
	:-
	accessObjStruct(cur_cref_mgr,ALSIDEObject,ThisSuiteMgr),
	gen_cref_mgrAction(show_report, xrf, ThisSuiteMgr).

gen_cref_mgrAction(show_report, Type, SuiteMgr)
	:-
	accessObjStruct(target, SuiteMgr, Target),
	(Type == xrf ->
		tcl_call(shl_tcli, [load_readonly, Target], _)
		;
		file_extension(Target, Name, _),
		file_extension(HTMLTarget, Name, html),
			%% Must determine if open will work on Linux & Windows:
		catenate('open ', HTMLTarget, Cmd),
		system(Cmd)
	).

als_ide_mgrAction(exist_reports, ALSIDEObject)
	:-
	accessObjStruct(cur_cref_mgr,ALSIDEObject,ThisSuiteMgr),
	gen_cref_mgrAction(exist_reports, ThisSuiteMgr).
	

gen_cref_mgrAction(exist_reports, SuiteMgr)
	:-
	accessObjStruct(target, SuiteMgr, Target),
	(exists_file(Target) -> XrfExists = true ; XrfExists = false),
	file_extension(Target, Name, _),
	file_extension(HTMLTarget, Name, html),
	(exists_file(HTMLTarget) -> HTMLExists = true ; HTMLExists = false),
	accessObjStruct(gui_spec, SuiteMgr, GuiPath),
	tcl_call(shl_tcli, [set_cref_rpt_btns, GuiPath, HTMLExists, XrfExists], _).

		
als_ide_mgrAction(cref_close, ALSIDEObject)
	:-
	accessObjStruct(cur_cref_mgr,ALSIDEObject,ThisSuiteMgr),
	gen_cref_mgrAction(cref_close, ThisSuiteMgr).

gen_cref_mgrAction(cref_close, State)
	:-
	gen_cref_mgrAction(shutdown_cref_panel, State),
	builtins:get_primary_manager(ALS_IDE_Mgr),
	setObjStruct(cur_cref_mgr,ALS_IDE_Mgr,nil),
	tcl_call(shl_tcli, [enable_cref_btns], _).

gen_cref_mgrAction(shutdown_cref_panel, State)
	:-
	accessObjStruct(gui_spec, State, GuiPath),
	tcl_call(shl_tcli, [destroy, GuiPath], _).

als_ide_mgrAction(save_cref_suite, ALSIDEObject)
	:-
	accessObjStruct(cur_cref_mgr,ALSIDEObject,ThisSuiteMgr),
	gen_cref_mgrAction(save_cref_suite, ThisSuiteMgr).

gen_cref_mgrAction(save_cref_suite, SuiteMgr)
	:-
	gen_cref_mgrAction(save_to_file, SuiteMgr).

gen_cref_mgrAction(save_to_file, State)
	:-
	read_gui_spec(State, PanelEqns),
	check4_suite_update(PanelEqns, State),
	!,
	accessObjStruct(title,State,Title),
	check_filepath(crf, State, FilePath),
	accessObjStruct(suite_dir,State,NewSuiteDir),
	path_directory_tail(FullNewSuiteDirPath, NewSuiteDir, FilePath),

	extract_cref_suite(State, NewCrefSuiteEqns),
	open(FullNewSuiteDirPath, write, OS, []),
	printf(OS, '/* Cref suite: %t\n', [Title]),
	printf(OS, '   Location: %t\n', [FullNewSuiteDirPath]),
	printf(OS, ' */\n', []),
	write_clauses(OS, NewCrefSuiteEqns, [quoted(true)]),
	close(OS).

gen_cref_mgrAction(save_to_file, State).

check4_suite_update(PanelEqns, State)
	:-
	dmember(suite_file=SuiteFile, PanelEqns),
	c4_su_sf(SuiteFile, State),

	dmember(suite_dir=SuiteDir, PanelEqns),
	c4_su_suitd(SuiteDir, State),

	dmember(src_dir=SrcDir, PanelEqns),
	c4_su_srcd(SrcDir, State),

	dmember(list_of_files=PrologFiles, PanelEqns),
	c4_su_pfiles(PrologFiles, State).

		%% Do we want to post the Cref panel titles too?::
%	accessObjStruct(title,State,Title),
%	Title \= '', 
%	accessObjStruct(gui_spec, State, GuiPath),
%	tcl_call(shl_tcli, [post_open_project,ProjTitle, GuiPath], _).

c4_su_sf('', State)
	:-
	Msg = 'CrefPanel must have a spec (*.crf) name',
	check4_suite_update_dialog(Msg),
	fail.
c4_su_sf(SuiteFile, State)
	:-
	setObjStruct(suite_file, State, SuiteFile).

c4_su_suitd('', State)
	:-
	setObjStruct(suite_dir, State, './'),
	Msg = 'CrefPanel missing Spec Dir. Will use: ./',
	check4_suite_update_dialog(Msg).
c4_su_suitd(SuiteDir, State)
	:-
	setObjStruct(suite_dir, State, SuiteDir).

c4_su_srcd('', State)
	:-
	setObjStruct(src_dir, State, './'),
	Msg = 'CrefPanel missing Source Dir. Will use: ./',
	check4_suite_update_dialog(Msg).

c4_su_srcd(SrcDir, State)
	:-
	setObjStruct(src_dir, State, SrcDir).

c4_su_pfiles('', State)
	:-
	Msg = 'Warning: CrefPanel has no prolog files',
	check4_suite_update_dialog(Msg).
c4_su_pfiles([], State)
	:-
	Msg = 'Warning: CrefPanel has no prolog files',
	check4_suite_update_dialog(Msg).
c4_su_pfiles(PrologFiles, State)
	:-
	setObjStruct(list_of_files, State, PrologFiles).


check4_suite_update_dialog(Msg)
	:-
	DialogTitle = 'Incomplete Cref Panel',
	info_dialog(shl_tcli, Msg, DialogTitle).

read_gui_spec(State, PanelEqns)
	:-
	accessObjStruct(gui_spec, State, GuiPath),
	tcl_call(shl_tcli, 
		[rd_cref_panel, GuiPath],
		InitResult),
	forbidden_slots(cref,Forbidden),
	do_flat_crf(InitResult, PanelEqns),
	weak_all_setObjStruct(PanelEqns, Forbidden, State).

do_flat_crf([], []).
do_flat_crf([ [X, Y] | CrefPanelVs], [X=Y | RestOut])
	:-
	do_flat_crf(CrefPanelVs, RestOut).

extract_cref_suite(State, [src_dir=SrcDir, list_of_files=Files, target=Target])
	:-
	accessObjStruct(src_dir, State, SrcDir),
	accessObjStruct(list_of_files, State, InitFullPaths),
	(InitFullPaths == '' -> FullPaths = [] ; FullPaths = InitFullPaths),
	strip_dirs(FullPaths, Files),
	accessObjStruct(target, State, PanelTarget),
	file_extension(PanelTarget, Name, _),
	file_extension(Target, Name, xrf).

strip_dirs([], []).
strip_dirs([FullPath | Paths], [File | Files])
	:-
	path_directory_tail(FullPath, _, File),
	strip_dirs(Paths, Files).
	
endmod.
