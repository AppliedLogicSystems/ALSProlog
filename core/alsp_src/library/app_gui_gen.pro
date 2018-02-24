/*===============================================================*
 |		app_gui_gen.pro
 |		Copyright (c) 2000-01  Applied Logic Systems, Inc.
 |	
 |		Generator for complex GUI items: stock user interfaces
 |
 |	Author: Ken Bowen
 *===============================================================*/
 

module app_gui_gen.
use tk_alslib.
use strings.

export do_app_gg/0.
export start_app_gui_gen/1.
export start_app_gui_gen/3.
export ggaf/2.
export create_new_app/1.

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
do_app_gg
	:-
	pbi_get_command_line(RawCommandLine),
	app_gg_cl(RawCommandLine, SpecFile, _),
	ggaf(SpecFile, [gen_time=new, tgtfolder='.']).

app_gg_cl(RawCommandLine, SpecFile, Opts)
	:-
	RawCommandLine = [PGM | RestCL],
	app_gg_parse(RestCL, SpecFile, Opts).

app_gg_parse(['-sf', SpecFile | Opts], SpecFile, Opts).
app_gg_parse(['-p' | Rest ], SpecFile, Opts)
	:-
	app_gg_parse(Rest, SpecFile, Opts).


/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
	/* CALLED FROM GUI WHEN THE SPEC FILE FOLDER IS NOT SPECIFIED: */
start_app_gui_gen(SpecFile)
	:-
	ggaf(SpecFile, [gen_time=new]),
	sprintf(atom(Msg),
		'Application files generated from spec: %t', [SpecFile]),
	info_dialog(shl_tcli, Msg, 'Application Generation').

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
	/* CALLED FROM GUI WHEN THE SPEC FILE FOLDER IS SPECIFIED: */
start_app_gui_gen(SpecFile, SpecFolder, InitTgtFolder)
	:-
	(InitTgtFolder = '' ->
		TgtFolder = SpecFolder
		;
		TgtFolder = InitTgtFolder
	),
	ggaf(SpecFile, [gen_time=new, tgtfolder=TgtFolder]),
	sprintf(atom(Msg),
		'Application files generated from spec: %t', [SpecFile]),
	info_dialog(shl_tcli, Msg, 'Application Generation').

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
	%% Generate from a source file:
ggaf(SpecFile, Opts)
	:-
	grab_terms(SpecFile, Terms),
	findall(SP, member(gui_spec(SP),Terms), SPs),
	ggaf0(SpecFile, Opts, Terms, SPs).

ggaf(_, Opts)
	:-
	printf('Error: No Main Spec for GUI!\n',[]).

ggaf0(SpecFile, Opts, Terms, SPs)
	:-
	find_console(SPs, ConsoleSP, RestSPs),
	!,
	(member(global_gui_options(GOptions),Terms),!;GOptions=[]),
	merge_plists(Opts, GOptions, Options),
	gga(ConsoleSP, RestSPs, [spec_file=SpecFile | Options]).

	%% More modern; expect just one global_gui_options([...])
	%% and just one gui_spec([ ... ])

ggaf0(SpecFile, Opts, Terms, _)
	:-
	dmember( gui_spec(SP), Terms),
	dmember(name=GuiName, SP),
	dmember( global_gui_options(GOptions), Terms),
	merge_plists(Opts, GOptions, Options),
	dmember( type=SpecType, SP),
	ggtype(SpecType, SP, Options, SpecFile, Options, GuiName).

find_console([ConsoleSP | RestSPs], ConsoleSP, RestSPs)
	:-
	dmember(type=console,ConsoleSP),
	!.
find_console([SP | TailSPs], ConsoleSP, [SP | RestSPs])
	:-
	find_console(TailSPs, ConsoleSP, RestSPs).

initial_setup(Options,AppName,Filename,GuiGenus,File)
	:-
	check_default(Options, name, genericapp, AppName),
	DefaultFN = Filename^catenate([AppName,'_',gui], Filename),
	check_default(Options, filename, DefaultFN, Filename),
	check_default(Options, gui_genus, tcl, GuiGenus),
	file_extension(BaseFile, Filename, GuiGenus),
	(dmember(tgtfolder = TgtFolder, Options) ->
		path_directory_tail(File, TgtFolder, BaseFile)
		;
		File = BaseFile
	),
	(exists_file(File) ->
		file_extension(BakFile, File, bak),
		pbi_copy_file(File, BakFile)
		;
		true
	).

gga(ConsoleSP, RestSPs, Options)
	:-
	initial_setup(Options,AppName,Filename,GuiGenus,File),
	dmember(name=GuiName, ConsoleSP),
/*
	check_default(Options, name, genericapp, AppName),
	DefaultFN = Filename^catenate([AppName,'_',gui], Filename),
	check_default(Options, filename, DefaultFN, Filename),
	check_default(Options, gui_genus, tcl, GuiGenus),
	file_extension(BaseFile, Filename, GuiGenus),
	(dmember(tgtfolder = TgtFolder, Options) ->
		path_directory_tail(File, TgtFolder, BaseFile)
		;
		File = BaseFile
	),
	(exists_file(File) ->
		file_extension(BakFile, File, bak),
		pbi_copy_file(File, BakFile)
		;
		true
	),
*/
	open(File, write, OS,[write_eoln_type(lf)]),
	tcl_cmmnt(OS, 'File: %t -- Generated',[File]),
	unwind_protect(
		lgui_gen0([ConsoleSP | RestSPs], Options, Filename, GuiGenus, dev, OS, AppName, GuiName),
		close(OS)  ),
	catenate(Filename, '_pack', PackFilename),
	file_extension(PackBaseFile, PackFilename, GuiGenus),
	(dmember(tgtfolder = TgtFolder, Options) ->
		path_directory_tail(PackFile, TgtFolder, PackBaseFile)
		;
		PackFile = PackBaseFile
	),
	open(PackFile, write, PackOS,[write_eoln_type(lf)]),
	tcl_cmmnt(PackOS, 'File: %t -- Generated',[PackFile]),
	unwind_protect(
		lgui_gen0([ConsoleSP | RestSPs], Options, Filename, GuiGenus, pack, PackOS, AppName, GuiName),
		close(PackOS)  ).


lgui_gen0(SPs, Options, Filename, GuiGenus, Mode, OS, AppName, GuiName)
	:-
	tcl_setup(Options, Mode, OS, AppName, GuiName),
	lgui_gen(SPs, Options, Filename, OS, GuiGenus, AppName,[]).

tcl_setup(Options, Mode, OS, AppName, GuiName)
	:-
	printf(OS,'global array agv\n',[]),
	header_lines(HLines),
	write_lines(OS,HLines),nl(OS),nl(OS),
	check_default(Options, build_num, 0, BuildNum),
	check_default(Options, version_num, 0, VersionNum),
	check_get_directory(Options, OS),
	write_lines(OS, [
		'    source [file join "$APPTCLPATH" gen_app.tcl]',
		'    source [file join "$APPTCLPATH" generic_menu.tcl]',
		'    source [file join "$APPTCLPATH" generic_document.tcl]'  ]),
	sourcing_tcl_files(Options, Mode, OS),

	tcl_cmmnt(OS, 'Images and GIFS'),

%	(member(about=AboutName,Options) -> true ; AboutName=genericapp),
	(member(name=AboutName,Options) -> true ; AboutName=genericapp),
	PL0='image create photo about_%t -file [file join "$APPTCLPATH" "images/about_%t.gif"]\n\n',
	printf(OS,PL0,[AboutName,AboutName]),
	write_lines(OS, [
		'load_photo_gif up_arrow_gif up-arrow-blue',
		'load_photo_gif down_arrow_gif down-arrow-blue',
		'load_photo_gif right_gif right-arrow-blue',
		'load_photo_gif left_gif left-arrow-blue',
		'load_photo_gif openFolder openfolder',
		'load_photo_gif closeFolder closefolder',
		'load_photo_gif openFolder2 openfolder2',
		'load_photo_gif closeFolder2 closefolder2',
		'switch $tcl_platform(platform) {',
		'    unix {',
		'    	load_photo_gif closed_ptr closed_unix',
		'    	load_photo_gif open_ptr open_unix',
		'    }',
		'    windows {',
		'    	load_photo_gif closed_ptr closed_wins',
		'    	load_photo_gif open_ptr open_wins',
		'    }',
		'    macintosh {',
		'    	load_photo_gif closed_ptr closed_mac',
		'    	load_photo_gif open_ptr open_mac',
		'    }',
		'    default {',
		'    	load_photo_gif closed_ptr closed_wins',
		'    	load_photo_gif open_ptr open_wins',
		'    }',
		'}\n'		]),
	tcl_cmmnt(OS, 'Global Array Variable Settings: agv'),
	printf(OS,'set agv(app_name) %t\n',[AppName]),
	printf(OS,'set agv(version_num) %t\n',[VersionNum]),
	printf(OS,'set agv(build_num) %t\n',[BuildNum]),
	printf(OS,'set agv(dflt_mod) %t\n',[AppName]),
	check_app_settings_vars(Options, OS),
	check_default(Options, doc_extension, dtf, DocExt),
	printf(OS,'set agv(doc_extension) %t\n',[DocExt]),
	printf(OS,'set agv(main_mb) .%t.menubar\n',[GuiName]),
	check_settings_file_tcl(Options, OS),
	write_lines(OS, [
		'set agv(posted_vis) {}',
		'set agv(untitled_counter) 0' 
		]),
	prefs_settings(Options, OS).

%	lgui_gen(SPs, Options, Filename, OS, GuiGenus, AppName,[]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
lgui_gen([], Options, Filename, OS, GuiGenus, AppName,Procs)
	:-
	gen_help_about(AppName, OS),
	DefaultFF = FuncFilename^catenate([Filename,'_',funcs],FuncFilename),
	check_default(Options, funcfile, DefaultFF, FuncFilename),
	file_extension(FuncFile, FuncFilename, GuiGenus),
	printf(OS, 'source %t\n', [FuncFile]),
	printf(OS,'\nwm withdraw .\n\n', []),
	write_lines(OS, Procs),
	nl(OS).

lgui_gen([SP | RestSPs], Options, Filename, OS, GuiGenus, AppName,Procs)
	:-
	gui_gen(SP, Options, Filename, OS, GuiGenus, AppName,TopProc),
	lgui_gen(RestSPs, Options, Filename, OS, GuiGenus, AppName,[TopProc | Procs]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
sourcing_tcl_files(Options, Mode, OS)
	:-
	dmember(source_files(tcl) = TclFiles, Options),
	!,
	(Mode = dev ->
		source_each_file(TclFiles, OS)
		;
		pack_source_each_file(TclFiles, OS)
	).

sourcing_tcl_files(_, _).

source_each_file([], OS).
source_each_file([File | TclFiles], OS)
	:-
	sprintf(atom(Line), '    source %t', [File]),
	write_lines(OS, [Line]),
	source_each_file(TclFiles, OS).

pack_source_each_file([], OS).
pack_source_each_file([File | TclFiles], OS)
	:-
	path_directory_tail(File, _, BaseFile),
	sprintf(atom(Line), '    source %t', [BaseFile]),
	write_lines(OS, [Line]),
	pack_source_each_file(TclFiles, OS).


/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
gui_gen(GUISpec, Options, Filename, OS, GuiGenus, AppName,TopProc)
	:-
	dmember(type = GuiType, GUISpec),
	!,
	gui_gen3(GUISpec, GuiType, GuiGenus, Options, Filename, OS, AppName,TopProc).

gui_gen(GUISpec, Options, Filename, OS, AppName,TopProc)
	:-
	printf('Error: No Name for GUI!\n',[]).

gui_gen3(GUISpec, GuiType, GuiGenus, Options, Filename, OS, AppName,TopProc)
	:-
	check_default(GUISpec, name, generic, GuiName),
    check_default(GUISpec, title, 'Generic Title', Title),
	check_default(GUISpec, menu, [file,edit,help], Menu),

	check_default(Options, gen_time, old, GenTime),
	(GenTime = old ->
		open(null_stream(nullstream),write, FnS)
		;
		DefaultFF = FileName^catenate([Filename,'_',funcs],FuncFilename),
		check_default(Options, funcfile, DefaultFF, FuncFilename),
		file_extension(FuncFile, FuncFilename, GuiGenus),
		(dmember(tgtfolder = TgtFolder, Options) ->
			path_directory_tail(FuncFilePath, TgtFolder, FuncFile)
			;
			FuncFilePath = FuncFile
		),
		(exists_file(FuncFilePath) ->
			file_extension(BakFuncFile, FuncFilePath, bak),
			pbi_copy_file(FuncFilePath, BakFuncFile)
			;
			true
		),
		open(FuncFilePath, write, FnS,[write_eoln_type(lf)])
	),
	unwind_protect( 
		gui_gen(GuiType,GuiGenus,GuiName,Title,Menu,GUISpec,Options,GenTime,OS,FnS,AppName,TopProc),
		close(FnS)  ).

gui_gen(console,tcl,GuiName,Title,Menu,GUISpec,
			Options,GenTime,OS,FnS,AppName,TopProc)
	:-
	tcl_cmmnt(OS, '%t Console Widget', [GuiName]),
	ggc_tcl(GuiName,Title,Menu,GUISpec,Options,GenTime,OS,FnS,AppName,TopProc).  

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/


tcl_menu_head(GuiName,DotGuiName,Title,Menu,GUISpec,Options,GenTime,OS,FnS,AppName)
	:-
	catenate('.',GuiName, DotGuiName),
	check_default(GUISpec, status_win, fail, StatusWin),
	check_default(GUISpec, abort_button, fail, AbortBtn),
	printf(OS,'set agv(toplevel) %t\n\n',[DotGuiName]),
	printf(OS,'set agv(title) "%t"\n\n',[Title]),

	printf(OS,'proc %t { } {\n\tset base %t\n',[GuiName,DotGuiName]),
	printf(OS,'   if {[winfo exists $base]} {wm deiconify $base; return}\n',[]),
    printf(OS,'   toplevel $base -class Toplevel -menu %t.menubar\n',[DotGuiName]),
	write_lines(OS, [
		'   global agv',
		'   global mod',
    	'   menu $base.menubar -cursor {} -relief sunken -tearoff 0',
    	'   wm focusmodel $base passive',
    	'   if [info exists agv(main,geometry)] {',
    	'      wm geometry $base $agv(main,geometry)',
    	'   } else {',
    	'      wm geometry $base 400x290+50+50',
    	'   }',
    	'   wm maxsize $base 1028 753',
    	'   wm minsize $base 104 1',
    	'   wm overrideredirect $base 0',
    	'   wm resizable $base 1 1',
    	'   wm deiconify $base'		]),
    printf(OS,'   wm title $base "%t"\n',[Title]),
    printf(OS,'   wm protocol $base WM_DELETE_WINDOW  exit_app',[]),
	nl(OS),

	sprintf(atom(Menubar),'%t.menubar',[DotGuiName]),
	gg_menus(Menu,[],Menubar,GUISpec,Options,GenTime,DotGuiName,AppName,OS,FnS),
	nl(OS).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/


ggc_tcl(GuiName,Title,Menu,GUISpec,Options,GenTime,OS,FnS,AppName,GuiName)
	:-
	tcl_menu_head(GuiName,DotGuiName,Title,Menu,GUISpec,Options,GenTime,OS,FnS,AppName),

	write_lines(OS, [
    	'   frame $base.console -borderwidth 1 -relief raised',
    	'   scrollbar $base.console.hsb \\',
    	'\t-borderwidth 1 -command "$base.console.text xview" -orient horiz',
    	'   scrollbar $base.console.vsb \\',
    	'\t-borderwidth 1 -command "$base.console.text yview" -orient vert',
    	'   text $base.console.text \\',
    	'\t-background "$agv(main,background)" \\',
    	'\t-foreground "$agv(main,foreground)" \\',
    	'\t-font "$agv(main,font)" \\',
    	'\t-tabs "$agv(main,tabs)" \\',
		'\t-height 16 -width 60 \\',
    	'\t-xscrollcommand "$base.console.hsb set" \\',
    	'\t-yscrollcommand "$base.console.vsb set"'
		]),
	((StatusWin; AbortBtn) ->
		write_lines(OS, [
    	'frame $base.notifier -borderwidth 2 -relief groove'
		])
		;
		true
	),
	(StatusWin ->
		write_lines(OS, [
    	'   label $base.notifier.status_label \\',
    	'\t-borderwidth 1 -relief sunken -text Status----'
		])
		;
		true
	),
	(AbortBtn ->
		write_lines(OS, [
    	'   button $base.notifier.abort_button \\',
    	'\t-borderwidth 1 -relief raised -text Abort \\',
		'\t-foreground red -command interrupt_action -state disabled'
		])
		;
		true
	),
	nl(OS), 
	write_lines(OS, [
    	'   grid columnconf $base 0 -weight 1',
    	'   grid rowconf $base 0 -weight 1',
    	'   grid $base.console -sticky nesw  \\',
    	'\t-in $base -column 0 -row 0 -columnspan 1 -rowspan 1',
    	'   grid columnconf $base.console 0 -weight 1',
    	'   grid rowconf $base.console 0 -weight 1',
    	'   grid $base.console.hsb -sticky ew  \\',
    	'\t-in $base.console -column 0 -row 1 -columnspan 1 -rowspan 1',
    	'   grid $base.console.vsb -sticky ns  \\',
    	'\t-in $base.console -column 1 -row 0 -columnspan 1 -rowspan 1',
    	'   grid $base.console.text -sticky nesw \\',
    	'\t-in $base.console -column 0 -row 0 -columnspan 1 -rowspan 1'
		] ),
	((StatusWin; AbortBtn) ->
		write_lines(OS, [
    	'   grid $base.notifier -sticky ew \\',
    	'\t-in $base -column 0 -row 1 -columnspan 1 -rowspan 1'
		])
		;
		true
	),
	(StatusWin ->
		write_lines(OS, [
    	'   pack $base.notifier.status_label -side left \\',
    	'\t-in $base.notifier -anchor center -expand 0 -fill none'
		])
		;
		true
	),
	(AbortBtn ->
		write_lines(OS, [
    	'   pack $base.notifier.abort_button -side right \\',
    	'\t-in $base.notifier -anchor center -expand 0 -fill none'
		] )
		;
		true
	),


    printf(OS,'\tbind_accelerators %t.console $mod main\n',[DotGuiName]),
	write_lines(OS, [
    	'\twm geometry $base ""',
		'}'				]),
	nl(OS), nl(OS),
	ggc_pro(console,GuiName,DotGuiName,GUISpec,Options,GenTime,AppName).


/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
gg_menus([],_,MenuName,GUISpec,Options,GenTime,DotGuiName,AppName,OS,FnS).
gg_menus([Item | Items],Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-
	gg_menu_entry(Item, Context, MenuName,GUISpec,Options,
							GenTime,DotGuiName,AppName,OS,FnS),
	gg_menus(Items,Context,MenuName,GUISpec,Options,
							GenTime,DotGuiName,AppName,OS,FnS).

gg_menu_entry(ItemName/Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-!,
	gg_me(ItemName,Details,Context,MenuName,GUISpec,Options,
							GenTime,DotGuiName,AppName,OS,FnS).

gg_menu_entry(ItemName,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-
	gg_me(ItemName,[],Context,MenuName,GUISpec,Options,
							GenTime,DotGuiName,AppName,OS,FnS).

gg_me(file,Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-
	dmember(state=minimal,Details),
	!,
	check_default(GUISpec, title, AppName, ExitTitle),
	printf(OS,
		'add_minimal_generic_file_menu %t.menubar listener %t.console\n', 
		[DotGuiName,DotGuiName]).

gg_me(file,Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-
	(dmember(entries=Entries, Details) -> Entries = [] ; fail),
	check_default(GUISpec, title, AppName, ExitTitle),
	printf(OS,
		'add_generic_file_menu %t.menubar listener %t.console\n', 
		[DotGuiName,DotGuiName]).

gg_me(edit,Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-
	dmember(state=minimal,Details),
	!,
	printf(OS,
		'add_minimal_generic_edit_menu %t.menubar listener %t.console\n', 
		[DotGuiName,DotGuiName]).

gg_me(edit,Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-
	(dmember(entries=Entries) -> Entries = [] ; fail),
	!,
	printf(OS,
		'add_generic_edit_menu %t.menubar listener %t.console\n', 
		[DotGuiName,DotGuiName]).

gg_me(windows,Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-!,
	printf(OS,
		'menu %t.menubar.windows -tearoff 0 -title Windows\n',[DotGuiName]),
	printf(OS,
	'%t.menubar add cascade -label "Windows" -menu %t.menubar.windows -underline 0\n',
		[DotGuiName,DotGuiName]).

gg_me(help,Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-!,
	((dmember(entries=Entries, Details),Entries \= []) ->
		gg_help_men(Entries, DotGuiName, OS)	
		;
		printf(OS,'add_help_menu %t.menubar \n', [DotGuiName])
	).

/*
		add_help_menu_sub .lasalle_main.menubar \
			{"Commands Overview" commands_overview "LST Manual (Acrobat)"
			lst_manual "About" help_about}
*/
gg_help_men(Entries, DotGuiName, OS)
	:-
	printf(OS,'add_help_menu_sub %t.menubar \\\n\t{', [DotGuiName]),
	gg_help_men0(Entries, OS),
	printf(OS,'}\n', []).

gg_help_men0([], OS).
gg_help_men0([Entry | Entries], OS)
	:-
	gg_help_men1(Entry, OS),
	gg_help_men0(Entries, OS).

gg_help_men1(Tag/Info, OS)
	:-
	check_default(Info, command, Tag, Command),
	create_button_label(Tag, Info, Title),
	gg_help_men2(Title, Command, OS).
	
gg_help_men1(Entry, OS)
	:-
	gg_help_men2(Title, Command, OS).

gg_help_men2(Title, Command, OS)
	:-
	printf(OS, '"%t" "%t" ', [Title, Command]).



gg_me(ItemName,Details,Context,MenuName,GUISpec,Options,
						GenTime,DotGuiName,AppName,OS,FnS)
	:-
	check_default(Details,entries, [], Entries),
	sprintf(atom(SubMenuName),'%t.%t',[MenuName,ItemName]),
	create_button_label(ItemName, Details, Label),
	entry_type(ItemName,Details,Entries,EType),
	append(Context,[ItemName], NewContext),
	gen_etype(EType,ItemName,MenuName,Label,SubMenuName,
					NewContext,GenTime,AppName,Details,Options,OS,FnS),
	gg_menus(Entries,NewContext,SubMenuName,GUISpec,Options,
							GenTime,DotGuiName,AppName,OS,FnS).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
entry_type(ItemName,_,_,sep)
	:-
	sub_atom(ItemName,0,2,_,'--'),
	!.
entry_type(_,[],_,command) :-!.
entry_type(ItemName,Details,Entries,EType)
	:-
	dmember(type=EType, Details),
	!.
entry_type(_,_,[],command) :-!.
entry_type(ItemName,Details,Entries,cascade).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
gen_etype(sep, ItemName,MenuName,Label,SubMenuName,NewContext,
						GenTime,AppName,_,Options,OS,FnS)
	:-
    printf(OS,'   %t add sep\n',[MenuName]).

gen_etype(cascade, ItemName,MenuName,Label,SubMenuName,NewContext,
						GenTime,AppName,Details,Options,OS,FnS)
	:-
	(dmember(state=disabled, Details) -> 
		State = ' -state disabled ' ; State = ''),
    printf(OS,
		'\n   %t add cascade -label {%t} -menu %t%t\n',
		[MenuName,Label,SubMenuName,State]),
   	printf(OS,
		'   menu %t -cursor {} -relief sunken -tearoff 0\n', 
		[SubMenuName]).

gen_etype(command, ItemName,MenuName,Label,SubMenuName,
				NewContext,GenTime,AppName,Details,Options,OS,FnS)
	:-
	dmember(command=Command, Details),
	!,
	(dmember(state=disabled, Details) -> 
		State = ' -state disabled ' ; State = ''),
    printf(OS,
		'   %t add command -label {%t} -command "%t"%t\n',
		[MenuName,Label,Command,State]).

gen_etype(command, ItemName,MenuName,Label,SubMenuName,
				NewContext,GenTime,AppName,Details,Options,OS,FnS)
	:-
	interleave_underbars(NewContext, IWs),
	catenate(IWs, Command),
	(dmember(state=disabled, Details) -> 
		State = ' -state disabled ' ; State = ''),
    printf(OS,
		'   %t add command -label {%t} -command "%t"%t\n',
		[MenuName,Label,Command,State]),
	gen_cmd_skel(GenTime, NewContext, ItemName, Command, AppName, Options, FnS).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
create_button_label(Item, Details, Label)
	:-
	dmember(title=Label, Details),
	!.

create_button_label(Item, Details, Label)
	:-
	atom_codes(Item, ICs),
	cbl(ICs, LCs, 0),
	atom_codes(Label, LCs).

cbl([], [], _).
cbl(ICs, LCs, F)
	:-
	asplit0(ICs, 0'_, Left, Right),
	!,
	( Left = [] ->
		LCs = RestLCs
		;
		Left = [C1 | RLeft],
		make_uc([C1], [UC1]),
		(F = 0 ->
			append([UC1 | RLeft], RestLCs, LCs)
			;
			append([0' ,UC1 | RLeft], RestLCs, LCs)
		)
	),
	cbl(Right, RestLCs, 1).
cbl(ICs, LCs, F)
	:-
	ICs = [C1 | RICs],
	make_uc([C1], [UC1]),
	(F = 0 ->
		LCs = [UC1 | RICs]
		;
		LCs = [0' ,UC1 | RICs]
	).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
interleave_underbars([Word],[Word])
	:-!.
interleave_underbars([Word | Words],[Word,'_' | IWs])
	:-
	interleave_underbars(Words,IWs).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
check_get_directory(Options, OS)
	:-
	dmember(getDirectory=true, Options),
	!,
	printf(OS,
		'\nif {$tcl_platform(platform) == "windows"} { load {} getDirectory }\n',
		[]),
	printf(OS,
		'if {$tcl_platform(platform) == "macintosh"} { load {} getDirectory }\n',
		[]),
	printf(OS,'package require getDirectory\n\n',[]).

check_get_directory(_, _).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
check_app_settings_vars(Options, OS)
	:-
	dmember( managed_vars(tcl) = MVs, Options),
	MVs \= [],
	!,
	printf(OS, 'set agv(app_settings) { \\\n', []),
	list_agvs(MVs, OS),
	printf(OS, '}\n', []),
	conditional_mv_inits(MVs, OS).

check_app_settings_vars(_, _).

list_agvs([], _).
list_agvs([VV | MVs], OS)
	:-
	(VV = V/_ -> true ; VV = V),
	printf(OS, '    agv(%t) \\\n', [V]),
	list_agvs(MVs, OS).

conditional_mv_inits([], _).
conditional_mv_inits([VV | MVs], OS)
	:-
	cnd_mv_ini(VV, OS),
	conditional_mv_inits(MVs, OS).

cnd_mv_ini(V/Default, OS)
	:-!,
	printf(OS, 'if ![info exists agv(%t)] {set agv(%t) "%t"}\n', [V,V,Default]).

cnd_mv_ini(V, OS)
	:-
	printf(OS, 'if ![info exists agv(%t)] {set agv(%t) ""}\n', [V,V]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
gen_cmd_skel(old, _, _, _,_,_,_) :-!.
gen_cmd_skel(new, [help,about], about, Command, AppName, Options, FnS)
	:-
%	(member(about=AboutName,Options) -> true ; AboutName=genericapp),
	printf(FnS, '# Menu: help | %t\n', [about]),
	printf(FnS,'proc %t { } {\n',[Command]),
	write_lines(FnS, [
    	'    global agv',
		'    toplevel .about_popup -bd 2 -relief raised -background #a4a4a4',
		'    wm overrideredirect .about_popup 1' ]),
	printf(FnS,'    label .about_popup.label -image about_%t \\\n',[AppName]),
	printf(FnS,'        -bd 1 -relief flat -background blue\n',[]),
	write_lines(FnS, [
		'    frame .about_popup.ff -relief flat -border 0 -background #a4a4a4',
		'    button .about_popup.ff.ok -text OK -bd 1 -relief raised \\',
		'\t-background black -foreground white \ -command {destroy .about_popup}',
		'    label .about_popup.ff.build -text OK -bd 1 -relief flat \\',
		'\t-text "Build $agv(build_num)" -background #a4a4a4',
		'    label .about_popup.ff.version -text OK -bd 1 -relief flat \\',
		'\t-text "Version $agv(version_num)" -background #a4a4a4',
		'    pack .about_popup.label -side top -expand 1 -fill both',
		'    pack .about_popup.ff -side top -fill x',
		'    pack .about_popup.ff.version -side left -padx 20',
		'    pack .about_popup.ff.build -side left -padx 20',
		'    pack .about_popup.ff.ok -side right -padx 30',
		'    wm geometry .about_popup ""',
		'    wm deiconify .about_popup', 
		'}\n\n'		]).

gen_cmd_skel(new, Context, ItemName, Command, AppName, Options, FnS)
	:-
	printf(FnS, '# Menu: ', []),
	print_context_cmt(Context, FnS),
	printf(FnS,'proc %t { } {\n   \n}\n\n',[Command]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
print_context_cmt([], FnS) :- nl(FnS).
print_context_cmt([Item], FnS)
	:-
	printf(FnS, '%t\n', [Item]).
print_context_cmt([Item | Items], FnS)
	:-
	printf(FnS, '%t | ', [Item]),
	print_context_cmt(Items, FnS).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
	%% Standard (vtcl) header lines; put into file once:
header_lines([
    'proc {Window} {args} {',
    'global vTcl',
    '    set cmd [lindex $args 0]',
    '    set name [lindex $args 1]',
    '    set newname [lindex $args 2]',
    '    set rest [lrange $args 3 end]',
    '    if {$name == "" || $cmd == ""} {return}',
    '    if {$newname == ""} {',
    '        set newname $name',
    '    }',
    '    set exists [winfo exists $newname]',
    '    switch $cmd {',
    '        show {',
    '            if {$exists == "1" && $newname != "."} {wm deiconify $newname; return}',
    '            if {[info procs vTclWindow(pre)$name] != ""} {',
    '                eval "vTclWindow(pre)$name $newname $rest"',
    '            }',
    '            if {[info procs vTclWindow$name] != ""} {',
    '                eval "vTclWindow$name $newname $rest"',
    '            }',
    '            if {[info procs vTclWindow(post)$name] != ""} {',
    '                eval "vTclWindow(post)$name $newname $rest"',
    '            }',
    '        }',
    '        hide    { if $exists {wm withdraw $newname; return} }',
    '        iconify { if $exists {wm iconify $newname; return} }',
    '        destroy { if $exists {destroy $newname; return} }',
    '    }',
    '}',
    ' ',
    'proc vTclWindow. {base} {',
    '    if {$base == ""} {',
    '        set base .',
    '    }',
    '    ###################',
    '    # CREATING WIDGETS',
    '    ###################',
    '    wm focusmodel $base passive',
    '    wm geometry $base 200x200+0+0',
    '    wm maxsize $base 1028 753',
    '    wm minsize $base 104 1',
    '    wm overrideredirect $base 0',
    '    wm resizable $base 1 1',
    '    wm withdraw $base',
    '    wm title $base "vt"',
    '    ###################',
    '    # SETTING GEOMETRY',
    '    ###################',
    '}'
]).			%% header_lines

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
prefs_settings(Options, OS)
	:-
	write_lines(OS, [
		'if {![info exists agv(.document,foreground)]} \
{set agv(.document,foreground)	black}',
		'if {![info exists agv(.document,background)]} \
{set agv(.document,background)	#ffffff}',
		'if {![info exists agv(.document,selectforeground)]} \
{set agv(.document,selectforeground)	systemHighlightText}',
		'if {![info exists agv(.document,selectbackground)]} \
{set agv(.document,selectbackground)	systemHighlight}',
		'if {![info exists agv(.document,font)]} \
{set agv(.document,font)	{user 10 normal}}',
		'if {![info exists agv(.document,tabs)]} \
{set agv(.document,tabs)	{}}',
		'if {![info exists agv(main,foreground)]} \
{set agv(main,foreground) black}',
		'if {![info exists agv(main,background)]} \
{set agv(main,background) #ffffff}',
		'if {![info exists agv(main,selectforeground)]} \
{set agv(main,selectforeground)	systemHighlightText}',
		'if {![info exists agv(main,selectbackground)]} \
{set agv(main,selectbackground)	systemHighlight}',
		'if {![info exists agv(main,font)]} \
{set agv(main,font)	{user 10 normal}}',
		'if {![info exists agv(main,tabs)]} \
		{set agv(main,tabs)	{}}' 
	]), nl(OS).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
tcl_cmmnt(OS, Line)
	:-
	printf(OS,'\n\t##############################################\n',[]),
	printf(OS,'\t## %t \n',[Line]),
	printf(OS,'\t##############################################\n\n',[]).

tcl_cmmnt(OS, Pattern, Args)
	:-
	printf(OS,'\n\t##############################################\n',[]),
	catenate('\t## ',Pattern,Line),
	printf(OS, Line, Args), nl(OS),
	printf(OS,'\t##############################################\n\n',[]).

pro_cmmnt(OS, Line)
	:-
	printf(OS,'\n\t%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',[]),
	printf(OS,'\t%% %t \n',[Line]),
	printf(OS,'\t%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n',[]).

pro_cmmnt(OS, Pattern, Args)
	:-
	printf(OS,'\n\t%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',[]),
	catenate('\t%% ',Pattern,Line),
	printf(OS, Line, Args), nl(OS),
	printf(OS,'\t%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n',[]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
ggc_pro(Mode,GuiName,DotGuiName,GUISpec,Options,GenTime,AppName)
	:-
	DefaultFN = Filename^catenate([AppName,'_',top], Filename),
	check_default(Options, filename, DefaultFN, Filename),
	file_extension(BaseFile, Filename, pro),
	(dmember(tgtfolder = TgtFolder, Options) ->
		path_directory_tail(File, TgtFolder, BaseFile),
		path_directory_tail(RawVFile, TgtFolder, 'rawversion.pro'),
		path_directory_tail(VTagFile, TgtFolder, 'versiontag.txt')
		;
		File = BaseFile,
		RawVFile = 'rawversion.pro',
		VTagFile = 'versiontag.txt'
	),
	(exists_file(File) ->
		file_extension(BakFile, File, bak),
		pbi_copy_file(File, BakFile)
		;
		true
	),
	open(File, write, POS,[write_eoln_type(lf)]),
	open(RawVFile, write, RVOS, [alias(rawversionfile)]),
	open(VTagFile, write, VTOS, [alias(vtagfile)]),
	(dmember(spec_file=SpecFile, Options) ->
		true
		;
		catenate(GuiName,'.spec',SpecFile)
	),
	gen_file_header(POS, SpecFile, File),
	unwind_protect(
		ggc_pro2(Mode,GUISpec, Options, File, Filename, POS, AppName),
		close(POS)  ).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
ggc_pro2(Mode,GUISpec, Options, File, Filename, OS, AppName)
	:-
	(dmember(doc_extension=DocExt, Options) -> true ; DocExt = ExeName),
	printf(OS,'module app_utils.\n', []),
	printf(OS,'app_doc_ext(%t).\n', [DocExt]),
	printf(OS,'endmod.\n\n', []),
	(dmember(full_name=FullName, Options) ->
		true
		;
		FullName = AppName
	),
	(member(splash=SplashName, Options) ->
		true
		;
		SplashName = genericapp
	),
	nl(OS),nl(OS),
	printf(OS,'%:-[\'app_cmn_utils.pro\'].\n\n', []),
	printf(OS,'use app_utils.\n', []),
	printf(OS,'use tcltk.\nuse tk_alslib.\n\n', []),

	(dmember(exe_name = ExeApp, Options) -> true ; ExeApp = AppName),

	printf(OS,'this_app_name(\'%t\').\n', [ExeApp]),
	printf(OS,'this_app_name(\'%t.exe\').\n', [ExeApp]),
	make_uc_sym(ExeApp,UCExeApp),
	printf(OS,'this_app_name(\'%t\').\n', [UCExeApp]),
	printf(OS,'this_app_name(\'%t.exe\').\n\n', [UCExeApp]),

	(dmember(version_num = VN, Options) ->
		printf(OS,'raw_version_num(%t).\n', [VN],[quoted(true)]),
		printf(rawversionfile,'raw_version_num(%t).\n', [VN],[quoted(true)]),
		printf(vtagfile,'V%t', [VN],[])
		;
		true
	),
	(dmember(build_num = BN, Options) ->
		printf(OS,'raw_build_num(%t).\n', [BN],[quoted(true)]),
		printf(rawversionfile,'raw_build_num(%t).\n', [BN],[quoted(true)]),
		printf(vtagfile,'B%t', [BN],[])
		;
		true
	),
	nl(OS),
	close(rawversionfile),
	close(vtagfile),

	printf(OS,'export coldstart_%t/0.\n', [AppName]),
	printf(OS,'coldstart_%t :-\n', [AppName]),
	printf(OS,'    catch(real_coldstart_%t,_,halt).\n\n',[AppName]),
	printf(OS,'real_coldstart_%t :-\n',[AppName]),
	printf(OS,'    start_%t,\n',[AppName]),
	printf(OS,'    tk_main_loop.\n\n',[]),
	printf(OS,'\nexport warmstart_%t/0.\n', [AppName]),
	printf(OS,'warmstart_%t :-\n', [AppName]),
	printf(OS,'\tstart_%t.\n',[AppName]),
	make_lc_sym(ExeApp,LCExeApp),
	printf(OS,'\nexport %t/0.\n', [LCExeApp]),
	printf(OS,'%t :-\n', [LCExeApp]),
	printf(OS,'\tstart_%t.\n',[AppName]),
		/*!-----------------------------------------------*
		 | tag=app_stub
		 | Determines the location of the standard
		 | generated application files (e.g., gen_app.tcl).
		 *------------------------------------------------*/
	(dmember(app_stub = AppStub, Options) -> 
		true 
		; 
%		AppStub = app_files
		AppStub = '\'.\''
	),

	printf(OS,'start_%t :-\n',[AppName]),
	printf(OS,'    % resize_memory(2000000, 3000000),\n', []),
	printf(OS,'    SPLASHFILE = \'%t.gif\',\n',[SplashName]),
	printf(OS,'    basic_app_init,\n',[]),
	printf(OS,'    get_cwd(CurDir),\n',[]),
	printf(OS,'    negotiate_path(%t,HomeDir,TclPath),\n',[AppStub]),
	printf(OS,'    assert(app_home_dir(HomeDir)),\n',[]),
		/*-----------------------------------------------*
		 | "compiled_local" determines whether the app
		 | is running in a packaged setting (compiled) or
		 | unpackaged, in the development tree.
		 *------------------------------------------------*/
	printf(OS,'    ((user:compiled_local) ->\n',[]),
	printf(OS,'        PPath=TclPath\n',[]),
	printf(OS,'        ;\n',[]),
	(AppStub = '\'.\'' ->
%		printf(OS,'        join_path([HomeDir, \'..\'], PPath)\n',[])
		printf(OS,'        join_path([HomeDir, ..], PPath)\n',[])
		;
		printf(OS,'        join_path([HomeDir, %t], PPath)\n',[AppStub])
	),
	printf(OS,'    ),\n',[]),
	printf(OS,'    assert(app_home_dir_tclpath(PPath)),\n',[]),
	printf(OS,'    stock_splash(PPath, SPLASHFILE, tcli, RemoveSplashCmd),\n',[]),
%	check_settings_file_pro(Options, OS),

	printf(OS,'    change_cwd(HomeDir),\n',[]),
	(dmember(preliminary_init = true, Options) -> 
		printf(OS,'    preliminary_init_%t,\n',[AppName])
		;
		true
	),
	printf(OS,'    tcl_call(tcli,[source,\'%t_gui.tcl\'],_),\n',[AppName]),
	printf(OS,'    change_cwd(CurDir),\n',[]),

	printf(OS,'    tcl_call(tcli,[after,800],_),\n',[]),
	printf(OS,'    call(RemoveSplashCmd),\n',[]),
	dmember(name=GuiName, GUISpec),
	(Mode = console ->
		printf(OS,
		'    open(tk_win(tcli,\'.%t.console.text\'),write,_,[alias(%t_console)]),\n',
		[GuiName,AppName])
		;
		printf(OS,'    tcl_call(tcli,[%t_main],_),\n',[AppName]),
		true
	),
	printf(OS,'    (A1Path = \'\' -> true ; change_cwd(A1Path) ),\n', []),
	(Mode = console ->
	printf(OS,'    greet(%t_console,\'.%t.console.text\'),\n', [AppName,GuiName])
		;
		true
	),
	printf(OS,'    initialize_%t.\n', [AppName]),
	nl(OS), nl(OS),
	gen_notifiers(Mode, AppName, GUISpec, OS).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
gen_notifiers(console, AppName, GUISpec, OS)
	:-!,
	check_default(GUISpec, name, generic, GuiName),
	nl(OS),nl(OS),
	printf(OS,'module app_utils.\n', []),
	printf(OS,'export notifier/1.\n', []),
	printf(OS,'notifier(_) :- notif_silent,!.\n', []),
	printf(OS,'notifier(Msg) :-\n', []),
	printf(OS,'    write(%t_console, Msg), nl(%t_console).\n', [AppName,AppName]),
	printf(OS,'export notifier/2.\n', []),
	printf(OS,'notifier(_,_) :- notif_silent,!.\n', []),
	printf(OS,'notifier(Pattern,Args) :-\n', []),
	printf(OS,'    printf(%t_console, Pattern,Args).\n', [AppName]),
	printf(OS,'export notifier/3.\n', []),
	printf(OS,'notifier(_,_,_) :- notif_silent,!.\n', []),
	printf(OS,'notifier(Pattern,Args,Options) :-\n', []),
	printf(OS,'    printf(%t_console, Pattern,Args,Options).\n', [AppName]),

	printf(OS,'export progress_show/1.\n', []),
	printf(OS,'progress_show(_) :- notif_silent,!.\n', []),
	printf(OS,'progress_show(Code) :-\n', []),
	printf(OS,
		'    put_code(%t_console, Code), flush_output(%t_console).\n', 
		[AppName,AppName]),

	printf(OS,'export status_notify/1.\n', []),
	printf(OS,'status_notify(_) :- notif_silent,!.\n', []),
	printf(OS,'status_notify(Line) :-\n', []),
	catenate(['\'.',GuiName,'.notifier.status_label\''], LabelName),
	printf(OS,
		'    tcl_call(tcli,[%t,configure,\'-text\',Line],_).\n',
		[LabelName]),

	printf(OS,'\nendmod.', []),
	nl(OS),nl(OS).

gen_notifiers(Mode, AppName, GUISpec, OS)
	:-
	printf(OS,'module app_utils.\n', []),
	printf(OS,'export notifier/1.\n', []),
	printf(OS,'notifier(_).\n', []),
	printf(OS,'export notifier/2.\n', []),
	printf(OS,'notifier(_,_).\n', []),
	printf(OS,'export notifier/3.\n', []),
	printf(OS,'notifier(_,_,_).\n', []),
	printf(OS,'export progress_show/1.\n', []),
	printf(OS,'progress_show(_).\n', []),
	printf(OS,'\nendmod.', []),
	nl(OS),nl(OS).


check_settings_file_pro(Options, OS)
	:-
	dmember(settings_file=FileName, Options),
	!,
	printf(OS,'    (exists_file(%t) ->\n', [FileName]),
    printf(OS,'        grab_terms(%t, SettingsTerms),\n', [FileName]),
    printf(OS,'        load_app_settings(SettingsTerms)\n', []),
	printf(OS,'        ; true),\n', []).
check_settings_file_pro(_, _).

check_settings_file_tcl(Options, OS)
	:-
	dmember(settings_file=FileName, Options),
	!,
	printf(OS,'set agv(settings_file) "%t"\n', [FileName]).

check_settings_file_tcl(_, OS)
	:-
	printf(OS,'set agv(settings_file) ""\n', [FileName]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/

gen_help_about(AppName, OS)
	:-
	printf(OS,'proc help_about { } {\n',[]),
	write_lines(OS, [
    	'    global agv',
		'    toplevel .about_popup -bd 2 -relief raised -background #a4a4a4',
		'    wm overrideredirect .about_popup 1' ]),
	printf(OS,'    label .about_popup.label -image about_%t \\\n',[AppName]),
	printf(OS,'        -bd 1 -relief flat -background blue\n',[]),
	write_lines(OS, [
		'    frame .about_popup.ff -relief flat -border 0 -background #a4a4a4',
		'    button .about_popup.ff.ok -text OK -bd 1 -relief raised \\',
		'\t-background black -foreground white \ -command {destroy .about_popup}',
		'    label .about_popup.ff.build -text OK -bd 1 -relief flat \\',
		'\t-text "Build $agv(build_num)" -background #a4a4a4',
		'    label .about_popup.ff.version -text OK -bd 1 -relief flat \\',
		'\t-text "Version $agv(version_num)" -background #a4a4a4',
		'    pack .about_popup.label -side top -expand 1 -fill both',
		'    pack .about_popup.ff -side top -fill x',
		'    pack .about_popup.ff.version -side left -padx 20',
		'    pack .about_popup.ff.build -side left -padx 20',
		'    pack .about_popup.ff.ok -side right -padx 30',
		'    wm geometry .about_popup ""',
		'    wm deiconify .about_popup', 
		'}\n\n'		]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*-----------------------------------------------------------------*
setup_app_dir(Path)
	:-
	change_cwd(Path),
	(exists_file(app_files) ->
		true
		;
		make_subdir(app_files)
	),
	change_cwd(app_files),
	(exists_file(images) ->
		true
		;
		make_subdir(images)
	),
	change_cwd(images),
	builtins:sys_searchdir(SysD),
	path_elements(SysD, SDElts),
	append(SDElts,[images,'*.gif'],StarElts),
	join_path(StarElts, GS),
	sys_env(OS,_,_),
	(OS = mswin32 -> 
		sprintf(atom(Cmd), 'copy "%t" .', [GS])
		; 
		sprintf(atom(Cmd), 'cp "%t" .', [GS])
	),
	system(Cmd),
	change_cwd('..'),
		%%% Now in app_files; need to locate and copy files for here;
	change_cwd('..').
 *-----------------------------------------------------------------*/

 /*
t00 :-
	Eqns = [app_name = foo_bar, exe_name =fb],
	create_new_app(Eqns).

setupnew :- tcl_call(shl_tcli, [source, 'new_app_setup.tcl'], _).
tee :- tcl_call(shl_tcli, [do_new_app_top], _).
*/



create_new_app(Eqns)
	:-
	create_new_app(Eqns, '.').

	/* THIS IS CALLED FROM THE ALS GUI: */
export create_new_app/5.
create_new_app(ShortName, ExeName, DocExt, CoName, TgtPath)
	:-
	create_new_app(
		[app_name=ShortName, exe_name=ExeName, 
		 doc_extension=DocExt, company=CoName], TgtPath),
	sprintf(atom(Msg), 
			'Finished creating application folder for %t', [ShortName]),
	info_dialog(shl_tcli, Msg, 'New Application Frame').

export create_new_app/2.
create_new_app(Eqns0, Path)
	:-
	get_cwd(CurDir),
%	change_cwd(Path),
	make_change_cwd(Path),

	dmember(app_name=OrigName, Eqns0),
	replace_char_atom(OrigName, 0' , 0'_, Name1),
	atom_codes(Name1, [CC | N1Cs]),
	((0'A =< CC, CC =< 0'Z) -> LC is CC + 32 ; LC = CC),
	atom_codes(Name, [LC | N1Cs]),
	list_delete(Eqns0, app_name=OrigName, Eqns1),
	Eqns2 = [name=Name | Eqns1],

	(dmember(full_name=FullName, Eqns2) -> 
		Eqns = Eqns2 
		; 
		Eqns = [full_name = OrigName | Eqns2]
	),
	file_extension(SpecFile, Name, spec),
	(exists_file(SpecFile) ->
		true
		;
		open(SpecFile, write, OS),
		unwind_protect(
			do_cna(Eqns, Name, SpecFile, OS),
			close(OS)  )
	),
	gen_prj(Name, Path),
	(dmember(company=Company, Eqns) -> true ; Company = ''),
	create_misc_files('.', Name, Company,Eqns),
	change_cwd(CurDir).

do_cna(Eqns, Name, SpecFile, OS)
	:-
	dmember(full_name=FullName, Eqns),
	cna_header(SpecFile, FullName, Eqns, OS),
	dmember(exe_name=ExeName, Eqns),
	global_gui_opts(Name,ExeName,Eqns, OS),
	setup_gui_spec(Name, FullName, Eqns, OS),
	setup_app_dir('.', Name).

cna_header(SpecFile, FullName, Eqns, OS)
	:-
	printf(OS,'/*====================================================================*\n',[]),
	printf(OS,' |\t%t\n', [SpecFile]),
	date(YY/MM/DD),
	(dmember(company=Company, Eqns) -> true ; Company = ''),
	printf(OS,' |\tCopyright (c) %t %t\n', [YY,Company]),
	printf(OS,' |\n', []),
	printf(OS,' |\tGUI Specification for the\n', []),
	printf(OS,' |\t\t%t GUI\n', [FullName]),
	printf(OS,' |\n', []),
	printf(OS,' *====================================================================*/\n', []),
	nl(OS).

global_gui_opts(Name,ExeName,Eqns, OS)
	:-
	printf(OS, 'global_gui_options([\n', []),
	printf(OS, '\tname = %t,\n', [Name]),
	printf(OS, '\texe_name = %t,\n', [ExeName]),
	dmember(full_name=FullName, Eqns),
	printf(OS, '\tfull_name = %t,\n', [FullName],[quoted(true)]),
	printf(OS, '\tversion_num = \'0.1\',\n', []),
	printf(OS, '\tbuild_num = 1,\n', []),
	(dmember(doc_extension=DocExt, Eqns) -> true ; DocExt = ExeName),
	printf(OS, '\tdoc_extension = %t,\n', [DocExt]),
%	catenate(ExeName, '_splash', SplashName),
	catenate(Name, '_splash', SplashName),
	printf(OS, '\tsplash = %t,\n', [SplashName]),
	printf(OS, '\tabout = %t,\n', [help_about]),
	printf(OS, '\tapp_stub = app_files,\n', []),
	printf(OS, '\tsource_files(tcl) = [\n', []),
	printf(OS, '\t\t\'misc_%t.tcl\'\n', [Name]),
	printf(OS, '\t\t],\n', []),
	printf(OS, '\tgetDirectory=true,\n', []),
	printf(OS, '\tpreliminary_init = false,\n', []),
	printf(OS, '\tsettings_file=%t_settings,\n', [ExeName]),
	printf(OS, '\tmanaged_vars(tcl) = [\n', []),
	printf(OS, '\t\t]\n', []),
	printf(OS, '\t]).\n', []).

setup_gui_spec(ExeName, FullName, Eqns, OS)
	:-
	catenate('About ', FullName, AboutName),

	printf(OS, 'gui_spec([\n', []),
	printf(OS, '\tname = %t_main,\n', [ExeName]),
	printf(OS, '\ttype = console,\n', []),
	printf(OS, '\ttitle = \'%t\',\n', [FullName]),
	printf(OS, '\tmenu = [\n', []),
	printf(OS, '/*\n', []),
	printf(OS, 'file/[entries = [\n', []),
	printf(OS, '\tnew,\n', []),
	printf(OS, '\topen,\n', []),
	printf(OS, '\tclose,\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\tsave,\n', []),
	printf(OS, '\tsave_as,\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\texit		\n', []),
	printf(OS, '\t],state=minimal],\n', []),
	printf(OS, 'edit/[entries = [\n', []),
	printf(OS, '\tcopy,\n', []),
	printf(OS, '\tcut,\n', []),
	printf(OS, '\tpaste,\n', []),
	printf(OS, '\tclear,\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\tselect_all,\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\tfind,\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\tpreferences	\n', []),
	printf(OS, '\t],state=minimal],\n', []),
	printf(OS, '*/\n', []),
	printf(OS, 'general/[entries = [\n', []),
	printf(OS, '\tsample1/[command=do_sample1],\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\tsample2/[command=do_sample2],\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\tset_working_directory/[command=set_working_directory],\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\tconsole_preferences/[command=font_color_prefs],\n', []),
	printf(OS, '\t---,\n', []),
	printf(OS, '\texit/[command=exit_app]		\n', []),
	printf(OS, '\t]],\n', []),
	printf(OS, 'windows,\n', []),
	printf(OS, 'help/[entries = [\n', []),
	printf(OS, '\tabout/[title=\'%t\', command=help_about]\n', [AboutName]),
	printf(OS, '\t]]\n', []),
	printf(OS, '],\n', []),
	printf(OS, '\tstatus_win=fail,\n', []),
	printf(OS, '\tabort_button=fail\n', []),
	printf(OS, ']).\n', []).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/

setup_app_dir(Path, Name)
	:-
	change_cwd(Path),
	(exists_file(app_files) ->
		true
		;
		make_subdir(app_files)
	),
	change_cwd(app_files),
	(exists_file(images) ->
		true
		;
		make_subdir(images)
	),
	change_cwd(images),
	builtins:sys_searchdir(SysD),
	path_elements(SysD, SDElts),
	append(SDElts,[images,'*.gif'],StarElts),
	join_path(StarElts, GS),
	sys_env(OS,_,_),
	(OS = mswin32 -> 
		sprintf(atom(Cmd), 'copy "%t" .', [GS])
		; 
		sprintf(atom(Cmd), 'cp "%t" .', [GS])
	),
	system(Cmd),
	(OS = mswin32 -> 
		sprintf(atom(Cmd1a), 'copy about_genericapp.gif about_%t.gif', [Name]),
		sprintf(atom(Cmd1b), 'copy genericapp.gif %t_splash.gif', [Name])
		; 
		sprintf(atom(Cmd1a), 'cp about_genericapp.gif about_%t.gif', [Name]),
		sprintf(atom(Cmd1b), 'cp genericapp.gif %t_splash.gif', [Name])
	),
	system(Cmd1a),
	system(Cmd1b),
	change_cwd('..'),
		%%% Now in app_files; need to copy generic app tcl files for here;
	append(SDElts,[shared,'gen*.tcl'],GenAppStarElts),
	join_path(GenAppStarElts, GATclP),
	(OS = mswin32 -> 
		sprintf(atom(Cmd0), 'copy "%t" .', [GATclP])
		; 
		sprintf(atom(Cmd0), 'cp "%t" .', [GATclP])
	),
	system(Cmd0),
	change_cwd('..'),
	append(SDElts,[library,'cmn_utils.pro'],CmnUtilsElts),
	join_path(CmnUtilsElts, CUA),
	(OS = mswin32 -> 
		sprintf(atom(Cmd2), 'copy "%t" app_cmn_utils.pro', [CUA])
		; 
		sprintf(atom(Cmd2), 'cp "%t" app_cmn_utils.pro', [CUA])
	),
	system(Cmd2).



gen_prj(Name, Folder)
	:-
	file_extension(File, Name, ppj),
	path_directory_tail(Path, Folder, File),
	(exists_file(File) ->
		true
		;
		open(File, write, OS),
		unwind_protect(gen_prj(Name, Folder, File, OS), close(OS))
	).

gen_prj(Name, Folder, File, OS)
	:-
	printf(OS, 'myClassPred = project_mgr.\n',[]),
	printf(OS, 'myModule = alsdev.\n',[]),
	printf(OS, 'myName = nil.\n',[]),
	printf(OS, 'title = \'Project %t\'.\n',[Name]),
	printf(OS, 'project_file = \'%t.ppj\'.\n',[Name]),
	printf(OS, 'primary_project_dir = \'%t\'.\n',[Folder]),
	printf(OS, 'list_of_files_slots = [prolog_files].\n',[]),
	printf(OS, 'list_slots = [].\n',[]),
	printf(OS, 'text_slots = [].\n',[]),
	printf(OS, 'search_dirs = [].\n',[Name]),
	printf(OS, 'search_trees = [].\n',[]),
	printf(OS, 'slot_names =\n',[]),
	printf(OS, '    [[production_goal,\'Startup Goal:\'],[debug_goal,\'Debug Goal:\'],\n',[]),
	printf(OS, '        [executable_name,\'Image Name:\'],[prolog_files,\'Files:\']].\n',[]),
	printf(OS, 'production_goal = start_.\n',[]),
	printf(OS, 'debug_goal = debug_start_.\n',[]),
	printf(OS, 'executable_name = nil.\n',[]),
	printf(OS, 'prolog_files = [\'%t_top.pro\', \'misc_%t.pro\', ', [Name,Name]),
	printf(OS, '\'%t_gui.tcl\',\'misc_%t.tcl\', \'%t_main.pro\',\n', [Name,Name,Name]),
	printf(OS, '\t\t\'cmn_utils.pro\'].\n',[]),
	printf(OS, 'library_files = [].\n',[]),
	printf(OS, 'file_types = [[prolog_files,[\'.*\']]].\n',[]),
	printf(OS, 'default_dirs = [].\n',[]).


create_misc_files(Path, Name, Company,Eqns)
	:-
	create_misc_pro_file(Path, Name, Company,Eqns),
	create_misc_tcl_file(Path, Name, Company,Eqns),
	create_main_pro_file(Path, Name, Company).

create_main_pro_file(Path, Name, Company)
	:-
	catenate(Name,'_main',FN),
	file_extension(F,FN,pro),
	(exists_file(F) ->
		true
		;
		open(F, write, OS),
		unwind_protect(
			write_main_pro_file(OS, Name, Company),
			close(OS)   )
	).

create_misc_pro_file(Path, Name, Company,Eqns)
	:-
	catenate('misc_',Name,FN),
	file_extension(F,FN,pro),
	(exists_file(F) ->
		true
		;
		open(F, write, OS),
		unwind_protect(
			write_misc_pro_file(OS, Name, Company,Eqns),
			close(OS)   )
	).

create_misc_tcl_file(Path, Name, Company,Eqns)
	:-
	catenate('misc_',Name,FN),
	file_extension(F,FN,tcl),
	(exists_file(F) ->
		true
		;
		open(F, write, OS),
		unwind_protect(
			write_misc_tcl_file(OS, Name, Company,Eqns),
			close(OS)   )
	).

write_misc_pro_file(OS, Name, Company,Eqns)
	:-
	printf(OS,'/*===============================================================================*\n', []),
	printf(OS,' |        misc_%t.pro\n', [Name]),
	printf(OS,' |        Copyright (c) 2001 %t\n', [Company]),
	printf(OS,' |    \n', []),
	printf(OS,' |        Miscellaneous system predicates   \n', []),
	printf(OS,' |          \n', []),
	printf(OS,' *===============================================================================*/\n', []),
	printf(OS,'module user.\n', []),
	printf(OS,'    %%%% DEVELOPMENT HOOK:\n', []),
	printf(OS,'export tki/0.\n', []),
	printf(OS,'tki :-  \n', []),
	printf(OS,'    tk_alslib:destroy_tcl_interpreter(tcli), \n', []),
	printf(OS,'    close(%t),\n', [Name]),
	printf(OS,'    tk_alslib:init_tk_alslib.\n', []),
	printf(OS,'\n', []),
	printf(OS,'    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n', []),
	printf(OS,'    %%%  INITIALIZATION\n', []),
	printf(OS,'    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n', []),
	printf(OS,'\n', []),
	printf(OS,'    %%%% Start-up greeting in Console window:\n', []),
	printf(OS,'\n', []),
	printf(OS,'greet(Stream,TextW)\n', []),
	printf(OS,'    :-\n', []),
	printf(OS,'    tcl_call(tcli, [greet,TextW], _).\n', []),
	printf(OS,'\n', []),
	printf(OS,'    %% Things that need to be done  before sourcing\n', []),
	printf(OS,'    %% the Tcl code:\n', []),
	printf(OS,'/*\n', []),
	printf(OS,'preliminary_init_%t\n', [Name]),
	printf(OS,'    :-\n', []),
	printf(OS,'    ????\n', []),
	printf(OS,'*/\n', []),
	printf(OS,'\n', []),
	printf(OS,'    % Main initialization:\n', []),
	printf(OS,'initialize_%t\n', [Name]),
	printf(OS,'    :-\n', []),
	printf(OS,'    user:app_home_dir(HomeDir),\n', []),
	printf(OS,'    get_cwd(CurDir),\n', []),
	printf(OS,'    change_cwd(HomeDir),\n', []),
	printf(OS,'    change_cwd(CurDir),\n', []),
	(dmember(full_name=FullName, Eqns) -> true ; FullName = Name),
	printf(OS,'    notifier(\'Hello...%%t initialized\\n\\n\',[\'%t\']).\n\n', [FullName]),
	printf(OS,'endmod.\n\n',[]).

write_misc_tcl_file(OS, Name, Company,Eqns)
	:-
	printf(OS,'#################################################################\n', []),
	printf(OS,'#			misc_%t.tcl\n', [Name]),
	printf(OS,'#		Copyright (c) 2000 %t\n', [Company]),
	printf(OS,'#\n', []),
	printf(OS,'#################################################################\n', []),
	printf(OS,'\n', []),
	printf(OS,'	# Print through to prolog\'s console printing:\n', []),
	printf(OS,'proc pwl { Text } {\n', []),
	printf(OS,'prolog call user write -atom $Text\n', []),
	printf(OS,'prolog call user nl\n', []),
	printf(OS,'}\n', []),
	printf(OS,'\n', []),
	printf(OS,'set agv(version_num) 0\n', []),
	printf(OS,'set agv(build_num) 0\n', []),
	printf(OS,'\n', []),
	printf(OS,'proc greet { TextW } {\n', []),
	printf(OS,'	global agv\n', []),
	printf(OS,'	$TextW delete 1.0 end \n', []),
	(dmember(full_name=FullName, Eqns) -> true ; FullName = Name),
	printf(OS,'	$TextW insert end "%t \\\n', [FullName]),
	printf(OS,'			Version $agv(version_num) Build $agv(build_num)\\n"\n', []),
	printf(OS,'	$TextW insert end "Copyright (c) 2001 %t\\n"\n', [Company]),
	printf(OS,'	$TextW insert end "All rights reserved.\\n\\n"\n', []),
	printf(OS,'}\n\n', []).

write_main_pro_file(OS, Name, Company)
	:-
	printf(OS,'/*===============================================================================*\n', []),
	printf(OS,' |        %t_main.pro\n', [Name]),
	printf(OS,' |        Copyright (c) 2001 %t\n', [Company]),
	printf(OS,' |    \n', []),
	printf(OS,' |    \n', []),
	printf(OS,' *===============================================================================*/\n', []),
	printf(OS,'module user.\n\n\n', []),
	printf(OS,'endmod.\n\n',[]).


ggtype(menu_snapin, SP, Options, SpecFile, Options)
	:-
	initial_setup(Options,AppName,Filename,GuiGenus,File),
	open(File, write, OS,[write_eoln_type(lf)]),
	tcl_cmmnt(OS, 'File: %t -- Generated',[File]),
	unwind_protect(
		menusnap(SP, Options, Filename, GuiGenus, Mode, OS, AppName, GuiName),
		close(OS)  ).

menusnap(SP, Options, Filename, tcl, Mode, OS, AppName, GuiName)
	:-
	tcl_setup(Options, Mode, OS, AppName, GuiName),
	check_default(SP, name, generic, GuiName),
	check_default(SP, title, 'Generic Title', Title),
	check_default(SP, menu, [file,edit,help], Menu),
	tcl_menu_head(GuiName,DotGuiName,Title,Menu,SP,Options,GenTime,OS,FnS,AppName),
	(dmember(core=CoreName, SP) -> 
		true 
		; 
		catenate(AppName, '_app_core', CoreName)
	),
	printf(OS, '   %t $base\n', [CoreName]),
	printf(OS, '}\n', []),
	ggc_pro(menu_snap,GuiName,DotGuiName,SP,Options,GenTime,AppName).

endmod.

t :-
	SpecFile = 'dictEx.spec',
	ggaf(SpecFile, [gen_time=new]).


/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
gga :-
	files('*.spec',SFs),
	SFs \= [],
	popup_select_items(shl_tcli, SFs, [], [TgtSF|_]),
	app_gui_gen:ggaf(TgtSF, [gen_time=new]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/

app_gg :- app_gui_gen:do_app_gg.
