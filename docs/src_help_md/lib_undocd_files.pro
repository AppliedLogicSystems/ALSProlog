/*======================================================================
 |                      lib_undocd_files.pro
 |      Copyright (c) 2019 Applied Logic Systems, Inc.
 |              Group: Maintenance
 |              DocTitle: check_lib_undocd
 |              -- Determines doc'd, undoc'd, and inactive lib files
 *=====================================================================*/

lib_path('../../core/alsp_src/library/').
ref_path('../docs/ref/').

cu :- check_lib_undocd.

check_lib_undocd
	:-
	printf('Analyzing library:\n',[]),
	lib_path(LibPath),
	printf('Library Path: %t\n', [LibPath]),
	files(LibPath, '*.alb', ALBList),
	files(LibPath, '*.pro', ProList),
	setof([Pf], [FN,AlbF]^(member(Pf, ProList), file_extension(Pf, FN, pro), 
                file_extension(AlbF, FN, alb), not(member(AlbF, ALBList)) ), ProNotAlb),

	ref_path(MDPath),
	printf('Markdown files Path: %t\n\n', [MDPath]),
	files(MDPath, '*.md', MDList),

	ck_docd(ALBList, MDList, AlbProList, DocdLibs, UndocdLibs),

	open('ResultDir/Library_Files_Status.txt', write, S0),
	printf(S0, 'Documented library files\n', []),
	printf(S0, '   (docs/docs/ref/<file>.md and library/<file>.[pro,alb] all exist:\n\n', []),
	columns([ u(['Alb/pro Files', 'Md File']) | DocdLibs], S0),

	printf(S0, '\nUndocumented library files\n', []),
	printf(S0, '   library/<file>.[pro,alb] both exist, but (docs/docs/ref/<file>.md does not\n\n', []),
	columns([u(['Alb/pro Files']) | UndocdLibs], S0),

	printf(S0, '\nInactive library files\n', []),
	printf(S0, '   library/<file>.pro exists, but library/<file>.alb does not\n\n', []),
	columns([u(['*.pro Files']) | ProNotAlb], S0),
	close(S0),

	printf('Output file for library analysis in ./ResultDir:\n', []),
	printf('      Library_Files_Status.txt\n',[]).

ck_docd([], _, [], [], []).
ck_docd([AlbFile | ALBList], MDList, [ PAFile | AlbProList], DocdLibs, UndocdLibs)
	:-
	file_extension(AlbFile, FileName, alb),
	file_extension(PAFile, FileName, '[alb,pro]'),
	file_extension(MdFile, FileName, md),
	determ(MdFile, PAFile, AlbFile, MDList, DocdLibs, UndocdLibs, NxDocdLibs, NxUndocdLibs),
	ck_docd(ALBList, MDList, AlbProList, NxDocdLibs, NxUndocdLibs).

determ(MdFile, PAFile, AlbFile, MDList, [[PAFile,MdFile] | NxDocdLibs], UndocdLibs, NxDocdLibs, UndocdLibs)
	:-
	member(MdFile, MDList),
	!.
determ(MdFile, PAFile, AlbFile, MDList, DocdLibs, [[ProFile] | NxUndocdLibs], DocdLibs, NxUndocdLibs)
	:-
	file_extension(AlbFile, FileName, alb),
	file_extension(ProFile, FileName, pro).
