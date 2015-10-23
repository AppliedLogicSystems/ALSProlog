/*======================================================================*
 |          alsdev_typs.typ
 |      Copyright (c) 1997-8 Applied Logic Systems, Inc.
 |
 |      Structure type defintion for source trace file records 
 |
 | Author: Ken Bowen
 | Started: December, 1997
 *======================================================================*/
		   

module debugger.


defStruct(dbstr, [
	propertiesList = [
		filename,			% base (no ext) of filename
		fcg_num,			% file_clause_group number of file
		winname,			% associated main window
		textwin,			% name (path to) the textwin part
		numlines/0,			% number of lines in file
		linesizes/[],		% list of num chars in each line
		invlineindex/[],	% list of char offsets to start of each line
			%% Note: last[lead/follow] are 0 if they don't exist:
		head_tag/0,			% i(S,E)-last colored "matching head (aph)tag" lcn
		call_tag/0			% i(S,E)-last colored "matching_call (apg)tag" lcn
		],
		accessPred =	access_dbstr,
		setPred =		set_dbstr,
		makePred =		make_dbstr,
		structLabel =	dbstr
		]).

endmod.

module builtins.

defStruct(alsprjs, [
    propertiesList=
        [
		files_expl/[],			'Explicitly listed project files (full paths)',
        search_dirs/[],			'dirs to search, but not subdirs',
        search_subs/[],			'dirs to search, including subdirs',
        files_search/[],		'project files, located by searching',
        targets_names/[],		'names of project targets',
        targets_defns/[],		'target_definitions',
        deps_specs/[],			'dependency rules',
        activities_names/[],	'project activities',
        activities_defns/[], 	'project activities defintions/specifications'
		],
    accessPred=access_alsprjs,
	setPred=set_alsprjs,
	makePred=make_alsprjs,
    structLabel=alsprjs
	]).

endmod.
