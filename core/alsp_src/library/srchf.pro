/*==========================================================================
 |			srchf.pro
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |
 |		Utilities for searching for files (based on search paths)
 *=========================================================================*/

module builtins.

export search_for_file/2.

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
search_for_file(File, File).
 
search_for_file(FileName, File)
	:-
	searchdir(SearchDir),
%	pathPlusFile(SearchDir,FileName,File).
	split_path(SearchDir, SDElts),
	dappend(SDElts, [FileName], FElts),
	join_path(FElts, File).


endmod.
