/*============================================================================*
 |		c2p.typ
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Type/Struct definitions for the c2pro translator
 |
 |	Author: Ken Bowen
 |	Date:	April, 1994 - Extracted from Raman's earlier version
 *============================================================================*/

module c2pro.

defStruct(c2p,
	  [propertiesList = [
		inFile/nil,		%% current input file being processed
		inStream/nil,		%% stream from input file
		curLine/nil,		%% code list form of current line
		curLineNum/0,		%% number of current line
		ifdef_stack/[[]],	%% stack for ifdef contexts
		fd_stack/[],		%% stack for file contexts

		tgt_os/unix,		%% target os (for path syntax)

		info_table/[],		%% avl_tree for storing info
		fcn_filter/all,		%% selective function filtering:
							%% 	values:	all, all_except(List), List
		outFile/nil,		%% current output file being processed
		outStream/nil		%% stream to output file

		],
	   accessPred =	accessC2P, 
	   setPred =	setC2P,
	   makePred = 	makeC2P,
	   structLabel = c2p
	  ] ).


endmod.
