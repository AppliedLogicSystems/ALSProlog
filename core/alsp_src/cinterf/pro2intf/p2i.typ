/*============================================================================*
 |		p2i.typ
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Type/Struct definitions for the pro2intf translator
 |
 |	Author: Ken Bowen
 |	Date:	April, 1994 - Extracted from Raman's earlier version
 *============================================================================*/

module pro2intf.

defStruct(p2i,
	  [propertiesList = [
		inFile/nil,	%% current input file being processed
		inStream/nil,	%% stream from input file
		baseName/nil,	%% base file name (no ext)
		hincs/[],		%% list of h files to be included (no .h ext)

		fPrefix/nil,	%% prefix for gen.preds corresp. to fcns.
		dPred/nil,	%% dup. elim/gen pred.
		tPred/nil,	%% translation predicate

		curONum/0,	%% current output file number
		curDispNum/0,	%% current dispatch number
%		cmpctFCnt/0,	%% compact file counter
		initFcns/[],	%% list of generate init functions
		outFile/nil,	%% current output file being processed
		outStream/nil,	%% stream to current output file
		cFileList/[],	%% C output files which have been written to

		pFileList/[],	%% Prolog output files which have been written to
		pfStreams/[],	%% stack of streams to .pro output files
		module/builtins, %% module for prolog intf preds
	
		fmax/100000,	%% max number of function interfaces per output file
		smax/100000,	%% max number of types/structure interfaces per output file
		cmax/100000	%% max number of constant interfaces per output file

		],
	   accessPred =	accessP2I, 
	   setPred =	setP2I,
	   makePred = 	makeP2I,
	   structLabel = p2i
	  ] ).


endmod.
