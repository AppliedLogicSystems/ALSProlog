/*===================================================================*
 | 		blt_shlr.pro         
 | 	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	ALS development shell: Resource file
 |
 |	Authors: Ken Bowen
 |	Date: 9/96
 *===================================================================*/

module builtins.
use objects.
%use ttyshlmk.
use shellmak.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% INTERFACE BETWEEN CONSULT AND SOURCE TO SOURCE TRANSFORMERS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%s2s_ext(typ).
%s2s_ext(spc).

s2s_ext([	typ,
			spc,
			pro ]).

src2src_inv(typ, BaseFile, SourceFile, TgtFile, Options)
	:-
	comptype(SourceFile, TgtFile, Options).

src2src_inv(spc, BaseFile, SourceFile, TgtFile, Options)
	:-
	mk_shell(SourceFile,TgtFile, BaseFile, Options).


endmod.
