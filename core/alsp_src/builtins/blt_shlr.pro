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
use shellmak.

%use objects.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% INTERFACE BETWEEN CONSULT AND SOURCE TO SOURCE TRANSFORMERS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transformer_db(typ, pro, no_del, []).
transformer_db(ssp, pro, no_del, []).
transformer_db(spc, pro, no_del, []).
transformer_db(mac, obp, no_del, []).
transformer_db(pl,  obp, no_del, []).
transformer_db(pro, obp, no_del, []).

:- findall(Ext, transformer_db(Ext, _, _, _), LL0), dappend(LL0, [psl], LL),
	assert_at_load_time(s2s_ext(LL)).

/*---------------------------------------------------*		
	Command above produces something like:
		s2s_ext([	typ, spc, pro ]).
 *---------------------------------------------------*/


src2src_inv(typ, BaseFile, SourceFile, OutFilePath, Options)
	:-
	xcomptype(SourceFile, OutFilePath, Options).

src2src_inv(spc, BaseFile, SourceFile, OutFilePath, Options)
	:-
	mk_shell(SourceFile, BaseFile, Options, OutFilePath).


endmod.
