/*--------------------------------------------------------------------
 |			crefxtra.pro
 |	Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Cross-referencer additional analytic tools
 |
 | Author(s): Ken Bowen
 | Date(s):	
 *--------------------------------------------------------------------*/

module cref.

export union_files_calls/2.

union_files_calls(FileList, Result)
	:-
	getMiscInfo(MIS),
	union_files_calls(FileList, MIS, Result).

union_files_calls(FileList, MIS, Result)
	:-
	accessMI(files_c_preds, MIS, FCPNs),

	bagof(FCs, F^(member(F, FileList),
			dmember(F+FCs, FCPNs)),
		FCsList),
	sorted_merge(FCsList, Result).
			
export union_files_defs/2.

union_files_defs(FileList, Result)
	:-
	getMiscInfo(MIS),
	union_files_defs(FileList, MIS, Result).

union_files_defs(FileList, MIS, Result)
	:-
	accessMI(files_d_preds, MIS, FDPNs),

	bagof(FDs, F^(dmember(F, FileList),
			dmember(F+FDs, FDPNs)),
		FDsList),
	sorted_merge(FDsList, Result).

export prefix_undefs/2.
prefix_undefs(Prefix, Result)
	:-
	user:undefs(Undefs),
	prefix_subset(Undefs, Prefix, Result).

export prefix_subset/3.
prefix_subset(SrcList, Prefix, Result)
	:-
	atom_length(Prefix, Len),
	prefix_subset(SrcList, Prefix, Len, Result).

prefix_subset([], Prefix, Len, []).
prefix_subset([P/A | SrcList], Prefix, Len, [P/A | Result])
	:-
	sub_atom(P, 1, Len, Prefix),
	!,
	prefix_subset(SrcList, Prefix, Len, Result).
prefix_subset([_ | SrcList], Prefix, Len, Result)
	:-
	prefix_subset(SrcList, Prefix, Len, Result).
			
export strip_prefix/3.
strip_prefix(List, Prefix, Result)
	:-
	atom_length(Prefix, PLen),
	PLen0 is PLen+1,
	strip_prefix0(List, PLen0, Result).

strip_prefix0([], _, []).
strip_prefix0([P/A | List], Start, [SP | Result])
	:-!,
	atom_length(P, PL),
	TailLen is PL - Start + 1,
	sub_atom(P,Start,TailLen,SP),
	strip_prefix0(List, Start,  Result).
strip_prefix0([P | List], Start, [SP | Result])
	:-!,
	atom_length(P, PL),
	sub_atom(P,Start,PL,SP),
	strip_prefix0(List, Start, Result).
strip_prefix0([_ | List], Start, Result)
	:-
	strip_prefix0(List, Start, Result).
	
export prefix_undefs_basis/2.
prefix_undefs_basis(Prefix, Result)
	:-
	prefix_undefs(Prefix, PUndefs),
	strip_prefix(PUndefs, Prefix, Result).

export prefix_undefs_basis_filt/1.
prefix_undefs_basis_filt(Prefix)
	:-
	prefix_undefs_basis(Prefix, PUndefs),
	getShellStruct(Struct),
	accessCRSH(suite,Struct,SuiteName),
	filePlusExt(SuiteName,flt,FiltFile),
	open(FiltFile,write,OutS,[]),
	write_clause(OutS, SuiteName=PUndefs),
	close(OutS).




endmod.

