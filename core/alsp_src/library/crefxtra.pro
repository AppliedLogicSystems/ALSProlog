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
export union_files_defs/2.
export prefix_undefs/2.
export prefix_subset/3.
export prefix_undefs_basis/2.

/*!-----------------------------------------------------------------------
 |	union_files_calls/2
 |	union_files_calls(FileList, Result)
 |	union_files_calls(+, -)
 |
 |	- union of cref-generated calls from files on FileList
 *-----------------------------------------------------------------------*/
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

/*!-----------------------------------------------------------------------
 |	union_files_defs/2
 |	union_files_defs(FileList, Result)
 |	union_files_defs(+, -)
 |
 |	- union of (cref-generated) definitions from (list of) files on FileList
 *-----------------------------------------------------------------------*/
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

/*!-----------------------------------------------------------------------
 |	prefix_undefs/2
 |	prefix_undefs(Prefix, Result)
 |	prefix_undefs(+, -)
 |
 |	- list of all undefined preds in cref context beginning with Prefix
 *-----------------------------------------------------------------------*/
prefix_undefs(Prefix, Result)
	:-
	user:undefs(Undefs),
	prefix_subset(Undefs, Prefix, Result).

/*!-----------------------------------------------------------------------
 |	prefix_subset/3
 |	prefix_subset(SrcList, Prefix, Result)
 |	prefix_subset(+, +, -)
 |
 |	- list of all P/A on SrcList beginning with Prefix
 *-----------------------------------------------------------------------*/
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
			
/*!-----------------------------------------------------------------------
 |	prefix_undefs_basis/2
 |	prefix_undefs_basis(Prefix, Result)
 |	prefix_undefs_basis(+, -)
 |
 |	- get undefined preds beginning with Prefix, with Prefix stripped off
 *-----------------------------------------------------------------------*/
prefix_undefs_basis(Prefix, Result)
	:-
	prefix_undefs(Prefix, PUndefs),
	strip_prefix(PUndefs, Prefix, Result).

/*!-----------------------------------------------------------------------
 |	prefix_undefs_basis_filt/1
 |	prefix_undefs_basis_filt(Prefix)
 |	prefix_undefs_basis_filt(+)
 |
 |	- write a c2pro filter file for undefines starting with Prefix
 *-----------------------------------------------------------------------*/
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

