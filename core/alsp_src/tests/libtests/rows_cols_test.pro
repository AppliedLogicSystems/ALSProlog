:- [test].

test_cols :-
	assert(silly_list( 
		[
        	  [abc, rjtir, x, y],
        	  [yrtbv, h8t, x, yacc],
        	  [34, f(4,5,6), some_silly(55,yurht, 123), thru]
        	] )  ),
	test([
	test_var_columns_file,
	test_var_columns_file_uh,
	test_fxd_columns_file, 
	test_fxd_columns_file_uh,
	true
	]),
	remove_file('silly.txt'),
	abolish(silly_list/1).

test_var_columns_file :-
	silly_list(ListOfRows),
	open('silly.txt', write, S),
	columns(ListOfRows, S),
	close(S),
	grab_lines('silly.txt', SillyColLines),
	T1 = 'abc    rjtir     x                         y     ',
	atom_length(T1, T1Len),
	SillyColLines = [L1,L2,L3],
	atom_length(L1, L1Len),
	T1Len == L1Len,
	T1 == L1.

test_var_columns_file_uh :-
	silly_list(ListOfRows),
	ListOfRows = [Header | Rest],
	open('silly.txt', write, S),
	columns([u(Header,0'+) | Rest], S),
	close(S),
	grab_lines('silly.txt', SillyColLines),

	T1 = 'abc    rjtir     x                         y     ',
	atom_length(T1, T1Len),
	SillyColLines = [L1,LUH,L2,L3],
	atom_length(L1, L1Len),
	T1Len == L1Len,
	T1 == L1,
	TH = '+++    +++++     +                         +     ',
	atom_length(TH, THLen),
	atom_length(LUH, LUHLen),
	THLen == LUHLen,
	TH == LUH.

test_fxd_columns_file :-
	silly_list(ListOfRows),
	open('silly.txt', write, S),
	columns(ListOfRows, [5,5,5,5], S),
	close(S),
	grab_lines('silly.txt', SillyColLines),
	T1 = 'abc   rjtir x     y     ',
	atom_length(T1, T1Len),
	SillyColLines = [L1,L2,L3],
	atom_length(L1, L1Len),
	T1Len == L1Len,
	T1 == L1.

test_fxd_columns_file_uh :-
	silly_list(ListOfRows),
	ListOfRows = [Header | Rest],
	open('silly.txt', write, S),
	columns([u(Header,0'+) | Rest], [5,5,5,5], S),
	close(S),
	grab_lines('silly.txt', SillyColLines),

	T1 = 'abc   rjtir x     y     ',
	atom_length(T1, T1Len),
	SillyColLines = [L1,LUH,L2,L3],
	atom_length(L1, L1Len),
	T1Len == L1Len,
	T1 == L1,
	TH = '+++++ +++++ +++++ +++++ ',
	atom_length(TH, THLen),
	atom_length(LUH, LUHLen),
	THLen == LUHLen,
	TH == LUH.

