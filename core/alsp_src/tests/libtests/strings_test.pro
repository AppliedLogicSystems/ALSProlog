:-[test].

test_strings_lib
	:-
	test([
	test_asplit,
	test_head0,
	test_head,
	test_asplit00,
	test_make_lc,
	test_make_uc,
	test_make_lc_sym,
	test_make_uc_sym,
	test_convert_to_uc,
	test_string_to_uia2,
	test_string_to_uia3,
	test_string_to_sized_uia,
	test_atomic_to_uia,
	test_cnvrt_to_UIA,
	test_truncate,
	test_strip_white,
	test_strip_tail_white,
	test_strip_both_white,
	test_strip_both_white_atom,
	test_read_to,
	test_read_to_blank,
	test_char_in,
	test_replace_char_string,
	test_replace_char_atom,
	true]).

test_asplit :-
	Atom = abcdefg,
	Splitter = 0'e,
	asplit(Atom,Splitter,LeftPart,RightPart),
	LeftPart = abcd,
	RightPart = fg,
	!.
test_asplit :-
	printf(user, 'asplit test failed\n', []).

test_head0 :-
	bagof((L,R), head0("abcdefg", 0'd, L,R), Ss),
	Ss == [("abc","efg"),("efg",[])],
	!.
test_head0 :-
	printf(user, 'head0 test failed\n', []).
	
test_head :-
	bagof((L,R), head(abcdefg, 0'd, L,R), Ss),
	Ss == [(abc,efg),(efg,'')],
	!.
test_head :-
	printf(user, 'head test failed\n', []).

	
test_asplit00 :-
	Cs = "abcdefght",
	SplitList = "gd",
	asplit00(Cs,SplitList,LeftPartCs,RightPartCs),
	LeftPartCs == "abc",
	RightPartCs == "efght",
	!.
test_asplit00 :-
	printf(user, 'asplit00 test failed\n', []).

test_make_lc :-
	Cs = "aBCd23Ef",
	make_lc(Cs, LCs),
	LCs == "abcd23ef",
	!.
test_make_lc :-
	printf(user, 'make_lc test failed\n', []).

test_make_uc :-
	Cs = "aBCd23Ef",
	make_uc(Cs, UCs),
	UCs == "ABCD23EF",
	!.
test_make_uc :-
	printf(user, 'make_uc test failed\n', []).

test_make_lc_sym :-
	Atom = aBCd23Ef,
	make_lc_sym(Atom, LCAtom),
	LCAtom == abcd23ef,
	!.
test_make_lc_sym :-
	printf(user, 'make_lc_sym test failed\n', []).

test_make_uc_sym :-
	Atom = aBCd23Ef,
	make_uc_sym(Atom, UCAtom),
	UCAtom == 'ABCD23EF',
	!.
test_make_uc_sym :-
	printf(user, 'make_uc_sym test failed\n', []).

test_convert_to_uc :-
	Items = [foo,bAr(34,west)],
	convert_to_uc(Items, UCItems),
	UCItems == ['FOO','BAR'(34,'WEST')].
test_convert_to_uc :-
	printf(user, 'convert_to_uc test failed\n', []).

test_string_to_uia2 :-
	String = "4fo#g",
	string_to_uia(String, UIA),
	UIA == '4fo#g'.	
test_string_to_uia2 :-
	printf(user, 'string_to_uia/2 test failed\n', []).

test_string_to_uia3 :-
	Chars = "4fo#g",
	UIA = 'g7ruTHbndkYrieyr5',
	Pos1 = 0,
	string_to_uia(Chars, Pos1, UIA),
	UIA == '4fo#gHbndkYrieyr5',
	Pos2 = 4,
	string_to_uia(Chars, Pos2, UIA),
	UIA == '4fo#4fo#gkYrieyr5',
	Pos3 = 16,
	not(string_to_uia(Chars, Pos3, UIA)).
test_string_to_uia3 :-
	printf(user, 'string_to_uia/3 test failed\n', []).
	
test_string_to_sized_uia :-
	Chars = "abcde",
	Size1 = 5,
	string_to_sized_uia(Size1, Chars, UIA1),
	UIA1 == 'abcde',
	Size2 = 10,
	string_to_sized_uia(Size2, Chars, UIA2),
	'$uia_size'(UIA2, ThisSize2),
	UIA2 == 'abcde',
	ThisSize2 == 12,
	Size3 = 2,
	not(string_to_sized_uia(Size3, Chars, UIA3)).
test_string_to_sized_uia :-
	printf(user, 'string_to_sized_uia/3 test failed\n', []).

test_atomic_to_uia :-
	Atom = ab23Tvu85p,
	atomic_to_uia(Atom, UIABuf),
	UIABuf == ab23Tvu85p,
	'$uia_size'(UIABuf, BufSize),
	atom_length(Atom, AL),
	AL0 is AL+2,
	BufSize == AL0.
test_atomic_to_uia :-
	printf(user, 'atomic_to_uia test failed\n', []).

test_cnvrt_to_UIA :-
	Term = p(g(7),fg(e4,23,j4,5),jd(9)),
	cnvrt_to_UIA(Term, UIABuf),
	UIABuf == 'p(g(7),fg(e4,23,j4,5),jd(9))'.
test_cnvrt_to_UIA :-
	printf(user, 'cnvrt_to_UIA test failed\n', []).

test_truncate :-
	MaxSize = 5,
	InField1 = 5657849,
	InField2 = thisSillyAtom,
	InField3 = f(3,9,rtu),
	truncate(InField1, MaxSize, OutField1),
	truncate(InField2, MaxSize, OutField2),
	truncate(InField3, MaxSize, OutField3),
	OutField1 == '56578',
	OutField2 == thisS,
	OutField3 == 'f(3,9'.
test_truncate :-
	printf(user, 'truncate test failed\n', []).

test_strip_white :-
		%% blank,blank,tab,blank:
	String = " 	 abc",
	strip_white(String, Result),
	Result == "abc".
test_strip_white :-
	printf(user, 'strip_white test failed\n', []).

test_strip_tail_white :-
		%% blank,blank,tab,blank:
	String = "abc 	 ",
	strip_tail_white(String, Result),
	Result == "abc".
test_strip_tail_white :-
	printf(user, 'strip_tail_white test failed\n', []).

test_strip_both_white :-
	    %% blank,blank,tab,blank / blank,blank,blank:
	String = " 	 abc   ",
	strip_both_white(String, Result),
	Result == "abc".
test_strip_both_white :-
	printf(user, 'strip_both_white test failed\n', []).

test_strip_both_white_atom :-
	    %% blank,blank,tab,blank / blank,blank,blank:
	Sym = '  	 foo zip bar   ',
	strip_both_white_atom(Sym, ResultSym),
	ResultSym == 'foo zip bar'.
test_strip_both_white_atom :-
	printf(user, 'strip_both_white_atom test failed\n', []).

test_read_to :-
	Chars = "A sly gag was running",
	Stoppers1 = "yg",
	read_to(Chars, Stoppers1, Head1, Tail1, Stopper1),
	Head1 == "A sl",
	Tail1 ==  " gag was running",
	Stopper1 = 0'y,

	Stoppers2 = "gy",
	read_to(Chars, Stoppers2, Head2, Tail2, Stopper2),
	Head2 == "A sl",
	Tail2 ==  " gag was running",
	Stopper2 = 0'y.
test_read_to :-
	printf(user, 'read_to test failed\n', []).

test_read_to_blank :-
	Chars = "quickly, quickly, run",
	read_to_blank(Chars, Head, Tail),
	Head == "quickly,",
	Tail == "quickly, run".
test_read_to_blank :-
	printf(user, 'read_to_blank test failed\n', []).

test_char_in :-
	Atom = myStrangeHome,
	Char1='g',
	char_in(Atom, Char1, Pos1),
	Pos1 == 8,

	Char2='z',
	not(char_in(Atom, Char2, Pos2)).
test_char_in :-
	printf(user, 'char_in test failed\n', []).

test_replace_char_string :-
	InString = "abgh3,biggbb", 
	OrigCharNum = 0'b, 
	NewCharNum = 0'Z, 
	replace_char_string(InString, OrigCharNum, NewCharNum, OutString),
	OutString == "aZgh3,ZiggZZ".
test_replace_char_string :-
	printf(user, 'replace_char_string test failed\n', []).

test_replace_char_atom :-
	AtomIn = 'abgh3,biggbb',
	OrigCharNum = 0'b, 
	NewCharNum = 0'Z, 
	replace_char_atom(AtomIn, OrigCharNum, NewCharNum, AtomOut),
	AtomOut == 'aZgh3,ZiggZZ'.
test_replace_char_atom :-
	printf(user, 'replace_char_atom test failed\n', []).


