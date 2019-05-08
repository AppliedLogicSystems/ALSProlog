:-[test].
read_toks
	:-
	sys_env(OS,_,_),
	NTks1 = 226,
	NTks2 = 919,
        (OS = mswin32 ->
		Path1 = '../alsp_src/tests/libtests/example.com',
		Path2 = '../alsp_src/tests/libtests/sample_awstats.html'
		;
		Path1 = '../../alsp_src/tests/libtests/example.com',
		Path2 = '../../alsp_src/tests/libtests/sample_awstats.html'
	),
	test([
	    do_read_toks(OS, Path1, NTks1),
	    do_read_toks(OS, Path2, NTks2) 
	]).

do_read_toks(OS, Path, NTks)
	:-
	get_cwd(CWD),
	printf(user,'    In %t|%t: reading path: %t\n', [OS, CWD, Path]),flush_output,
	check_tokenize_file(Path, NTks),
	check_read_tokens(Path, NTks),
	check_grab_html_tokens(Path, NTks).

check_tokenize_file(Path, NTks)
	:-
	tokenize_file(Path, Tokens1),
	length(Tokens1, NumTokens1),
	printf('tokenize_file read %t tokens\n', [NumTokens1]),
	NumTokens1 == NTks.

check_read_tokens(Path, NTks)
	:-
	open(Path, read, IS),
	read_tokens(IS, Tokens2),
	close(IS),
	!,
	length(Tokens2, NumTokens2),
	printf('read_tokens got %t tokens\n', [NumTokens2]),
	NumTokens2 == NTks.
	
check_grab_html_tokens(Path, NTks)
	:-
	grab_html_tokens(Path, Tokens3),
	length(Tokens3, NumTokens3),
	printf('tokenize_file read %t tokens\n', [NumTokens3]),
	NumTokens3 == NTks.

