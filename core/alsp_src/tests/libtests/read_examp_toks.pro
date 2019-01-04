read_toks
	:-
	sys_env(OS,_,_),
        (OS = mswin32 ->
		Path = '../alsp_src/tests/libtests/example.com'
		;
		Path = '../../alsp_src/tests/libtests/example.com'
	),
	printf('In %t reading path: %t\n', [OS, Path]),
	check_tokenize_file(Path),
	check_read_tokens(Path),
	check_grab_html_tokens(Path).


check_tokenize_file(Path)
	:-
	tokenize_file(Path, Tokens1),
	length(Tokens1, NumTokens1),
	printf('tokenize_file read %t tokens\n', [NumTokens1]),
	NumTokens1 == 226.


check_read_tokens(Path)
	:-
	open(Path, read, IS),
	read_tokens(IS, Tokens2),
	close(IS),
	!,
	length(Tokens2, NumTokens2),
	printf('read_tokens got %t tokens\n', [NumTokens2]),
	NumTokens2 == 226.
	
check_grab_html_tokens(Path)
	:-
	grab_html_tokens(Path, Tokens3),
	length(Tokens3, NumTokens3),
	printf('tokenize_file read %t tokens\n', [NumTokens3]),
	NumTokens3 == 226.

