:-['./html_tokens.pro'].

read_226_toks
	:-
	check_tokenize_file,
	check_read_tokens,
	check_grab_html_tokens.


check_tokenize_file
	:-
	Path = '../../alsp_src/tests/libtests/example.com',
	tokenize_file(Path, Tokens1),
	length(Tokens1, NumTokens1),
	printf('tokenize_file read %t tokens\n', [NumTokens1]),
	NumTokens1 == 226.


check_read_tokens
	:-
	Path = '../../alsp_src/tests/libtests/example.com',
	open(Path, read, IS),
	read_tokens(IS, Tokens2),
	close(IS),
	!,
	length(Tokens2, NumTokens2),
	printf('read_tokens got %t tokens\n', [NumTokens2]),
	NumTokens2 == 226.
	
check_grab_html_tokens
	:-
	Path = '../../alsp_src/tests/libtests/example.com',
	grab_html_tokens(Path, Tokens3),
	length(Tokens3, NumTokens3),
	printf('tokenize_file read %t tokens\n', [NumTokens3]),
	NumTokens3 == 226.

