
% symetric_path_test

% Unix tests

% Test basic elements
st(unix, '', []).
st(unix, 'a', ['a']).
st(unix, 'ab', ['ab']).
st(unix, 'abc', ['abc']).
st(unix, 'abcefghijklmnopqrstuvwxyz', ['abcefghijklmnopqrstuvwxyz']).
st(unix, 'ABCEFGHIJKLMNOPQRSTUVWXYZ', ['ABCEFGHIJKLMNOPQRSTUVWXYZ']).
% Note the zero character is not being tested because ALS Prolog doesn't support all characters.
st(unix, '\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037',
      ['\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037']).
st(unix, ' !"#$%&\'()*+,-.0123456789:;<=>?@[\\]^_`{|}~', [' !"#$%&\'()*+,-.0123456789:;<=>?@[\\]^_`{|}~']).
st(unix, '.', ['.']).
st(unix, '..', ['..']).
st(unix, '/', ['/']).

% Check absolute paths with names
st(unix, '/a', ['/', 'a']).
st(unix, '/a/b', ['/', 'a', 'b']).
st(unix, '/a/b/c', ['/', 'a', 'b', 'c']).

% Check absolute paths with . and ..
st(unix, '/.', ['/', '.']).
st(unix, '/./.', ['/', '.', '.']).
st(unix, '/././.', ['/', '.', '.', '.']).
st(unix, '/..', ['/', '..']).
st(unix, '/../..', ['/', '..', '..']).
st(unix, '/../../..', ['/', '..', '..', '..']).

% Check relative paths with . and ..
st(unix, './.', ['.', '.']).
st(unix, '././.', ['.', '.', '.']).
st(unix, '../..', ['..', '..']).
st(unix, '../../..', ['..', '..', '..']).
st(unix, './..', ['.', '..']).
st(unix, '../.', ['..', '.']).
st(unix, '.././..', ['..', '.', '..']).
st(unix, './../.', ['.', '..', '.']).

% Check relative paths with names
st(unix, 'a/b', ['a', 'b']).
st(unix, 'a/b/c', ['a', 'b', 'c']).

% Check interactions of ., .. and names
st(unix, './a', ['.', 'a']).
st(unix, '././a', ['.', '.', 'a']).
st(unix, './a/.', ['.', 'a', '.']).
st(unix, 'a/./.', ['a', '.', '.']).
st(unix, '../a', ['..', 'a']).
st(unix, '../../a', ['..', '..', 'a']).
st(unix, '../a/..', ['..', 'a', '..']).
st(unix, 'a/../..', ['a', '..', '..']).


% Check for incorrect interactions of . and .. with names
st(unix, '.a', ['.a']).
st(unix, 'a.', ['a.']).
st(unix, '..a', ['..a']).
st(unix, 'a..', ['a..']).
st(unix, '.a.', ['.a.']).
st(unix, '..a.', ['..a.']).
st(unix, '.a..', ['.a..']).
st(unix, '..a..', ['..a..']).

% MacOS tests

% Test basic elements
st(macos, '', []).
st(macos, 'a', ['a']).
st(macos, 'ab', ['ab']).
st(macos, 'abc', ['abc']).
st(macos, 'abcefghijklmnopqrstuvwxyz', ['abcefghijklmnopqrstuvwxyz']).
st(macos, 'ABCEFGHIJKLMNOPQRSTUVWXYZ', ['ABCEFGHIJKLMNOPQRSTUVWXYZ']).
% Note the zero character is not being tested because ALS Prolog doesn't support all characters.
st(macos, '\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037',
      ['\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037']).
st(macos, ' !"#$%&\'()*+,-./0123456789;<=>?@[\\]^_`{|}~', [' !"#$%&\'()*+,-./0123456789;<=>?@[\\]^_`{|}~']).
st(macos, ':', [':']).
st(macos, '::', ['::']).
st(macos, 'd:', ['d:']).

% Check absolute paths with names
st(macos, 'd:a', ['d:', 'a']).
st(macos, 'd:a:b', ['d:', 'a', 'b']).
st(macos, 'd:a:b:c', ['d:', 'a', 'b', 'c']).

% Check relative paths with :, ::, etc
st(macos, ':', [':']).
st(macos, '::', ['::']).
st(macos, ':::', ['::', '::']).
st(macos, '::::', ['::', '::', '::']).

% Check relative paths with names
st(macos, ':a:b', ['a', 'b']).
st(macos, ':a:b:c', ['a', 'b', 'c']).

% Check interactions of :: and names
st(macos, '::a', ['::', 'a']).
st(macos, ':::a', ['::', '::', 'a']).
st(macos, '::::a', ['::', '::', '::', 'a']).
st(macos, '::a::', ['::', 'a', '::']).
st(macos, ':a:::', ['a', '::', '::']).
st(macos, '::a:::', ['::', 'a', '::', '::']).
st(macos, ':a::::', ['a', '::', '::', '::']).

% Win32 Tests

% Test basic elements
st(win32, '', []).
st(win32, 'a', ['a']).
st(win32, 'ab', ['ab']).
st(win32, 'abc', ['abc']).
st(win32, 'abcefghijklmnopqrstuvwxyz', ['abcefghijklmnopqrstuvwxyz']).
st(win32, 'ABCEFGHIJKLMNOPQRSTUVWXYZ', ['ABCEFGHIJKLMNOPQRSTUVWXYZ']).
% Note the zero character is not being tested because ALS Prolog doesn't support all characters.
st(win32, '\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037',
      ['\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037']).
st(win32, ' !"#$%&\'()*+,-.0123456789:;<=>?@[]^_`{|}~', [' !"#$%&\'()*+,-.0123456789:;<=>?@[]^_`{|}~']).
st(win32, '.', ['.']).
st(win32, '..', ['..']).
st(win32, '\\', ['\\']).
st(win32, 'a:', ['a:']).
st(win32, 'a:\\', ['a:\\']).
st(win32, '\\\\a\\b\\', ['\\\\a\\b\\']).

% Check absolute paths with names
st(win32, '\\a', ['\\', 'a']).
st(win32, '\\a\\b', ['\\', 'a', 'b']).
st(win32, '\\a\\b\\c', ['\\', 'a', 'b', 'c']).

st(win32, 'c:\\a', ['c:\\', 'a']).
st(win32, 'c:\\a\\b', ['c:\\', 'a', 'b']).
st(win32, 'c:\\a\\b\\c', ['c:\\', 'a', 'b', 'c']).

st(win32, 'c:a', ['c:', 'a']).
st(win32, 'c:a\\b', ['c:', 'a', 'b']).
st(win32, 'c:a\\b\\c', ['c:', 'a', 'b', 'c']).

st(win32, '\\\\h\\s\\a', ['\\\\h\\s\\', 'a']).
st(win32, '\\\\h\\s\\a\\b', ['\\\\h\\s\\', 'a', 'b']).
st(win32, '\\\\h\\s\\a\\b\\c', ['\\\\h\\s\\', 'a', 'b', 'c']).

% Check absolute paths with . and ..
st(win32, '\\.', ['\\', '.']).
st(win32, '\\.\\.', ['\\', '.', '.']).
st(win32, '\\.\\.\\.', ['\\', '.', '.', '.']).
st(win32, '\\..', ['\\', '..']).
st(win32, '\\..\\..', ['\\', '..', '..']).
st(win32, '\\..\\..\\..', ['\\', '..', '..', '..']).

st(win32, 'c:\\.', ['c:\\', '.']).
st(win32, 'c:\\.\\.', ['c:\\', '.', '.']).
st(win32, 'c:\\.\\.\\.', ['c:\\', '.', '.', '.']).
st(win32, 'c:\\..', ['c:\\', '..']).
st(win32, 'c:\\..\\..', ['c:\\', '..', '..']).
st(win32, 'c:\\..\\..\\..', ['c:\\', '..', '..', '..']).

st(win32, 'c:.', ['c:', '.']).
st(win32, 'c:.\\.', ['c:', '.', '.']).
st(win32, 'c:.\\.\\.', ['c:', '.', '.', '.']).
st(win32, 'c:..', ['c:', '..']).
st(win32, 'c:..\\..', ['c:', '..', '..']).
st(win32, 'c:..\\..\\..', ['c:', '..', '..', '..']).

st(win32, '\\\\h\\s\\.', ['\\\\h\\s\\', '.']).
st(win32, '\\\\h\\s\\.\\.', ['\\\\h\\s\\', '.', '.']).
st(win32, '\\\\h\\s\\.\\.\\.', ['\\\\h\\s\\', '.', '.', '.']).
st(win32, '\\\\h\\s\\..', ['\\\\h\\s\\', '..']).
st(win32, '\\\\h\\s\\..\\..', ['\\\\h\\s\\', '..', '..']).
st(win32, '\\\\h\\s\\..\\..\\..', ['\\\\h\\s\\', '..', '..', '..']).

% Check relative paths with . and ..
st(win32, '.\\.', ['.', '.']).
st(win32, '.\\.\\.', ['.', '.', '.']).
st(win32, '..\\..', ['..', '..']).
st(win32, '..\\..\\..', ['..', '..', '..']).
st(win32, '.\\..', ['.', '..']).
st(win32, '..\\.', ['..', '.']).
st(win32, '..\\.\\..', ['..', '.', '..']).
st(win32, '.\\..\\.', ['.', '..', '.']).

% Check relative paths with names
st(win32, 'a\\b', ['a', 'b']).
st(win32, 'a\\b\\c', ['a', 'b', 'c']).

% Check interactions of ., .. and names
st(win32, '.\\a', ['.', 'a']).
st(win32, '.\\.\\a', ['.', '.', 'a']).
st(win32, '.\\a\\.', ['.', 'a', '.']).
st(win32, 'a\\.\\.', ['a', '.', '.']).
st(win32, '..\\a', ['..', 'a']).
st(win32, '..\\..\\a', ['..', '..', 'a']).
st(win32, '..\\a\\..', ['..', 'a', '..']).
st(win32, 'a\\..\\..', ['a', '..', '..']).


% Check for incorrect interactions of . and .. with names
st(win32, '.a', ['.a']).
st(win32, 'a.', ['a.']).
st(win32, '..a', ['..a']).
st(win32, 'a..', ['a..']).
st(win32, '.a.', ['.a.']).
st(win32, '..a.', ['..a.']).
st(win32, '.a..', ['.a..']).
st(win32, '..a..', ['..a..']).

% asymetric path tests

% split tests

% Test dangling directory separators
spt(unix, 'a/', [a]).
spt(unix, 'a/b/', [a, b]).
spt(unix, 'a/b/c/', [a, b, c]).
spt(unix, '/a/', ['/', a]).
spt(unix, '/a/b/', ['/', a, b]).
spt(unix, '/a/b/c/', ['/', a, b, c]).

% Check backslash spliting
spt(win32, 'a\\', [a]).
spt(win32, 'a\\b\\', [a, b]).
spt(win32, 'a\\b\\c\\', [a, b, c]).
spt(win32, '\\a\\', ['\\', a]).
spt(win32, '\\a\\b\\', ['\\', a, b]).
spt(win32, '\\a\\b\\c\\', ['\\', a, b, c]).
spt(win32, 'c:a\\', ['c:', a]).
spt(win32, 'c:a\\b\\', ['c:', a, b]).
spt(win32, 'c:a\\b\\c\\', ['c:', a, b, c]).
spt(win32, 'c:\\a\\', ['c:\\', a]).
spt(win32, 'c:\\a\\b\\', ['c:\\', a, b]).
spt(win32, 'c:\\a\\b\\c\\', ['c:\\', a, b, c]).
spt(win32, '\\\\h\\s\\a\\', ['\\\\h\\s\\', a]).
spt(win32, '\\\\h\\s\\a\\b\\', ['\\\\h\\s\\', a, b]).
spt(win32, '\\\\h\\s\\a\\b\\c\\', ['\\\\h\\s\\', a, b, c]).

% Check slash splitting.
spt(win32, 'a/', [a]).
spt(win32, 'a/b/', [a, b]).
spt(win32, 'a/b/c/', [a, b, c]).
spt(win32, '/a/', ['/', a]).
spt(win32, '/a/b/', ['/', a, b]).
spt(win32, '/a/b/c/', ['/', a, b, c]).
spt(win32, 'c:a/', ['c:', a]).
spt(win32, 'c:a/b/', ['c:', a, b]).
spt(win32, 'c:a/b/c/', ['c:', a, b, c]).
spt(win32, 'c:/a/', ['c:/', a]).
spt(win32, 'c:/a/b/', ['c:/', a, b]).
spt(win32, 'c:/a/b/c/', ['c:/', a, b, c]).
spt(win32, '//h/s/a/', ['//h/s/', a]).
spt(win32, '//h/s/a/b/', ['//h/s/', a, b]).
spt(win32, '//h/s/a/b/c/', ['//h/s/', a, b, c]).

% Check mixed splitting.
spt(win32, 'a/', [a]).
spt(win32, 'a\\b/', [a, b]).
spt(win32, 'a\\b/c\\', [a, b, c]).
spt(win32, '/a\\', ['/', a]).
spt(win32, '/a\\b/', ['/', a, b]).
spt(win32, '\\a/b\\c/', ['\\', a, b, c]).
spt(win32, 'c:a\\', ['c:', a]).
spt(win32, 'c:a/b\\', ['c:', a, b]).
spt(win32, 'c:a/b\\c/', ['c:', a, b, c]).
spt(win32, 'c:\\a/', ['c:\\', a]).
spt(win32, 'c:\\a/b\\', ['c:\\', a, b]).
spt(win32, 'c:/a\\b/c\\', ['c:/', a, b, c]).
spt(win32, '/\\h/s\\a/', ['/\\h/s\\', a]).
spt(win32, '\\/h\\s/a\\b/', ['\\/h\\s/', a, b]).
spt(win32, '\\/h\\s/a\\b/c\\', ['\\/h\\s/', a, b, c]).

spt(macos, ':a', [a]).
spt(macos, ':a:', [a]).
spt(macos, ':a:b:', [a, b]).
spt(macos, ':a:b:c:', [a, b, c]).
spt(macos, 'a:b:', ['a:', b]).
spt(macos, 'a:b:c:', ['a:', b, c]).

% Join tests.

jt(unix, ['a/', b], 'a/b').
jt(unix, [a, 'b/'], 'a/b/').
jt(unix, [a, b, 'c/', 'd/', 'e/'], 'a/b/c/d/e/').

jt(win32, ['a/', b], 'a/b').
jt(win32, ['a\\', b], 'a\\b').
jt(win32, [a, 'b/'], 'a\\b/').
jt(win32, [a, 'b\\'], 'a\\b\\').
jt(win32, [a, b, 'c/', 'd/', 'e/'], 'a\\b\\c/d/e/').
jt(win32, [a, b, 'c\\', 'd\\', 'e\\'], 'a\\b\\c\\d\\e\\').

% MacOS requires alot of join tests because of the ambiguous nature of
% the colon.

jt(macos, [':a', b], ':a:b').
jt(macos, [':a', ':b'], ':a:b').
jt(macos, [':a', '::b'], ':a::b').
jt(macos, [':a', ':::b'], ':a:::b').
jt(macos, [':a:', 'b'], ':a:b').
jt(macos, [':a:', ':b'], ':a:b').
jt(macos, [':a:', '::b'], ':a::b').
jt(macos, [':a:', ':::b'], ':a:::b').
jt(macos, [':a::', 'b'], ':a::b').
jt(macos, [':a::', ':b'], ':a::b').
jt(macos, [':a::', '::b'], ':a:::b').
jt(macos, [':a::', ':::b'], ':a::::b').
jt(macos, [':a:::', 'b'], ':a:::b').
jt(macos, [':a:::', ':b'], ':a:::b').
jt(macos, [':a:::', '::b'], ':a::::b').
jt(macos, [':a:::', ':::b'], ':a:::::b').

jt(macos, ['a:', 'b', ':c', ':d:', 'e'], 'a:b:c:d:e').
jt(macos, ['a:', 'b', '::c', ':d::', 'e'], 'a:b::c:d::e').
jt(macos, ['a:', 'b', ':c::', '::d:', 'e'], 'a:b:c:::d:e').

check_result(Test, ExpectedResult, Result) :-
	nonvar(Result),
	ExpectedResult = Result,
	!.
	%%printf('Done: %t\n', [Test]).
check_result(Test, ExpectedResult, Result) :-
	printf('Error in: %t\nExpected: %t But got: %t\n', [Test, ExpectedResult, Result]),
	throw(error(check_result(Test, ExpectedResult, Result),[])).

do_split_path(OS, Path, NewList) :-
	builtins:split_path(OS, Path, NewList), !.
do_split_path(_, _, split_path_failed).

do_join_path(OS, List, NewPath) :-
	builtins:join_path(OS, List, NewPath), !.
do_join_path(_, _, join_path_failed).

test :-
	st(OS, Path, List),
	do_split_path(OS, Path, NewList),
	check_result(split_path(OS, Path, List), List, NewList),
	do_join_path(OS, List, NewPath),
	check_result(join_path(OS, List, Path), Path, NewPath),
	fail.
test :-
	spt(OS, Path, List),
	do_split_path(OS, Path, NewList),
	check_result(split_path(OS, Path, List), List, NewList),
	fail.
test :-
	jt(OS, List, Path),
	do_join_path(OS, List, NewPath),
	check_result(join_path(OS, List, Path), Path, NewPath),
	fail.
test.

