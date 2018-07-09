/*
Tests for http/3 and curl/1, /2, /3

Run simple PHP echo server in tsuite dir script:

tsuite/echo/serve

*/

test :- test([
	test_sio_url,
	test_http,
	test_curl_porceline,
	test_errors,
	
	true
]).

test(List) :-
	test(List, Result),
	Result.
	
test([], true) :- !.
test([], fail).
test([true | Tail], Result) :- !, test(Tail, Result).
test([Goal | Tail], Result) :-
	(
		copy_term(Goal, RGoal),
		catch(RGoal, Error, (write('Uncaught Error: '), write(Error), nl, fail)),
		write('\033[32m  OK: ')
		;
		Result=fail,
		write('\033[31mFAIL: ')
	),
	write_term(Goal, [quoted(true), line_length(180)]
	),
	write('\033[0m'), nl,
	!, test(Tail, Result).

test_sio_url :- 
	printf('>>>> BEGIN test_sio_url <<<<\n', []),
	test([
	( open(url('http://localhost:8888/abc'), read, S), get_line(S, abc), close(S) ),
	( open(url('http://localhost:8888/abc', []), read, S), get_line(S, abc), close(S) ),

	%% Test term reading
	( open(url('http://localhost:8888/myatom.'), read, S), read(S, myatom), close(S) ),
	( open(url('http://localhost:8888/myfunc(a,b,[l1,l2]).'), read, S), read(S, myfunc(a, b, [l1, l2])), close(S) ),

	( open(url('http://localhost:8888/?REQUEST_METHOD'), read, S), get_line(S, 'GET'), close(S) ),

	( open(url('http://localhost:8888/'), write, S), write(S, abc), close(S) ),
	( open(url('http://localhost:8888/', [ result=abc ]), write, S), write(S, abc), close(S) ),
	( open(url('http://localhost:8888/', [ result=R ]), write, S), write(S, abc), close(S), abc == R ),
	true
]).

test_http :- 
	printf('>>>> BEGIN test_http <<<<\n', []),
	test([

	%% test minimal REST requests
	http(get, 'http://localhost:8888/', []),
	http(post, 'http://localhost:8888/', []),
	http(put, 'http://localhost:8888/', []),
	http(delete, 'http://localhost:8888/', []),

	%% test basic get results
	http(get, 'http://localhost:8888/', [ result='' ]),
	http(get, 'http://localhost:8888/abc', [ result=abc ]),
	http(get, 'http://localhost:8888/abc', [ result='abc' ]),
	not http(get, 'http://localhost:8888/abc', [ result=xyz ]),
	(http(get, 'http://localhost:8888/abc', [ result=R ]), R == abc),
	http(get, 'http://localhost:8888/?REQUEST_METHOD', [ result='GET' ]),

	%% test option variations
	http(get, 'http://localhost:8888/?HTTP_USER_AGENT', [useragent='007', result='007']),
	http(get, 'http://localhost:8888/?HTTP_USER_AGENT', ['CURLOPT_USERAGENT'='007', result='007']),
	http(get, 'http://localhost:8888/?HTTP_USER_AGENT', ['USERAGENT'='007', result='007']),
	http(get, 'http://localhost:8888/?HTTP_USER_AGENT', ['UsErAgEnT'='007', result='007']),

	%% test int option
	http(get, 'http://localhost/abc', [port=8888, result=abc]),

	%% test info string, int, float
	http(get, 'http://localhost:8888/abc', [effective_url='http://localhost:8888/abc', result=abc]),
	http(get, 'http://localhost:8888/abc', [response_code=200, result=abc]),
	http(get, 'http://localhost:8888/abc', [size_download=3.0, result=abc]),

	%% test methods
	http(get, 'http://localhost:8888/?REQUEST_METHOD', [result='GET']),
	http(post, 'http://localhost:8888/?REQUEST_METHOD', [result='POST']),
	http(put, 'http://localhost:8888/?REQUEST_METHOD', [result='PUT']),
	http(delete, 'http://localhost:8888/?REQUEST_METHOD', [result='DELETE']),

	%% test postfields
	http(post, 'http://localhost:8888/', [fields=abc, result=abc]),
	not http(post, 'http://localhost:8888/', [fields=abc, result=xyz]),
	true
]).


test_curl_porceline :- 
	printf('>>>> BEGIN test_curl_porceline <<<<\n', []),
	test([

	%% curl/1 with url atom
%	curl('http://localhost:8888'),
	not curl('http://localhost:8888'),

	%% curl/2
	(curl('http://localhost:8888/',R), '' == R),
	curl('http://localhost:8888/', ''),
	curl('http://localhost:8888/abc', abc),
	curl('http://localhost:8888/abc', 'abc'),
	not curl('http://localhost:8888/abc', 'xyz'),
	(curl('http://localhost:8888/abc', R), abc == R),
	curl('http://localhost:8888/?REQUEST_METHOD', 'GET'),

	%% curl/3
	curl('http://localhost:8888/abc', [], 'abc'),
	not curl('http://localhost:8888/abc', [], 'xyz'),
	(curl('http://localhost:8888/abc', [], R), abc == R),
	curl('http://localhost:8888/', [], ''),
	curl('http://localhost:8888/?REQUEST_METHOD', [], 'GET'),

	%% test option variations
	curl('http://localhost:8888/?HTTP_USER_AGENT', [useragent='007'], '007'),
	curl('http://localhost:8888/?HTTP_USER_AGENT', ['CURLOPT_USERAGENT'='007'], '007'),
	curl('http://localhost:8888/?HTTP_USER_AGENT', ['USERAGENT'='007'], '007'),
	curl('http://localhost:8888/?HTTP_USER_AGENT', ['UsErAgEnT'='007'], '007'),

	%% test int option
	curl('http://localhost/abc', [port=8888], abc),
	
	%% test info string, int, float
	curl('http://localhost:8888/abc', [effective_url='http://localhost:8888/abc'], abc),
	curl('http://localhost:8888/abc', [response_code=200], abc),
	curl('http://localhost:8888/abc', [size_download=3.0], abc),
	
	%% test post
	curl('http://localhost:8888/?REQUEST_METHOD', [post=1], 'POST'),

	%% test postfields
	curl('http://localhost:8888/', [postfields=abc], abc),
	not curl('http://localhost:8888/', [postfields=abc], xyz),
	true
]).


test_errors :- 
	printf('>>>> BEGIN test_errors <<<<\n', []),
	test([
	catch(curl, error(existence_error(procedure,user:curl),[user:curl]), true),
	catch(curl(_), error(instantiation_error,[curl:curl(tbd)]), true),
	catch(curl(1), error(domain_error(tbd,tbd),[curl:curl(tbd)]), true),
	catch(curl(functor(a)), error(domain_error(tbd,tbd),[user:curl(tbd)]), true),
	true
]).
