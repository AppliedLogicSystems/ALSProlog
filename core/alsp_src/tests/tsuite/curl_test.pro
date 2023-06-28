/*
Tests for http/3 and curl/1, /2, /3

Run simple PHP echo server in tsuite dir script:

tsuite/echo/serve

*/

:- [test].

test :- test([
	test_sio_url,
	test_http,
	test_curl_porceline,
	test_curl_plumbing,
	test_errors,
	
	true
]).

test_sio_url :- 
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
	(http(get, 'http://localhost:8888/abc', [ result(R) ]), R == abc),
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
	test([

	%%curl/1
	not curl([]),

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

test_curl_plumbing :- test([

	%% curl/1 with url atom, for debugging
%	curl('http://localhost:8888'),
% SEE test_errors: catch(curl(1), error(type_error(list,1),[curl:curl(1)]), true),
	
	%% curl/1 with options list
	curl([url='http://localhost:8888']),
	(curl([url='http://localhost:8888', result=R]), '' == R),
	curl([url='http://localhost:8888', result='']),
	curl([url='http://localhost:8888/abc', result=abc]),

	true
]).


test_errors :- 
	test([
	catch(curl, error(existence_error(procedure,user:curl),[user:curl]), true),
	catch(curl(_), error(instantiation_error,[curl:curl(tbd)]), true),
	catch(curl(1), error(type_error(list,1),[curl:curl(1)]), true),
	catch(curl(functor(a)), error(type_error(list,functor(a)),[curl:curl(functor(a))]), true),

%	catch(curl([]), error(curl_error(_tbd)), true),
% see test_curl_porceline: not(curl([])): curl_c_builtin([],_8521) fails.
	
	%% test string option type errors (url and useragent should behave identically)
	catch(curl([url=_]), error(instantiation_error, _), true),
	catch(curl([url=1]), error(type_error('atom_non-[]',1), _), true),
	catch(curl([url=[]]), error(type_error('atom_non-[]',[]), _), true),
	catch(curl([url=functor(a)]), error(type_error('atom_non-[]',functor(a)), _), true),
	catch(curl([useragent=_]), error(instantiation_error, _), true),
	catch(curl([useragent=1]), error(type_error('atom_non-[]',1), _), true),
%	catch(curl([useragent=[]]), error(type_error('atomic_or_[]',[]), _), true),
	catch(curl([useragent=[]]), error(type_error('atom_non-[]',[]), _), true),
%	catch(curl([useragent=functor(a)]), error(type_error('atomic_or_[]',functor(a)), _), true),
	catch(curl([useragent=functor(a)]), error(type_error('atom_non-[]',functor(a)), _), true),
	
	%% test int option type errors
	catch(curl([port=_]), error(instantiation_error, _), true),
	catch(curl([port=a]), error(type_error(integer,a), _), true),
	catch(curl([port=[]]), error(type_error(integer,[]), _), true),
	catch(curl([port=functor(a)]), error(type_error(integer,functor(a)), _), true),
	
	true
]).
