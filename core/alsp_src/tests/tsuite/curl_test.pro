/*
Tests for curl/1, /2, /3

Run simple PHP echo server in tsuite dir script:

tsuite/echo/serve

*/

test :-
	test_porceline,
%%	test_plumbing,
%%	test_errors,
	
	true.

test_porceline :-

	%% curl/1 with url atom
	nyi( curl('http://localhost:8888') ),

	%% curl/2
	curl('http://localhost:8888/', ''),
	curl('http://localhost:8888/abc', abc),
	curl('http://localhost:8888/abc', 'abc'),
	not curl('http://localhost:8888/abc', 'xyz'),
	curl('http://localhost:8888/abc', X1), X1 = abc,
	curl('http://localhost:8888/?REQUEST_METHOD', 'GET'),

	%% curl/3
	curl('http://localhost:8888/abc', [], 'abc'),
	not curl('http://localhost:8888/abc', [], 'xyz'),
	curl('http://localhost:8888/abc', [], X2), X2 = abc,
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
	curl('http://localhost:8888/abc', [postfields='data'], abc),
	not curl('http://localhost:8888/abc', [postfields='data'], xyz),
	
	true.
	
test_porceline :-
	write('test_porceline failed'), nl,
	fail.

nyi(X) :- write('Not Yet Implemented: '), write(X), nl.


test_errors :-
	catch(curl, error(existence_error(procedure,user:curl),[user:curl]), true),
	catch(curl(_), error(existence_error(procedure,user:curl),[user:curl]), true),
	catch(curl(1), error(existence_error(procedure,user:curl),[user:curl]), true),
	catch(curl(functor(a)), error(existence_error(procedure,user:curl),[user:curl]), true),
	true.


test_plumbing :-
	catch(curl([]), error(existence_error(procedure,user:asdfas),[user:asdfas]), true),
	true.	
