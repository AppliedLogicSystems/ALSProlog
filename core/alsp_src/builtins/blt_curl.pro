/*=====================================================================*
 |		blt_curl.pro		
 |	Copyright (c) 2018 Applied Logic Systems, Inc.
 |
 |	Direct to curl interface
 |
 | Author: Ken Bowen
 | "Direct to curl" suggested by Chuck Houpt
 |
 |	Simple Curl:
 |
 | curl(URL, Target) where URL addresses a remote page:
 |
 |    A) If Target is an uninstantiated variable, success will bind
 |	 Target to a UIA containing the contents of the remote page;
 |    B) If Target is an atom/UIA which is a path to a local file,
 |       success will copy the contents of the remote page to the
 |	 local file.
 |    C) If Target is of the form postfields(DD) where DD is a sequence
 |	 of equations with amperstands (e.g: 'name=admin&shoesize=12'),
 |	 then DD is POSTED (as JSON) to URL.
 |
 | curl/3  allows for explicit options; e.g:
 |
 | 	curl(URL, [rc(RC),result(RR)], Target) 
 |
 |	RC will be bound to the HTTP response status code
 |	RR will be bound to a UIA containing the response result from the
 |      remote server, except for case B above, where RR will be left unbound.
 |
 | ------ Special options:
 |      verbose	      		write prolog side details
 |	curl_verbose  		CURLOPT_VERBOSE = 1  (to be implemented)
 |
 *=====================================================================*/

module curl.

	/* ==================================== *
 	 |     SIMPLE CURL 
	 | Common opts are automatically added
	 | by curl_intf in bcurl.c:
 	 |	CURLOPT_NOPROGRESS = 1
 	 |      CURLOPT_USERAGENT  = 'curl/7.54.0'
 	 |      CURLOPT_MAXREDIRS  = 50
 	 |      CURLOPT_TCP_KEEPALIVE = 1
	 * ==================================== */

export curl/2.
curl(URL, Target)
	:-
	Opts = [], 
	add_url(URL, Opts, Options),
	cont_curl(Options, Target).

export curl/3.
curl(URL, Opts, Target)
	:-
	add_url(URL, Opts, Options),
	cont_curl(Options, Target).

add_url(URL, SourceOptions, SourceOptions)
	:-
	member( url =_, SourceOptions),
	!.
add_url(URL, SourceOptions, [url=URL | SourceOptions]).
	
cont_curl(Options, Target)
	:-
	ck_curl_vb(Options),
	add_implied_options(Options, ImpOptions),
	handle_target(Target, ImpOptions, TOptions),
	do_curl(TOptions).

ck_curl_vb(SourceOptions)
        :-
        member(verbose, SourceOptions),
	!,
	write(curlSourceOptions=SourceOptions),nl.
ck_curl_vb(_).

	/* For future use: */
add_implied_options(Options, Options).


handle_target(Target, Options, [uia(Target) | Options])
	:-
	var(Target),
	!.

handle_target(Target, Options, [file(Target) | Options])
	:-
	atom(Target),
	!.

handle_target(postfields(FF), Options, [postfields(FF) | Options])
	:-!.


/* Modes are passed to curl_intf in bcurl.c as integers: 

	Mode			Code
	----			----
	GET_uia			0
	GET_file		1
	POST_postfields		2
	POST_readcallback	3
 */

determine_modecode(Options, 0)
	:-
	member(uia(_), Options),
	!.
determine_modecode(Options, 1)
	:-
	member(file(_), Options),
	!.
determine_modecode(Options, 2)
	:-
	member(postfields(_), Options),
	!.
determine_modecode(Options, 3)
	:-
	member(postdata(_), Options),
	!.
	
setup_result(Options, Result)
	:-
	member(uia(Result), Options),
	(member(result(RR), Options) -> RR=Result ; true),
	!.
setup_result(Options, Result)
	:-
	member(result(Result), Options),
	!.
setup_result(Options, Result)
	:-
	member(res(Result), Options),
	!.
setup_result(Options, Result).

setup_response_code(Options, RC)
	:-
	member(rc(RC), Options),
	!.
setup_response_code(Options, RC).

do_curl(Options)
	:-
	determine_modecode(Options, ModeCode),
	member(url=URL, Options),
	setup_result(Options, Result),
	setup_response_code(Options, RC),
	cont_do_curl(ModeCode, URL, Result, RC, Options).

	%% mode = GET_uia:
cont_do_curl(0, URL, Result, RC, Options)
	:-!,
write('*calling '= curl_intf(0, URL, Result)),nl,
	curl_intf(0, URL, Result, RC, Options).

	%% mode = GET_file:
cont_do_curl(1, URL, Result, RC, Options)
	:-!,
	member(file(FilePath), Options),
write('*calling '= curl_intf(1, URL, FilePath)),nl,
	curl_intf(1, URL, Result, RC, FilePath).

cont_do_curl(2, URL, Result, RC, Options)
	:-!,
	member(postfields(FieldsUIA), Options),
write('*calling '= curl_intf(2, URL, FieldsUIA)),nl,
	curl_intf(2, URL, Result, RC, FieldsUIA).

cont_do_curl(3, URL, Result, RC, Options)
	:-!,
	member(postdata(PostData), Options),
write('*calling '= curl_intf(3, URL, PostData)),nl,
	curl_intf(3, URL, Result, RC, PostData).

endmod.

/* ================= SAMPLES ================== *
?- curl('http://example.com', X).

?- curl('http://example.com', './my_local_file.txt').

?- curl('https://postman-echo.com/post', postfields('name=admin&shoesize=12')).

 * ============================================ */

/* ==================EXAMPLES ================= *

?- curl('http://example.com', [rc(RC),result(RR)], './my_local_file.txt').

RC=200 
RR=RR 

yes.

?- curl('http://example.com/foo', [rc(RC),result(RR)], X).

RC=404 
RR='<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset="utf-8" />\n    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />\n    <meta name="viewport" content="width=device-width, initial-scale=1" />\n    <style type="text/css">\n    body {\n        background-color: #f0f0f2;\n        margin: 0;\n        padding: 0;\n        font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;\n        \n    }\n    div {\n        width: 600px;\n        margin: 5em auto;\n        padding: 50px;\n        background-color: #fff;\n        border-radius: 1em;\n    }\n    a:link, a:visited {\n        color: #38488f;\n        text-decoration: none;\n    }\n    @media (max-width: 700px) {\n        body {\n            background-color: #fff;\n        }\n        div {\n            width: auto;\n            margin: 0 auto;\n            border-radius: 0;\n            padding: 1em;\n        }\n    }\n    </style>    \n</head>\n\n<body>\n<div>\n    <h1>Example Domain</h1>\n    <p>This domain is established to be used for illustrative examples in documents. You may use this\n    domain in examples without prior coordination or asking for permission.</p>\n    <p><a href="http://www.iana.org/domains/example">More information...</a></p>\n</div>\n</body>\n</html>\n' 

X='<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset="utf-8" />\n    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />\n    <meta name="viewport" content="width=device-width, initial-scale=1" />\n    <style type="text/css">\n    body {\n        background-color: #f0f0f2;\n        margin: 0;\n        padding: 0;\n        font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;\n        \n    }\n    div {\n        width: 600px;\n        margin: 5em auto;\n        padding: 50px;\n        background-color: #fff;\n        border-radius: 1em;\n    }\n    a:link, a:visited {\n        color: #38488f;\n        text-decoration: none;\n    }\n    @media (max-width: 700px) {\n        body {\n            background-color: #fff;\n        }\n        div {\n            width: auto;\n            margin: 0 auto;\n            border-radius: 0;\n            padding: 1em;\n        }\n    }\n    </style>    \n</head>\n\n<body>\n<div>\n    <h1>Example Domain</h1>\n    <p>This domain is established to be used for illustrative examples in documents. You may use this\n    domain in examples without prior coordination or asking for permission.</p>\n    <p><a href="http://www.iana.org/domains/example">More information...</a></p>\n</div>\n</body>\n</html>\n' 

yes.

?- curl('http://example.com', [rc(RC),result(RR)], X).

RC=200 
RR='<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset="utf-8" />\n    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />\n    <meta name="viewport" content="width=device-width, initial-scale=1" />\n    <style type="text/css">\n    body {\n        background-color: #f0f0f2;\n        margin: 0;\n        padding: 0;\n        font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;\n        \n    }\n    div {\n        width: 600px;\n        margin: 5em auto;\n        padding: 50px;\n        background-color: #fff;\n        border-radius: 1em;\n    }\n    a:link, a:visited {\n        color: #38488f;\n        text-decoration: none;\n    }\n    @media (max-width: 700px) {\n        body {\n            background-color: #fff;\n        }\n        div {\n            width: auto;\n            margin: 0 auto;\n            border-radius: 0;\n            padding: 1em;\n        }\n    }\n    </style>    \n</head>\n\n<body>\n<div>\n    <h1>Example Domain</h1>\n    <p>This domain is established to be used for illustrative examples in documents. You may use this\n    domain in examples without prior coordination or asking for permission.</p>\n    <p><a href="http://www.iana.org/domains/example">More information...</a></p>\n</div>\n</body>\n</html>\n' 

X='<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset="utf-8" />\n    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />\n    <meta name="viewport" content="width=device-width, initial-scale=1" />\n    <style type="text/css">\n    body {\n        background-color: #f0f0f2;\n        margin: 0;\n        padding: 0;\n        font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;\n        \n    }\n    div {\n        width: 600px;\n        margin: 5em auto;\n        padding: 50px;\n        background-color: #fff;\n        border-radius: 1em;\n    }\n    a:link, a:visited {\n        color: #38488f;\n        text-decoration: none;\n    }\n    @media (max-width: 700px) {\n        body {\n            background-color: #fff;\n        }\n        div {\n            width: auto;\n            margin: 0 auto;\n            border-radius: 0;\n            padding: 1em;\n        }\n    }\n    </style>    \n</head>\n\n<body>\n<div>\n    <h1>Example Domain</h1>\n    <p>This domain is established to be used for illustrative examples in documents. You may use this\n    domain in examples without prior coordination or asking for permission.</p>\n    <p><a href="http://www.iana.org/domains/example">More information...</a></p>\n</div>\n</body>\n</html>\n' 

yes.

?- curl('https://postman-echo.com/post', [rc(RC),result(RR)], postfields('name=admin&shoesize=12')).

RC=200 
RR='{"args":{},"data":"","files":{},"form":{"name":"admin","shoesize":"12"},"headers":{"host":"postman-echo.com","content-length":"22","accept":"*/*","content-type":"application/x-www-form-urlencoded","user-agent":"curl/7.54.0","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"name":"admin","shoesize":"12"},"url":"https://postman-echo.com/post"}' 

yes.

 * ============================================ */
