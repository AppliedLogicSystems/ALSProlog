/*=====================================================================*
 |		blt_curl.pro		
 |	Copyright (c) 2018 Applied Logic Systems, Inc.
 |
 |	Direct to curl interface
 |
 | Author: Chuck Houpt, Ken Bowen
 *=====================================================================*/

module curl.

export curl/2.
curl(URL, Target)
	:-
	Options = [url=URL, writedata=true],
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
	handle_target(Target, Options, TOptions),
	do_curl(TOptions).


handle_target(Target, Options, TOptions)
	:-
	var(Target), 
	!,
	(member(uia=Target, Options) ->
		TOptions = Options 
		; 
		TOptions = [uia=Target | Options] ).

handle_target(Target, Options, [file=Target | Options])
	:-
	atom(Target), !.

handle_target(postfields=FF, Options, [postfields=FF | Options])
	:-!.
handle_target(postfields(FF), Options, [postfields=FF | Options])
	:-!.

handle_target(postdata=FF, Options, [postdata=FF | Options])
	:-!.
handle_target(postdata(FF), Options, [postdata=FF | Options])
	:-!.

do_curl(Options)
	:-
	adjust_opts(Options, AdjOptions),
	cont_do_curl(AdjOptions).

        /* make "stdopts" expand to set these "standard" common options
    ret += curl_easy_setopt(easyhandle, CURLOPT_MAXREDIRS, 50L);
    ret += curl_easy_setopt(easyhandle, CURLOPT_TCP_KEEPALIVE, 1L);
    ret += curl_easy_setopt(easyhandle, CURLOPT_USERAGENT, "curl/7.54.0");
        */

adjust_opts([], []).
adjust_opts([Opt | Options], [AdjOpt | AdjOptions])
	:-
	adjust_req(Opt, AdjOpt),
	adjust_opts(Options, AdjOptions).


adjust_req(L=R, AdjOpt=R)
	:-
	atom(L),
	make_uc_sym(L, UC_L),
	adj_opt(UC_L, AdjOpt),
	!.

adjust_req(Opt, AdjOpt=A)
	:-
	functor(Opt, F, 1),
	make_uc_sym(F, UC_F),
	adj_opt(UC_L, AdjOpt),
	!,
	arg(1, Opt, A).

adjust_req(Opt, _)
	:-
	Ball = error(curl_error(unknown_option(Opt))),
	throw(Ball).
	

adj_opt('UIA', 'RESULT') :-!.
adj_opt('RES', 'RESULT') :-!.  
adj_opt(Opt, Opt).


cont_do_curl(Options)
	:-
write('>>curl_c_builtin'=Options),nl,
	curl_c_builtin(Options, Error),
	finish_curl_c(Error, Options).


finish_curl_c(Error, Options)
	:-
	var(Error), !.

finish_curl_c(Error, Options)
	:-
	Ball = error(curl_error(Error), Options),
	throw(Ball).

endmod.

/* ================= SAMPLES ================== *
?- curl('http://example.com', X).

?- curl('http://example.com', './my_local_file.txt').

?- curl('https://postman-echo.com/post', postfields('name=admin&shoesize=12')).

 * ============================================ */
