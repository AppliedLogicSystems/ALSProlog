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
	handle_target(Target, Options, TOptions),
	do_curl(TOptions).


handle_target(Target, Options, [uia=Target | Options])
	:-
	var(Target), !.

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

adjust_opts([], []).
/* set config to handle undefined skip_opt/1 correctly: 
adjust_opts([Opt | Options], AdjOptions)
	:-
	skip_opt(Opt),
	!,
	adjust_opts(Options, AdjOptions).
*/
adjust_opts([Opt | Options], [AdjOpt | AdjOptions])
	:-
	adjust_opt(Opt, AdjOpt),
	adjust_opts(Options, AdjOptions).

adjust_opt(L=R, UC_L=R)
	:-
	make_uc_sym(L, UC_L),
	!.
	
adjust_opt(Opt, UC_F=A)
	:-
	functor(Opt, F, 1),
	make_uc_sym(F, UC_F),
	!,
	arg(1, Opt, A).

adjust_opt(Opt, F=A)
	:-
	Ball = error(curl_error(unknow_option(Opt))),
	throw(Ball).
	
cont_do_curl(Options)
	:-
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
