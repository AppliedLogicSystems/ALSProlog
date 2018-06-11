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
	Options = [url=URL],
	cont_curl(Options, Target).

export curl/3.
curl(URL, [], Target)
	:-
	cont_curl([url=URL], Target).
curl(URL, Opts, Target)
	:-
	functor(Opts, '.', 2),
	!,
	add_url(URL, Opts, Options),
	cont_curl(Options, Target).
curl(URL, Opts, Target)
	:-
	printf('Arg %t must be a list!\n',[Opts]),
	fail.

add_url(URL, SourceOptions, SourceOptions)
	:-
	member( url =_, SourceOptions),
	!.
add_url(URL, SourceOptions, ['URL'=URL | SourceOptions]).
	
cont_curl(Options, Target)
	:-
	handle_target(Target, Options, TOptions),
	do_curl(TOptions).


handle_target(Target, Options, FTOptions)
	:-
	var(Target), 
	!,
	(member(uia=Target, Options) ->
		TOptions = Options 
		; 
		TOptions = [result=Target | Options] ),
	((member(writedata=_, TOptions); member('WRITEDATA'=_, TOptions)) -> 
		FTOptions = TOptions
		;
		FTOptions = ['WRITEDATA'=true | TOptions]
	).

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

export do_curl/1.
do_curl(Options)
	:-
	adjust_opts(Options, AdjOptions),
	cont_do_curl(AdjOptions).

adjust_opts([], []).
adjust_opts([Opt | Options], [AdjOpt | AdjOptions])
	:-
	adjust_req(Opt, AdjOpt),
	adjust_opts(Options, AdjOptions).


adjust_req(file(F), 'WRITEDATA'=F)
	:-!.
adjust_req(file = F, 'WRITEDATA'=F)
	:-!.

adjust_req(postfields(FF), 'POSTFIELDS'=FF)
	:-!.
adjust_req(postfields=FF, 'POSTFIELDS'=FF)
	:-!.

adjust_req(postdata(FF), 'READDATA'=FF)
	:-!.
adjust_req(postdata=FF, 'READDATA'=FF)
	:-!.

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
	adj_opt(UC_F, AdjOpt),
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
	curl_c_builtin(Options, Error),
	finish_curl_c(Error, Options).


finish_curl_c(Error, Options)
	:-
	var(Error), !.

finish_curl_c(Error, Options)
	:-
	Ball = curl_error(Error),
	throw(Ball).

endmod.

/* ================= SAMPLES ================== *
?- curl('http://example.com', X).

X='<!doctype html>\n<html>\n<head>\n    <title>......</a></p>\n</div>\n</body>\n</html>\n'

yes.

?- curl('http://example.com', ['RESPONSE_CODE'=RC,total_time=TT], X).

RC=200 
TT=1.172075 
X='<!doctype html>\n<html>\n<head>\n    <title>......</a></p>\n</div>\n</body>\n</html>\n'

yes.

?- curl('http://example.com', [writedata='./my_local_file.txt'], _).

yes.

Note: The local file ./my_local_file.txt is created an contains:
'<!doctype html>\n<html>\n<head>\n    <title>......</a></p>\n</div>\n</body>\n</html>\n'

?- curl('https://postman-echo.com/post', [postfields('name=admin&shoesize=12'),result=RR],_).

RR='{"args":{},"data":"","files":{},"form":{"name":"admin","shoesize":"12"},"headers":{"host":"postman-echo.com","content-length":"22","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"name":"admin","shoesize":"12"},"url":"https://postman-echo.com/post"}'

yes.

?- curl('https://postman-echo.com/post', [postdata='lorem ipsum doler',response_code(RC),total_time(TT)],X).

RC=200 
TT=2.970517 
X='{"args":{},"data":"","files":{},"form":{"lorem ipsum doler":""},"headers":{"host":"postman-echo.com","content-length":"17","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"lorem ipsum doler":""},"url":"https://postman-echo.com/post"}'

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
 * ============================================ */
