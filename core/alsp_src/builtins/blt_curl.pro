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
	Options = ['URL'=URL],
	cont_curl(Options, Target).

export curl/3.
curl(URL, [], Target)
	:-!,
	cont_curl(['URL'=URL], Target).
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
	(member( url =_, SourceOptions); member( 'URL' =_, SourceOptions)),
	!.
add_url(URL, SourceOptions, ['URL'=URL | SourceOptions]).
	
cont_curl(Options, Target)
	:-
	uc_unw(Options, MOptions),
	handle_target(Target, MOptions, TOptions),
	do_curl(TOptions).

uc_unw([], []).
uc_unw([Opt | Options], [MOpt | MOptions])
	:-
	mod_uc_unw(Opt, MOpt),
	uc_unw(Options, MOptions).

mod_uc_unw(Opt, AdjTag=A)
	:-
	functor(Opt, F, 1),
	make_uc_sym(F, UC_F),
	adj_opt(UC_F, AdjTag),
	!,
	arg(1, Opt, A).

mod_uc_unw(L=R, AdjOpt=R)
	:-
	atom(L),
	make_uc_sym(L, UC_L),
	adj_opt(UC_L, AdjOpt),
	!.

mod_uc_unw(Exp, _)
	:-
	printf('Unknown curl option: %t\n', [Exp]),
	!,
	fail.
	
adj_opt('UIA', 'RESULT') :-!.
adj_opt('FILE', 'WRITEDATA') :-!.
adj_opt('RES', 'RESULT') :-!.  
adj_opt(Opt, Opt).

handle_target(Target, Options, WTOptions)
	:-
	var(Target), 
	!,
	check_uia_target(Target, Options, TOptions),
	check_writedata(TOptions, WTOptions).

handle_target(Target, Options, ['RESULT' = Target | WOptions])
	:-
	check_writedata(Options, WOptions).

check_uia_target(Target, Options, Options)
	:-
	(member('UIA'=Target, Options) ; 
		member('RESULT'=Target, Options) ),
	!.

check_uia_target(Target, Options, ['RESULT' = Target | Options]).

check_writedata(Options, Options)
	:-
	( member('WRITEDATA'=true, Options) ;
	  member('POSTFIELDS'=_, Options) ;
	  member('READDATA'=_, Options) ),
	!.
check_writedata(Options, ['WRITEDATA'=true | Options]).


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

Note: The local file ./my_local_file.txt is created and contains:
'<!doctype html>\n<html>\n<head>\n    <title>......</a></p>\n</div>\n</body>\n</html>\n'

?- curl('https://postman-echo.com/post', [postfields('name=admin&shoesize=12'),result=RR],_).

RR='{"args":{},"data":"","files":{},"form":{"name":"admin","shoesize":"12"},"headers":{"host":"postman-echo.com","content-length":"22","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"name":"admin","shoesize":"12"},"url":"https://postman-echo.com/post"}'

yes.

?- curl('https://postman-echo.com/post', [readdata='lorem ipsum doler',response_code(RC),total_time(TT)],X).

RC=200 
TT=1.816543 
X='{"args":{},"data":"","files":{},"form":{"lorem ipsum doler":""},"headers":{"host":"postman-echo.com","content-length":"17","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"lorem ipsum doler":""},"url":"https://postman-echo.com/post"}' 

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
 * ============================================ */
