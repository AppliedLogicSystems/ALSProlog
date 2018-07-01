/*=====================================================================*
 |		blt_curl.pro		
 |	Copyright (c) 2018 Applied Logic Systems, Inc.
 |
 |	Direct to curl interface
 |	REST-inspired user level
 |
 | Author: Chuck Houpt, Ken Bowen
 *=====================================================================*/

module curl.

/* ------------------------------------------------------------------------------*
 |    http(RESTVerb, URL, Options)
 |
 | http(get, URL, [RESULT=VAR | <other CURLOPT/CURLINFO options>])
 |	Performs GET from URL, makes result into a UIA, and unifies with VAR.
 |	
 | http(get, URL, [FILE=FF | <other CURLOPT/CURLINFO options>])
 |	Performs GET from URL, and writes result into local file FF.
 |	
 |	Below: if RESULT=VAR is on the options list, any result returned from
 |		 the server is made into a UIA and unified with VAR:
 | http(post, URL, [FIELDS=FF | <other CURLOPT/CURLINFO options>])
 |	Performs POST to URL, using option CURLOPT_POSTFIELDS=FF; assumes that
 |	FF is an atom expressing fields values.
 |		
 | http(post, URL, [FIELDS='', FILE=FF | <other CURLOPT/CURLINFO options>])
 |	Reads local file FF to obtain an atom AFF expressing fields values, and
 |	then performs POST to URL, using option CURLOPT_POSTFIELDS=AFF; 
 |		
 | http(post, URL, [DATA=DD | <other CURLOPT/CURLINFO options>])
 |	Performs POST to URL, using option CURLOPT_READDATA=FF; assumes that
 |	DD is an atom.
 |		
 | http(post, URL, [DATA='', FILE=FF | <other CURLOPT/CURLINFO options>])
 |	Reads local file FF to obtain an atom AFF, and then performs POST to URL, 
 |	using option CURLOPT_READDATA=AFF.
 |		
 | http(put, URL, [DATA=DD | <other CURLOPT/CURLINFO options>])
 |	Performs a PUT to URL, using option CURLOPT_UPLOAD together with
 |	CURLOPT_READDATA=FF; assumes that DD is an atom.
 |		
 | http(put, URL, [DATA='', FILE=FF | <other CURLOPT/CURLINFO options>])
 |	Reads local file FF to obtain an atom AFF, and then performs a PUT to URL, 
 |	using option CURLOPT_UPLOAD together with CURLOPT_READDATA=FF; 
 |	assumes that DD is an atom.
 |		
 * ------------------------------------------------------------------------------*/

export http/3.
http(RESTVerb, URL, Options)
	:-
	member(RESTVerb, [get,post,put,delete]),
	!,
	uppercase_unwind(Options, UUOptions),
	refine_opts(RESTVerb, URL, UUOptions, ROptions),
	do_curl(ROptions).
	
	
http(RESTVerb, URL, Options)
	:-
	printf('Unsupported or unknown REST verb: %t\n', [RESTVerb]).

export uppercase_unwind/2.
uppercase_unwind([], []).
uppercase_unwind([Opt | Options], [MOpt | MOptions])
	:-
	uc_unw(Opt, MOpt),
	uppercase_unwind(Options, MOptions).

export uc_unw/2.
uc_unw(Opt, UC_F=A)
	:-
	functor(Opt, F, 1),
	!,
	make_uc_sym(F, UC_F),
	arg(1, Opt, A).

uc_unw(L=R, UC_L=R)
	:-
	atom(L),
	!,
	make_uc_sym(L, UC_L).

uc_unw(Exp, _)
	:-
	printf('Unknown curl option: %t\n', [Exp]),
	!,
	fail.
	
refine_opts(get, URL, Options, ['HTTPGET'=1,'URL'=URL | ROptions])
	:-
	    % In all cases of duplicate tags, we'll use the outermost (first encountered):
	delete_from(Options, 'FILE', Options1, FILEExprs),
	delete_from(Options1, 'WRITEDATA', Options2, WRITEDATAExprs),
	get_ck_file_wrd(FILEExprs, WRITEDATAExprs, Options2, ROptions).

refine_opts(post, URL, Options, ['POST'=1, 'URL'=URL |ROptions])
	:-
	    % In all cases of duplicate tags, we'll use the outermost (first encountered):
	delete_from(Options, 'FILE', Options1, FILEExprs),
	delete_from(Options1, 'FIELDS', Options2, FIELDSExprs),
	delete_from(Options2, 'DATA', Options3, DATAExprs),
	post_ck_file_fd(FILEExprs, FIELDSExprs, DATAExprs, Options3, ROptions).

refine_opts(put, URL, Options, ['PUT'=1, 'URL'=URL, 'UPLOAD'=1 | ROptions])
	:-
	delete_from(Options, 'PUT', Options0, _),
	delete_from(Options0, 'UPLOAD', Options1, _),
	delete_from(Options1, 'FILE', Options2, FILEExprs),
	delete_from(Options2, 'DATA', Options3, DATAExprs),
	put_ck_file_fd(FILEExprs, DATAExprs, Options3, ROptions).

refine_opts(delete, URL, Options, _)
	:-
	printf('HTTP DELETE not yet implemented.\n', []),
	!,
	fail.

	/* ------------------------------------------------------------------ *
	 |    get_ck_file_wrd(FILEExprs, WRITEDATAExprs, Options2, ROptions)
	 * ------------------------------------------------------------------ */
	%% No 'FILE'= and no 'WRITEDATA' =
	%% so for 'get', we need to specify 'WRITEDATA'=true
get_ck_file_wrd([], [], Options2, ['WRITEDATA'=true | Options2]) 
	:-!.

	%% There is a 'FILE'=FileExpr but no 'WRITEDATA' =
	%% Assume FileExpr is an atom naming a file, 
	%% and so for 'get', we need to specify 'WRITEDATA'=FileExpr
get_ck_file_wrd([FileExpr | _], [], Options2, ['WRITEDATA'=FileExpr |  Options2]) 
	:-!.

	%% There is no 'FILE'=FILEExpr but there is a 'WRITEDATA' =
	%% Assume WRITEDATAExpr is 'true' or is an atom naming a file, 
	%% and so for 'get', we need to specify 'WRITEDATA'=FileExpr
get_ck_file_wrd([], [WRITEDATAExpr | _], Options2, ['WRITEDATA'=WRITEDATAExpr |  Options2]) 
	:-!.

	%% There are both a 'FILE'=FileExpr and a 'WRITEDATA' = WRITEDATAExpr
	%% Assume FileExpr is an atom naming a file, 
	%% Let the 'FILE'=FileExpr trump the 'WRITEDATA' = WRITEDATAExpr, but
	%% warn the user if the two expressions are different
get_ck_file_wrd([FileExpr | _], [WRITEDATAExpr | _], Options2, ['WRITEDATA'=FileExpr |  Options2])
	:-
	(FileExpr \== WRITEDATAExpr ->
		printf('Warning: CURL conflict between options ''FILE''=%t and ''WRITEDATA''=%t\n',
			[FileExpr, WRITEDATAExpr]),
		printf('Using: ''WRITEDATA''=%t\n', [FileExpr])
		;
		true
	).
		
	/* ---------------------------------------------------------------------- *
	 | post_ck_file_fd(FILEExprs, FIELDSExprs, DATAExprs, Options3, ROptions)
	 * ---------------------------------------------------------------------- */
	%% No 'FILE'= and no 'FIELDS' = and no 'DATA'=
	%% For 'post', we need some sort of fields or data to send;
	%% So this is an error:
post_ck_file_fd([], [], [], Options, Options).
	:-!.

	%% No 'FILE'= and no 'DATA'= but there is a 'FIELDS'=
post_ck_file_fd([], [FIELDSExpr | _], [], Options3, ['POSTFIELDS'=FIELDSExpr |  Options3]) 
	:-!.

	%% No 'FILE'= and no 'FIELDS'= but there is a 'DATA'=
post_ck_file_fd([], [], [DATAExpr | _], Options3, ['READDATA'=DATAExpr |  Options3]) 
	:-!.

	%% No 'FILE'= but there is a 'FIELDS'= and there is a 'DATA'=
	%% Let the 'FIELDS'=FF trump the 'DATA'=DD:
post_ck_file_fd([], [FIELDSExpr | _], [DATAExpr | _], Options3, ['POSTFIELDS'=FIELDSExpr |  Options3]) 
	:-!.

	%% There is a 'FILE'=fields(...), but no 'FIELDS'='', and no 'DATA'= 
post_ck_file_fd([fields(SourceFile) | _], [], [], Options3, ['POSTFIELDS'=FIELDSExpr |  Options3]) 
	:-!,
	grab_as_atom(SourceFile, FIELDSExpr).

	%% There is a 'FILE'=data(...), but no 'FIELDS'=, and no 'DATA'=
post_ck_file_fd([data(SourceFile) | _], [], [], Options3, ['READDATA'=DATAExpr |  Options3]) 
	:-!,
	grab_as_atom(SourceFile, DATAExpr).

post_ck_file_fd([FileExpr | _], [FieldsExpr | _], [DataExpr | _], _, _) 
	:-
	printf('Error: Can''t resolve CURL conflict between options ''FILE''=%t, ''FIELDS''=%t, ''DATA''=%t\n',
		[FileExpr, FieldsExpr, DataExpr]),
	fail.


	/* ---------------------------------------------------------------------- *
	 | put_ck_file_fd(FILEExprs, DATAExprs, Options3, ROptions)
	 * ---------------------------------------------------------------------- */
	%% No 'FILE'= and no 'DATA'=
	%% For 'put', we need some sort of fields or data to send;
	%% So this is an error:
put_ck_file_fd([], [],  _, _) 
	:-!,
	fail.

	%% No 'FILE'= but there is a 'DATA'=
put_ck_file_fd([], [DATAExpr | _], Options3, ['READDATA'=DATAExpr |  Options3]) 
	:-!.

	%% There is a 'FILE'=data(...), but no 'DATA'=
put_ck_file_fd([data(SourceFile) | _],  [], Options3, ['READDATA'=DATAExpr |  Options3]) 
	:-!,
	grab_as_atom(SourceFile, DATAExpr).

put_ck_file_fd([FileExpr | _], [DataExpr | _], _, _) 
	:-
	printf('Error: Can''t resolve CURL conflict between options ''FILE''=%t, ''FIELDS''=%t, ''DATA''=%t\n',
		[FileExpr, FieldsExpr, DataExpr]),
	fail.


export delete_from/4.
delete_from([], _, [], []).

delete_from([Tag=Val | RestList], Tag, RestResult, [Val | RestDel])
	:-!,
	delete_from(RestList, Tag, RestResult, RestDel).

delete_from([Eq | RestList], Tag, [Eq | RestResult], RestDel)
	:-!,
	delete_from(RestList, Tag, RestResult, RestDel).
	
export grab_as_atom/2.
grab_as_atom(File, Atom)
        :-
        grab_lines(File, Lines),
        open(string(String), write, S),
        write_lines_to_string(Lines, S),
        close(S),
	atom_codes(Atom, String).

export write_lines_to_string/2.
write_lines_to_string([], _).
write_lines_to_string([Line | Lines], S)
        :-
        write(S, Line),
        write_lines_to_string(Lines, S).

export do_curl/1.
do_curl(Options)
	:-
	uppercase_unwind(Options, UCOptions),
	curl_c_builtin(UCOptions, Error),
	finish_curl_c(Error, Options).

finish_curl_c(Error, Options)
	:-
	var(Error), !.

finish_curl_c(Error, Options)
	:-
	Ball = curl_error(Error),
	throw(Ball).


%%% ===================== curl/1-3 ===================================== +++
%%%	Similarly built over do_curl(Options).

export curl/1.
curl(Options)
	:-
  	do_curl(Options).

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
  	handle_target(Target, Options, TOptions),
  	do_curl(TOptions).

handle_target(Target, Options, FTOptions)
  	:-
  	var(Target), 
  	!,
 	((member( result =_, Options); member( 'RESULT' =_, Options)) ->
  		TOptions = Options 
		;
 		TOptions = ['RESULT'=Target | Options] ),
 	((member(writedata=_, TOptions); member('WRITEDATA'=_, TOptions)) -> 
 		FTOptions = TOptions
 		;
 		FTOptions = ['WRITEDATA'=true | TOptions]
 	).






endmod.


/* ================= SAMPLES ================== *

?- http(get,'http://example.com', [result=RR,response_code(RC),total_time(TTT)]).

RR='<!doctype html>\n<html>\n<head>\n    <title>Example Domain .... More information...</a></p>\n</div>\n</body>\n</html>\n' 
RC=200 
TTT=0.693073 

yes.
--------------

?- http(get,'http://example.com', [response_code(RC),total_time(TTT),file='./myfile.txt'])

RC=200 
TTT=1.172075 

yes.
--------------

NOTE: Local file ./myfile.txt was created and now contains the same response as TT in the sample above.


?- http(post, 'https://postman-echo.com/post', [fields='name=admin&shoesize=12', result=RR]).

RR='{"args":{},"data":"","files":{},"form":{"name":"admin","shoesize":"12"},"headers":{"host":"postman-echo.com","content-length":"22","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"name":"admin","shoesize":"12"},"url":"https://postman-echo.com/post"}' 

yes.

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
--------------

Let file ./lorem.txt contain:

lorem ipsum doler
zip zap zing

?- http(post, 'https://postman-echo.com/post', [file=data('./lorem.txt'), result=RR]).

RR='{"args":{},"data":"","files":{},"form":{"lorem ipsum dolerzip zap zing":""},"headers":{"host":"postman-echo.com","content-length":"29","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"lorem ipsum dolerzip zap zing":""},"url":"https://postman-echo.com/post"}' 

yes.

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
--------------
 * ============================================ */
