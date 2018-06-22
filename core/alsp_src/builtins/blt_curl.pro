/*=====================================================================*
 |		blt_curl.pro		
 |	Copyright (c) 2018 Applied Logic Systems, Inc.
 |
 |	Direct to curl interface
 |	REST-inspired user level
 |
 | Author: Chuck Houpt, Ken Bowen
 *=====================================================================*/

module inet.

/* ---------------------------------------*
 |    inet(RESTVerb, URL, Options)
 * ---------------------------------------*/
export inet/3.
inet(RESTVerb, URL, Options)
	:-
	member(RESTVerb, [get,post]),
	!,
	uppercase_unwind(Options, UUOptions),
	refine_opts(RESTVerb, URL, UUOptions, ROptions),
	do_curl(ROptions).
	
inet(RESTVerb, URL, Options)
	:-
	printf('Unsupported or unknown REST verb: %t\n', [RESTVerb]).

uppercase_unwind([], []).
uppercase_unwind([Opt | Options], [MOpt | MOptions])
	:-
	uc_unw(Opt, MOpt),
	uppercase_unwind(Options, MOptions).

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
	
refine_opts(get, URL, Options, ['URL'=URL | ROptions])
	:-
	    % In all cases of duplicate tags, we'll use the outermost (first encountered):
	delete_from(Options, 'FILE', Options1, FILEExprs),
	delete_from(Options1, 'WRITEDATA', Options2, WRITEDATAExprs),
	get_ck_file_wrd(FILEExprs, WRITEDATAExprs, Options2, ROptions).

refine_opts(post, URL, Options, ['URL'=URL |ROptions])
	:-
	    % In all cases of duplicate tags, we'll use the outermost (first encountered):
	delete_from(Options, 'FILE', Options1, FILEExprs),
	delete_from(Options1, 'FIELDS', Options2, FIELDSExprs),
	delete_from(Options2, 'DATA', Options3, DATAExprs),
	post_ck_file_fd(FILEExprs, FIELDSExprs, DATAExprs, Options3, ROptions).

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
post_ck_file_fd([], [], [], _, _) 
	:-!,
	printf('Error: No fields or data supplied for POST\n'),
	fail.

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

?- inet(get,'http://example.com', [result=RR,response_code(RC),total_time(TTT)]).

RR='<!doctype html>\n<html>\n<head>\n    <title>Example Domain .... More information...</a></p>\n</div>\n</body>\n</html>\n' 
RC=200 
TTT=0.693073 

yes.
--------------

?- inet(get,'http://example.com', [response_code(RC),total_time(TTT),file='./myfile.txt'])

RC=200 
TTT=1.172075 

yes.
--------------

NOTE: Local file ./myfile.txt was created and now contains the same response as TT in the sample above.


?- inet(post, 'https://postman-echo.com/post', [fields='name=admin&shoesize=12', result=RR]).

RR='{"args":{},"data":"","files":{},"form":{"name":"admin","shoesize":"12"},"headers":{"host":"postman-echo.com","content-length":"22","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"name":"admin","shoesize":"12"},"url":"https://postman-echo.com/post"}' 

yes.

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
--------------

Let file ./lorem.txt contain:

lorem ipsum doler
zip zap zing

?- inet(post, 'https://postman-echo.com/post', [file=data('./lorem.txt'), result=RR]).

RR='{"args":{},"data":"","files":{},"form":{"lorem ipsum dolerzip zap zing":""},"headers":{"host":"postman-echo.com","content-length":"29","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"lorem ipsum dolerzip zap zing":""},"url":"https://postman-echo.com/post"}' 

yes.

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
--------------
 * ============================================ */
