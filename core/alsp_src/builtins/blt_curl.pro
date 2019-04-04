/*=====================================================================*
 |		blt_curl.pro		
 |	Copyright (c) 2018-2019 Applied Logic Systems, Inc.
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
 | In all of the following:
 | * if result=V is present in the options list, make a UIA out the incoming data 
 |   returned from the URL server, and unify that UIA with V;
 | * if resultfile=FF is present in the options list, and if FF is a path to a local 
 |   file, write any incoming data returned from the URL server into the file FF;
 | * it is possible/permitted to have both result=V and resultfile=FF present in 
 |   the options list.
 |
 | http(get, URL, [result=V | <other CURLOPT/CURLINFO options>])
 |	Performs a GET to obtain data from server URL, makes that data into a UIA, 
 |	and unifies that UIA with V (typically an uninstantiated variable).
 |	
 | http(get, URL, [resultfile=FF | <other CURLOPT/CURLINFO options>])
 |	Performs a GET to obtain data from server URL, and writes that data into 
 |	local file FF.
 |	
 | http(post, URL, [fields=FF | <other CURLOPT/CURLINFO options>])
 |	Performs a POST to URL, uploading explicit formatted field data encoded
 |	by an atom FF (e.g. 'name=admin&shoesize=12'); the user is responsible for 
 |	formatting FF in a manner appropriate for server URL.
 |	
 | http(post, URL, [fieldsfile=FF, (**)  | <other CURLOPT/CURLINFO options>])
 |	Reads local file FF to obtain an atom AFF expressing fields values, and
 |	then performs POST to URL.  Reading of FF can be conditioned by the
 |	use of eol('EOL') or eolcode('EOLCODE') equations on the options list.
 |		
 | http(post, URL, [data=DD | <other CURLOPT/CURLINFO options>])
 |	Performs a POST to URL, where DD is an atom.
 |		
 | http(post, URL, [datafile=FF, (**) | <other CURLOPT/CURLINFO options>])
 |	Reads local file FF to obtain an atom AFF, and then performs a POST to URL.
 |	Reading of FF can be conditioned by the use of eol('EOL') or 
 |	eolcode('EOLCODE') equations on the options list.
 |		
 | http(put, URL, [data=DD | <other CURLOPT/CURLINFO options>])
 |	Performs a PUT to URL, where DD is an atom.
 |		
 | http(put, URL, [datafile=FF, (**) | <other CURLOPT/CURLINFO options>])
 |	Reads local file FF to obtain an atom AFF, and then performs a PUT to URL.
 |		
 | (**) One of the two optional expressions 
 |		eol=C or eolcode=N
 |	may be included in the Options list to indicate the end-of-line character
 |	to be used in reading file FF.  If neither eol=C nor eolcode=N is included,
 |	the file FF is read without end-of-line characters and all the lines are 
 |	concatenated into a single UIA. If eol=C is included (say,
 |		 eol='\n' or eol=' ' or eol='&')
 |	or if eolcode=N (say,
 |		eolcode=10 or eolcode=32 or eolcode=38)
 |	then the indicated character is used as a line ending for every line 
 | 	except the last line of the file (i.e., the character is used as a line
 |	separator for each pair of lines read.
 * ------------------------------------------------------------------------------*/

export http/3.
http(RESTVerb, URL, Options)
	:-
	check_verb(RESTVerb),
	check_url(URL),
	check_http_options(Options, ChkdOptions),
	!,
	refine_opts(RESTVerb, URL, ChkdOptions, ROptions),
	do_curl(ROptions).
	
http(RESTVerb, URL, Options)
	:-
	var(RESTVerb),
	instantiation_error(2).

check_verb(RESTVerb)
	:-
	var(RESTVerb),
	!,
	instantiation_error(2).
check_verb(RESTVerb)
	:-
	atom(RESTVerb),
	not(member(RESTVerb, [get,post,put,delete,head,options,patch])),
	!,
	domain_error(http_rest_verb,RESTVerb,2).
check_verb(_).

check_url(URL)
	:-
	var(URL),
	!,
	instantiation_error(2).
check_url(URL)
	:-
	atom(URL),
	!.
check_url(URL)
	:-
	domain_error(atom,URL,2).

check_url(_).

check_http_options(Options, ChkdOptions)
	:-
	var(Options),
	!,
	instantiation_error(2).
check_http_options([], []).
check_http_options([Opt | Options], [CkdOpt | ChkdOptions])
	:-
	check_http_opt(Opt, CkdOpt),
	!,    
	check_http_options(Options, ChkdOptions).
check_http_options(CurlOptions,_)
	:-
	type_error(list,CurlOptions,2).

check_http_opt(Opt, UC_F=A) 
	:-
	functor(Opt, F, 1),
	!,
	make_uc_sym(F, UC_F),
	arg(1, Opt, A),
	(member(UC_F, ['DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'FIELDS', 'FIELDSFILE', 'RESULT', 'RESULTFILE', 'URL', 'POST']), 
		! ;
	(not(lookup_opt_info(UC_F)) ->
		domain_error(curl_option,(Opt=_),3)
		;
		true
	) ).

check_http_opt(Tag=X, UC_Tag=X) 
	:-
	make_uc_sym(Tag, UC_Tag),
	(member(UC_Tag, ['DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'FIELDS', 'FIELDSFILE', 'RESULT', 'RESULTFILE', 'URL', 'POST']),
		! ;
		lookup_opt_info(UC_Tag)  ).

check_http_opt(Tag=_) 
	:-
	make_uc_sym(Tag, UC_Tag),
	not(lookup_opt_info(UC_Tag)),
	!,
        domain_error(curl_option,(Tag=_),3).

check_http_opt(Tag=X, Tag=X) :-!.

check_http_opt(Opt,_) 
	:-
	type_error('equation (_=_)', Opt,3).

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
	
refine_opts(RESTVerb, URL, Options, [PrimeOption,'URL'=URL | RefinedOptions])
	:-
	setup_primary_option(RESTVerb, PrimeOption),
	allowed_options(RESTVerb, OkOptions),
	check_options(Options, OkOptions, Options, RefinedOptions).

setup_primary_option(RESTVerb, PrimeOpt=1)
	:-
	primary_option(RESTVerb, PrimeOpt),
	!.
setup_primary_option(RESTVerb, 'CUSTOMREQUEST'=UC_RESTVerb)
	:-
	simple_option(RESTVerb),
	make_uc_sym(RESTVerb, UC_RESTVerb).
	

primary_option(get, 'HTTPGET').
primary_option(post, 'POST').
primary_option(put, 'PUT').
primary_option(head, 'NOBODY').

simple_option(delete).
simple_option(options).
simple_option(patch).

allowed_options(get,  ['RESULT', 'RESULTFILE', 'URL', 'HTTPGET']).
allowed_options(post, ['DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'FIELDS', 'FIELDSFILE', 'RESULT', 'RESULTFILE', 'URL', 'POST']).
allowed_options(put,  ['DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'RESULT', 'RESULTFILE', 'URL', 'PUT']).
allowed_options(delete,  ['URL','RESULT','RESULTFILE']).
allowed_options(head,  ['URL','RESULT','RESULTFILE','NOBODY']).
allowed_options(options,  ['URL','RESULT','RESULTFILE']).
allowed_options(patch,  ['URL','RESULT','RESULTFILE']).

check_options([], _, _, []).
	%% 'EOL' and 'EOLCODE' are ok in InitOptions, but strip them out of the final RefinedOptions
check_options(['EOL'=Val | Options], OkOptions, InitOptions, RefinedOptions)
	:-!,
	check_options(Options, OkOptions, InitOptions, RefinedOptions).
check_options(['EOLCODE'=Val | Options], OkOptions, InitOptions, RefinedOptions)
	:-!,
	check_options(Options, OkOptions, InitOptions, RefinedOptions).

check_options([Opt=Val | Options], OkOptions, InitOptions, [RefinedOpt | RefinedOptions])
	:-
		%% if Opt is explicitly listed as OK, then refine it:
	member(Opt, OkOptions),
	!,
	refine_opt(Opt=Val, InitOptions, RefinedOpt),
	check_options(Options, OkOptions, InitOptions, RefinedOptions).

check_options([Opt=Val | Options], OkOptions, InitOptions, [RefinedOpt | RefinedOptions])
	:-
	is_curl_opt_or_info(Opt),
	!,
	    %Do we want to blacklist things like WRITEDATA, READDATA here?
	RefinedOpt = (Opt=Val),
	check_options(Options, OkOptions, InitOptions, RefinedOptions).

check_options([Opt | _], OkOptions, _, _)
	:-
		%% Make into prolog error, throwing exception:
	printf('Unrecognized or unsupported http/3 option: %t\nExpect one of: %t\n',[Opt,OkOptions]),
	fail.

refine_opt('DATA'=Data, InitOptions, 'UPLOADDATA'=Data) :-!.

refine_opt('DATAFILE'=FilePath, InitOptions, 'UPLOADDATA'=FileData)
	:-!,
	handle_from_file(FilePath, InitOptions, FileData).

refine_opt('FIELDS'=FormData, InitOptions, 'POSTFIELDS'=FormData) :-!.

refine_opt('FIELDSFILE'=FilePath, InitOptions, 'POSTFIELDS'=FileData)
	:-!,
	handle_from_file(FilePath, InitOptions, FileData).

refine_opt(Opt=Val, _, Opt=Val).

is_curl_opt_or_info(Opt)
	:-
	    %% builtin, defined in generic/bcurl.c
	lookup_opt_info(Opt).

handle_from_file(FilePath, InitOptions, FileData)
	:-
	determine_char(InitOptions, Char),
	grab_lines(FilePath, RawLines),
	concat_lines_char(RawLines, Char, FileData).
	
determine_char(InitOptions, Char)
	:-
	member('EOL'=Char, InitOptions), !.
determine_char(InitOptions, Char)
	:-
	member('EOLCODE'=Num, InitOptions), 
	!,
	open(atom(Char), write, S), put_code(S, Num), close(S).
determine_char(InitOptions, '').

concat_lines_char([], _, '') :-!.

concat_lines_char([Line], Char, Line) :-!.

concat_lines_char([RawLine1 | RestRawLines], Char, FileData)
	:-
	do_concat_lines_char(RestRawLines, Char, RawLine1, FileData).

do_concat_lines_char([], _, Inter, Inter) :-!.

do_concat_lines_char([RawLine], Char, Inter, FinalLine)
	:-!,
	'$atom_concat'(Char, RawLine, L2),
	'$atom_concat'(Inter, L2, FinalLine).

do_concat_lines_char([RawLine | RestRawLines], Char, Inter, FileData)
	:-
	'$atom_concat'(Char, RawLine, L2),
	'$atom_concat'(Inter, L2, Inter2),
	do_concat_lines_char(RestRawLines, Char, Inter2, FileData).


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
%%printf('do_curl: Options=%t\n',[Options]),
	curl_c_builtin(Options, Error),
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
	check_curl_options(Options),
	uppercase_unwind(Options, UUOptions),
	!,
  	do_curl(UUOptions).

export curl/2.
curl(URL, Target)
  	:-
	check_url(URL),
 	Options = ['URL'=URL],
	!,
  	cont_curl(Options, Target).
  
export curl/3.
curl(URL, [], Target)
 	:-!,
	check_url(URL),
	!,
 	cont_curl(['URL'=URL], Target).

curl(URL, Opts, Target)
  	:-
	check_url(URL),
  	add_url(URL, Opts, Options),
	check_curl_options(Options),
	!,
  	cont_curl(Options, Target).

add_url(URL, SourceOptions, SourceOptions)
  	:-
 	(member( url =_, SourceOptions); member( 'URL' =_, SourceOptions)),
  	!.
add_url(URL, SourceOptions, ['URL'=URL | SourceOptions]).

cont_curl(Options, Target)
  	:-
  	handle_target(Target, Options, TOptions),
	uppercase_unwind(TOptions, UUOptions),
	curl_refine_opts(UUOptions, ROptions),
	!,
  	do_curl(ROptions).

handle_target(Target, Options, [result=Target | Options]).

	%% A stub in case we want to do some refining on direct curl options
curl_refine_opts(UUOptions, UUOptions).


check_curl_options(Options)
	:-
	var(Options),
	!,
	instantiation_error(2).

check_curl_options([]).
check_curl_options([Opt | Options])
	:-
	check_curl_opt(Opt),
	!,    
	check_curl_options(Options).
check_curl_options(CurlOptions)
	:-
	type_error(list,CurlOptions,2).

check_curl_opt(Tag=Val) 
	:-
	make_uc_sym(Tag, UC_Tag),
	check_uc_tag(UC_Tag),
	!,
	check_val(UC_Tag, Val).

check_uc_tag(UC_Tag)
	:-
	not(member(UC_Tag, ['DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'FIELDS', 'FIELDSFILE', 'RESULT', 'RESULTFILE', 'URL', 'POST'])),
	not(lookup_opt_info(UC_Tag)),
	!,
        domain_error(curl_option,(Tag=_),3).
check_uc_tag(UC_Tag).
	

	%% 'ins' indicates 'instantiated'; unins indicates 'uninstantiated':
	%% 'nl' indicates 'non-list':
expects_ins_integer('PORT').

expects_ins_atom_nl('URL').
expects_ins_atom_nl('USERAGENT').

check_val(UC_Tag, Val)
	:-
	expects_ins_integer(UC_Tag),
	!,
	check_ins_integer_val(Val).

check_ins_integer_val(Val)
	:-
	var(Val),
	!,
	instantiation_error(2).

check_ins_integer_val(Val)
	:-
	not(integer(Val)),
	!,
        type_error(integer,Val,3).
check_ins_integer_val(_).

check_val(UC_Tag, Val)
	:-
	expects_ins_atom_nl(UC_Tag),
	!,
	check_ins_atom_nl(Val).

check_ins_atom_nl(Val)
	:-
	var(Val),
	!,
	instantiation_error(3).
check_ins_atom_nl([])
	:-!,
        type_error('atom_non-[]',[],3).
check_ins_atom_nl(Val)
	:-
	not(atom(Val)),
	!,
        type_error('atom_non-[]',Val,3).
check_ins_atom_nl(_).


check_val(UC_Tag, Val)
	:-
	nonvar(Val),
	(not(atomic(Val)); Val==[]),
	!,
        type_error('atomic_or_[]',Val,3).
check_val(UC_Tag, Val).

check_curl_opt(Tag=_) :-!.

check_curl_opt(Opt) 
	:-
	type_error('equation (_=_)', Opt,3).
	
endmod.


/* ================= SAMPLES ================== *

?- http(get,'http://example.com', [result=RR,response_code=RC,total_time=TTT]).

RR='<!doctype html>\n<html>\n<head>\n    <title>Example Domain .... More information...</a></p>\n</div>\n</body>\n</html>\n' 
RC=200 
TTT=0.693073 

yes.
--------------

?- http(get,'http://example.com', [response_code=RC,total_time=TTT,file='./myfile.txt'])

RC=200 
TTT=1.172075 

yes.
--------------

NOTE: Local file ./myfile.txt was created and now contains the same response as TT in the sample above.


?- http(post, 'https://postman-echo.com/post', [postfields='name=admin&shoesize=12', result=RR]).

RR='{"args":{},"data":"","files":{},"form":{"name":"admin","shoesize":"12"},"headers":{"host":"postman-echo.com","content-length":"22","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"name":"admin","shoesize":"12"},"url":"https://postman-echo.com/post"}' 

yes.

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
--------------

Let file ./lorem.txt contain:

lorem ipsum doler
zip zap zing

?- http(post, 'https://postman-echo.com/post', [datafile='./lorem.txt', result=RR]).

RR='{"args":{},"data":"","files":{},"form":{"lorem ipsum dolerzip zap zing":""},"headers":{"host":"postman-echo.com","content-length":"29","accept":"* / *","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443","x-forwarded-proto":"https"},"json":{"lorem ipsum dolerzip zap zing":""},"url":"https://postman-echo.com/post"}' 

yes.

Note: Above, "accept":"* / *" should not have any spaces, but that conflicts with being in a comment.
--------------
 * ============================================ */
