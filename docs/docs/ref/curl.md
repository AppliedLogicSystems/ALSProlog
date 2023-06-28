---
title: 'curl/[1,2,3]'
group: Input Output
module: curl
predicates:
- {sig: 'curl', args: {
    1: 'Access to the internet via the curl package with URL, Options, Target in one list',
    2: 'Access to the internet via the curl package with separate URL arg, combined Options, Target in one list',
    3: 'Access to the internet via the curl package with separate URL, Options, Target args'
  }}
- {sig: 'http/3', desc:  'REST-inspired user-level interface for curl'}
---

## FORMS

`curl(Options)`

`curl(URL, Target)`

`curl(URL, Options, Target)`

`http(RESTVerb, URL, Options)`


## DESCRIPTION

[curl](https://curl.haxx.se/) is an open source software command line tool and library ([libcurl](https://curl.haxx.se/libcurl/)) "for transferring data with URLs".  The ALS Prolog interface to curl is built using the so-called “Easy interface” of the curl library.  In this approach, everything about any of the calls `curl/[1,2,3]` or `http/3` is converted into a single list of Easy-oriented curl option equations, and that list is then passed to a C program built over the curl Easy interface API.  The predicates `curl/[1,2,3]` provide three variations on the theme of a rather direct interface to curl, while `http/3` provides slightly higher-level REST-inspired transfer services.

For all equations on the `Options` list passed to the C program, the left side of every equation, which must be an atom,  is first converted to uppercase. Then, with the exception of the special equation tags 
```
'DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'FIELDS', 'FIELDSFILE', 'RESULT', 'RESULTFILE', 'URL', 'POST',
```
(see below), the left side L of every equation L=R passed to the C plumbing over curl is prefixed with either `'CURLOPT_'` or `'CURLINFO_'`. The result of this prefixing must be either an [easy option](https://curl.haxx.se/libcurl/c/curl_easy_setopt.html)  or an [info option](https://curl.haxx.se/libcurl/c/curl_easy_getinfo.html).

Several simple transformations are applied to the elements of the list:

i) Unary prolog expressions `<F>(A)` are allowed on the `Options` list. These are converted to equations `FF = A`, where `FF` is the uppercase version of `<F>`.

ii) Equations `E = A` are converted to `EE = A` where `EE` is the uppercase version of `E`.

`Options` must be list of terms of the form `Tag = Value`.  `Tag` must be an atom, and when converted to all upper case, must either be one of the following
```
'DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'FIELDS', 'FIELDSFILE', 'RESULT', 'RESULTFILE', 'URL', 'POST',
```
or must be a [`CURLOPT` expression](https://curl.haxx.se/libcurl/c/curl_easy_setopt.html), or must be a [`CURLINFO` expression](https://curl.haxx.se/libcurl/c/curl_easy_getinfo.html).  Those prefixed with `CURLOPT` request specific services or pass information into curl.  Those prefixed with `CURLINFO` request various information to be returned from curl.

The acceptable expressions for `Value` will depend on `Tag` (see below).

Among `curl/[1,2,3]`, `curl/1` is basic:
```
curl/1:
curl(Options)
Cleans up option capitalization and invokes the low-level interface to curl.
```
The other two are defined in terms of `cur/1`:
```
curl/2:
Effectively:
curl(URL, Target)
    :-
    curl(URL, [], Target).

curl/3:
Effectively:
curl(URL, Options, Target)
    :-
    XOptions = [url=URL, result=Target | Options],
    curl(XOptions).
```


In `curl/1`, the one `Tag` which absolutely must be set is `'URL'=<URL>` (effectively `'CURLOPT_URL'=<URL>`), since this specifies where the file transfer will take place (to or from).  `<URL>` must be an atom (typically a UIA) which describes a proper url.

**Common options:**

- The special equation
```
'RESULT' = Expr [or, result = Expr]
```
can appear on `Options`. Whatever transfer result is produced from the underlying curl call is turned into a UIA and is unified with `Expr`, which can be any prolog expression, including an unbound variable.

- The special equation
```
'RESULTFILE' = '<local file path>' [or resultfile = '<local file path>']
```
can also appear on `Options`. Whatever transfer result is produced from the underlying curl call is written into the file at `<local file path>`.  Note that both `result = Expr` and `resultfile = '<local file path>'` can occur together on an `Options` list: the transfer result will be unified (as a UIA) with `Expr` and also written into the file at `<local file path>`.

Further `CURLOPT_` options are described below in connection with `http/3`.

Appropriate `CURLINFO` options can be used with any of `curl/[1,2,3]` or `http/3`. For example, including `response_code(RC)` on `Options` will bind `RC` to the HTTP response code returned by the server (e.g., 200, 404, etc.), and including `total_time(TTT)` will bind `TTT` to the number of seconds expended in the transaction.

For transferring information outbound, data for POSTing can be supplied in one of two forms: 1) structured data such as is uploaded from Web forms (e.g., 'name=admin&shoesize=12') and 2) free-form text data (e.g., 'lorem ipsum doler'). Either type of data can be supplied either directly in an equation on the `Options` list, or in a file specified on the `Options` list:

- If `data = <atom> ('DATA'=<atom>)` (where `<atom>` is a symbol or UIA) occurs on the `Options` list, then the underlying curl program will POST the text of `<atom>` via the curl equation `'READDATA'=<atom>`.

- If `datefile=<FilePathName>('DATAFILE'=<FilePathName>)` occurs on the `Options` list, then the complete text occurring in `<FilePathName>` is read into a single UIA `DT`, and the information is POSTED as if `'DATA'=DT` had been included on `Options`. The basic method of reading the file is to read it line by line, concatenating the lines to make the single UIA `DT`. For convenience, two special options are provided which allow one to specify a character to be used in concatenating the file lines. If the equation `eol = <char>` (or, `'EOL'=<char>`) is present on the options list, when `<FilePathName>` is read, the character `<char>` will be used as a separator between the concatenated lines. Equivalently, if `eolcode=<charcode>` (or, `'EOLCODE'=<charcode>`) is present on the list, the character with code `<charcode>` will be used as the separator. (So, for example, spaces or newlines could be supplied as the separators.)

- If `fields = <atom>` (or, `'FIELDS'=<atom>`) occurs on `Options`, then `<atom>` should represent a structured fields expression (e.g., `'name=admin&shoesize=12'`). The underlying curl program will POST the text of `<atom>` via `'POSTFIELDS'=<atom>`.

- If `fieldsfile = <FilePathName>` (or, `'FIELDSFILE'= <FilePathName>`) occurs on `Options`, then the complete text occurring in `<FilePathName>` is read into a UIA `FE`, just as for `'DATAFILE'`, and the information is POSTED as if `'FIELDS'=FE` had been included on `Options`. As with `'DATAFILE'`, the basic method of reading the file concatenates all the lines into a single UIA, but this can be modified by use of the `eol = <char> ('EOL'=<char>)` or `eolcode=<charcode> ('EOLCODE'=<charcode>)` special equations. (So, for example, ampersands (`&`) could be supplied as the separator.)

Inspired by the REST approach to internet file transfer,
```
http/3
http(RESTVerb, URL, Options)
```
provides REST-inspired transfer services. At present, the following RESTVerbs are implemented:
```
get, post put, delete, head, options, patch.
```
The available `Options` overall are the same as for `curl/[1,2,3]`, but in individual calls, depend on the RESTVerb. The special equations and special equation tags are the also the same as for `curl/[1,2,3]`.

## EXAMPLES

These three calls provide the same service and response, except that `curl/2` cannot provide the `response_code`:
```
?- curl('http://example.com', [response_code(RC)], RR).
?- curl('http://example.com', RR).
?- curl([url='http://example.com', response_code=RC, result=RR]).
RC=200 
RR='<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>......</html>\n' 

yes.

http(get, 'http://example.com', [response_code(RC), result=RR]).

RC=200 
RR='<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>......</html>\n'

yes.

?- http(post, 'https://postman-echo.com/post', [fields='name=admin&shoesize=12', result=RR]).

RR='{"args":{},"data":"","files":{},"form":{"name":"admin","shoesize":"12"},"headers":{"x-forwarded-proto":"https","host":"postman-echo.com","content-length":"22","accept":"*/*","content-type":"application/x-www-form-urlencoded","x-forwarded-port":"443"},"json":{"name":"admin","shoesize":"12"},"url":"https://postman-echo.com/post"}' 

yes.
```
## ERRORS
`curl(Options)`
`curl(URL, Target)`
`curl(URL, Options, Target)`
`http(RESTVerb, URL, Options)`

`Options` is a variable

-- -- -- -- &gt; `instantiation_error`

`Options` is neither a variable nor a list

-- -- -- -- > `type_error(list, Option)`

`Options` is a list an element of which is a variable

-- -- -- -- > `instantiation_error`

`Options` is a list containing an element `E` which is neither a curl equation nor a unary prolog expression

-- -- -- -- > `domain_error(type_error('equation (_=_)', E)`

`URL` is a variable

-- -- -- -- &gt; `instantiation_error`

`URL` is not an atom

-- -- -- -- &gt; `domain_error(atom,URL)`

`URL` does not indicate an internet host

-- -- -- -- &gt; `curl_error: 'curl_easy_perform() failed: Couldn\'t resolve host name\n'`

`Target` is a variable

-- -- -- -- &gt; `instantiation_error`

`RESTVerb` is not one of the listed REST verbs

-- -- -- -- &gt; `domain_error(http_rest_verb,RESTVerb)`


## NOTES

Access to curl is also provided by ALS Prolog streams.  See the discussion under  _URLs: Internet Streams_ in **_Section 10.2.2 SourceSink Terms_**, in the **User Guide (Prolog I/O)**.

## SEE ALSO

- [curl](https://curl.haxx.se/)
- [libcurl - the multiprotocol file transfer library](https://curl.haxx.se/libcurl/)
- [curl_easy_setopt - set options for a curl easy handle](https://curl.haxx.se/libcurl/c/curl_easy_setopt.html)
- [curl_easy_getinfo - extract information from a curl handle](https://curl.haxx.se/libcurl/c/curl_easy_getinfo.html)
