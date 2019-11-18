---
title: 'bufread/[2,3]'
group: Input Output
predicates:
- {sig: 'bufread', args: {
    2: 'runs the Prolog parser on a string of text',
    3: 'similar to bufread/2, allowing read options'
  }}
- {sig: 'old_bufread/2', desc: 'similar to bufread/2, giving additional information on variables'}
---

## FORMS
```
bufread(Buffer, Term)

bufread(Buffer, Term, Options)

old_bufread(Buffer, [Term | VarNames ])
```
## DESCRIPTION

`bufread/2` takes a Prolog string, `Buffer`, and attempts to transform it into a Prolog term. It does this by :

- Reading the first term out of Buffer(trailing characters from Buffer are ignored) and unifying it with Term.  A closing fullstop (period) is not need: The implementation of `bufread/2` utilizes the `attach_fullstop(true)` read-option.

- If an error has occurred, the error message is printed on the terminal in a manner similar to that for `consult` and for `read` generally.

`old_bufread/2` behaves similarly, except that it returns in VarNames a list of the quoted variable names occurring in the term read.  The same information can be obtain using `bufread/3` with Options containing either `variable_names(VNs)` or `vars_and_names(Vars,Names)`.

## EXAMPLES

The following examples read the string
```
"f(abc, Bob)"

?- bufread("f(abc, Bob)", Term).

Term=f(abc,_A) 

yes.

?- bufread("f(abc, Bob)", Term, [vars_and_names(Vars,Names)]).

Term=f(abc,_A) 
Vars=[_A] 
Names=['Bob'] 

yes.
```
Here is how syntax error (a missing comma)  in the string is handled:
```
f(abc Bob)stream_descriptor(,open,string,string([102,40,97,98,99,32,66,111,98,41]),[input|nooutput],false,53,[],0,0,0,true,1,wt_opts(78,400,flat),[],wait,text,eof_code,true,0)
      ^Syntax error 
	string("f(abc Bob)"), line 1: Illegal token after primary expression
```

## NOTE

`old_bufread`/2 is defined as follows:

```
old_bufread(String,[Term|VarNames]) :-
        open(string(String),read,Stream),
        read_term(Stream,Term,
                  [attach_fullstop(true),vars_and_names(_,VarNames)]),
        close(Stream).
```

