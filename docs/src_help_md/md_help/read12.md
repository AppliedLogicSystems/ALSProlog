---
title: 'read/[1,2]'
predicates:
 - 'read/1' : read a term from the current input stream
 - 'read/2' : read a term from specified stream
 - 'read_term/2' : read term from current input with options
 - 'read_term/3' : read term from specified stream with options
---
`read/1` — read a term from the current input stream

`read/2` — read a term from specified stream

`read_term/2` — read term from current input with options

`read_term/3` — read term from specified stream with options


## FORMS

read(Term)

read(Stream_or_Alias, Term)


read_term(Term, Options)

read_term(Stream_or_Alias, Term, Options)


## DESCRIPTION

These predicates are used to read a term from a stream using the operator declarations in effect at the time of the read. The end of the term read from the stream is indicated by a fullstop token appearing in the stream. The fullstop token is a period(' . ') followed by a newline, white space character, or line comment character. If the stream is positioned so that there are no more terms to be read and the stream has the property eof_action(eof_code), then Term will be unified with the atom end_of_file.

read/1 reads a term from the current input stream and unifies it with Term.

read/2 reads a term from the input stream specified by Stream_or_Alias and unifies it with Term.

read_term/2 reads a term from the current input stream with options Options(see below) and unifies the term read with Term.

read_term/3 reads a term from the input stream specified by Stream_or_Alias and unifies it with Term. The options specified by Options are used in the process of reading the term.

read_term/2 and read_term/3 take the parameter Options which is a list of options to read_term. These options either affect the behavior of read_term or are used to retrieve additional information about the term which was read.

The following options supported by ALS Prolog are options specified by the March ' 93 ISO Prolog Standard..

li([],
    [   [ariables,(,Vars,),--,After,reading,a,term,,,Vars,shall,be,a,list,of,
            the,variables,in,the,term,read,,,in,left-to-right,traversal,
            order.],
        p([],[])]) li([],
    [   [ariable_names,(,VN_list,),--,After,reading,a,term,,,VN_list,will,be,
            unified,with,a,list,of,elements,of,the,form,V,=,A,where,V,is,a,
            variable,in,the,term,and,A,is,an,atom,representing,the,name,of,
            the,variable.,Anonymous,variables,(,variables,whose,name,is,',_,
            ',),will,not,appear,in,this,list.],
        p([],[])])
singletons(VN_list) -- After reading a term, VN_list will be unified with a list of elements of the form V = A, where V is a variable occurring in the term only once and A is an atom which represents the name of the variable. Anonymous variables will not appear in this list.

The following option does not conform to the standard, but is supported by ALS Prolog.

attach_fullstop(Bool) -- Bool is either true or false. The default is false. When Bool is true, a fullstop is inserted before the end of the stream. This option is most useful for reading single terms from atom, character, and character code streams.


## EXAMPLES

```
?- read(Term).
[+(3,4),9+8].
Term=[3+4,9+8]
?- read_term(Term,[variables(V),variable_names(VN),singletons(SVN)]).
f(X,[Y,Z,W],g(X,Z),[_,U1,_,U2]).
Term=f(_A,[_B,_C,_D],g(_A,_C),[_E,_F,_G,_H])
V=[_A,_B,_C,_D,_E,_F,_G,_H]
VN=[_A='X',_B='Y',_C='Z',_D='W',_F='U1',_H='U2']
SVN=[_B='Y',_D='W',_F='U1',_H='U2']
?- open(atom('[X,2,3]'),read,S),
?-_read_term(S,Term,[attach_fullstop(true)]),
?-_close(S).
S=stream_descriptor('',closed,atom,atom('[X,2,3]'),
[input|nooutput],false,3,'[X,2,3]',7,7,0,true,0,
wt_opts(78,40000,flat),[],true,text,eof_code,0,0)
Term=[_A,2,3]
?- read_term(user_input,Term,not_an_option_list).
Error:Argumentoftypelistexpectedinsteadofnot_an_option_list.
-Goal:sio:read_term(user_input,_A,not_an_option_list)
-Throwpattern:error(type_error(list,not_an_option_list),
[sio:read_term(user_input,_A,
not_an_option_list)])
?- read(X).
foobar.
foobar.
^
Syntaxerror:'$stdin',line17:Fullstop(period)expected
foo.
X=foo
```

## ERRORS

Stream_or_Alias is a variable

-- -- -- -- &gt; instantiation_error.

Stream_or_Alias is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_Alias, Stream_or_Alias) .

Stream_or_Alias is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

Stream_or_Alias is not an input stream

-- -- -- -- &gt; permission_error(input, stream, Stream_or_Alias) .

Options is a variable

-- -- -- -- &gt; instantiation_error.

Options is neither a variable nor a list

-- -- -- -- &gt; type_error(list, Option) .

Options is a list an element of which is a variable

-- -- -- -- &gt;

instantiation_error.

Options is a list containing an element E which is neithera variable nor a valid read option

-- -- -- -- &gt;

domain_error(read_option, E)

## .

One or more characters were read, but they could not be parsed as a term using the current set of operator definitions

-- -- -- -- &gt; syntax_error. [This does not happen now; see notebelow ]

The stream associated with Stream_or_Alias is at the end of the stream and the stream has the property eof_action(error)

-- -- -- -- &gt; existence_error(past_end_of_stream, Stream_or_Alias) .

The stream associated with Stream_or_Alias has no input ready to be read and the stream has the property snr_action(error)

-- -- -- -- &gt; existence_error(stream_not_ready, Stream_or_Alias) .


## NOTES

The ISO Prolog Standard requires that an error be thrown when there is a syntax error in a stream being read. The default action at the present time for ALS Prolog is to print out an error message describing the syntax error and then attempt to read another term. This action is consistent with the behavior of older DEC-10 compatible Prologs. It is expected that ALS Prolog will eventually comply with the standard in this respect.


## SEE ALSO

- `write/[1,2]`  
`write_term/[2,3]`  
`open/4`  
`close/1`  
`get_char/[1,2]`  
`get_code/[1,2]`  

`User Guide (Prolog I/O)`  
- [Bowen 91, 7.8]  
- [Sterling 86, 12.2]  
- [Bratko 86, 6.2.1]  
- [Clocksin 81, 5.1].
