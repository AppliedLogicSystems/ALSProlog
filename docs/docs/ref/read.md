---
title: 'read/[1,2]'
group: Input Output
module: sio
iso: readterm
predicates:
- {sig: 'read', args: {
    1: 'read a term from the current input stream',
    2: 'read a term from specified stream'
  }}
- {sig: 'read_term', args: {
    2: 'read term from current input with options',
    3: 'read term from specified stream with options'
  }}
---

## FORMS
```
read(Term)

read(Stream_or_Alias, Term)


read_term(Term, Options)

read_term(Stream_or_Alias, Term, Options)
```
## DESCRIPTION

These predicates are used to read a term from a stream using the operator declarations in effect at the time of the read. The end of the term read from the stream is indicated by a fullstop token appearing in the stream. The fullstop token is a period(' . ') followed by a newline, white space character, or line comment character. If the stream is positioned so that there are no more terms to be read and the stream has the property `eof_action(eof_code)`, then `Term` will be unified with the atom `end_of_file`.

`read/1` reads a term from the current input stream and unifies it with `Term`.

`read/2` reads a term from the input stream specified by `Stream_or_Alias` and unifies it with `Term`.

`read_term/2` reads a term from the current input stream with options `Options` (see below) and unifies the term read with `Term`.

`read_term/3` reads a term from the input stream specified by `Stream_or_Alias` and unifies it with `Term`. The options specified by `Options` are used in the process of reading the term.

`read_term/2` and `read_term/`3 take the parameter `Options` which is a list of options to `read_term`. These options either affect the behavior of `read_term` or are used to retrieve additional information about the term which was read.

The following options supported by ALS Prolog are options specified by the March'93 ISO Prolog Standard:

- `variables(Vars)` -- After reading a term, `Vars` shall be a list of the variables in the term read in left-to-right traversal order.
- `variable_names(VN_list)` -- After reading a term, `VN_list` will be unified with a list of elements of the form `V = A` where `V` is a variable in the term and `A` is an atom representing the name of the variable. Anonymous variables (variables whose name is `'_'`) will not appear in this list.

- `singletons(VN_list)` -- After reading a term, `VN_list` will be unified with a list of elements of the form `V = A`, where `V` is a variable occurring in the term only once and `A` is an atom which represents the name of the variable. Anonymous variables will not appear in this list.

The following additional options are supported by ALS Prolog:

- `vars_and_names(Vars,Names)` -- After reading a term, `Vars` shall be a list of the variables in the term read in left-to-right traversal order, `Names` is a list of the associated names of the variables. `'_'` is the only variable name which may occur more than once on the list `Names`.

- `attach_fullstop(Bool)` -- `Bool` is either `true` or `false`. The default is `false`. When `Bool` is `true`, a fullstop is inserted before the end of the stream. This option is most useful for reading single terms from atom, character, and character code streams.

- `blocking(Bool)` -- For streams such as socket streams or IPC queue streams, it is possible for the stream to remain open, yet there be no characters available at the time a read is issued. The value of `Bool` controls the behavior of the read in this setting. The values of `Bool` are as follows:

    **`true`** In this type of read, the read suspends or waits until enough characters are available to parse as a valid Prolog term.

    **`false`** In this type of read, if no characters are available, the read will immediately return with success, unifying the 'read term argument' (which is argument 1 for `read_term/2` and is argument 2 for `read_term/3`) with the term `unfinished_read`; any tokens consumed in the read attempt are saved in the stream data structure, and subsequent attempts to read from this stream begin with these tokens, followed by tokens created from further characters which arrive at later times.

- `syntax_errors(Val)` -- Val indicates how the system is to handle any syntax errors which occur during the reading of `Term`.  The possible values of `Val` and their interpretations are:

    **`error`** Occurrence of a syntax error will cause the system to raise an exception, which includes outputting a warning message. This is the default.

    **`fail`** Occurrence of a syntax error will cause the attempt to read `Term` to fail, and and error message will be output.

    **`quiet`** Occurrence of a syntax error will cause the attempt to read `Term` to fail quietly, with no message output.

    **`dec10`** Occurrence of a syntax error will cause the attempt to read `Term` to output an error message, to skip over the offending input characters, and attempt to re-read `Term` from the source stream.

## EXAMPLES
`?- read(Term).`
<br>`[+(3,4),9+8].        <<`          _Typed on console (current input)_
```
Term=[3+4,9+8]

yes.
```

`?- read_term(Term,[variables(V),variable_names(VN),singletons(SVN)]).`
<br>`f(X,[Y,Z,W],g(X,Z),[_,U1,_,U2]).        <<`          _Typed on console (current input)_
```
Term=f(_A,[_B,_C,_D],g(_A,_C),[_E,_F,_G,_H]) 
V=[_A,_B,_C,_D,_E,_F,_G,_H] 
VN=[_A = 'X',_B = 'Y',_C = 'Z',_D = 'W',_F = 'U1',_H = 'U2'] 
SVN=[_B = 'Y',_D = 'W',_F = 'U1',_H = 'U2'] 

yes.
```

```
?- open(atom('[X,2,3]'),read,S),
   read_term(S,Term,[attach_fullstop(true)]), close(S).

S=stream_descriptor('\002',closed,atom,atom('[X,2,3]'),[input|nooutput],
    false,42,'[X,2,3]',7,7,0,true,0,wt_opts(78,400,flat),[],wait,text,
    eof_code,true,0) 
Term=[_A,2,3] 

yes.
```

```
?- read_term(user_input,Term,not_an_option_list). 
Error: Argument of type list expected instead of not_an_option_list.
- Goal:          sio:read_term(user_input,_A,not_an_option_list)
- Throw pattern: error(type_error(list,not_an_option_list),
                     [sio:read_term(user_input,_A,not_an_option_list)])
```

`?- read(X).`
<br>`foobar        <<`          _Typed on console (current input)_
<br>`zipper.        <<`          _Typed on console (current input)_
```
zipper.
^Syntax error 
	'standard input', line 9: Fullstop (period) expected
```

## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_Alias, Stream_or_Alias) .

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

`Stream_or_Alias` is not an input stream

-- -- -- -- &gt; permission_error(input, stream, Stream_or_Alias) .

`Options` is a variable

-- -- -- -- &gt; instantiation_error.

`Options` is neither a variable nor a list

-- -- -- -- &gt; type_error(list, Option) .

`Options` is a list an element of which is a variable

-- -- -- -- &gt; instantiation_error.

`Options` is a list containing an element E which is neither a variable nor a valid read option

-- -- -- -- &gt; domain_error(read_option, E)

The stream associated with Stream_or_Alias is at the end of the stream and the stream has the property eof_action(error)

-- -- -- -- &gt; existence_error(past_end_of_stream, Stream_or_Alias) .

The stream associated with Stream_or_Alias has no input ready to be read and the stream has the property snr_action(error)

-- -- -- -- &gt; existence_error(stream_not_ready, Stream_or_Alias) .


## SEE ALSO

- [`write/[1,2]`](write.html)
- [`write_term/[2,3]`](write.html)
- [`open/4`](open.html)
- [`close/1`](close.html)
- [`get_char/[1,2]`](get_char.html)
- [`get_code/[1,2]`](get_code.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
- {% include book.md id="bowen91"    sec="7.8" %}
- {% include book.md id="sterling86" sec="12.2" %}
- {% include book.md id="bratko86"   sec="6.2.1" %}
- {% include book.md id="clocksin81" sec="5.1" %}
