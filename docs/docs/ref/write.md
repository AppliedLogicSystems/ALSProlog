---
title: 'write/[1,2]'
group: Input Output
module: sio
iso: write_term
predicates:
- {sig: 'write', args:
    {1: 'write term to current output stream', 2: 'write term to specified stream'}}
- {sig: 'nl', args:
    {0: 'output a newline to the current output stream', 
     1: 'output a newline to a specified output stream'}}
- {sig: 'writeq',args: {
    1: 'write term to current output stream so that it may be read back in',
    2: 'write term to specified stream so that it may be read back in'
  }}
- {sig: 'write_canonical', args: {
    1: 'write term to current output stream in canonical form(no operators)',
    2: 'write term to specified stream in canonical form'
  }}
- {sig: 'write_term', args: {
    2: 'write term to current output stream with options',
    3: 'write term to specified output stream with options'
  }}
- {sig: 'display/1', desc: 'write term to current output stream in canonical form'}
---

## FORMS

```
write(Term)

write(Stream_or_Alias, Term)

nl

nl(Stream_or_Alias)

writeq(Term)

writeq(Stream_or_Alias, Term)

write_canonical(Term)

write_canonical(Stream_or_Alias, Term)

write_term(Term, Options)

write_term(Stream_or_Alias, Term, Options)

display(Term)
```

## DESCRIPTION

These predicates will output the term bound to `Term` to a stream. The format of the term is controlled by which variant is called or by an option given to `write_term`. None of these procedures output a fullstop or newline after the term written unless the term is a single-quoted atom containing a fullstop or a backslash newline: `\n` placed appropriately.

`write/1` behaves as if `write/2` were called with the current output stream as the `Stream_or_Alias` argument.

`write/2` behaves as if `write_term/3` were called with `Options` bound to

&nbsp;&nbsp;`[quoted(false), numbervars(true), lettervars(false), line_length(1024)]`

In addition, the default line length is ignored and is temporarily set to a large number, causing the output of long terms to not be pretty printed. Variables are printed as underscore followed by some number.

`nl/0` behaves as if `nl/1` were called with the current output stream as the `Stream_or_Alias` argument.

`nl/1` outputs a newline character to the `Stream_or_Alias` argument.

`writeq/1` behaves as if `writeq/2` were called with the current output stream as the `Stream_or_Alias` argument.

`writeq/2` behaves as if `write_term/3` were called with `Options` bound to

&nbsp;&nbsp;`[quoted(true), numbervars(true), lettervars(true) ]`

For a call to `writeq/2`, the line length is set to the default line length for the stream which is being output to. Variables are printed as an underscore followed by a capital letter. `writeq` is useful for outputting a term which might be later subject to a read from Prolog.

`write_canonical/1` behaves as if `write_canonical/2` were called with the current output stream bound to the `Stream_or_Alias` argument.

`write_canonical/2` behaves as if `write_term/3` were called with `Options` bound to

&nbsp;&nbsp;`[quoted(true), ignore_ops(true), lettervars(true) ]`

This is the same behavior supplied by the DEC-10 compatibility predicate `display/1`. `write_canonical` is useful in situations where it is desirable to output a term in a format which may subsequently read in without regard to operator definitions. Such terms are not particularly pleasing to look at, however.

`write_term/2` behaves as if `write_term/3` were called with the current output stream bound to `Stream_or_Alias`.

`write_term/3` writes out the term `Term` to the output stream associated with `Stream_or_Alias` and subject to the options in the write option list `Options`. The options in the write options list control how a term is output.

The options mandated by the draft standard are :

`quoted(Bool)` -- `Bool` is true or false. When `Bool` is true, atoms and functors are written out in such a manner so that `read/[1, 2]` may be used to read them back in; when `Bool` is false indicates that symbols should be written out without any special quoting; control characters embedded in an atom will be written out as is.

`ignore_ops(Bool)` -- `Bool` may be true or false. When `Bool` is true, compound terms are output in prefix functional notation.

`numbervars(Bool)` -- When `Bool` is true, a term of the form `' $VAR'(N)`, where `N` is a non-negative integer, will be output as a variable name consisting of a capital letter possibly followed by an integer. The capital letter is the (i + 1) th letter of the alphabet, and the integer is j, where i = N mod 26 and j = N div 26 The integer j is omitted if it is zero.

Other options not mandated by the draft standard but supported by ALS Prolog are :

`lettervars(Bool)` -- If `Bool` is true, variables will be printed out as an underscore followed by a letter and digits if necessary. If `Bool` is false, variables will be printed as `_N`, where `N` is computed using the address where the variable lives at. This latter mode is more suited to debugging purposes where correspondences between variables in various calls is required.

`maxdepth(N, Atom1, Atom2)` -- `N` is the maximum depth to print to. `Atom1` is the atom to output when this depth has been reached. `Atom2` is the atom to output when this depth has been reached at the tail of a list.

`maxdepth(N)` -- same as `maxdepth(N, '*', '...')`

`depth_computation(Val)` -- `Val` may be either `flat` or `nonflat`. This indicates the method of depth computation. If `Val` is bound to `flat`, all arguments of a term or list will be treated as being at the same depth. If `Val` is `nonflat`, then each subsequent argument in a term (or each subsequent element of a list) will be considered to be at a depth one greater than the preceding structure argument (or list element).

`line_length(N)` -- `N` is the length in characters of the output line. The pretty printer will attempt to break lines before they exceed the given line length.

`indent(N)` -- `N` specifies the initial indentation in characters to use for the second and subsequent lines output (if any).

`quoted_strings(Bool)` -- If `Bool` is true, lists of suitable character codes will print out as double quoted strings. If false, these lists will print out as lists of small integers.

## EXAMPLES

```
?- X='Hello\tthere', write(X),nl, writeq(X),nl, write_canonical(X),nl.
Hello	there
'Hello\tthere'
'Hello\tthere'

X='Hello\tthere' 
yes.

?- X='Hello\tthere\nJohn', write(X),nl, writeq(X),nl, write_canonical(X),nl.
Hello	there
John
'Hello\tthere\nJohn'
'Hello\tthere\nJohn'

X='Hello\tthere\nJohn' 
yes.

?- T=[3+4,'$VAR'(26)*X-'Y'],
?-_write(T),nl,
?-_writeq(T),nl,
?-_write_canonical(T),nl.
[3+4,A1*_4100-Y]
[3+4,A1*_A-'Y']
.(+(3,4),.(-(*('$VAR'(26),_A),'Y'),[]))
T=[3+4,'$VAR'(26)*X-'Y']
X=X

?- L=
[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],

?-_write_term(L,[line_length(20)]),nl,
?-_write('list:'),
?-_write_term(L,[line_length(26),indent(6)]),nl.
[a,b,c,d,e,f,g,h,i,
j,k,l,m,n,o,p,
q,r,s,t,u,v,w,
x,y,z]
list:[a,b,c,d,e,f,g,h,i,
j,k,l,m,n,o,p,
q,r,s,t,u,v,w,
x,y,z]
L=[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]

?- S="Astring",
?-_write_term(S,[quoted_strings(true)]),nl,
?-_write_term(S,[quoted_strings(false)]),nl.
"Astring"
[65,32,115,116,114,105,110,103]
S="Astring"

?- T=[a(b(c(d))),a(b(c(d))),a(b(c(d))),a(b(c(d)))],
?-_write_term(T,[maxdepth(3),depth_computation(flat)]),nl,
?-_write_term(T,[maxdepth(3),depth_computation(nonflat)]),nl.
[a(b(*)),a(b(*)),a(b(*)),a(b(*))]
[a(b(*)),a(*),*,...]
T=[a(b(c(d))),a(b(c(d))),a(b(c(d))),a(b(c(d)))]
```

## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- > `instantiation_error`

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- > `domain_error(stream_or_alias, Stream_or_Alias)`

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- > `existence_error(stream, Stream_or_Alias)`

`Stream_or_Alias` is not an output stream

-- -- -- -- > `permission_error(output, stream, Stream_or_Alias)`

`Options` is a variable

-- -- -- -- > `instantiation_error`

`Options` is neither a variable nor a list

-- -- -- -- > `type_error(list, Option)`

`Options` is a list an element of which is a variable

-- -- -- -- > `instantiation_error`

`Options` is a list containing an element `E` which is neither a variable nor a valid write option

-- -- -- -- > `domain_error(write_option, E)`


## SEE ALSO

- [`read_term/[2,3]`](read.html)
- [`read/[1,2]`](read.html)
- [`open/4`](open.html)
- [`close/1`](close.html)
- [`put_char/[1,2]`](put_char.html)
- [`put_code/[1,2]`](put_code.html)
- [`set_line_length/2`](set_line_length.html)
- [`op/3`](op.html)
- [`tell/1`](tell.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
- {% include book.md id="bowen91"    sec="7.8" %}
- {% include book.md id="clocksin81" sec="6.9" %}
- {% include book.md id="sterling86" sec="12.1" %}
- {% include book.md id="bratko86"   sec="6.2.1" %}
