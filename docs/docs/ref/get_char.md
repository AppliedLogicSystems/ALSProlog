---
title: 'get_char/[1,2]'
group: Input Output
module: sio
iso: getchar
predicates:
- {sig: 'get_char', args: {
    1: 'read a character from current input stream',
    2: 'read character from a specific stream'
  }}
---

## FORMS
```
get_char(Char)

get_char(Stream_or_Alias, Char)
```
## DESCRIPTION

`get_char/1` will retrieve the next character from the current input stream and unify it with `Char`.  `get_char/2` will retrieve a character from the input stream associated with Stream_or_Alias and unify it with `Char`.

If there are no more data left in the stream to be read and if the stream has the property `eof_action(eof_code)`, then `Code` will be unified with `end_of_file`.

## EXAMPLES
```
?- get_char(C1),get_char(C2),get_char(C3),get_char(C4).
test<newline>

C1=t 
C2=e 
C3=s 
C4=t 

yes.
```
## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, Stream_or_Alias) .

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

`Stream_or_Alias` is not an input stream

-- -- -- -- &gt; permission_error(input, stream, Stream_or_Alias) .

`Char` is neither a variable nor a character

-- -- -- -- &gt; type_error(character, Char) . [See notes below ]

The stream associated with `Stream_or_Alias` is at the end of the stream and the stream has the property eof_action(error)

-- -- -- -- &gt; existence_error(past_end_of_stream, Stream_or_Alias) .

The stream associated with `Stream_or_Alias` has no input ready to be read and the stream has the property snr_action(error)

-- -- -- -- &gt; existence_error(stream_not_ready, Stream_or_Alias) .

## NOTES

A character is simply an atom with length 1. [`get_code/[1,2]`](get_code.html) is used to retrieve a character code.

If `get_char/[1,2]` is called with `Char` instantiated to a term which is not a character, an error will be thrown. The error thrown though will in all likelihood be from [`char_code/2`](char_code.html), not `get_char/[1,2]`.

## SEE ALSO

- [`put_char/2`](put_char.html)
- [`get_code/2`](get_code.html)
- [`open/4`](open.html)
- [`close/1`](close.html)
- [`char_code/2`](char_code.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
