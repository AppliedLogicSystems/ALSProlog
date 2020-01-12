---
title: 'get_code/[1,2]'
group: Input Output
module: sio
iso: getcode
predicates:
- {sig: 'get_code', args: {
    1: 'read a character code from current input stream',
    2: 'read character code from a specific stream'
  }}
---

## FORMS
```
get_code(Code)

get_code(Stream_or_Alias, Code)
```
## DESCRIPTION

`get_code/1` will retrieve the next character code from the current input stream and unify it with `Code`.  `get_code/2` will retrieve a character code from the input stream associated with `Stream_or_Alias` and unify it with `Code`.

If there are no more data left in the stream to be read and if the stream has the property eof_action(eof_code), then `Code` will be unified with -1

## EXAMPLES
```
?- get_code(C1),get_code(C2),get_ccode(C3),get_code(C4).
test<newline>

C1=116
C2=101
C3=115
C4=116
```
## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, `Stream_or_Alias`) .

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, `Stream_or_Alias`) .

`Stream_or_Alias` is not an input stream

-- -- -- -- &gt; permission_error(input, stream, `Stream_or_Alias`) .

Code is neither a variable nor a character code

-- -- -- -- &gt; type_error(integer, Code) .

The stream associated with `Stream_or_Alias` is at the end of the stream and the stream has the property eof_action(error)

-- -- -- -- &gt; existence_error(past_end_of_stream, `Stream_or_Alias`) .

The stream associated with `Stream_or_Alias` has no input ready to be read and the stream has the property snr_action(error)

-- -- -- -- &gt; existence_error(stream_not_ready, `Stream_or_Alias`) .


## NOTES

A character code is simply an integer restricted to a certain range of values.

## SEE ALSO

- [`put_code/2`](put_code.html)
- [`get_char/2`](get_char.html)
- [`open/4`](open.html)
- [`close/1`](close.html)
- [`char_code/2`](char_code.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
