---
title: 'set_input/1'
group: Input Output
module: sio
iso: setoutput
predicates:
- {sig: 'set_input/1', desc: 'set current input stream'}
- {sig: 'set_output/1', desc: 'set current output stream'}
---

## FORMS

```
set_input(Stream_or_Alias)

set_output(Stream_or_Alias)
```

## DESCRIPTION

For both forms `Stream_or_Alias` must be either a stream descriptor returned by a call to [`open/3` or `open/4`](open.html) or an alias created during a successful call to `open`. For the sake of the following discussion, let us suppose that `Stream` is `Stream_or_Alias` if `Stream_or_Alias` is a stream descriptor. If `Stream_or_Alias` is an alias, then let `Stream` be the stream associated with that alias.

`set_input/1` will set the stream associated with the current input stream to `Stream`. The current input stream is the stream that is read when predicates such as [`get_char/1`](get_char.html) or [`read/1`](read.html) are called. The current input stream may be retrieved by calling [`current_input/1`](current_input.html).

`set_output/1` will set the stream associated with the current output stream to `Stream`. The current output stream is the stream which is written to when predicates such as [`put_char/1`](put_char.html) or [`write/1`](write.html) are called. The current output stream may be retrieved by calling [`current_output/1`](current_input.html).


## EXAMPLES

Suppose that the file "test" is comprised of the characters "abcdefgh\n".

Open the file "test" and set the current input stream to the stream descriptor returned from open.

```
?- open(test,read,S), set_input(S).
S = stream_descriptor('',open,file,test,[input|nooutput],true,
4,0,0,0,0,true,0,wt_opts(78,40000,flat),[],true,
text,eof_code,0,0)
```

Get two characters from the current input stream.

```
?- get_char(C1), get_char(C2).
C1 = a
C2 = b
```

Close the stream associated with "test".

```
?- current_input(S), close(S).
S = stream_descriptor('',closed,file,test,[input|nooutput],true,
4,0,0,0,0,true,0,wt_opts(78,40000,flat),[],true,
text,eof_code,0,0)
```

## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- > `instantiation_error`

`Stream_or_Alias` is not a variable nor a stream descriptor nor an alias

-- -- -- -- > `domain_error(stream_or_alias, Stream_or_Alias)`

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- > `existence_error(stream, Stream_or_Alias)`

`Stream_or_Alias` is not an input stream (for `set_input/1`)

-- -- -- -- > `permission_error(input, stream, Stream_or_Alias)`

`Stream_or_Alias` is not an output stream (for `set_output/1`)

-- -- -- -- > `permission_error(output, stream, Stream_or_Alias)`


## NOTES

[`close/1`](close.html) may also change the setting of the current input or output streams.


## SEE ALSO

- [`current_input/1`](current_input.html)
- [`open/3`](open.html)
- [`close/1`](close.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)

