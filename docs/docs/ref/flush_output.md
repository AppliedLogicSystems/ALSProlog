---
title: 'flush_output/[0,1]'
group: Input Output
module: sio
predicates:
- {sig: 'flush_output', args: {
    0: 'flush current output stream',
    1: 'flush specific output stream'
   }}
---

## FORMS
```
flush_output

flush_output(Stream_or_Alias)
```
## DESCRIPTION

A call to `flush_output/0` will cause the buffer contents associated with the current output stream to be written out.

A call to `flush_output/1` will cause the buffer contents associated with the open output stream `Stream_or_Alias` to be written out.

An output operation such as [`put_char/2`](put_char.html) or [`write/2`](write.html) is used to put some data out to a stream. Unless byte buffering `[buffering((byte)]` is specified when the stream is opened, the data will not be immediately output. Rather, the data will be buffered in an area associated with the open output stream. The `flush_output` builtins cause this buffer to be written out to the sink associated with the open stream.

## EXAMPLES

The following two procedures illustrate the use of `flush_output` to write out a single dot in between potentially long computations.

```
?- listing.
process(N)
    :-
    N>0,
    put_char(user_output,'.'),
    flush_output(user_output),
    compute,
    NNisN-1,
    process(NN).

process(_)
    :-
    nl(user_output).

compute
    :-
    system('sleep2').

yes.

?- process(5).
.....

yes.
```
## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, `Stream_or_Alias`)

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, `Stream_or_Alias`)

`Stream_or_Alias` is not an output stream

-- -- -- -- &gt; permission_error(output, stream, `Stream_or_Alias`)

`Stream_or_Alias` is a valid output stream, but the flush operation could not be completed due to some other problem detected by the operating system

-- -- -- -- &gt; system_error

## SEE ALSO

- [`open/4`](open.html)
- [`close/1`](close.html)
- [`current_output/1`](current_input.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
