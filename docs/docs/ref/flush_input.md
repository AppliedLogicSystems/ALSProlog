---
title: 'flush_input/1'
group: Input Output
module: sio
predicates:
- {sig: 'flush_input/1', desc: 'discard buffer contents of stream'}
---

## FORMS
```
flush_input(Stream_or_Alias)
```
## DESCRIPTION

`flush_input/1` will cause the buffer contents associated with the input stream `Stream_or_Alias` to be discarded. This will cause the next input operation to read in a new buffer from the source attached to `Stream_or_Alias`.

## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, Stream_or_Alias)

`Stream_or_Alias` is not an input stream

-- -- -- -- &gt; permission_error(input, stream, Stream_or_Alias)


## NOTES

This operation is useful on streams which have an associated output stream on which prompts are being written to. Flushing the input and then performing the requisite input operation will cause a prompt to be written out prior to reading input.

This operation is also useful for use with datagram sockets. The buffer contents associated with a datagram socket represent the entire datagram. When the end of the datagram is reached, an `end-of-file` condition will be triggered so that the program reading the datagram will not inadvertently read beyond the datagram into the next datagram (if any). `flush_input/1` should be used to reset the `end-of-file` indication after the datagram has been processed.

## SEE ALSO

- [`open/4`](open.html)
- [`flush_output/[0,1]`](flush_output.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
