---
title: 'at_end_of_stream/[0,1]'
group: Input Output
module: sio
iso: streamproperty
predicates:
- {sig: 'at_end_of_stream', args: {
     0: 'test for end of the curent input stream',
     1: 'test for end of a specific input stream'
   }}
---

## FORMS
```
at_end_of_stream

at_end_of_stream(Stream_or_Alias)
```
## DESCRIPTION

`at_end_of_stream/0` will succeed when the stream position associated with the current input stream is located at or past the end of stream.

`at_end_of_stream/1` will succeed when the stream position associated with `Stream_or_Alias` is located at or past the end of stream.


## ERRORS

`Stream_or_Alias` is a variable.

––––> instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

––––> domain_error(stream_or_alias,Stream_or_Alias)

`Stream_or_Alias` is not associated with an open stream

––––> existence_error(stream,Stream_or_Alias)

## NOTES

Calling `at_end_of_stream` may cause an input operation to take place. Thus a call to this predicate could block.

## SEE ALSO

- [`open/4`](open.html)
- [`current_input/1`](current_input.html)
- [`flush_output/1`](flush_output.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
