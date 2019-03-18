---
title: 'poll/2'
group: Input Output
module: sio
predicates:
- {sig: 'poll/2', desc: 'Determine whether I/O is possible'}
---

## FORMS
```
poll(Stream_or_Alias, TimeOut)
```

## DESCRIPTION

`poll/2` will wait at most `TimeOut` microseconds and then succeed if a non-blocking I/O operation may be started on the stream associated with `Stream_or_Alias`. Failure will occur if the I/O operation would block.

## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, Stream_or_Alias) .

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

`TimeOut` is a variable

-- -- -- -- &gt; instantiation_error.

`TimeOut` is not an integer

-- -- -- -- &gt; type_error(integer, TimeOut) .


## NOTES

Note that an input operation such as [`read/2`](read.html) may block anyway if there is insufficient input to syntactically complete the term and the terminating full-stop. It is possible for poll to succeed when only the first character of the term is ready. The [`open/[3,4]`](open.html) option, `snr_action`, is better used for situations where reading a term from a stream which might not be ready is desirable.

## SEE ALSO

- [`open/[3,4]`](open.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
