---
title: 'put_string/[1,2]'
group: Input Output
module: sio
predicates:
- {sig: 'put_string', args: {
    1: 'output a string to the current output stream',
    2: 'output a string to a specific output stream'
  }}
---

## FORMS
```
put_string(String)

put_string(Stream_or_Alias, String)
```
## DESCRIPTION

`put_string/1` will write out the string bound to `String` to the current output stream.

`put_string/2` will write out the string bound to `String` to the output stream associated with `Stream_or_Alias`.

## EXAMPLES

```
?- put_string("ice"),put_string("cream").
icecream
yes.
```
## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, Stream_or_Alias) .

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

`Stream_or_Alias` is not an output stream

-- -- -- -- &gt; permission_error(output, stream, Stream_or_Alias) .

`String` is a variable

-- -- -- -- &gt; instantiation_error.

`String` is neither a variable nor a string

-- -- -- -- &gt; type_error(string, String) .

