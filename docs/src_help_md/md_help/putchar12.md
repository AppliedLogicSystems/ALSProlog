---
title: 'put_char/[1,2]'
predicates:
 - 'put_char/1' : output a character to the current output stream
 - 'put_char/2' : output a character to a specific output stream
---
`put_char/1` — output a character to the current output stream

`put_char/2` — output a character to a specific output stream

## FORMS
```
put_char(Char)

put_char(Stream_or_Alias, Char)
```
## DESCRIPTION

`put_char/1` will write out the character bound to `Char` to the current output stream.

`put_char/2` will write out the character bound to `Char` to the output stream associated with `Stream_or_Alias`.


## EXAMPLES
```
?- put_char('\t'),put_char(h),put_char(o),
?_ put_char(w),put_char(d),put_char(y),nl.
	howdy

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

`Char` is a variable

-- -- -- -- &gt; instantiation_error.

`Char` is neither a variable nor a character

-- -- -- -- &gt; type_error(character, Char) .


## SEE ALSO

- `get_char/1`  
`put_code/1`  
`open/4`  
`close/1`  
`char_code/2`  
`nl/1`

- User Guide (Prolog I/O)
