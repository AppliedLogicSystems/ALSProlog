---
title: 'put_atom/[1,2]'
group: Input Output
module: sio
predicates:
- {sig: 'put_atom', args: {
    1: 'output an atom to the current output stream',
    2: 'output an atom to a specific output stream'
  }}
---

## FORMS
```
put_atom(Atom)

put_atom(Stream_or_Alias, Atom)
```
## DESCRIPTION

`put_atom/1` will write out the atom bound to `Atom` to the current output stream.

`put_atom/2` will write out the atom bound to `Atom` to the output stream associated with `Stream_or_Alias`.


## EXAMPLES

```
?- put_atom(ice),put_atom(cream).
icecream
yes.
```
## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- &gt; instantiation_error.

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, `Stream_or_Alias) .

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

`Stream_or_Alias` is not an output stream

-- -- -- -- &gt; permission_error(output, stream, Stream_or_Alias) .

`Atom` is a variable

-- -- -- -- &gt; instantiation_error.

`Atom` is neither a variable nor an atom

-- -- -- -- &gt; type_error(atom, Atom) .

