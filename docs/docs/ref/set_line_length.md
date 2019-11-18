---
title: 'set_line_length/2'
group: Input Output
module: sio
predicates:
- {sig: 'set_line_length/2', desc: 'set length of line for output stream'}
- {sig: 'set_max_depth/2', desc: 'set maximum depth that terms will be written to'}
- {sig: 'set_depth_computation/2', desc: 'set method of computing term depth'}
---

## FORMS

```
set_line_length(Stream_or_Alias, Length)

set_maxdepth(Stream_or_Alias, Depth)

set_depth_computation(Stream_or_Alias, Flat_Nonflat)
```

## DESCRIPTION

`set_line_length/2` sets the default line length for the output stream associated with `Stream_or_Alias` to the integer value bound to `Length`. The default line length is an integer parameter used by [`writeq/[1,2]`, `write_canonical/[1,2]`, and `write_term/[2,3]`](write.html) to determine where line breaks should occur when outputting a term. A call to `write_term` may temporarily overide this parameter by specifying the `line_length` option in the `write` options list. The default line length may also be set at the time the stream is opened by specifying the `line_length` option in the options list to [`open/[3,4]`](open.html).

`set_maxdepth/2` sets the default depth limit to which terms are output for the output stream associated with `Stream_or_Alias` to the integer value bound to `Depth`. The default depth limit is used by the term output predicates to determine the maximum depth to write to. This parameter may also be set at the time of an open with the appropriate open option and may be overridden in calls to [`write_term/[3,4]`](write.html) with the appropriate write option.

`set_depth_computation/2` sets the manner in which the depth of a term is computed for the output stream associated with `Stream_or_Alias` to the atomic value bound to `Flat_Nonflat`. As the name of the variable implies, `Flat_Nonflat` must be bound to one of the two atoms, flat or non-flat. If the depth computation method is flat, all arguments in a structured term and all list elements are considered to be at the same level. If the method is non-flat, then each subsequent structure argument or list element is considered to be at a depth one greater than the previous element.

## EXAMPLES

```
?- set_line_length(user_output,20),
?-_L=[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
?-_write(L),nl.
[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
L = [a,b,c,d,e,f,g,
h,i,j,k,l,
m,n,o,p,q,
r,s,t,u,v,
w,x,y,z]
```

```
?- set_maxdepth(user_output,8),
?-_set_depth_computation(user_output,nonflat),
?-_L=[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
?-_write(L),nl.
[a,b,c,d,e,f,g,h,...]
L = [a,b,c,d,e,f,
g,...]
```

## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- > `instantiation_error`

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- > `domain_error(stream_or_alias, Stream_or_Alias)`

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- > `existence_error(stream, Stream_or_Alias)`

`Stream_or_Alias` is not an output stream

-- -- -- -- > `permission_error(output, stream, Stream_or_Alias)`

`Length`, `Depth`, or `Flat_or_Nonflat` is a variable

-- -- -- -- > `instantiation_error`

`Length` is not a variable or an integer

-- -- -- -- > `type_error(integer, Length)`

`Length` is not an integer greater than four

-- -- -- -- > `domain_error(line_length, Length)`

`Depth` is not a variable or an integer

-- -- -- -- > `type_error(integer, Depth)`

`Depth` is an integer, but not a positive integer

-- -- -- -- > `domain_error(positive_integer, Depth)`

`Flat_or_Nonflat` is neither a variable nor an atom

-- -- -- -- > `type_error(atom, Flat_or_nonflat)`

`Flat_or_Nonflat` is an atom, but is neither flat nor non-flat

-- -- -- -- > `domain_error(depth_computation, Flat_or_nonflat)`

## NOTES

Note in the above examples that [`write/[1,2]`](write.html) does not pay attention to the line length. It does however, observe the default maximum depth and the method for computing the depth.


## SEE ALSO

- [`stream_property/2`](stream_property.html)
- [`open/4`](open.html)
- [`write_term/3`](write.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)

