---
title: 'set_stream_position/2'
predicates:
 - 'set_stream_position/2' : seek to a new position in a stream
---
`set_stream_position/2` `--` seek to a new position in a stream


## FORMS

set_stream_position(Stream_or_Alias, Position)


## DESCRIPTION

set_stream_position/2 is used to change the stream position for a stream which is repositionable.

Stream_or_Alias is the stream for which to change the stream position.

Position is a term which represents the new position to set. It takes one of the following forms :

- An absolute integer which represents the address of a character in the stream. The beginning of the stream or the first character in the stream has position 0 The second character in the stream has position 1 and so on.

- The atom
beginning_of_stream.

- The term beginning_of_stream(N) where N is an integer greater than zero. The position represented by this term is the beginning of the stream plus N bytes.

- The atom
end_of_stream.

- The term end_of_stream(N) where N is an integer less than or equal to zero. This allows positions(earlier than the end of stream) to be specified relative to the end of the stream. The position represented by this term is the end-of-stream position plus N bytes.

- The atom
current_position.

- The term current_position(N) where N is an integer. This allows positions to be specified relative to the current position in the file.


## EXAMPLES

Suppose that the file &quot; test &quot; is comprised of the characters &quot; abcdefgh\n &quot; .

Open and read the first character from the file test.

```
?- open(test,read,_,[alias(test_alias)]),get_char(test_alias,C).
C=a
Seektotwocharactersbeforeendoffileandgetthecharacteratthisposition.
?- set_stream_position(test_alias,end_of_stream(-2)),
?-_get_char(test_alias,C).
C=h
Seektocurrentpositionminustwoandgetthecharacteratthisposition.
?- set_stream_position(test_alias,current_position(-2)),
?-_get_char(test_alias,C).
C=g
Seektofourthcharacterinfileandgetit.Recallthatthefirstcharacterhasaddress0
?- set_stream_position(test_alias,3),get_char(test_alias,C).
C=d
Getthissamecharacteragainbybackingupone.
?- set_stream_position(test_alias,current_position(-1)),
?-_get_char(test_alias,C).
C=d
Getthenextcharacterinthestream.
?- get_char(test_alias,C).
C=e
Getthecurrentstreamposition.
?- stream_property(test_alias,position(P)).
P=5
Getthecurrentstreamposition,seektothebeginningofthestreamandgetthatcharacter,thenseekbacktotheoldpositionandgetthecharacteratthatposition.
?- stream_property(test_alias,position(P)),
?-_set_stream_position(test_alias,beginning_of_stream),
?-_get_char(test_alias,C1),
?-_set_stream_position(test_alias,P),
?-_get_char(test_alias,C2).
P=5
C1=a
C2=f
```

## ERRORS

Stream_or_Alias is a variable

-- -- -- -- &gt; instantiation_error.

Position is a variable

-- -- -- -- &gt; instantiation_error.

Stream_or_Alias is neither a variable nor a stream descriptor nor an alias

-- -- -- -- &gt;

domain_error(stream_position, Position) .

Position is neither a variable nor a stream position

-- -- -- -- &gt; domain_error(stream_position, Position) .

Stream_or_Alias is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

Stream_or_Alias has stream property reposition(false)

-- -- -- -- &gt; permission_error(reposition, stream, Stream_or_Alias) .


## NOTES

As the example above demonstrates, set_stream_position/2 may be used when used in conjunction with stream_property/2. Typically a program will get the current position using stream_property/2 and later set the stream position using this saved position.


## SEE ALSO

- `open/4`  
`get_char/2`  
`stream_property/2`
- `UserGuide(PrologI/O).
