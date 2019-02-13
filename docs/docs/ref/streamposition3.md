---
title: 'stream_position/[2,3]'
group: Input Output
module: sio
predicates:
- {sig: 'stream_position/[2,3]', desc: 'reposition a stream'}
---

## FORMS

```
set_stream_position(Stream_or_alias, Current_position, New_position)
```

## DESCRIPTION

If the stream associated with `Stream_or_alias` supports repositioning, `Current_position` is unified with the current stream position of the stream, and, as a side effect, the stream position of this stream is set to the position represented by `New_position`. `New_position` may be one of the following values:

- An absolute integer which represents the address of a character in the stream. The beginning of the stream or the first character in the stream has position 0 The second character in the stream has position 1 and so on.
- The atom `beginning_of_stream`.
- The term `beginning_of_stream(N)` where `N` is an integer greater than zero. The position represented by this term is the beginning of the stream plus `N` bytes.
- The atom `end_of_stream`.
- The term `end_of_stream(N)` where `N` is an integer less than or equal to zero. This allows positions (earlier than the end of stream) to be specified relative to the end of the stream. The position represented by this term is the end-of-stream position plus `N` bytes.
- The atom `current_position`.
- The term `current_position(N)` where `N` is an integer. This allows positions to be specified relative to the current position in the file.

## ERRORS

`Stream_or_Alias` is a variable

-- -- -- -- > `instantiation_error`

`New_Position` is a variable

-- -- -- -- > `instantiation_error`

`Stream_or_Alias` is neither a variable nor a stream descriptor nor an alias

-- -- -- -- > `domain_error(stream_position, Position)`

`New_Position` is neither a variable nor a stream position

-- -- -- -- > `domain_error(stream_position, Position)`

`Stream_or_Alias` is not associated with an open stream

-- -- -- -- > `existence_error(stream, Stream_or_Alias)`

`Stream_or_Alias` has stream property `reposition(false)`

-- -- -- -- > `permission_error(reposition, stream, Stream_or_Alias)`

