---
title: 'peek_code/[1,2]'
group: Input Output
module: sio
iso: peekcode
predicates:
- {sig: 'peek_code', args: {
    1: 'obtain char code from stream',
    2: 'obtain char from stream'
   }}
---

## FORMS
```
peek_code(Charcode)

peek_code(Stream_or_alias, Charcode)
```
## DESCRIPTION

`peek_code(Charcode)` unifies `Charcode` with the ASCII code of the next character obtained from the default input stream. However, the character is not consumed from the stream.

`peek_char(Alias_or_Stream, Charcode)` unifies `Charcode` with the ASCII code of the next character obtained from the stream associated with `Stream_or_alias`. However, the character is not consumed from that stream.

