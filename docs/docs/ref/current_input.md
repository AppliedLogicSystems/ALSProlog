---
title: 'current_input/1'
group: Input Output
module: sio
iso: currentoutput
predicates:
- {sig: 'current_input/1', desc: 'retrieve current input stream'}
- {sig: 'current_output/1', desc: 'retrieve current output stream'}
---

## FORMS
```
current_input(Stream)

current_output(Stream)
```
## DESCRIPTION

`current_input/1` will unify `Stream` with the stream descriptor associated with the current input stream. The current input stream is the stream that is read when predicates such as [`get_char/1`](get_char.html) or [`read/1`](read.html) are called. The current input stream may be set by calling [`set_input/1`](set_input.html).

`current_output/1` will unify `Stream` with the stream descriptor associated with the current output stream. The current output stream is the stream which is written to when predicates such as [`put_char/1`](put_char.html) or [`write/1`](write.html) are called. The current output stream may be set by calling [`set_output/1`](set_input.html).

## EXAMPLES

Suppose that the file &quot;test&quot; is comprised of the characters &quot;abcdefgh\n&quot;.  `Open` the file &quot;test&quot; and set the current input stream to the stream descriptor returned from `open`:

```
?- open(test,read,S1),set_input(S1), current_input(S2).
S1=stream_descriptor('',open,file,test,[input|nooutput],true,42,0,0,0,0,true,
    0,wt_opts(78,400,flat),[],wait,text,eof_code,true,0) 
S2=stream_descriptor('',open,file,test,[input|nooutput],true,42,0,0,0,0,true,
    0,wt_opts(78,400,flat),[],wait,text,eof_code,true,0) 

yes.
```
Get two characters from the current input stream:
```
?- get_char(C1),get_char(C2).
C1=a
C2=b
```
Close the stream associated with &quot;test&quot;:
```
?- current_input(S),close(S).

S=stream_descriptor('\002',closed,file,test,[input|nooutput],true,42,0,0,0,0,
    true,0,wt_opts(78,400,flat),[],wait,text,eof_code,true,0) 

yes.
```
## NOTES

[`close/1`](close.html) applied to an input or output stream  may change the current output or current input streams.

## SEE ALSO

- [`set_input/1`](set_input.html)
- [`get_char/1`](get_char.html)
- [`read/1`](read.html)
- [`put_char/1`](put_char.html)
- [`write/1`](write.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
