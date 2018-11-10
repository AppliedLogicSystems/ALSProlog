---
title: 'current_input/1'
predicates:
 - 'current_input/1' : retrieve current input stream
 - 'current_output/1' : retrieve current output stream
---

## FORMS
```
current_input(Stream)

current_output(Stream)
```
## DESCRIPTION

`current_input/1` will unify `Stream` with the stream descriptor associated with the current input stream. The current input stream is the stream that is read when predicates such as `get_char/1` or `read/1` are called. The current input stream may be set by calling `set_input/1`.

`current_output/1` will unify `Stream` with the stream descriptor associated with the current output stream. The current output stream is the stream which is written to when predicates such as `put_char/1` or `write/1` are called. The current output stream may be set by calling `set_output/1`.

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

`close/1` applied to an input or output stream  may change the current output or current input streams.

## SEE ALSO

- `set_input/1`  
`get_char/1`  
`read/1`  
`put_char/1`  
`write/1`

- `User Guide (Prolog I/O)`
