---
title: 'set_input/1'
predicates:
 - 'set_input/1' : set current input stream
 - 'set_output/1' : set current output stream
---
`set_input/1` `--` set current input stream

`set_output/1` `--` set current output stream


## FORMS

set_input(Stream_or_Alias)

set_output(Stream_or_Alias)


## DESCRIPTION

For both forms Stream_or_Alias must be either a stream descriptor returned by a call to open/3 or open/4 or an alias created during a successful call to open. For the sake of the following discussion, let us suppose that Stream is Stream_or_Alias if Stream_or_Alias is a stream descriptor. If Stream_or_Alias is an alias, then let Stream be the stream associated with that alias.

set_input/1 will set the stream associated with the current input stream to Stream. The current input stream is the stream that is read when predicates such as get_char/1 or read/1 are called. The current input stream may be retrieved by calling current_input/1.

set_output/1 will set the stream associated with the current output stream to Stream. The current output stream is the stream which is written to when predicates such as put_char/1 or write/1 are called. The current output stream may be retrieved by calling current_output/1.


## EXAMPLES

Suppose that the file &quot; test &quot; is comprised of the characters &quot; abcdefgh\n &quot; .

Open the file &quot; test &quot; and set the current input stream to the stream descriptor returned from open.

```
?- open(test,read,S),set_input(S).
S=stream_descriptor('',open,file,test,[input|nooutput],true,
4,0,0,0,0,true,0,wt_opts(78,40000,flat),[],true,
text,eof_code,0,0)
Gettwocharactersfromthecurrentinputstream.
?- get_char(C1),get_char(C2).
C1=a
C2=b
Closethestreamassociatedwith&quot;test&quot;.
?- current_input(S),close(S).
S=stream_descriptor('',closed,file,test,[input|nooutput],true,
4,0,0,0,0,true,0,wt_opts(78,40000,flat),[],true,
text,eof_code,0,0)
```

## ERRORS

Stream_or_Alias is a variable

-- -- -- -- &gt; instantiation_error.

Stream_or_Alias is not a variable nor a stream descriptor nor an alias

-- -- -- -- &gt; domain_error(stream_or_alias, Stream_or_Alias) .

Stream_or_Alias is not associated with an open stream

-- -- -- -- &gt; existence_error(stream, Stream_or_Alias) .

Stream_or_Alias is not an input stream(for set_input/1)

-- -- -- -- &gt; permission_error(input, stream, Stream_or_Alias) .

Stream_or_Alias is not an output stream(for set_output/1)

-- -- -- -- &gt; permission_error(output, stream, Stream_or_Alias) .


## NOTES

close/1 may also change the setting of the current input or output streams.


## SEE ALSO

- `current_input/1`  
`open/3`  
`close/1`
- `UserGuide(PrologI/O).

