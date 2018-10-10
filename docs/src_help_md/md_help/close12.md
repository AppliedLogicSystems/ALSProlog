—-
title: 'close/1'
predicates:
 - 'close/1' : close an open stream
 - 'close/2' : close an open stream with options
—-
`close/1` `—` close an open stream

`close/2` `—` close an open stream with options


## FORMS

close(Stream_or_Alias)

close(Stream_or_Alias, CloseOptions)


## DESCRIPTION

close/1 and close/2 close a stream previously opened with open/3 or open/4. Closing a stream consists of flushing the buffer associated with the stream and freeing resources associated with maintaining an open stream. If there is an alias associated with the stream, this alias is disassociated and freed up for potential use by some other stream. If the stream to be closed is the current input stream, then the current input stream is set to the stream associated with the alias user_input. If the stream to be closed is associated with the current output stream, then the current output stream is set to the stream associated with the alias user_output.

Certain types of streams may have other actions performed. Atom streams opened for write will have the stream contents unified with the atom A which appeared in the sink specificat

ion in the call to open.

Stream_or_Alias is either a stream descriptor or an alias established via a call to open/3 or open/4.

CloseOptions is a list consisting of options to close/2. The close options are :

force(false) — This is the default. A system error or resource error which occurs while closing the stream may prevent the stream from being closed.

force(true) — Errors occuring while closing the stream are ignored and resources associated with the stream are freed anyway.


## ERRORS

Stream_or_Alias is a variable

— — -- -- &gt; instantiation_error.

Stream_or_Alias is neither a variable nor a stream identifier or alias

— — -- -- &gt; domain_error(stream_or_alias, Stream_or_Alias)

CloseOptions is a variable

— — -- -- &gt; instantiation_error.

CloseOptions is neither variable nor list

— — -- -- &gt; type_error(list, CloseOptions) .

CloseOptions is a list which contains a variable element

— — -- -- &gt; instantiation_error.

CloseOptions is a list which contains an element E which is neither a variable nor a valid close option

— — -- -- &gt; domain_error(close_option, E) .


## NOTES

Certain streams which are opened at system startup time can not be closed. Among these streams are user_input and user_output. Calling close on these aliases will neither throw an error nor really close the stream.


## SEE ALSO

- `open/4`  
`current_input/1`  
`flush_output/1`
- `UserGuide(PrologI/O).
