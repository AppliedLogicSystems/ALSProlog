---
title: 'open/[3,4]'
group: Input Output
module: sio
iso: open
predicates:
- {sig: 'open', args: {
    3: 'open a stream',
    4: 'open a stream with options'
   }}
---

## FORMS
```
open(Name, Mode, Stream)

open(Name, Mode, Stream, Options)
```
## DESCRIPTION

`open/3` and `open/4` are used to open a file or other entities for input or output. Calling `open/3` is the same as calling `open/4` where `Options` is the empty list.

Once a stream has been opened with `open/3` or `open/4` for reading or writing, [`read_term/3`](read.html) or [`write_term/3`](write.html) can then be used to read terms from or write terms to the opened stream. [`get_char/2`](get_char.html) or [`put_char/2`](put_char.html) can be used to read or write characters. 

A **_source_** refers to some entity which may be opened as a stream for read access. Such streams, once open, are called _input streams_. A **_sink_** refers to some entity which may be written to. Such a stream is called an _output stream_. There are streams which are both sources and sinks; such streams may be both read from and written to.

'Name' specifies the source/sink to be opened. Whether 'Name' is a source or a sink will depend on the value of 'Mode'. 'Mode' may either be read, indicating that 'Name' is a source. Or 'Mode' may be write, which indicating that `Name` is a sink. If the source/sink specified by `Name`, `Mode`, and the options in `Options` is successfully opened, then `Stream` will be unified with a stream descriptor which may be used in I/O operations on the stream. If the stream could not be successfully opened, then an error is thrown.

If `Name` is an atom, then the contents of the stream are the contents of the file with name `Name`. `Mode` may take on additional values for file streams. The values which `Mode` may take on for file streams are:

`read` -- open the file for read access

`write` -- open the file for write access; truncate or create the file as necessary

`read_write` -- both reading and writing are permitted; file is not truncated on open

`append` -- open file with write access and position at end of stream; file is not truncated on open

If `Name` has the form `atom(A)`, then `Name` represents an atom stream. When opened with `Mode` equal to read, `A` must be an atom. The contents of the stream are simply the characters comprising the atom `A`. When opened for write access, `A` will be unified with the atom formed from the characters written to the stream during the time that the stream was open. The unification is carried out at the time that the stream is closed.

If `Name` has the form `code_list(CL)` or `string(CL)`, then `Name` represents a character-code list stream. When opened for read access, the contents of such a stream are the character codes found in the list `CL`. When opened for write access, `CL` will be unified with a list of character codes. This list is formed from the codes of characters written to the stream during the lifetime of the stream. The unification is carried out when the stream is closed.

If `Name` has the form `char_list(CL)`, then `Name` represents a character list stream. The behavior of character list streams is identical to that of code list streams with the exception that the types of the objects in the lists are different. A code list consists of character codes, whereas a character list consists of a list of characters.

`Name` may also take on one of the following forms representing a socket stream:
```
socket(unix, PathName)

socket(inet_stream, Host)

socket(inet_stream, Host, Port)

socket(inet_dgram, Host, Port)

socket(clone, Stream_Or_Alias)
```
Regardless of the mode(read or write), the call to `open/[3,4]` will attempt to open the socket stream as a server (if appropriate). Failing that, it will attempt to open the stream as a client. If a connection-oriented socket is opened as a server, then prior to the first buffer read or write, the stream will wait for a client to connect. If the stream is opened as a client, the connection (in a connection-oriented socket) is established as part of the open.

`socket(unix, PathName)`: Open a unix domain socket. Addresses in the unix domain are merely path names which are specified by `PathName`. Unix domain sockets are somewhat limited in that both the server and client process must reside on the same machine. The stream will be opened as a server if the name specified by `PathName` does not currently exist (and the requisite permissions exist to create a directory entry). Otherwise, the stream will be opened as a client.

`socket(inet_stream, Host)`: Open an internet domain stream socket on the host given by `Host` using port number 1599. The stream will be opened as a server if `Host` is set to the name of the host on which the process is running and no other process has already established a server stream on this port. Otherwise, the stream will be opened as a client. A permission error will be generated if neither operation can be performed. The host specification may either be a host name or an internet address.

`socket(inet_stream, Host, Port)`: Similar to the above, but the Port may specified. This permits an application to choose its own &quot;well known&quot; port number and act as either a server or client. Alternately, both `Host` and `Port` may be variable, in which case the system will open a stream at a port of its choosing. When variable, `Host` and `Port` will be instantiated to values of the current host and the port which was actually opened.

`socket(inet_dgram, Host, Port)`: Similar to inet_stream, but a datagram socket is created instead. A datagram socket is an endpoint which is not connected. The datagram socket will be opened as a server socket if the `Host` is either variable or bound to the name of the current host and `Port` is either variable or bound to a port number which is not currently in use. Otherwise, a client socket is established which (by default) will write to the host and port indicated by `Host` and `Port`. A server socket is initially set up to write out to UDP port 9 which will discard any messages sent. Datagram sockets will set the end-of-file indication for each datagram read. This mechanism permits code written in Prolog to fully process each incoming datagram without having to worry about running over into the next datagram. [`flush_input/1`](flush_input.html) should be used to reset the stream attached to the datagram in order for more input to be read. Programmers using datagrams should strive to make each datagram self contained. If this is a hardship, stream sockets should be used instead.

`socket(clone, Stream_or_Alias)`: This specification will create a new(prolog) stream descriptor for a socket from an existing socket stream descriptor. This gives the programmer the ability to create more than one buffer which refers to the same socket. This cloning mechanism has two uses. Firstly, sockets are full duplex which means that they may be both read from and written to. Yet, the interface which ALS Prolog provides will only naturally provide read access or write access, not both simultaneously. The cloning mechanism accommodates this problem by allowing separate(prolog) stream descriptors for each mode which refer to the same unix socket descriptor. Secondly, server socket streams will only act as a server until a connection is established. Once the connection is made, they lose their &quot;server&quot; property. An application which wants to service more than one client will want to clone its &quot;server&quot; descriptor prior to performing any reads to or writes from the stream.

`Name` may also take the form `url(URL)` or `url(URL, Options)`, representing a Curl-based internet-stream. A stream `S` opened with
```
open(url(<URL>, <CurlOptionList>), read, S, <possible generic stream options>)
```
will contain the characters returned by a GET to `<URL>`, conditioned by `<CurlOptionList>` and `<possible generic stream options>`.

A stream S opened with
```
open(url(<URL>, <CurlOptionList>), write, S, <possible generic stream options>)
```
will capture all characters written to `S` (write,nl,printf, etc.), and when `S` is closed, the sequence of characters in `S` will be POSTed to ``, conditioned by `<CurlOptionList>` and `<possible generic stream options>`.  WARNING: The implementation of open(url(,), write, S, …) utilizes the freeze construct. Everything will work correctly so long as no gc (garbage collection) takes place; otherwise, Issue #87 will cause incorrect and unpredictable behavior.

For both read and write internet url streams, the options available for `<CurlOptionList>` are exactly the same as for [`http/3` or `curl/[1,3]`](curl.html).

The behavior and disposition of a stream may be influenced by the (generic) `Options` arguments. `Options` is a list comprising one or more of the following terms :

`type(T)` -- `T` may be either text or binary. This defines whether the stream is a text stream or a binary stream. At present, ALS Prolog makes no distinction between these two types.

`alias(Alias)` -- `Alias` must be an atom. This option specifies an alias for the stream. If an alias is established, the alias may be passed in lieu of the stream descriptor to predicates requiring a stream handle.

`reposition(R)` -- `R` is either true or false. `reposition(true)` indicates that the stream must be repositionable. If it is not, `open/3` or `open/4` will throw an error. `reposition(false)` indicates that the stream is not repositionable and any attempt to reposition the stream will result in an error. If neither option is specified, the stream will be opened as repositionable if possible. A program can find out if a stream is repositionable or not by calling [`stream_property/2`](stream_property.html).

`eof_action(Action)` -- `Action` may be one of `error, eof_code`, or `reset`. `Action` instantiated to `error` indicates that an existence error should be triggered when a stream attempts to read past `end-of-file`. The default `Action` is `eof_code` which will cause an input predicate reading past `end-of-file` to return a distinguished value as the output of the predicate (either `end_of_file`, or -1) . Finally, `Action` instantiated to reset indicates that the stream should be reset upon `end-of-file`.

`snr_action(Action)` -- `Action` may be one of `error, snr_code`, or `wait`. As with `eof_action`, `Action` instantiated to `error` will generate an existence error when an input operation attempts to read from a stream for which no input is ready. As with `eof_action`, `Action` instantiated to `snr_code` will force the input predicate to return a distinguished code when the stream is not ready. This code will be either -2 or the atom `stream_not_ready`. The default `Action` is `wait` which will force the input operation to wait until the stream is ready.

`buffering(B)` -- `B` is either `byte, line`, or `block`. This option applies to streams open for output. If `buffering(byte)` is specified, the stream buffer will be flushed (actually written out) after each character. `buffering(line)` is useful for streams which interact with a user; the buffer is flushed when a newline character is put into the buffer. `buffering(block)` is the default; the buffer is not flushed until the block is full or until a call to [`flush_output/1`](flush_output.html) is made.

`bufsize(Size)` -- `Size` must be a positive integer. The `Size` parameter indicates the size of buffer to allocate the associated stream. The default size should be adequate for most streams.

`prompt_goal(Goal)` -- `Goal` should be a ground callable term. This option is used when opening an input stream. `Goal` will be run each time a new buffer is read. This option is most useful when used in conjunction with opening an output stream where a prompt should be written to whenever new input is required from the input stream.

`maxdepth(Depth)` -- `Depth` is a positive integer. This option when specified for an output stream sets the default maximum depth used to write out a term with [`write_term/3`](write.html), et. al. Explicit options to `write_term/3` may be used to override this option.

`depth_computation(DC)` -- `DC` should be either `flat` or `nonflat`. This option indicates the default mechanism to be used for [`write_term/3`](write.html) to compute the depth of a term. `flat` indicates that all arguments in a list or structured term should be considered to be at the same depth. `nonflat` indicates that each successive element of a structured term or list is at depth one greater than its predecessor.

`line_length(Length)` -- `Length` is a positive integer. This option is used to set the default line length associated with the (output) stream. Predicates which deal with term output use this parameter to break the line at appropriate points when outputting a term which will span several lines. Explicit options to [`write_term/3`](write.html) may be used to override this option.

`write_eoln_type(Type)` - allows control over which end-of-line(eoln) characters are output by `nl/1`. The values for `Type` and the corresponding eoln characters are: `cr("\r")`, `lf("\n")`, and `crlf("\r\n")`. The default is determined by the operating system : MacOSX(lf), Unix(lf), and Win32/DOS(crlf).

`read_eoln_type(Type)` - determines what [`read/2`](read.html) and `get_line/3` recognize as an `end-of-line`. The values for `Type` and the corresponding `ends-of-line` are: `cr` - carriage return("\r") `lf` - line feed("\n"), `crlf` - carriage return followed by a line feed("\r\n"), `universal` - indicates that any of the end-of-line types(`cr,lf,crlfl`) should be interpreted as an `end-of-line`. The default is `universal` since this allows the correct `end-of-line` interpretation for text files on all operating systems.

## EXAMPLES
Open a file named example.dat for write access, write a term to it and close it.
```
?- open('example.dat',write,S), writeq(S, example(term)),
?-_ put_char(S,'.'), nl(S), close(S).

S = stream_descriptor('',closed,file,'example.dat', [noinput|output], true,
2,0,0,0,0,true,0, wt_opts(78,40000,flat),[],true,text,eof_code,0,0)

yes.
```
Open a file named example.dat for read access with alias example.
```
?- open('example.dat',read,_,[alias(example)]).

yes.
```
Read a term from stream with alias example.
```
?- read(example,T).

T = example(term)

yes.
```
Read another term from with stream with alias example.
```
?- read(example,T).

T = end_of_file

yes.
```
Close the stream aliased example.
```
?- close(example).

yes.
```

## ERRORS

`Name`, `Mode`, or `Options` is a variable

-- -- -- -- &gt; instantiation_error.

`Name` does not refer to either a variable or a source/sink

-- -- -- -- &gt; domain_error(source_sink, Name) .

`Mode` is neither a variable nor an atom

-- -- -- -- &gt; type_error(atom, Mode) .

`Mode` is an atom, but not a valid I/O mode for the given source/sink

-- -- -- -- &gt; domain_error(io_mode, Mode) .

`Stream` is not a variable

-- -- -- -- &gt; type_error(variable, Stream) .

`Options` is neither a variable nor a list

-- -- -- -- &gt; type_error(list, Options) .

`Options` is a list with a variable element

-- -- -- -- &gt; instantiation_error.

`Options` is a list with element E which is not a valid stream option

-- -- -- -- &gt; domain_error(stream_option, E) .

`Name` specifies a valid source/sink, but can not be opened. If `Name` refers to a file, the file may not exist or the protection on the file or containing directory might be set to be incompatible with the open mode

-- -- -- -- &gt; permission_error(open, source_sink, Name) .

`Options` contains an element `alias(A)` and `A` is already associated with another stream

-- -- -- -- &gt; permission_error(open, source_sink, alias(A)) .

`Options` contains an element `reposition(true)` and it is not possible to reposition a stream corresponding to the source/sink `Name`.

-- -- -- -- &gt; permission_error(open, source_sink, reposition(true)) .


## NOTES

The structured term comprising a stream descriptor is visible to the programmer. The programmer should not directly use the stream descriptor to learn of properties or attributes associated with the stream or otherwise rely on the representation of stream descriptors. Use [`stream_property/2`](stream_property.html) to examine the properties associated with a stream.

The DEC-10 compatibility predicates [`see/1`](see.html) and [`tell/1`](tell.html) are defined in terms of `open/4`. When a stream is opened with either of these predicates it is assigned an alias which is the name of the source/sink. Thus the single argument to `see/1` and `tell/1` may be considered to be both the name of the stream and an alias for the stream.


## SEE ALSO

- [`close/1`](close.html)
- [`current_input/1`](current_input.html)
- [`current_output/1`](current_input.html)
- [`flush_input/1`](flush_input.html)
- [`flush_output/1`](flush_output.html)
- [`stream_property/2`](stream_property.html)
- [`curl/[1,2,3]`](curl.html)
- [`http/3`](curl.html)
- [`read_term/3`](read.html)
- [`write_term/3`](write.html)
- [`get_char/1`](get_char.html)
- [`put_char/1`](put_char.html)
- [`set_stream_position/2`](set_stream_position.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
