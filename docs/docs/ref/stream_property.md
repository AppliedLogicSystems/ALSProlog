---
title: 'stream_property/2'
group: Input Output
module: sio
iso: streamproperty
predicates:
- {sig: 'stream_property/2', desc: 'retrieve streams and their properties'}
---

## FORMS

```
stream_property(Stream, Property)
```

## DESCRIPTION

`stream_property/2` is used to retrieve information on a particular stream. It may also be used to find those streams satisfying a particular property.

`Stream` may be either an input or output argument. If used as an input argument it should be bound to either a stream descriptor as returned by [`open/4`](open.html) or an alias established in a call to `open/4`. If used as an output argument, `Stream` will only be bound to stream descriptors. This predicate may be used to retrieve those streams whose handles were "lost" for some reason.

`Property` is a term which may take any of the following forms:

`file_name(F)` -- When the stream is connected to a source/sink which is a file, `F` will be the name of that file.

`stream_name(N)` -- `N` is unified with the name of the source/sink regardless of whether the stream is connected to a file or not.

`mode(M)` -- `M` is unified with the I/O mode which was specified at the time the stream was opened.

`input` -- The stream is connected to a source.

`output` -- The stream is connected to a sink. It is possible for a stream to have both input and output properties.

`alias(A)` -- If the stream has an alias, `A` will be unified with that alias.

`position(P)` -- If the stream is repositionable, `P` will be unified with the current position in the stream.

`end_of_stream(E)` -- If the stream position is located at the end of the stream, then `E` is unified with 'at' . If the stream position is past the end of stream, then `E` is unified with 'past'. Otherwise, `E` is unified with 'no'. In the current implementation of ALS Prolog, querying about the `end_of_stream` property may cause an I/O operation to result which may block.

`eof_action(A)` -- If the stream option `eof_action(Action)` was specified in the options list when the stream was opened, then `A` will be unified with this action. Otherwise, `A` will be unified with the default action appropriate for the stream.

`snr_action(A)` -- If the stream option `snr_action(Action)` was specified in the options list when the stream was opened, then `A` will be unified with this action. Otherwise, `A` will be unified with the default action appropriate for the stream.

`reposition(R)` -- If positioning is possible on this stream then `R` is unified with true; if not `R` is unified with false.

`type(T)` -- `T` will be unified with either text or binary, indicating the type of stream.

`maxdepth(D)` -- `D` will be unified with the default depth with which terms are written to.

`depth_computation(DC)` -- `DC` will be unified with the atom indicating the method of depth computation when writing out terms.

`line_length(L)` -- `L` will be unified to the default line length parameter which is used for determining where line breaks should be placed when writing terms.


## EXAMPLES


```
%% Open 'foo' for write, but "lose" the stream descriptor.
?- open(foo,write,_).

%% Use stream_property to retrieve the stream descriptor and close it.
?- stream_property(S,file_name(foo)), close(S).
S = stream_descriptor('',closed,file,foo,[noinput|output],true,
1,0,0,0,0,true,0,wt_opts(78,40000,flat),[],true,text,
eof_code,0,0)

%% Open 'foo' for read with an alias.
?- open(foo,read,_,[alias(foo_alias)]).

%% Call stream_property to find out where end-of-stream is. Note that foo was created as the empty file above.
?- stream_property(foo_alias,end_of_stream(Where)).
Where = at

%% Call stream_property again to find out about the end-of-stream.
?- stream_property(foo_alias,end_of_stream(Where)).
Where = past

%% Call stream_property to find out the name of the file associated with the alias.
?- stream_property(foo_alias,file_name(Name)).
Name = foo

%% Get all of the names attached to streams.
?- setof(N,S^stream_property(S,stream_name(N)),L).
N = N
S = S
L = ['$stderr','$stdin','$stdout',foo]
```

## SEE ALSO

- [`open/4`](open.html)
- [`close/1`](close.html)
- [`set_stream_position/2`](set_stream_position.html)
- [`at_end_of_stream/1`](at_end_of_stream.html)
- [`set_line_length/2`](set_line_length.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)

