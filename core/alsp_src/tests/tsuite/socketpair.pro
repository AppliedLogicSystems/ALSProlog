
socketpair(Host, ReadAlias, WriteAlias) :-
    open(socket(inet_stream,Host), read, RS, [alias(ReadAlias)]),
    (is_server_socket(RS) -> accept_socket_connection(RS) ; true),
    open(socket(clone,RS), write, _, [alias(WriteAlias)]).
