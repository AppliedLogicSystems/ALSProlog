
:- op(400, xfy, '&').
:- op(400, xfy, '--').

:- [comp_d10, sio_d10].

prompt(_, P) :- write(P),flush_output.



