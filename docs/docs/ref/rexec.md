---
title: 'rexec/2'
module: sio
predicates:
- {sig: 'rexec/2', desc: 'Execute an operating system command remotely, or possibly locally.'}
---

## FORMS
```
rexec(Command, Options)
```
## DESCRIPTION

`rexec/2` is an interface to the rexec system call which may be used to run commands on remote machines. When remote execution is not desired, fork and exec (Unix system calls) are used to run the command on the local machine. `Command` should be an atom representing the command to run. `Options` is a list containing zero or more of the following forms :

`host(HostName)` -- execute the command on the machine named by `HostName`.

`username(User)` -- run the command as user `User`.

`password(Password)` -- provides the password for authentication purposes. If no password is supplied, you will be prompted for one (by the rexec daemon) .

`rstream(Stream, OpenOpts)` -- designates the input stream from which to read the output of the command. This stream will be connected to the standard output of the command. `Stream` will be bound to a stream descriptor. `OpenOpts` is a list containing options suitable for a call to [`open/4`](open.html).

`wstream(Stream, OpenOpts)` -- designates the output stream to write to. This output stream will be connected to standard input of the command.

`estream(Stream, OpenOpts)` -- designates the input stream for use in obtaining error messages from the command. This stream will be connected to standard error for the command.

If any of host, username, or password are specified, `rexec/2` will attempt to contact the rexec daemon to remotely run the command on the specified machine. The remote execution daemon, rexecd, requires authentication. This means that either a username and password must be supplied in the program(with the `username/1` and `password/1` forms), interactively, or via the .netrc file. Not all remote execution daemons support authentication via the .netrc file. See your local system documentation for information about the .netrc file.

If none of host, username, or password are specified, then `rexec/2` will use the traditional fork and exec mechanism to run the command on the local machine. If remote execution is still desired, but the rexec daemon's authentication mechanisms deemed too odious, then rsh (running on the local machine) may be used to run a command on a remote machine.

## EXAMPLES

The following procedure will call the unix word count program, wc, to determine the length of an atom.

```
slow_atom_length(A, Len) 
    :-
    rexec(' wc -c ', [rstream(RS, []), wstream(WS, [])]),
    write(WS, A),
    close(WS),
    read_term(RS, Len, [attach_fullstop(true)]),
    close(RS).

?- slow_atom_length('The rain in Spain',Len).
Len=17
yes.
```

The version of `slow_atom_length/2` above assumes one is running on a Unix machine and calls wc running on the same machine. The version below, `slow_atom_length/3`, will work on systems that support the `rexec`/`rexecd` client-server remote execution protocol (now deprecated):
```
rstrlen(Host, A, Len) 
    :-
    rexec(' wc -c ', [host(Host), rstream(RS, []), wstream(WS, [])]),
    write(WS, A),
    close(WS),
    read_term(RS, Len, [attach_fullstop(true) ]),
    close(RS) .
```
Here is an interaction running over the internet :
```
?- rstrlen('bongo.cs.anywhere.edu','abcdef',X).
Name(bongo.cs.anywhere.edu:ken):mylogin
Password(bongo.cs.anywhere.edu:ken):<mypasswd>
X=6
```

## NOTES

This function is not very consistent with regards to error handling. Some errors will be thrown, while others will print a diagnostic. Other errors will cause failure. 

## SEE ALSO

- [`open/[3,4]`](open.html)
