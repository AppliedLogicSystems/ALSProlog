---
title: 'see/1'
group: Input Output
module: sio
predicates:
- {sig: 'see/1', desc: 'sets the current input stream'}
- {sig: 'seeing/1', desc: 'returns the name of the current input stream'}
- {sig: 'seen/0', desc: 'closes the current input stream'}
---

## FORMS

```
see(File)

seeing(File)

seen
```

## DESCRIPTION

`see/1` sets the current input stream to the file named `File`. If `File` is already open for input, the existing file descriptor will be used. Otherwise, a new file descriptor will be allocated, and input operations will start at the beginning of the file.

`seeing/1` unifies `File` with the name of the current input stream. If no stream has been explicitly opened by `see/1`, then file will be unified with the atom user.

`seen/0` closes the current input stream and deallocates its file descriptor. The current input stream is then set to the special file
`user`.


`user` is the name of the default input stream which is normally connected to the keyboard. The special file user is always present, and `seen/0` can never close it. Although `seen/0` can never close the file `user`, `seen/0` will reset the user I/O descriptor as follows. If a `read/n` has been executed from `user`, and if an end of file character sequence is encountered (Control-D on Unix or the Mac, or Control-Z on Windows), then the `read/n` returns `end_of_file` and a subsequent `seen/0` on user will reset the I/O descriptor for `user` so that the EOF condition is no longer present.


## EXAMPLES

The following program will

- preserve the current input stream
- open a file
- read one term from it
- restore the previous input stream

```
firstTerm(File, Term) :-
    seeing(CurrentInput),
    see(File),
    read(Term),
    seen,
    see(CurrentInput).
```

## SEE ALSO

- [`see/1`](see.html)
- [`seeing/1`](see.html)
- [`seen/1`](see.html)
- [`open/[3,4]`](open.html)
- [`close/1`](close.html)
- [`close/2`](close.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
- {% include book.md id="bowen91"    sec="7.8" %}
- {% include book.md id="clocksin81" sec="5.4" %}
- {% include book.md id="bratko86"   sec="6.1" %}
