---
title: 'tell/1'
group: Input Output
module: sio
predicates:
- {sig: 'tell/1', desc: 'sets the standard output stream'}
- {sig: 'telling/1', desc: 'returns the name of the standard output stream'}
- {sig: 'told/0', desc: 'closes the standard output stream'}
---

## FORMS

```
tell(File)

telling(File)

told
```

## DESCRIPTION

`tell/1` sets the current output stream to the file named `File`. If `File` is already open for output, the existing file descriptor will be used. Otherwise, a new file descriptor will be allocated, and output operations will start at the beginning of the file, overwriting the previous contents.

`telling/1` unifies `File` with the name of the current output stream. If no stream has been explicitly opened by tell, then `File` will be unified with the atom user.

`told/0` closes the current output stream and deallocates its file descriptor. The current output stream is then set to `user`. `user` is the name of the default output stream which is normally connected to the console. `user` is always present, and `told/0` can never close it.


## EXAMPLES

The following program will preserve the current output stream, open a file and write one term to it, and then restore the previous output stream.

```
firstTerm(File, Term) :-
  telling(CurrentOutput),
  tell(File),
  write(Term),
  told,
  tell(CurrentOutput).
```

## SEE ALSO

- [`see/1`](see.html)
- [`seeing/1`](see.html)
- [`seen/0`](see.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
- {% include book.md id="bowen91"    sec="7.8" %}
- {% include book.md id="clocksin81" sec="5.4" %}
- {% include book.md id="bratko86"   sec="6.1" %}
