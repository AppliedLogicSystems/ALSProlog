---
title: 'tell/1'
predicates:
 - 'tell/1' : sets the standard output stream
 - 'telling/1' : returns the name of the standard output stream
 - 'told/0' : closes the standard output stream
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

- `see/1`
- `seeing/1`
- `seen/0`
- User Guide (Prolog I/O)
- [Bowen 91, 7.8]  
- [Clocksin 81, 5.4]  
- [Bratko 86, 6.1]
