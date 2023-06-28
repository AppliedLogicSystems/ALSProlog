---
title: 'sprintf/3'
group: Input Output
predicates:
- {sig: 'sprintf/3', desc: 'formatted write to atoms and strings'}
- {sig: 'bufwrite/2', desc: 'formatted write to strings'}
- {sig: 'bufwriteq/2', desc: 'formatted write to strings with quoting'}
---

## FORMS

```
sprintf(Target, Format, Args)

bufwrite(String, Term)

bufwriteq(String, Term)
```

## DESCRIPTION

`sprintf/3` is very similar to [`printf/3`](printf.html), except that instead of writing the formatted out put to a stream, `sprintf/3` writes the output to either a Prolog atom or string. The `Format` and `Args` arguments are identical to the corresponding arguments for `printf/3`. The `Target` argument can be an uninstantiated variable, or can be of one of the forms `atom(A)`, `string(S)`, or `list(S)`. In case `Target` is an uninstantiated variable, on success, `Target` is a Prolog double-quoted string containing the formatted output. Similarly, if `Target` is `string(S)` or `list(S)`, on success, `S` is a Prolog double-quoted string containing the formatted output. And if `Target` is `atom(A)`, on success `A` is an atom containing the formatted output.

`bufwrite/2` creates a printed representation of the `Term` using the operator declaration as [`write/1`](write.html) would. However, instead of writing the result to the current output stream, `bufwrite/2` converts the printed representation into a list of ASCII codes. `bufwriteq/2` behaves similarly to `bufwrite/2`, but quotes items exactly the way [`writeq/2`](write.html) would.


## EXAMPLES

```
?- sprintf(X, 'Contents: %t, Amount: %t', [pocket(keys),1]).
X = "Contents: pocket(keys), Amount: 1"

?- sprintf(string(X), 'Contents: %t, Amount: %t', [pocket(keys),1]).
X = "Contents: pocket(keys), Amount: 1"

?- sprintf(atom(X), 'Contents: %t, Amount: %t',[pocket(keys),1]).
X = 'Contents: pocket(keys), Amount: 1'
```
```
?- bufwrite("jack+f(tom)", +(jack, f(tom))).
yes.
```
```
?- bufwrite(S, 'Enterprise').
S = [69,110,116,101,114,112,114,105,115,101]
yes.
```


## SEE ALSO

- [`printf/[1-4]`](printf.html)
- [`write/1`](write.html)
- [`op/3`](op.html)
- [`writeq/1`](write.html)
