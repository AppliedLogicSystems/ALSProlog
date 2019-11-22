---
title: 'make_gv/1'
predicates:
- {sig: 'make_gv/1', desc: 'create named global variable and access methods'}
- {sig: 'make_det_gv/1', desc: 'create named global variable and access methods which preserve instantiations of structures'}
- {sig: 'free_gv/1', desc: 'release store associated with a named global variable'}
---

## FORMS

```
make_gv(Name)

make_det_gv(Name)

free_gv(Name)
```

## DESCRIPTION

`make_gv/1` allocates an internal global variable and creates two access predicates called `setNAME/1` and `getNAME/1` where `NAME` is the atom `Name`. These access predicates are installed in the module from which `make_gv/1` is called.

The `setNAME/1` predicate is used to set the allocated global variable to the term given to `setNAME` as its (only) argument. This operation is safe in that the contents of the global variable will survive backtracking without any dangling references. Care should be taken when using these global variables with backtracking as it is easy to create a ground structure in which &quot;holes&quot; will appear upon backtracking. These holes are uninstantiated variables where there used to be a term. They are caused by some bit of non-determinism when creating the term. If the non-determinism is removed via cut prior to a global variable operation, these &quot;holes&quot; will often not show up upon backtracking. If the non-determinism is removed after the global variable operation takes place, these holes will very likely show up on backtracking. The reason that this is so is because the global variable mechanism will (as a consequence of making the structure safe to backtrack over) eliminate the ability of cut to discriminate among those trail entries which may be safely cut and those which are needed in the event of failure.

In situations where this is a problem, a call to [`copy_term/2`](copy_term.html) may be used to create a copy of the term prior to setting the global variable. The instantiation of the term that exists at the time of the copy will be the instantiation of the term which survives backtracking over the copy operation.

Also of interest is the time complexity of the set operation. So long as the argument to `setNAME/1` is a non-pointer type that is a suitably small integer or is of certain types of atoms (the non-UIA variety), the set operation is a constant time operation. Otherwise it requires time linearly proportional to the current depth of the choicepoint stack.

The `getNAME/1` predicate created by `make_gv/1` is used to get the contents of one of these global variables. The contents of the global variable is unified with the single parameter passed to `getNAME/1`.

`make_det_gv/1` creates access methods just like `make_gv/1` but the `setNAME/1` method avoids the problems referred to above concerning certain instantiations in structure becoming undone. It does this by making a copy of the term prior to setting the global variable. Making a copy of the term has the disadvantage of the increased space and time requirements associated with making copies.

`free_gv/1` removes access methods created by `make_gv/1` and frees up the global variable.

## EXAMPLES

Here is a demonstration program to be consulted:
```
:- make_gv('_demo').     %% Create get_demo/1 and set_demo/1.

print_demo(N) :- get_demo(X), printf('demo%d: %t\n', [N,X]).

demo1 :- demo1(_).
demo1 :- print_demo(1) .

demo1(_) 
    :- 
    X = f(Y), (Y = i ; Y = j), set_demo(X), print_demo(1), fail.

demo2 :- demo2(_) .
demo2 :- print_demo(2) .

demo2(_) 
    :- 
    X = f(Y), (Y = i ; Y = j), !, set_demo(X), print_demo(2), fail.

demo3 :- demo3(_) .
demo3 :- print_demo(3) .

demo3(_) 
    :- 
    X = f(Y), (Y = i ; Y = j), set_demo(X), !, print_demo(3), fail.

demo4 :- demo4(_) .
demo4 :- print_demo(4) .

demo4(Y) 
    :- 
    X = f(Y), (Y = i ; Y = j), !, set_demo(X), print_demo(4), fail.

demo5 :- _ = f(Y), set_demo([a ]), demo5(Y) .
demo5 :- print_demo(5) .

demo5(Y) 
    :- 
    X = f(Y), (Y = i ; Y = j), !, set_demo(X), print_demo(5), fail.

demo6 :- set_demo([a ]), _ = f(Y), demo6(Y) .
demo6 :- print_demo(6) .

demo6(Y) 
    :- 
    X = f(Y), (Y = i ; Y = j), !, set_demo(X), print_demo(6), fail.

demo7 :- demo7(_) .
demo7 :- print_demo(7) .

demo7(_) 
    :- 
    X = f(Y), (Y = i ; Y = j), copy_term(X, Z),
    set_demo(Z), !, print_demo(7), fail.

demo 
    :- 
    demo1, nl, demo2, nl, demo3, nl, demo4, nl, demo5, nl, demo6, nl, demo7.
```

The above program demonstrates the subtleties of combining global variables with backtracking. Here is a sample run of this program :

```
?- demo.
demo1:f(i)
demo1:f(j)
demo1:f(_A)
demo2:f(i)
demo2:f(i)
demo3:f(i)
demo3:f(_A)
demo4:f(i)
demo4:f(i)
demo5:f(i)
demo5:f(_A)
demo6:f(i)
demo6:f(i)
demo7:f(i)
demo7:f(i)
```

In each of these seven different tests, some non-determinism is introduced through the use of [`;/2`](semicolon.html).
demo1 makes no attempt to eliminate this non-determinism. Yet the results might be somewhat surprising.  `set_demo/1` is called twice; once with X instantiated to f(i), the second time with X instantiated to f(j).  Yet when we fail out of `demo1/1`, `print_demo/1` reports the &quot;demo&quot; variable to have an uninstantiated portion.

demo2 eliminates the non-determinism in a straightforward fashion through the use of a cut.  Here the f(i) is made to &quot;stick&quot;.

demo3 is a slight variation on demo2.  It shows that eliminating determinism after setting the global variable is too late to make the instantiations &quot;stick&quot;.

demo4 is similar to demo3, but shows that it is alright for Y to be &quot;older&quot; than the structure containing it.

demo5 shows that an intervening global variable operation may screw things up by making Y live in a portion of the heap which must be trailed when Y is bound.  The cut prior to setting the global variable is not permitted to remove the trail entry which eventually causes Y to lose its instantiation.

demo6 shows that creating the variable after the global variable operation has the same effect as demo4.

demo7 demonstrates a technique that may be used to always make instantiations &quot;stick&quot;. It creates a new copy of the term and calls `set_demo/1` with this new copy.

If the call to `make_gv/1` at the top of the file were replaced with a call to `make_det_gv/1`, then all of the instantiations would &quot;stick&quot; as `make_det_gv/1` automatically makes a copy of the term thus doing implicitly what demo7 does explicitly.

## BUGS

`free_gv/1` does not work for access methods created by `make_det_gv/1`.


## SEE ALSO

- [`make_hash_table/1`](make_hash_table.html)
- [`copy_term/2`](copy_term.html)
- [`mangle/3`](mangle.html)
