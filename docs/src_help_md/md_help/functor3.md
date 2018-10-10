---
title: 'functor/3'
predicates:
 - 'functor/3' : builds structures and retrieves information about them
---
`functor/3` `--` builds structures and retrieves information about them


## FORMS

functor(Structure, Functor, Arity)


## DESCRIPTION

The principal functor of term Structure has name Functor and arity Arity, where Functor is an atom. Either Structure must be instantiated to a term or an atom, or Functor and Arity must be instantiated to an atom and a non-negative integer respectively.

In the case where Structure is initially unbound, functor/3 will unify Structure with a structured term of Arity arguments, where the principal functor of the term is Functor. Each argument of the new structure will be a new uninstantiated variable.

When Structure is instantiated to a structured term, Functor will be unified with the principal functor of Structure and Arity will be unified with the arity. functor/3 treats atoms as structured terms with arity 0 The principal functor of a list is ' . ' with arity 2


## EXAMPLES

```
?- functor(Structure,fish,2).
Structure=fish(_123,_124)
yes.
```

```
?- functor(city('SantaMonica','CA','USA'),Functor,Arity).
Functor=city
Arity=3
yes.
```


## SEE ALSO

- `arg/3`  
`mangle/3`

- [Bowen 91, 7.6 ]
- [Clocksin 81, 6.5 ]
- [Bratko 86, 7.2 ]
- [Sterling 86, 9.2 ]. 
