---
title: 'recorda/3'
group: Terms
predicates:
- {sig: 'recorda/3', desc: 'records item in internal term database'}
- {sig: 'recordz/3', desc: 'records item in internal term database'}
- {sig: 'recorded/3', desc: 'retrieves item from internal term database'}
---

## FORMS
```
recorda(Key, Term, Ref)

recordz(Key, Term, Ref)

recorded(Key, Term, Ref)
```
## DESCRIPTION

`Key` may be an atom or a compound term, but in the latter case, only its functor is significant. Both predicates will fail in all other cases of `Key`. `recorda(Key, Term, Ref)` enters `Term` into the internal term database at the first item associated with `Key`, and returns a database reference `Ref`. `recordz(Key, Term, Ref)` enters `Term` as the last item associated with `Key`.

`recorded(Key, Term, Ref)` searches the internal term database for an item `Term` associated with `Key` such that the associated database reference unifies with `Ref`.

## EXAMPLES
```
?- recordz(sing,slowly,_),
   recorda(sing,sweetly,_),
   recorda(sing(along),loudly,_).


yes.

?- recorded(sing,Term,_).

Term=loudly ;

Term=sweetly ;

Term=slowly ;

no.
```

## NOTES

Provided for compatibility. These predicates are defined by:
```
recorda(Key, Term, Ref) 
    :-
    rec_getkey(Key, KeyFuncotr),
    asserta(recorded(KeyFuncotr, Term), Ref).

recordz(Key, Term, Ref) 
    :-
    rec_getkey(Key, KeyFunctor),
    assertz(recorded(KeyFunctor, Term), Ref).

recorded(Key, Term, Ref) 
    :-
    rec_getkey(Key, KeyFunctor),
    clause(recorded(KeyFunctor, Term), true, Ref).

rec_getkey(Key, Key) 
    :- 
    atomic(Key), !.

rec_getkey(S, Key) 
    :- 
    functor(S, Key, _) .

```
