---
title: 'gensym/2'
group: Terms
predicates:
- {sig: 'gensym/2', desc: 'generates families of unique symbols'}
---

## FORMS
```
gensym(Prefix, Symbol)
```
## DESCRIPTION

If `Prefix` is a symbol (either an interned or uninterned atom), then `Symbol` is a new UIA which has not previously existed in the system and which involves `Prefix` as a sub-symbol. The string also involves the system time that the current ALS Prolog image was started, together with the value of a counter for these generated symbols. Consequently, the symbols are almost guaranteed to be unique across invokations of the system, except for the possibility of the system clock wrapping around.

## EXAMPLES
```
?- gensym('<Prefix>', Symbol).

Symbol='?<Prefix>_1540251652_0' 

yes.

?- gensym(airplane, X).

X=?airplane_1540251652_1

yes.
```
