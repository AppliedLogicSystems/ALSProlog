---
title: '=..(univ)'
group: Terms
iso: univ
predicates:
- {sig: '=../2', desc: 'composes/decomposes structures from/to components'}
---

## FORMS

`Term =.. List`

`Term =..[Functor | Arguments]`

## DESCRIPTION

`Term =.. List` succeeds if and only if `Term` is a compound term with principal functor `Functor`, with arity N, and `Arguments` is a list of terms of length N, consisting the of the arguments of `Term` in left-to-right order.


## EXAMPLES

```
?- likes(john, ice_cream) =.. Parts.

Parts=[likes,john,ice_cream] 

yes.
?- What =.. [likes,john,ice_cream].

What=likes(john,ice_cream) 

yes.
```

## ERRORS

Term is a variable and List is a partial list  
	-- -- -- -- &gt; instantiation_error  

List is neither a partial list nor a list  
	-- -- -- -- &gt;type_error(list, List)  

Term is a variable and List is a list whose head is a variable  
	-- -- -- -- &gt; instantiation_error  

List is a list whose head H is neither an atom nor a variable and whose tail is not the empty list  
	-- -- -- -- &gt; type_error(atom, H)  

List is a list whose head H is a compound term and whose tail is the empty list  
	-- -- -- -- &gt; type_error(atomic, H)  

Term is a variable and List is the empty list  
	-- -- -- -- &gt; domain_error(non_empty_list, [])  

Term is a variable and the tail of List has a length > [`max_arity flag`](current_prolog_flag.html) (ISO section 7.11.2.3)  
	-- -- -- -- &gt; representation_error(max_arity)  


## NOTES

Notes text...

## SEE ALSO

- [`functor/3`](functor.html)
- [`arg/3`](arg.md)
- [Bowen 91, 7.6.1 ]
- [Sterling 86, 9.2 ]
- [Bratko 86, 7.2 ]
- [Clocksin 81, 6.5 ]
