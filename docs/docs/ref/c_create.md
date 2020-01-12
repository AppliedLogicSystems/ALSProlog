---
title: 'c_create/3'
package: C Interface
group: C Data
predicates:
- {sig: 'c_create/3', desc: 'equivalent to c_alloc/2 plus c_set/3'}
- {sig: 'c_create_abs/3', desc: 'equivalent to c_alloc_abs/2 plus c_set/3'}
- {sig: 'c_createn/4', desc: 'allocate and initialize an array of data values in a UIA'}
- {sig: 'c_createn_abs/4', desc: 'allocate and initialize an array of data values in a C area'}
- {sig: 'c_create_arglist/[2,3]', desc: 'create an initialized ARGLIST structure for X Toolkit'}
- {sig: 'c_free_arglist/1', desc: 'perform c_free_abs on each item of PtrList'}
- {sig: 'c_call/3', desc: 'calls a C function'}
---

## FORMS
```
c_create(Type, Val, UIA)

c_create_abs(Type, Val, Ptr)

c_createn(Type, Num,[ Val,...], UIA)

c_createn_abs(Type, Num,[ Val,...], Ptr)

c_create_arglist([Name, Val,...], Ptr)

c_free_arglist(PtrList)

c_call(CFuncPtr, Arglist, RetVal)
```
## DESCRIPTION

**`c_create(Type, Val, UIA)`**
<br>**`c_create(SType,[ FieldName, Val,...], UIA)`** equivalent to [`c_alloc/2` plus `c_set/3`](c_alloc.html). `Type` can be an integral type or `'str'`. `SType` must be a structure type.

**`c_create_abs(Type, Val, Ptr)`**
<br>**`c_create_abs(SType,[ FieldName, Val,...], Ptr)`** equivalent to [`c_alloc_abs/2` plus `c_set/3`](c_alloc.html). `Type` can be an integral type or `'str'`. `SType` must be a structure type.

**`c_createn(Type, Num,[ Val,...], UIA)`**
<br>**`c_createn( SType, Num,[[ FieldName, Val,...], ...],  UIA)`** allocate and initialize an array of data values in a UIA.  equivalent to [`c_allocn/3` plus several `c_setn/4`](c_alloc.html).

**`c_createn_abs(Type, Num,[ Val,...], Ptr)`**
<br>**`c_createn_abs( SType, Num,[[ FieldName, Val,...], ...],  Ptr)`** same as `c_createn/4`, except that the data is created in a C malloced area.

**`c_create_arglist([Name, Val,...], Ptr)`**
<br>**`c_create_arglist([Name, Val,...], Ptr, PtrList)`** create an initialized ARGLIST structure for X Toolkit and return a pointer to the arg structure in `Ptr` and a list of pointers that can be freed using `c_free_arglist/1`.

**`c_free_arglist(PtrList)`** performs `c_free_abs` on each item of `PtrList`.

**`c_call(CFuncPtr, Arglist, RetVal)`** calls the C function whose address is `CFuncPtr` with arguments from `Arglist` that are assumed to be longs and binds `RetVal` with the return value of function.


{% comment %}
## EXAMPLES

```
1st example
```

```
2nd example
```

## ERRORS

Errors text...
{% endcomment %}

## NOTES

Implemented in ~builtins/cutils.pro over the predicates in ['$c_malloc'/2](c_malloc.html).

## SEE ALSO

- [`'$c_malloc'/2`](c_malloc.html)
- [`c_alloc/[2,3]`](c_alloc.html)

