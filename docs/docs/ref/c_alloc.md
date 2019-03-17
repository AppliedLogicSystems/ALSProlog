---
title: 'c_alloc/[2,3]'
package: C Interface
group: C Data
predicates:
- {sig: 'c_const/2', desc: 'get the value of a C defined constant'}
- {sig: 'c_rconst/2', desc: 'get the value of a C defined runtime constant'}
- {sig: 'c_alloc/2', desc: 'allocate a UIA that can hold C data'}
- {sig: 'c_alloc_abs/2', desc: 'allocate a C-malloced data area'}
- {sig: 'c_allocn/3', desc: 'allocate a UIA to hold Num items'}
- {sig: 'c_allocn_abs/3', desc: 'allocated a C-malloced area to hold Num items'}
- {sig: 'c_free/1', desc: 'applied to UIA does nothing (freeing handled by gc)'}
- {sig: 'c_free_abs/1', desc: 'mfree the C area pointed by Ptr.'}
- {sig: 'c_set/3', desc: 'modify a UIA or C data area'}
- {sig: 'c_set/3', desc: 'multiply modify a UIA or C data area'}
- {sig: 'c_set_str/4', desc: 'insert C string in a UIA or C data area'}
- {sig: 'c_set_raw/4', desc: 'writes chars from a UIA into a UIA or C data area'}
- {sig: 'c_setn/4', desc: 'perform c_set/3 on a component of an array'}
- {sig: 'c_setn/4', desc: 'multiply perform c_set/3 on a component of an array'}
- {sig: 'c_examine/3', desc: 'examine a UIA or C data area'}
- {sig: 'c_examine/3', desc: 'multiply examine a UIA or C data area'}
- {sig: 'c_examine_str/4', desc: 'extract chars from a UIA or C data area'}
- {sig: 'c_examine_raw/4', desc: 'extract raw chars from a UIA or C data area'}
- {sig: 'c_examinen/4', desc: 'examine a component of an array'}
- {sig: 'c_examinen/4', desc: 'multiply examine a component of an array'}
---

## FORMS
```
c_const(Name,Val)

c_rconst(Name,Val)

c_alloc(Type,UIA)

c_alloc_abs(Type,Ptr)

c_allocn(Type,Num,UIA)

c_allocn_abs(Type,Num,Ptr)

c_free(UIA)

c_free_abs(Ptr)

c_set(Obj,Type1,Val)

c_set(Obj,Type2,[FieldName,Val,...])

c_set_str(Obj,Off,Len,SymOrUIA)

c_set_raw(Obj,Off,Len,UIA)

c_setn(Obj,Type,I,Val)

c_setn(Obj,SType,I,[FieldName,Val,...])

c_examine(Obj,Type,Val)

c_examine(Obj,SType,[FieldName,Val,...])

c_examine_str(Obj,Off,Len,Val)

c_examine_raw(Obj,Off,Len,Val)

c_examinen(Obj,Type,I,Val)

c_examinen(Obj,SType,I,[FieldName,Val,...])
```
## DESCRIPTION

These predicates make use of the same C type encoding as used on ['$c_malloc'/2](c_malloc.html):
```
      1  -- int
      2  -- unsigned int
      3  -- long
      4  -- unsigned long
      5  -- pointer
      6  -- char
      7  -- unsigned char
      8  -- short
      9  -- unsigned short
      10 -- string
      11 -- string of given length (length is 4th arg)
      12 -- float
      13 -- double
      14 -- far pointer  (DOS only)
      15 -- raw data of given length
```
**`c_const(Name,Val)`** unifies `Val` with the value of the C defined constant `Name`.

**`c_rconst(Name,Val)`** unifies `Val` with the value of a C defined runtime constant `Name`. This should not be used to access C globals.

**`c_alloc(Type,UIA)`** allocates a `UIA` that can hold data of type `Type`.

**`c_alloc_abs(Type,Ptr)`** mallocs a C data area that can hold data of type `Type` and unifies Ptr with the address of the first byte of that area.

**`c_allocn(Type,Num,UIA)`** allocates a `UIA` that can hold `Num` items of type `Type`.

**`c_allocn_abs(Type,Num,Ptr)`** mallocs a C data area that can hold data of type `Type` and unifies Ptr with the address of the first byte of that area.

**`c_free(UIA)`** is a no-op, doing nothing (allowing freeing taken care of by gc).

**`c_free_abs(Ptr)`** C mfrees the C data area pointed to by `Ptr`.

**`c_set(Entity,Type,Val)`** destructively modifies an Entity (a UIA or C data area), where `Val` is of type `Type`, by using `Val` to replace a segment of Entity starting at the beginning of Entity. 

**`c_set(Obj,Type2,[FieldName,Val,...])`** destructively modify a UIA or C data area `Obj` with `Val` of type `Type2`.  `Type2` must be a structure type, and `FieldName` is the name of a field to be modified with value `Val`. Subfields are identified by their (C-style) dot-separated pathname, except when the sub-structure is a type-reference, in which case `FieldName` is the name of the field, and `Val` is (recursively) a list of `FieldName - Value` pairs.

**`c_set_str(Obj,Off,Len,SymOrUIA)`** destructively modify a segment of data in the UIA or C data `Obj` at an offset `Off` by no more than `Len` bytes taken from `SymOrUIA`.

**`c_set_raw(Obj,Off,Len,UIA)`** writes `Len` characters from UIA into `Obj` starting at offset `Off`.

**`c_setn(Obj,Type,I,Val)`**
<br>**`c_setn(Obj,SType,I,[FieldName,Val,...])`** perform `c_set/3` on the `I-th` component of an array, where `I=0` refers to the first element and so on. In the first form, `Type` must be a base type. In the second form `SType` must be a structure type (substructures are handled in the same manner as in c_set/3).

**`c_examine(Obj,Type,Val)`**
<br>**`c_examine(Obj,SType,[FieldName,Val,...])`** examine a UIA or C ptr `Obj`, assuming its type to be `Type`. In the first form, `Type` is either an intergral type or `'str'`. In the second form, `SType` must be a structure type.

**`c_examine_str(Obj,Off,Len,Val)`** examine a segment of `Obj` starting at offset `Off` until first null character or `Len` characters have been read, whichever comes first and return the value as as a UIA in `Val`.

**`c_examine_raw(Obj,Off,Len,Val)`** reads `Len` characters starting at offset `Off` into `Obj` and returns the result as a UIA in `Val`.  Note that the resulting UIA can have null and other funny characters.

**`c_examinen(Obj,Type,I,Val)`**
<br>**`c_examinen(Obj,SType,I,[FieldName,Val,...])`** examine the `I-th` the component of an array of type `Type/SType`.

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
- [`c_create/3`](c_create.html)
