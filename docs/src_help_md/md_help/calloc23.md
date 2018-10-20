---
title: 'c_alloc/[2,3]'
predicates:
 - 'c_const/2' : get the value of a C defined constant
 - 'c_rconst/2' : get the value of a C defined runtime constant
 - 'c_alloc/2' : allocate a UIA that can hold C data
 - 'c_alloc_abs/2' : allocate a C-malloced data area
 - 'c_allocn/3' : allocate a UIA to hold Num items
 - 'c_allocn_abs/3' : allocated a C-malloced area to hold Num items
 - 'c_free/1' : applied to UIA does nothing (freeing handled by gc)
 - 'c_free_abs/1' : mfree the C area pointed by Ptr.
 - 'c_set/3' : modify a UIA or C data area
 - 'c_set/3' : multiply modify a UIA or C data area
 - 'c_set_str/4' : insert C string in a UIA or C data area
 - 'c_set_raw/4' : writes chars from a UIA into a UIA or C data area
 - 'c_setn/4' : perform c_set/3 on a component of an array
 - 'c_setn/4' : multiply perform c_set/3 on a component of an array
 - 'c_examine/3' : examine a UIA or C data area
 - 'c_examine/3' : multiply examine a UIA or C data area
 - 'c_examine_str/4' : extract chars from a UIA or C data area
 - 'c_examine_raw/4' : extract raw chars from a UIA or C data area
 - 'c_examinen/4' : examine a component of an array
 - 'c_examinen/4' : multiply examine a component of an array
---
`c_const/2` `--` get the value of a C defined constant

`c_rconst/2` `--` get the value of a C defined runtime constant

`c_alloc/2` `--` allocate a UIA that can hold C data

`c_alloc_abs/2` `--` allocate a C-malloced data area

`c_allocn/3` `--` allocate a UIA to hold Num items

`c_allocn_abs/3` `--` allocated a C-malloced area to hold Num items

`c_free/1` `--` applied to UIA does nothing (freeing handled by gc)

`c_free_abs/1` `--` mfree the C area pointed by Ptr.

`c_set/3` `--` modify a UIA or C data area

`c_set/3` `--` multiply modify a UIA or C data area

`c_set_str/4` `--` insert C string in a UIA or C data area

`c_set_raw/4` `--` writes chars from a UIA into a UIA or C data area

`c_setn/4` `--` perform c_set/3 on a component of an array

`c_setn/4` `--` multiply perform c_set/3 on a component of an array

`c_examine/3` `--` examine a UIA or C data area

`c_examine/3` `--` multiply examine a UIA or C data area

`c_examine_str/4` `--` extract chars from a UIA or C data area

`c_examine_raw/4` `--` extract raw chars from a UIA or C data area

`c_examinen/4` `--` examine a component of an array

`c_examinen/4` `--` multiply examine a component of an array


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

These predicates make use of the same C type encoding as used on ['$c_malloc'/2](cmalloc2.html):
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


## EXAMPLES

```
1st example
```

```
2nd example
```

## ERRORS

Errors text...

## NOTES

Implemented in ~builtins/cutils.pro over the predicates in ['$c_malloc'/2](cmalloc2.html).

## SEE ALSO

- ['$c_malloc'/2](cmalloc2.html)
- [c_create/3](ccreate3.html)
