---
title: '$c_malloc/2'
package: C Interface
group: C Data
predicates:
- {sig: '$c_malloc/2', desc: 'Allocates a C data area using the system malloc call'}
- {sig: '$c_free/1', desc: 'Frees a C data area'}
- {sig: '$c_set/2', desc: 'Modifies the contents of a C data area or a UIA'}
- {sig: '$c_examine/2', desc: 'Examines the contents of a C data area or a UIA'}
---

## FORMS
```
'$c_malloc'(Size, Ptr)

'$c_free'(Ptr)

'$c_set'(Ptr_or_UIA, FormatList)

'$c_examine'(Ptr_or_UIA, FormatList)
```
## DESCRIPTION

The following predicates are low-level C-defined builtins in ALS Prolog, providing access to C data areas, as well as UIAs treated as such data areas.

**`'$c_malloc'/2`** allocates a C data area.  `'$c_malloc'(Size, Ptr)` is true if `Size` is a positive integer and `Ptr` (an integer) unifies with the address of the first byte of a data area allocated by the system call &quot;malloc&quot;. The call fails if malloc returns a null pointer. 

**`'$c_free'/1`** frees a C data area.
`'$c_free'(Ptr)` is true if `Ptr` is a number, and it invokes the system call &quot;free&quot; to free the data area pointed by `Ptr`. 

The next two predicates make use of the following encoding of C types:
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

**`'$c_set'/2`** is used to modify the contents of a C data area or a UIA. 
```
'$c_set'(Ptr_or_UIA, FormatList)`
```
is true if `Ptr_or_UIA` is bound to the address of a C data area or a `UIA`, and `FormatList` is a non-empty list of 3-ary or 4-ary terms of the form

`f(Offset, Type, Value {, Length })`    _(Length is used only with type code 11)_

The complete call modifies the contents of the data area corresponding to each such term as follows:
<br>In each term, `Offset` is the offset of the field from the start address of the data area. `Type` is the C type code (as given above) of the field.  `Value` is the data that the field should be set to.  `Value` should be a prolog entity whose type corresponds naturally to the C entity type coded by `Type`.  Numeric C types require an appropriate prolog number. If `Type` is 10 or 11, then `Value` must be an atom (including possibly a `UIA`), and a null terminated C string name of the atom is copied into the receiving data are without overflow checks. In the case of `Type`==11, at most `Length` bytes are copied into the receiving data area.

**`'$c_examine'/2`** is used to examine the contents of a C data area or a `UIA`. 
`'$c_examine'(Ptr_or_UIA, FormatList)` is true if `Ptr_or_UIA` is bound to the address of a C data area or a `UIA`, and `FormatList` is a nonempty list of 3-ary or 4-ary terms of the form
```
f(Offset, Type, Value{, Length })
```
whose arguments are interpreted as in `'$c_set/2` (above) except that now a data item of the specified type is extracted from the data area and unified with `Value`.


## EXAMPLES

```
?- Size = 1000, '$c_malloc'(Size, Ptr).

Size=1000 
Ptr=33558528 

yes.

?- '$c_malloc'(1000, Ptr), '$c_set'(Ptr,[f(0,12,14.5)]), '$c_examine'(Ptr,[g(0,12,X)]).

Ptr=41947136 
X=14.5 

yes.

?- UIA = 'Phantom', Value = w2g, '$c_set'(UIA, [f(3, 10, Value)]).

UIA='Phaw2g' 
Value=w2g 

yes.

?- UIA = 'Phantom', '$c_examine'(UIA, [f(2, 10, Value)]).

UIA='Phantom' 
Value=antom 

yes.

?- UIA = 'Phantom', '$c_examine'(UIA, [f(2, 11, Value, 3)]).

UIA='Phantom' 
Value=ant 

yes.
```

## NOTES
A slightly higher-level collection of C data access predicates is implemented over the predicates above in ~builtins/cutils.pro.  See [`c_alloc/[2,3]`](c_alloc.html) and [`c_create/3`](c_create.html).
