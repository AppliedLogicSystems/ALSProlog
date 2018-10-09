---
title: '$c_malloc/2'
predicates:
 - '$c_malloc/2' : Allocates a C data area using the system malloc call
 - '$c_free/1' : Frees a C data area
 - '$c_set/2' : Modifies the contents of a C data area or a UIA
 - '$c_examine/2' : Examines the contents of a C data area or a UIA
---
`$c_malloc/2` `--` Allocates a C data area using the system malloc call

`$c_free/1` `--` Frees a C data area

`$c_set/2` `--` Modifies the contents of a C data area or a UIA

`$c_examine/2` `--` Examines the contents of a C data area or a UIA


## FORMS

' $c_malloc '(+ Size, -Ptr)

' $c_free '(+ Ptr)

' $c_set '(+ Ptr_or_UIA, + FormatList)

' $c_examine '(+ Ptr_or_UIA, + FormatList)


## DESCRIPTION

The following predicates are C defined builtins in ALS Prolog. ' $c_malloc ' /2 allocates a C data area, and ' $c_free ' /1 frees it, ' $c_set ' /2 is used to modify the contents of a C data area or a UIA, and ' $c_examine ' /2 is used to examine the contents of a C data area or a UIA. ' $c_malloc '(Size, Ptr) is true if Size is a positive integer and Ptr(an integer) unifies with the address of the first byte of a data area allocated by the system call &quot; malloc &quot; . The call fails if malloc returns a null pointer. ' $c_free '(Ptr) is true if Ptr is a number, and it invokes the system call &quot; free &quot; to free the data area pointed by Ptr. ' $c_set '(Ptr_or_UIA, FormatList) is true if Ptr_or_UIA is bound to the address of a C data area or a UIA, and FormatList is a non-empty list of 3-ary or 4-ary terms of the form


f(+ Offset, + Type, + Value{, + Length })


and the call modifies the contents of the data area as explained below. In each term, Offset is the offset of the field from the start address of the data area. Type is the &quot; C type &quot; of the field, which is one of the following symbols : int, long, short, ptr, char, str, float, double. They have the obvious correspondence with C data types. Value is the data that the field should be set to. If Type is one of int, long, short, ptr, char, float or double, then Value must be a number, otherwise Type is str and Value must be an atom and a null terminated string name of the atom is copied into the receiving data are without overflow checks. Length(optional) must be a number and has meaning only when Type is str, and at most Length bytes are copied into the receiving data area. ' $c_examine '(Ptr_or_UIA, FormatList) is true if Ptr_or_UIA is bound to the address of a C data area or a UIA, and FormatList is a nonempty list of 3-ary or 4-ary terms of the form


f(+ Offset, + Type, -Value{, + Length })


whose arguments are interpreted as in ' $c_set ' /2(above) except that now a data item of the specified type is extracted from the data area and unified with Value.


