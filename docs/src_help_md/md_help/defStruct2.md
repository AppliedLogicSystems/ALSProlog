---
title: 'defStruct/2'
package: ALSDev
group: Prolog Objects
predicates:
- {sig: 'defStruct/2', desc: 'specify an abstract data type'}
---

## FORMS
```
:- defStruct(TypeID, Equations) .
```
## DESCRIPTION

Used as a directive to specify an abstract datatype.  `TypeID` is an atom functioning identifying the type.  `Equations` is a list of _[equality statements]_ of the form :
```
      Left = Right
```
where the left component of the equality statements must be one of :

- `propertiesList`

- `accessPred`

- `setPred`

- `makePred`

- `structLabel`

The right sides of equality statements are Prolog terms whose structure depends on the left side entry, as follows.

**`propertiesList`**
<br>&nbsp;&nbsp;&nbsp;&nbsp;A list of atoms or expressions which are the symbolic names, or names plus values, of the properties or slots of the structure being defined. Details a presented below.

For all of the rest of the equality statements, the right side is a single atom.

**`accessPred`**
<br>&nbsp;&nbsp;&nbsp;&nbsp;The name of the ternary(3-argument) predicate to be used for accessing the values of the slots in the structure. Calls on a generated accessPred take the form

&lt;accessPred&gt;(&lt;slotname&gt;, &lt;Structure&gt;, &lt;Value&gt;)

**`setPred`**
<br>&nbsp;&nbsp;&nbsp;&nbsp;The name of the ternary(3-argument) predicate to be used for setting or changing the values of the slots in the structure. Calls on a generated accessPred take the form

&lt;setPred&gt;(&lt;slotname&gt;, &lt;Structure&gt;, &lt;Value&gt;)

**`makePred`**
<br>&nbsp;&nbsp;&nbsp;&nbsp;The name of the unary predicate used for obtaining a fresh structure of the defined type.

**`structLabel`**
<br>&nbsp;&nbsp;&nbsp;&nbsp;The name of the functor of the structure defined.

In more detail, a **`propertiesList`** is a list of **_[slot  specifications]_** as follows :

- an atom, which is the name of the particular slot, or

- an expression of the form
```
     SlotName/Term,
```
where `SlotName` is an atom serving as the name of this slot, and `Term` is an arbitrary Prolog term which is the default value of this particular slot, or

- an
**_[include]_**
expression which is a term of the form
```
     include(File, Type)
```
where `File` is a path to a file, and `Type` is the name of a `defStruct` which appears in that file; if `File` can be located, and if the `defStruct` `Type` appears in `File`, the elements of propertiesList for `Type` are interpolated at the point where the include expression occurred; include expressions may be recursively nested. [Note : Relative paths in recursive includes must be valid from the directory in which the `defStruct` directive was invoked. ]

## EXAMPLES
```
:- defStruct(windows,
     [ propertiesList =
        [ windowName, % name of the window
          windowNum,  % assigned by window sys
          borderColor/blue, % for color displays
          borderType/sing,  % single or double lines
          uLR, uLC,   % coords(Row, Col) of upper Left corner
          lRR, lRC,   % coords(Row, Col) of lower Right corner
          fore/black, % foreground/background
          back/white  % text attribs
        ],
        accessPred = accessWI,
        setPred = setWI,
        makePred = makeWindowStruct,
        structLabel = wi
     ] ).
```
Then :
```
     ..., makeWindowStruct(WIN),
     ..., setWI(windowName, WIN, foo),
     ..., accessWI(borderColor, WIN, BColor), ...
```


