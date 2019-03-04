---
---

# 5 Abstract Data Types: Structure Definition
{:.no_toc}

* TOC
{:toc}

One powerful modern programming idea is the use of abstract data types to hide the
inner details of the implementation of data types. The arguments in favor of this
technique are well-known (cf. [Ref: Liskov] ). Of course, it is possible to use the
abstract data type idea 'by hand' as a matter of discipline when developing programs. However, like many other things, programming life becomes easier if useful tools supporting the practice are available. In particular, good tools make it easy to modify abstract data type definitions while still maintaining efficient code.

The defStruct tool provides such support for a common construct: the use of Prolog
structures (i.e., compound terms) which must be accessed for values and may be
(destructively) updated. For example, the implementation of a window system often
passes around structures with many slots representing the various properties of particular windows. When programming in C, one would use a C struct for the entity. The analogue in Prolog is a flat compound term.

For speed of access to the slot values, one wants to use the arg/3 builtin. For destructively updating the slot values, one uses the companion mangle/3 builtin. The difficulty with using these builtins is that both require the slot number as an argument.
As is well-known, hard-coding such numbers leads to opaque code which is difficult to change. The defStruct approach allows one to assign symbolic names to the
slots, with the corresponding numbers being computed once and for all at compile
time. Instead of making calls on arg/3 and mangle/3, the programmer makes calls
on access predicates which are defined in terms of arg/3 and mangle/3. (These calls
can themselves be macro-processed to replace the access predicate calls by direct
calls on arg and mangle, thus making it possible to utilize good coding practice with
no loss in performance. See Section [Ref: Macros] for more information.)

Consider the following example which is a simplified version of a defStruct used in
an early ALS windowing package. The definition of the structure is declaratively
specified by the following in a file with extension .typ, say wintypes.typ :
```
:- defStruct(windows,
  [
    propertiesList =
       [windowName,        % name of the window
        windowNum,         % assigned by window sys
        borderColor/blue,  % for color displays
        borderType/sing,   % single or double lines
        uLR, uLC,          % coords(Row,Col) of upper Left corner
        lRR, lRC,          % coords(Row,Col) of lower Right corner
        fore/black,        % foreground/background
        back/white         % text attribs
       ],
    accessPred = accessWI,
    setPred = setWI,
    makePred = makeWindowStruct,
    structLabel = wi
  ]
).
```
We will discuss the details of this specification below. It can be placed anywhere in
a source file, and acts like a macro, generating the following code in its place:
```
export accessWI/3.
export setWI/3.

accessWI(windowName,_A,_B) :- arg(1,_A,_B).
setWI(windowName,_A,_B) :- mangle(1,_A,_B).
accessWI(windowNum,_A,_B) :- arg(2,_A,_B).
setWI(windowNum,_A,_B) :- mangle(2,_A,_B).
...
accessWI(back,_A,_B) :- arg(10,_A,_B).
setWI(back,_A,_B) :- mangle(10,_A,_B).

export makeWindowStruct/1.
makeWindowStruct(_A) :_A=..[wi,_B,_C,blue,sing,_D,_E,_F,_G,black,white].

export makeWindowStruct/2.
makeWindowStruct(_A,_B) :-
  struct_lookup_subst(
         [windowName,windowNum,borderColor,
          borderType,uLR,uLC,lRR,lRC,fore,back],
         [_C,_D,blue,sing,_E,_F,_G,_H, black,white],_B,_I),
  _A=..[wi|_I].

export xmakeWindowStruct/2.
xmakeWindowStruct(wi(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J),
                  [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J]).
```
Now let us examine the details.

## 5.1 Specifying Structure Definitions

defStructs directives are simply expressions of the form:

    :-defStruct(Name, EqnsList).

These are simply binary Prolog terms whose functor is defStruct. The first argument is an atom functioning as an identifying name for the type (it has no other
use at present). The second argument is a list of equality statements providing the
details of the definition. An equality statement is an expression of the form:

    Left = Right

For defStructs, the left component of the equality statements must be one of the following atoms:
* propertiesList
* accessPred
* setPred
* makePred
* structLabel
The right sides of the defStruct equality statements are Prolog terms whose structure depends on the left side entry. The right side corresponding to 'propertiesList'
is a list of atoms which are the symbolic names of the properties or slots of the structure being defined. For all of the rest of the equality statements, the right side is
a single atom. The roles of these right side atoms are described below:

### 5.1.1 accessPred

The name of the ternary (3-argument) predicate to be used for accessing the values
of the slots in the structure.

### 5.1.2 setPred

The name of the ternary (3-argument) predicate to be used for setting or changing
the values of the slots in the structure.

### 5.1.3 makePred

The name of the unary predicate used for obtaining a fresh structure of the defined
type.

### 5.1.4 structLabel

The name of the functor of the structure defined.

### 5.1.5 propertiesList

This is a list of slot specifications. A slot specification is one of the following:
* an atom, which is the name of the particular slot, or
* an expression of the form

    SlotName/Term,

where SlotName is an atom serving as the name of this slot, and Term is an
arbitrary Prolog term which is the default value of this particular slot, or
* an include expression which is a term of the form

    include(File, Type)

where File is a path to a file, and Type is the name of a defStruct which appears in that file; if File can be located, and if the defStruct Type appears in File, the elements of propertiesList for Type are interpolated at the point where the include expression occurred; include expressions may be
recursively nested. [Note: The typecomp compiler does not change its directory location when handling include expressions. Thus, if you utilize relative paths in recursive includes, these paths must always be valid from the directory in which the compiler was invoked.]

## 5.2 Using Structure Definitions

As can be seen from the generated code for the wintypes example at the beginning
of this section, the atoms on the right sides of the accessPred and setPred equality
statements become names for ternary predicates which are surrogates for arg/3 and
mangle/3, respectively. And the atom on the right side of the makePred equality
statement becomes the name of a unary predicate producing a new instance of the
structure when called with a variable as its argument. Formally:
```
accessPred=acpr 
    acpr(Slot_name,Struct,Value) succeeds precisely when Slot_name 
    is an atom occurring on the propertiesList in the defStruct, Struct is a structure
    generated by the makePred of the defStruct, and Value is the argument of Struct corresponding to
    the slot Slot_name.

setPred=stpr

    stpr(Slot_name,Struct,Value) succeeds precisely when
    Slot_name is an atom occurring on the propertiesList in the defStruct, Struct is a 
    structure generated by the makePred of the defStruct, and Value is any legal Prolog term; 
    as a side-effect, the argument of Struct corresponding to Slot_name is changed to become Value.

makePred=mkpr 

    mkpr(Struct) creates a new structured term whose functor is the right side 
    of the structLabel equality statement of the defStruct and whose airty is the length of the     
    propertiesList of the defStruct, and unifies Struct with that newly created term; as a
    matter of usage, Struct is normally an uninstantiated variable for this call.
```
Thus, the goal

    makeWindowStruct(ThisWinStruct)

will create a wi(...) structure with default values and bind it to ThisWinStruct.
Besides the generated unary 'make' predicates, two other construction predicates
are created:
```
makePred=mkpr 
    mkpr(Struct, ValsList) creates a new structured term whose functor is the right side of the
    structLabel equality statement of the defStruct and whose arty list the length of the 
    propertiesList of the defStruct, and unifies Struct with that newly created
    term; 
    ValsList should be a list of equations of the form SlotName = Value, where SlotName is one of 
    the slots specified on PropertiesList; the newly created structured term will have
    value Val at the postion corresponding to SlotName; 
    these “local defaults” will override any “global defaults” specified in the defStruct.

makePred=xmkpr 
    mkpr(Struct,SlotVars) creates a new structured term whose functor is the right side of the     
    structLabel equality statement of the defStruct and whose arty list the length of the 
    propertiesList of the defStruct, and unifies Struct with that newly created term; 
    no defaults are installed, and SlotVars is a list of the variables occurring in Struct. 
    This binary predicate xmkprmkpr(Struct,SlotVars) is equivalent to

        Struct =.. [ StructLabel | SlotVars].
```
