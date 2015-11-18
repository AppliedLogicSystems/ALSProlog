One powerful modern programming idea is the use of abstract data types to hide the
inner details of the implementation of data types. The arguments in favor of this
technique are well-known (cf. [Ref: Liskov] ). Of course, it is possible to use the
abstract data type idea ’by hand’ as a matter of discipline when developing programs. However, like many other things, programming life becomes easier if useful tools supporting the practice are available. In particular, good tools make it easy to modify abstract data type definitions while still maintaining efficient code.

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
````
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
````
We will discuss the details of this specification below. It can be placed anywhere in
a source file, and acts like a macro, generating the following code in its place:
````
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
````
