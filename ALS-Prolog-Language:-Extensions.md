##[5 Abstract Data Types: Structure Definition](id:5-Abstract)

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
