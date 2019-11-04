---
title: 'defineClass/1'
package: ALSDev
group: Prolog Objects
predicates:
- {sig: 'defineClass/1', desc: 'specify an ObjectPro class'}
---

## FORMS
```
:- defineClass(SpecEqns) .
```
## DESCRIPTION

Used as a directive to specify an ObjectPro class. `SpecEqns is` a list of equations of the form
```
     Keyword = Value

```
The acceptable keywords, together with their associated `Value` types, are the following :

**`name`** - atom

**`subclassOf`** - atom _(name of an existing class to serve as parent)_

**`addl_slots`** - list of atoms _(to serve as names of local slots)_

**`defaults`** - list of default values for slots

**`constrs`** - list of constraint expressions for slots

**`export`** - yes **_[or]_** no

**`action`** - atom

The _name equation_ and the _subclassOf equation_ are both required. The single top-level pre-defined class is called **`genericObjects`**. Atoms on the _addl_slots list_ specify slots in the structure defining the state of objects which are instances of this class. These slot names must be distinct from slot names in any of the ancestor classes from which the new class inherits. The **__state-schema__** of a class is the union of the local _addl_slots_ of the class with the _addl_slots_ of all classes of which the class is a subclass. An object which is instance of a class has a slot in its state structure corresponding to each entry in the state-schema for the class.

Class definitions can supply default values for slots using an equation of the type
```
defaults = [..., <SlotName> = <Value>, ...]
```
where each &lt;`SlotName`&gt; is any one of the slot names from the complete state schema of the class, and &lt;`Value`&gt; is any appropriate value for that slot. Omitting this keyword in a class definition is equivalent to including
```
defaults = []
```
If the _`export = yes`_ equation appears on `SpecEqns`, the class methods and other information concerning the class are exported from the module in which the directive is executed.

The _constraints equation_ is used to impose constraints on the values of particular slots in the states of objects which instances of the class. The general form of a constraint specification is
```
    constrs = list of constraint expressions
```
Three types of constraint expressions are supported:

- `slotName = value`

- `slotName < valueList`

- `slotName - Var^Condition`

The left side of each of the the equations is the name of a slot occurring in the complete state-schema of the class being defined.  The first two types of expressions are special cases of the third. 

In the first constraint expression, `slotName = value`, `value` is any Prolog term, and specifies a fixed value for this slot. 

The second constraint expression, `slotName < valueList`, requires the values installed under `slotName` to be among the Prolog terms appearing on the list `valueList`. Here `'<'` is a short hand for 'is an element of'. 

The third constraint expression subsumes the first two. `Var` is a Prolog variable, and `Condition` is an arbitrary Prolog call in which `Var` occurs. The test is imposed by binding the incoming candidate value to the variable `Var`, and then calling the test `Conditon`. Installation of the incoming value in the slot named `slotName` takes place only if the test `Condition` succeeds.

If an equation `action = Name` occurs on `SpecEqns`, where `Name` is an atom, then methods of this class must be implemented by a binary predicate `Name/2`. If this equation is absent, the methods predicate for this class will be 
```
    <className>Action/2, 
```
where `<className>` is the name of the class(i.e., `name = <className>` occurs on `SpecEqns`). The format of the calls to this predicate is
```
    <className> Action(Message, State)
```
where `State` is the state of an object of this class, and  `Message` is an arbitrary Prolog term.

The structure of a State is opaque. Access to the slots is provided by two predicates:
```
setObjStruct(SlotDescrip, State, Value)

accessObjStruct(SlotDescrip, State, VarOrValue)
```
`SlotDescrip` is a slot description, which is either a slot name, or an expression of the form
```
     SlotName^SlotDescrip
```
The latter is used in cases of compound objects in which the value installed in a slot may be the state of another object. Thus,
```
     <what>ObjStruct(Slot1^Slot2, State, Value)
```
is equivalent to
```
     accessObjStruct(Slot1, State, Obj1),
     <what>ObjStruct(Slot2, Obj1, Value)
```
The call
```
    setObjStruct(SlotName, State, Value)
```
destructively updates the slot `SlotName` of `State` to contain `Value`, provided that:

 - `Value` is not an uninstantiated variable, and
 - any constraints imposed on this slot by the class are satisfied by the incoming `Value`. 

However, note that `Value` can be a term containing uninstantiated variables. 

The second call
```
    accessObjStruct(SlotName, State, ValueOrValue)
```
accesses the slot `SlotName` of `State` and unifies the value obtained with `VarOrValue`. For compactness, the following syntactic sugar is provided:
```
     State^SlotDescrip := Value
```
for
```
     setObjStruct(SlotDescrip, State, Value)
```
and
```
     VarOrValue := State^SlotDescrip
```
for
```
     accessObjStruct(SlotDescrip, State, VarOrValue)
```
The bodies of clauses defining the action predicate of a class can contain calls on [`accessObjStruct/3`, `setObjStruct/3`](setObjStruct.html), `:=`, [`send/2`, `send_self/2`](send.html), and any other built-in or program-defined Prolog predicate.

## EXAMPLES
```
:- defineClass(
     [ name = stacker,
       subclassOf = [genericObjects],
       addl_slots = [theStack, depth]
     ] ).

stackerAction(push(Item), State)
     :-
     accessObjStruct(theStack, State, CurStack),
     setObjStruct(theStack, State, [Item | CurStack ]),
     accessObjStruct(depth, State, CurDepth),
     NewDepth is CurDepth + 1,
     setObjStruct(depth, State, NewDepth).

stackerAction(pop(Item), State)
     :-
     accessObjStruct(theStack, State, [Item | RestStack ]),
     setObjStruct(theStack, State, RestStack),
     accessObjStruct(depth, State, CurDepth),
     NewDepth is CurDepth - 1,
     setObjStruct(depth, State, NewDepth).

stackerAction(cur_stack(Stack), State)
     :-
     accessObjStruct(theStack, State, Stack).

stackerAction(cur_depth(Depth), State)
     :-
     accessObjStruct(depth, State, Depth).

...create_object([name=stack, instanceOf=stacker, values=[theStack=[], depth=0] ], Obj),...
```
