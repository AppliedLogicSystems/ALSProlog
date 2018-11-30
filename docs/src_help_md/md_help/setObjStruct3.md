---
title: 'setObjStruct/3'
package: ALSDev
group: Prolog Objects
predicates:
- {sig: 'setObjStruct/3', desc: 'set the value of a slot in an object'}
- {sig: 'accessObjStruct/3', desc: 'access the value of a slot in an object'}
---

## FORMS

```
setObjStruct(SlotDescrip, State, Value)

accessObjStruct(SlotDescrip, State, VarOrValue)
```

## DESCRIPTION

The predicates provide access to the slots of objects. The call

`setObjStruct(SlotName, State, Value)`

destructively updates the slot `SlotName` of `State` to contain `Value`, which cannot be an uninstantiated variable, provided that any constraints imposed on this slot by the class are satisfied by the incoming `Value`. However, `Value` can contain uninstantiated variables. The second call

`accessObjStruct(SlotName, State, Value)`

accesses the slot `SlotName` of `State` and unifies the value obtained with `VarOrValue`.
`SlotDescrip` is a slot description, which is either a slot name, or an expression of the form

`SlotName^SlotDescrip`

The latter is used in cases of compound objects in which the value installed in a slot may be the state of another object. Thus,

`<what>ObjStruct(Slot1^Slot2, State, Value)`

is equivalent to

`accessObjStruct(Slot1, State, Obj1),`    
`<what>ObjStruct(Slot2, Obj1, Value)`

For compactness, the following syntactic sugar is provided:

```
State^SlotDescrip := Value
for
setObjStruct(SlotDescrip, State, Value)
and
VarOrValue := State^SlotDescrip
for
accessObjStruct(SlotDescrip, State, VarOrValue)
```

## EXAMPLES

```
setObjStruct(theStack, State, [Item | CurStack])
accessObjStruct(theStack, State, Stack)
```