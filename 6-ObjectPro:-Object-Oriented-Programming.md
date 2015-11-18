ALS ObjectPro is an object-oriented programming toolkit fully integrated with
ALS Prolog. Unlike some other approaches to object-oriented programming in
Prolog, it is not implemented as a system on top of Prolog. Instead, it is seamlessly
integrated with Prolog: object-oriented facilities can be smoothly accessed from ordinary Prolog programs, and the full power of Prolog can be used in the definition
of object methods.

## 6.1 Overview of ObjectPro

The objects of ObjectPro are frame-like entities possessing state which survives
backtracking. Each object belongs to a class from which it obtains its methods.
Classes are arranged in a hierarchy, with lower classes inheriting methods from parent classes. The behavior of an object is determined by two aspects:

* The object’s state, and
* The object’s methods.

An object’s state is a frame-like entity consisting of named slots which can hold
values. The table below illustrates the state of a simple object:

| slot name      | slot value |
|:---------------|:-----------|
| myName         |            |
| locomotionType |            |
| powerSource    |            |
| numWheels      |            |
| engine         |            |
| autoClass      |            |
| manufacturer   |            |

Changes to the object’s state amount to changes in the values of one or more slots.
Such changes are permanent and survive backtracking. The values which appear in
slots can be any Prolog entity, including (the state of) other objects. Messages are
sent to objects by calls of the form
send(Object, Message).
In general, objects are created, held in variables, passed around among routines, and
sent messages in the stype above. When necessary, an object can be assigned a global name when it is created which can be used for sending messages to the object.