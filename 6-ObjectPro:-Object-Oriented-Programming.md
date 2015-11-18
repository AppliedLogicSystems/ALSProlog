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

An object’s methods are determined by the class to which it belongs.
A class is determined by three things:
* A local state-schema which describes the structure of part of the state of any
object belonging to the class;
* The methods directly associated with the class;
* The classes from which this class inherits.
Classes are also required to have names -- these are principally used in defining objects. 

The complete _state-schema_ for a class C is a structure whose collection of
slots is the union of all of the slots appearing in the local state-schemata of classes
from which C inherits, together with the slots from the local state-schemata of C.
Slots in child classes must be distinct from slots in all ancestor classes. 

The methods associated with a class are defined by Prolog clauses which can utilize various
primitive predicates for manipulating objects, as well as any ordinary Prolog predicates.

Objects are activated by sending them message . The methods of the class to which
the object belongs (or from which its class inherits) determine the object’s reaction
to the message. A message can be an arbitrary Prolog term which may include uninstantiated variables, thus implementing the partially-instatiated message paradigm of Concurrent Prolog [Ref]
The ALS ObjectPro system is integrated with the module system of ALS Prolog, in
that class definitions in ALS ObjectPro may be exported from their defining modules so as to be visible in other modules, or may be left unexported, rendering them
local to the defining module. However, each object ‘knows’ the module of its defining class, so that if one has hold of the object in a variable Object, then the call

    send(Object, Message)

can be made from the context of any module.

