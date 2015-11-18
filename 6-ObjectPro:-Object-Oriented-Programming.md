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

##6.2 Defining Objects and Sending Messages

An object is defined by an expression of the form

    create_object(Eqns, Obj)

where Obj is an uninstantiated variable which will be bound to the new object, and
Eqns is a list of equations of the form

    Keyword = Value

The acceptable keywords, together with their associated Value types, are the following:
````
instanceOf - atom (name of a class)
values     - list of equations
````
The instanceOf keyword equation is the only required equation; the value on
the right side of this equation must be an atom which is the name of class which is
visible from the module in which the create_object call is made. Here is an
example of a simple object definition, where iC_Engine must be the name of
class:

    create_object([instanceOf=iC_Engine ], Obj)

The equations appearing on a list which is the right side of a

    values =ValuesList

equation are expressions of the form

    SlotName = SlotValue

where SlotName is one of the named slots in the structure defining the object’s
state. These slots are determined by the class to which the object belongs, and may
be slots from the state-schema of the immediate class parent, or may also be slots
from any of the state-schemata of ancestor classes. The intent of the values equation is to enable the programmer to prescribe initial values for some of the object’s
slots when it is created.

When a global atomic name for the object is required, one includes an equation of
the form

    name = <atom> .

A message is sent to an object with a call of the form

    send(Object, Message)

where Object is the target object (or an atom naming the object), and Message
is an arbitrary Prolog term. The Message may include uninstantiated variables
which might be instantiated by the object’s method for dealing with Message.
Such calls to send/2 can occur both in ordinary Prolog code, and in the code defining methods of classes (and hence objects). For convenience, or conceptual emphasis, a call

    send_self(Object, Message)

is provided. This is merely syntactic sugar for

    send(Object, Message)

That is, the implementation makes no attempt to verify that a send_self message
is being truly sent from an object to itself.

##6.3 Defining Classes

A class is defined by a directive of the form

    :- defineClass(Eqns).

Here Eqns is a list of equations of the form

    Keyword = Value

The acceptable keywords, together with their associated Value types, are the following:
````
name       - atom
subclassOf - atom (name of a (parent) classe)
addl_slots - list of atoms (names of local slots)
defaults   - list of default values for slots
consorts   - list of constraint expressions for slots
export     - yes or no
action     - atom
````
The name equation and the subclassOf equation are both required.

The ObjectPro system pre-defines one top-level class named genericObjects;
all classes are ultimately subclasses of the genericObjects class. genericObjects provides one visible slot, myName, which is always instantiated to the
object’s name. Several other slots, normally non-visible, are also provided.

A class is said to be an immediate subclass of the (parent) class named in the subclassOf equation. The relation subclass is the transitive closure of the immediate subclass relation.

The atoms on the addl_slots list specify slots in the structure defining the state
of objects which are instances of this class. These new slot names must not be slot
names in any of the ancestor classes from which the new class inherits; hence the
nomenclature “addl_slots”. The state-schema of a class is the union of the
addl_slots of the class with the addl_slots of all classes of which the class
is a subclass. Reiterating, it is required that the slot names occurring on all these
addl_slot lists be distinct.

Here are several examples of simple class definitions:
````
:- defineClass([name=vehicle,
                subclassOf=genericObjects,
                addl_slots=[locomotionType, powerSource] ]).

:- defineClass([name=wheeledVehicle,
                subclassOf=vehicle,
                addl_slots=[numWheels] ]).

:- defineClass([name=automobile,
                subclassOf=wheeledVehicle,
                addl_slots=[engine,autoClass,manufacturer] ]).

:- defineClass([name=wingedVehicle,
                subclassOf=vehicle,
                addl_slots=[numWings] ]).
````
The inheritance relations among these classes is shown in the Figure below.

{ADD FIGURE}

Figure. Example Class Inheritance Relations.