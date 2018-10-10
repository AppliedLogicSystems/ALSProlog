---
title: 'create_object/2'
predicates:
 - 'create_object/2' : create an object
---
`create_object/2` `--` create an object


## FORMS

create_object(Eqns, Obj)


## DESCRIPTION

The call

create_object(Eqns, Obj)

creates an object Obj as specified by Eqns, a list of
equations

of the form

Keyword = Value .

The acceptable keywords, together with their associated Value types, are :

instanceOf - atom(name of a class)

alues - list of equations
The
instanceOf keyword equation is the only required equation; the value on the right side of this equation must be an atom which is the name of class which is visible from the module in which the create_object call is made.

The equations appearing on the list which is the right side of a

alues = ValuesList
equation are expressions of the form

SlotName = SlotValue

where SlotName is one of the named slots in the structure defining the object ' s state. These slots are determined by the class to which the object belongs, and may be slots from the state-schema of the immediate class parent, or may also be slots from any of the state-schemata of ancestor classes. Entries in values equations prescribe initial values for some of the object ' s slots when it is created.

When a global atomic name for the object is required, Eqns includes an equation of the form

name = &lt; atom &gt; .


## EXAMPLES

..., create_object([instanceOf = stacker ], Obj), ...


..., create_object([instanceOf = iC_Engine ], Engine1),

create_object([instanceOf = automobile,

values = [engine = Engine1 ] ], Auto1), ..


