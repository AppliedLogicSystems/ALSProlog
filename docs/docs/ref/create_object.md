---
title: 'create_object/2'
package: ALSDev
group: Prolog Objects
predicates:
- {sig: 'create_object/2', desc: 'create an object'}
---

## FORMS
```
create_object(Eqns, Obj)
```
## DESCRIPTION

The call

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`create_object(Eqns, Obj)`

creates an object `Obj` as specified by `Eqns`, which is a list of equations of the form

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Keyword = Value`.

The acceptable keywords, together with their associated `Value` types, are:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`instanceOf =` &lt;atom&gt; _(name of a class)_

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`values =` &lt;list of equations&gt;

The `instanceOf` keyword equation is the only required equation; the value on the right side of this equation must be an atom which is the name of a previously defined class which is visible from the module in which the `create_object` call is made.

The equations appearing on the list which is the right side of a

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`values = ValuesList`

equation are expressions of the form

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`SlotName = SlotValue`

where `SlotName` is one of the named slots in the structure defining the object's state. These slots are determined by the class to which the object belongs, and may be slots from the state-schema of the immediate class parent, or may also be slots from any of the state-schemata of ancestor classes. Entries in `values equations` prescribe initial values for some of the object's slots when it is created.

When a global atomic name for the object is required, Eqns includes an equation of the form

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`name =` &lt;atom&gt;.


## EXAMPLES

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_From ~examples/objectpro/oopex_stack.pro:_
```
..., create_object([instanceOf = stacker ], Obj), ...
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_From ~examples/objectpro/oopex_vehicles.pro:_
```
..., create_object([instanceOf = iC_Engine ], Engine1),
     create_object([instanceOf = automobile,
     values = [engine = Engine1] ], Auto1),...
```
## SEE ALSO

- [`defStruct/2`](defStruct.html)
- [`defineClass/1`](defineClass.html)
