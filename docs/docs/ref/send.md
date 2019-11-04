---
title: 'send/2'
package: ALSDev
group: Prolog Objects
predicates:
- {sig: 'send/2', desc: 'send a message to an object'}
---

## FORMS

```
send(Object, Message)
```

## DESCRIPTION

A message is sent to an object with a call of the form

`send(Object, Message)`

where `Object` is the target object (or an atom naming the object), and `Message` is an arbitrary Prolog term. The `Message` may include uninstantiated variables which might be instantiated by the object's method for dealing with `Message`. Such calls to `send/2` can occur both in ordinary Prolog code, and in the code defining methods of classes. For convenience, the call

`send_self(Object, Message)`

is provided as syntactic sugar for

`send(Object, Message)`

No attempt to verify that a `send_self` message is being sent from an object to itself.


## EXAMPLES

```
send(Object, push(2))

send(Object, pop(X))
```

