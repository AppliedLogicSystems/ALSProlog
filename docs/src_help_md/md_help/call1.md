---
title: 'call/1'
predicates:
 - 'call/1' : calls a goal
---
`call/1` — calls a goal


## FORMS

call(Goal)

Goal


## DESCRIPTION

If Goal is instantiated to a structured term or atom which would be acceptable as the body of a clause, the goal call(Goal) is executed exactly as if that term appeared textually in place of the expression call(Goal) .


## EXAMPLES

The following example illustrates the use of call/1 :

```
?- [user].
Consultinguser.
jim:-printf(&quot;HellothisisJimRockford.\n&quot;),
_printf(&quot;Pleaseleaveyournameandnumber,\n&quot;),
_printf(&quot;andI'llgetbacktoyou.\n&quot;).
userconsulted.
yes.
```

```
?- call(jim).
HellothisisJimRockford.
Pleaseleaveyournameandnumber,
andI'llgetbacktoyou.
yes.
```

The following example shows how an instantiated variable can be used to run a goal :

```
?- Goal=write(Message),Message='WrongWay!',Goal,nl.
WrongWay!
Goal=write('WrongWay!')
Message='WrongWay!'
yes.
```


## ERRORS

If Goal is an uninstantiated variable or a number, call/1 will fail.


## SEE ALSO

- `!/0`  
`:/2`

- [Clocksin 81, 6.7]
- [Bratko 86, 7.2]
- [Sterling 86, 10.4]. 
