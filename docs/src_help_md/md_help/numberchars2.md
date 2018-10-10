—-
title: 'number_chars/2'
predicates:
 - 'number_chars/2' : convert between a number and the list of characters which represent the number
 - 'number_codes/2' : convert between a number and the list of character codes which represent the number
—-
`number_chars/2` `—` convert between a number and the list of characters which represent the number

`number_codes/2` `—` convert between a number and the list of character codes which represent the number


## FORMS

number_chars(Number, CharList)

number_codes(Number, CodeList)


## DESCRIPTION

If CharList is bound to a list of characters then it is parsed according to the syntax rules for numbers. Should the parse be successful, the resulting value is unified with Number in a call to number_chars/2.

If CodeList is bound to a list of character codes then it is is parsed according to the syntax rules for numbers. Should the parse be successful, the resulting value is unified with Number in a call to number_codes/2.

In Number is bound to a number in either number_chars/2(or number_codes/2), after first ascertaining that CharList(or CodeList) is bound to a ground list, then CharList(or CodeList) will be bound to a list of characters(character codes) that would result as output from write_canonical(Number) .


## EXAMPLES

```
?- number_chars(-2.3,L).
L=[-,'2',.,'3']
yes.
```

```
?- number_codes(N,&quot;123&quot;).
N=123
yes.
```

```
?- number_codes(N,&quot;123.4&quot;).
N=123.4
yes.
```

```
?- number_chars(123.4,['1',A,B,.,C]).
A='2'
B='3'
C='4'
yes.
```

```
?- number_codes(N,&quot;0xffe&quot;).
N=4094
yes.
```

```
?- number_codes(N,&quot;foobar&quot;).
Error:Syntaxerror.
-Goal:builtins:number_codes(_A,&quot;foobar&quot;)
-Throwpattern:error(syntax_error,[builtins:number_codes(_A,*)])
```

## ERRORS

Number and CharList are variables(number_chars/3)

— — -- -- &gt; instantiation_error.

Number and CodeList are variables(number_codes/3)

— — -- -- &gt; instantiation_error.

Number is neither a number nor a variable

— — -- -- &gt; type_error(number, Number)

CharList is neither a variable nor a list of characters

— — -- -- &gt; domain_error(character_list, List)

CodeList is neither a variable nor a list of character codes

— — -- -- &gt; domain_error(character_code_list, List)

CharList(or CodeList) is not parsable as a number

— — -- -- &gt; syntax_error


## SEE ALSO

- `read_term/3`  
`write_canonical/2.
