---
title: 'number_chars/2'
group: Terms
iso: numberchars
predicates:
- {sig: 'number_chars/2', desc: 'convert between a number and the list of characters which represent the number'}
- {sig: 'number_codes/2', desc: 'convert between a number and the list of character codes which represent the number'}
---

## FORMS
```
number_chars(Number, CharList)

number_codes(Number, CodeList)
```
## DESCRIPTION

In a call to `number_chars/2`, if `CharList` is bound to a list of characters then it is parsed according to the syntax rules for numbers; should the parse be successful, the resulting value is unified with `Number`. 

If `Number` is bound to a number in `number_chars/2`, after first ascertaining that `CharList` is bound to a ground list, then `CharList` will be unified with a list of characters that would result as output from `write_canonical(Number)`.

In a call to `number_codes/2`, if `CodeList` is bound to a list of character codes, then it is is parsed according to the syntax rules for numbers; should the parse be successful, the resulting value is unified with `Number`.

If `Number` is bound to a number in `number_codes/2`, after first ascertaining that `CodeList` is bound to a ground list, then `CodeList` will be unified with a list of character codes that would result as output from `write_canonical(Number)`.

## EXAMPLES

```
?- number_chars(-2.3,L).

L=[-,'2',.,'3']

yes.

?- number_codes(N, "123" ).

N=123

yes.

?- number_codes(N, "123.4" ).

N=123.4

yes.

?- number_chars(123.4,['1',A,B,.,C]).

A='2' 
B='3' 
C='4' 

yes.

?- number_codes(N,"0xffe" ).

N=4094

yes.

?- number_codes(N,"foobar").
Error: Syntax error.
- Goal:          builtins:number_codes(_A,"foobar")
- Throw pattern: error(syntax_error,[builtins:number_codes(_A,*)])
```

## ERRORS

Number and CharList are variables(number_chars/3)

-- -- -- -- &gt; instantiation_error.

Number and CodeList are variables(number_codes/3)

-- -- -- -- &gt; instantiation_error.

Number is neither a number nor a variable

-- -- -- -- &gt; type_error(number, Number)

CharList is neither a variable nor a list of characters

-- -- -- -- &gt; domain_error(character_list, List)

CodeList is neither a variable nor a list of character codes

-- -- -- -- &gt; domain_error(character_code_list, List)

CharList(or CodeList) is not parsable as a number

-- -- -- -- &gt; syntax_error


## SEE ALSO

- [`read_term/3`](read.html)
- [`write_canonical/2`](write.html)
