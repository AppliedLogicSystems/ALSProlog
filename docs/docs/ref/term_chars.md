---
title: 'term_chars/2'
group: Terms
predicates:
- {sig: 'term_chars/2', desc: 'convert between a term and the list of characters which represent the term'}
- {sig: 'term_codes/2', desc: 'convert between a term and the list of character codes which represent the term'}
---

## FORMS

```
term_chars(Term, CharList)

term_codes(Term, CodeList)
```

## DESCRIPTION

If `CharList` is bound to a list of characters then it is parsed according to the syntax rules for terms. Should the parse be successful, the resulting value is unified with `Term` in a call to `term_chars/2`.

If `CodeList` is bound to a list of character codes then it is is parsed according to the syntax rules for terms. Should the parse be successful, the resulting value is unified with `Term` in a call to `term_codes/2`.

Otherwise `CharList` (or `CodeList`) will be bound to the list of characters (character codes) which would result as output from `write_canonical(Term)`.


## EXAMPLES

```
?- term_chars(p(a,X), L).
X = X
L = [p,'(',a,',','_','A',')']
yes.
```
```
?- term_codes(A,"X = /* a comment */ 3+4").
A = (_A = 3+4)
yes.
```
```
?- term_codes(A,"foo bar").
Error: Syntax error.
- Goal: builtins:term_codes(_A,"foo bar")
- Error Attribute: syntax('foo barend_of_file\n ^',
'Fullstop (period) expected',1,
stream_descriptor('',closed,string,
string("foo bar"),[input|nooutput],false,3,
[],0,0,0,true,1,wt_opts(78,40000,flat),[],
true,text,eof_code,0,0))
- Throw pattern: error(syntax_error,
[builtins:term_codes(_A,*),
syntax('foo barend_of_file\n ^',
'Fullstop (period) expected',1,
stream_descriptor('',closed,string,*,*,
false,3,[],0,0,0,true,1,*,[],true,
text,eof_code,0,0))])
```

## ERRORS

`CharList` is bound to a list, but the list does not contain characters

-- -- -- -- > `domain_error(character_list, CharList)`

`CodeList` is bound to a list, but the list does not contain character codes

-- -- -- -- > `domain_error(character_code_list, CodeList)`

`CharList` (or `CodeList`) is not parsable as a term

-- -- -- -- > `syntax_error`


## SEE ALSO

- [`read_term/3`](read.html)
- [`write_canonical/2`](write.html)
- [`number_chars/2`](number_chars.html)
- [`atom_chars/2`](atom_chars.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
