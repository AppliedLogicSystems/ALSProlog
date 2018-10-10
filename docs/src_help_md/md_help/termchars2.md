—-
title: 'term_chars/2'
predicates:
 - 'term_chars/2' : convert between a term and the list of characters which represent the term
 - 'term_codes/2' : convert between a term and the list of character codes which represent the term
—-
`term_chars/2` `—` convert between a term and the list of characters which represent the term

`term_codes/2` `—` convert between a term and the list of character codes which represent the term


## FORMS

term_chars(Term, CharList)

term_codes(Term, CodeList)


## DESCRIPTION

If CharList is bound to a list of characters then it is parsed according to the syntax rules for terms. Should the parse be successful, the resulting value is unified with Term in a call to term_chars/2.

If CodeList is bound to a list of character codes then it is is parsed according to the syntax rules for terms. Should the parse be successful, the resulting value is unified with Term in a call to term_codes/2.

Otherwise CharList(or CodeList) will be bound to the list of characters(character codes) which would result as output from write_canonical(Term)


## EXAMPLES

```
?- term_chars(p(a,X),L).
X=X
L=[p,'(',a,',','_','A',')']
yes.
```

```
?- term_codes(A,&quot;X=/*acomment*/3+4&quot;).
A=(_A=3+4)
yes.
```

```
?- term_codes(A,&quot;foobar&quot;).
Error:Syntaxerror.
-Goal:builtins:term_codes(_A,&quot;foobar&quot;)
-ErrorAttribute:syntax('foobarend_of_file\n^',
'Fullstop(period)expected',1,
stream_descriptor('',closed,string,
string(&quot;foobar&quot;),[input|nooutput],false,3,
[],0,0,0,true,1,wt_opts(78,40000,flat),[],
true,text,eof_code,0,0))
-Throwpattern:error(syntax_error,
[builtins:term_codes(_A,*),
syntax('foobarend_of_file\n^',
'Fullstop(period)expected',1,
stream_descriptor('',closed,string,*,*,
false,3,[],0,0,0,true,1,*,[],true,
text,eof_code,0,0))])
```

## ERRORS

CharList is bound to a list, but the list does not contain characters

— — -- -- &gt; domain_error(character_list, CharList)

CodeList is bound to a list, but the list does not contain character codes

— — -- -- &gt; domain_error(character_code_list, CodeList)

CharList(or CodeList) is not parsable as a term

— — -- -- &gt; syntax_error


## SEE ALSO

- `read_term/3`  
`write_canonical/2`  
`number_chars`  
`atom_chars`

- `UserGuide(PrologI/O).
