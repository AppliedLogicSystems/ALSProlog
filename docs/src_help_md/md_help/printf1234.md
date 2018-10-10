—-
title: 'printf/[1,2,3,4]'
predicates:
 - 'printf/1' : print out a string to the current output
 - 'printf/2' : print out a string with arguments
 - 'printf/3' : print out a string with a format and arguments
 - 'printf_opt/3' : print out string with format, arguments, options
 - 'printf/4' : print out string with format, arguments, options
—-
`printf/1` `—` print out a string to the current output

`printf/2` `—` print out a string with arguments

`printf/3` `—` print out a string with a format and arguments

`printf_opt/3` `—` print out string with format, arguments, options

`printf/4` `—` print out string with format, arguments, options


## FORMS

printf(Format)

printf(Format, ArgList)

printf(Steam_or_Alias, Format, ArgList)

printf_opt(Format, ArgList, Options)

printf(Stream_or_Alias, Format, ArgList, WriteOptions)


## DESCRIPTION

printf/2 takes a format string and a list of arguments to include in the format string. printf/1 is the same as printf/2 except no argument list is given. The following is a list of the special formatting possible within the format string :

\n — prints a newline(same as nl/0)

\t — prints a tab character

\\ — prints a backslash

\% — prints a percent sign

%c — prints the corresponding Prolog character(atom) in the argument list

%d — prints the corresponding decimal number in the argument list

%s — prints the corresponding Prolog string in the argument list

%t — prints the corresponding Prolog term in the argument list(same as write/1)

All other characters are printed as they appear in the format string.

Using printf is generally much easier than using the equivalent write/1, put/1, and nl/0 predicates because the whole message you want to print out can be done by one call to printf.


## EXAMPLES

```
?- printf(&quot;helloworld\n&quot;).
helloworld
yes.
```

```
?- printf(&quot;Letters:%c%c%c\n&quot;,[a,b,c]).
Letters:abc
yes.
```

```
?- printf(&quot;Contents:%t,Amount:%d\n&quot;,
[pocket(keys,wallet,watch),3]).
Contents:pocket(keys,wallet,watch),Amount:3
yes.
```


## SEE ALSO

- `nl/0`  
`put/1`  
`write/0`  
`UserGuide(PrologI/O)`  
`[Unix/CReferenceManuals:printf(3S)].
