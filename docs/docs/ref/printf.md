---
title: 'printf/[1,2,3,4]'
group: Input Output
predicates:
- {sig: 'printf', args: {
    1: 'print out a string to the current output',
    2: 'print out a string with arguments',
    3: 'print out a string with a format and arguments',
    4: 'print out string with format, arguments, options'
  }}
- {sig: 'printf_opt/3', desc: 'print out string with format, arguments, options'}
---

## FORMS
```
printf(Format)

printf(Format, ArgList)

printf(Stream_or_Alias, Format, ArgList)

printf_opt(Format, ArgList, WriteOptions)

printf(Stream_or_Alias, Format, ArgList, WriteOptions)
```
## DESCRIPTION

`Format` can be either a (quoted) prolog atom or prolog string describing output to be created.  It can contain ordinary characters to be printed as is, format characters (e.g., %t, %d, ...) which can be used to interpret expressions appearing on `ArgList`, and special "backslash expressions" (e.g., \n, \t,, ...) for outputting special expressions.

`printf(Format)` outputs the atom `Format` as is, with the exception that the special "backslash expressions" are interpreted as follows:

`\n` -- prints a newline character (the same character as output by `nl/0`)

`\t` -- prints a tab character

`\v` -- prints a vertical tab character

`\b` -- prints a backspace character

`\r` -- prints a carriage return character

`\\` -- prints a backslash

`\%` -- prints a percent sign

Any format characters (to be listed below) appearing in `Format` will be output unchanged by `printf(Format)`.  And any format characters appearing in `Format` which do not correspond to an element of `ArgList` will be output unchanged by `printf(Format)`.  For example:
```
?- printf('%d %t').
%d %t
yes.

?- printf('%d %t', [44]).
44 %t
yes.
```

`printf(Format, ArgList)` is equivalent to `printf(Stream_or_Alias, Format, ArgList)`, where `Stream_or_Alias` has been set to `user_output`.  `ArgList` is a list of prolog expressions which will be output as elements of the atom `Format` being output.  Conceptually, one scans `Format` from left-to-right, assembling the list of format expressions occurring in `Format`, and matching them against the corresponding prolog expression in `ArgList`.  The basic format characters are listed below, together with their effects on the matching prolog expressions in `ArgList`:

`%t`&nbsp;&nbsp;print an arbitrary prolog term, as output by write/1;  
`%d`&nbsp;&nbsp;print an integer number in decimal;  
`%o`&nbsp;&nbsp;print an integer number in octal;  
`%x`&nbsp;&nbsp;print an integer number in hexadecimal with lower case letters;  
`%X`&nbsp;&nbsp;print an integer number in hexadecimal with upper case letters;  
`%f`&nbsp;&nbsp;print a floating point number (printed in the form dddd.dddddd);  
`%E`&nbsp;&nbsp;print a floating point number (printed in scientific notation: d.dddEddd);  
`%g`&nbsp;&nbsp;print a floating point number (printed either as f or E, depending on value and precision);  
`%c`&nbsp;&nbsp;print the individual character C corresponding to an integer 0'C;  
`%s`&nbsp;&nbsp;print a prolog string or atom;  

Integer and floating point numbers are preceded by a minus sign if the number is negative.

The action of placeholders can be modified by *extent expressions*, which appear between the `%` sign and the placeholder type letter (`t, d, ...`).  An *extent expression* is defined as either a sequence of digits, or two sequences of digits, separated by a period (`.`).  Any extent expression prefixed by a minus sign is also an extent expression.  A single sequence of digits is call a *width modifier*, while in a pair of expressions, the portion following the period is call the *precision modifier*.  For example, `%5d` indicates that the integer beingoutput should occupy at least 5 spaces.  And `%08.2f` indicates that the floating point number being output should occupy 8 spaces, with 2 digits following the decimal point. (See examples below).  More exotic modifiers can be found in [printf format string](https://en.wikipedia.org/wiki/Printf_format_string); see also [printf, Format Specifiers, Format Conversions and Formatted Output](https://c.camden.rutgers.edu/c_resources/printf.html) and [Format specifiers in C](https://www.geeksforgeeks.org/format-specifiers-in-c/).

The actions of `%t` and `%p` are defined by the builtin ALS Prolog machinery.  The actions all other placeholders, include the effects of extent expressions are defined by passing them off to the underlying C machinery.

Both `printf_opt(Format, ArgList, WriteOptions)` and `printf(Stream_or_Alias, Format, ArgList, WriteOptions)` allow one to apply *write options* (see [Write options](../guide/10-Prolog-I-O/#1072-write-options)).






Using printf is generally much easier than using the equivalent [`write/1`](write.html), [`put/1`](put.html), and `nl/0` predicates because the whole message you want to print out can be assembled and carried out by one call to `printf`.

## EXAMPLES
```
?- printf('hello world').
hello world
yes.

?- printf("Hello World!\t Wake Up!\nRise And Shine!").
Hello World!	 Wake Up!
Rise And Shine!
yes.

?- printf('Letters: %c%c%c\n', [0'a,0'B,0'c]).
Letters: aBc

yes.

?- printf('Contents: %t, Number of items: %d\n', [pocket(keys,wallet,watch), 3]).
Contents: pocket(keys,wallet,watch), Number of items: 3

yes.

?- printf('%t', ['ABC']).
ABC
yes.

?-  printf_opt('%t', ['ABC'], [quoted(true)]).
'ABC'
yes.

?- printf('%o %x %X\n', [147,167,167]).
223 a7 A7
yes.

?- printf(user_output, '%t', [a*(b+c)]).
a*(b+c)
yes.

?- printf(user_output, '%t', [a*(b+c)], [ignore_ops(true)]).
*(a,+(b,c))
yes.

?- printf('|%12.4f|', [10.3456]).
|     10.3456|
yes.

?- printf('|%-10s| indeed', ['Hello']).
|Hello     | indeed
yes.

?- printf("|%20.5s|", [geeksforgeeks]).
|               geeks|
yes.

?- current_output(Stream),printf(Stream, '%p ABC', [Stream^write(23 > anm)]).
23>anm ABC
Stream=stream_descriptor('\a',open,console,'standard output',
    [noinput|output],false,-2,0,0,0,0,true,0,wt_opts(78,400,flat),[],wait,
    text,eof_code,true,0) 

yes.

```
## SEE ALSO

- `nl/0` {%- comment %} TODO: missing {% endcomment %}
- [`put/1`](put.html)
- [`write/[1,2]`](write.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
- [Printf in the User Guide](../guide/10-Prolog-I-O/#1074-printf)
- Unix/C Reference Manuals: printf(3S)
