##[1 The Syntax of ALS Prolog](id:1-Syntax)
This chapter describes the syntax of ALS Prolog, which is for the most part the syntax of the ISO Prolog standard . Prolog syntax is quite simple and regular, which is
a great strength.

###1.1 Constants
The simplest Prolog data type is a constant, which comes in two flavors:
* atoms (sometimes called symbols)
* numbers  

The notion of a constant corresponds roughly to the notion of a name in a natural
language. Names in natural languages refer to things (which covers a lot of
ground), and constants in Prolog are be used to refer to things when the language is
interpreted.

####1.1.1 Numbers

Prolog uses two representations for numbers:
* integer
* floating point  

When it is impossible to use an integer representation due to the size of a nominal
integer , a floating point representation can be used instead. This means that extremely large integers may actually require the extended precision of a floating point value. Any operation involving integers, such as a call to is/2, will first attempt to use an integer representation for the result, and will use a floating point value only when necessary.
This type coercion is carried out consistently within the Prolog system.
There is no automatic conversion of floating point numbers into integers.  (Note that the ISO Prolog standard now forbids this kind of conversion.)

#####Integers
The textual representation of an integer consists of a sequence of one or more digits (0 through 9) optionally preceded by a ‘-’ to signify a negative number. The parser
assumes that all integers are written using base ten, unless the special binary, octal,
or hexadecimal notation is used.
The hexadecimal notation is a 0x followed by a sequence of valid hexadecimal digits. The following are valid hexadecimal digits:  

    0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F

The octal notation is a 0o followed by a sequence of valid octal digits. The octal digits are:  

    0 1 2 3 4 5 6 7

The binary notation is a 0b follwed by a sequence of 0’s and 1’s.
Here are some examples of integers:  

    0  4532  -273 0000001  0x1fff  0b1001 0o123  

It is important to note that a term of the form +5 is not an integer, but instead is a structured term.

#####Floating point numbers

Floating point numbers are slightly more complex than integers in that they may
have either a fractional part, an exponent, or both. A fractional floating point number consists of a sequence of one or more numeric characters, followed by a dot (‘.’), in turn followed by another sequence of one or more numeric characters; the entire expression may optionally be preceded by a ‘-’. Here are some examples of
floating point numbers:  

    0.0    3.1415927    -3.4    000023.540000

You can also specify an exponent using scientific notation. An exponent is either
an e or an E followed by an optional ‘-’, signifying a negative exponent, followed
by a sequence of one or more numeric characters. Here are examples of floating
point numbers with exponents:  

    0.1e-3  10E99  -44.66e-88  0E-0

#####ASCII Codes

ASCII (American Standard Code for Information Interchange) codes are small integers between 0 and 255 inclusive that represent characters. The parser will translate any printable character into its corresponding ASCII integer. In order to get the ASCII code for a character, precede the character by the characters 0’. For example, the code for the characters ‘A’, ‘8’, and ‘%’ would be given by:  

    0’A    0’8    0’%

In addition, the ANSI C-style octal and hex forms expression can be used. Thus, all of the expressions below denote the number 65: 

    0’A    0’\101    0’\x41

The table below displays several example ASCII sequences:
<table>
<tr><th>Expr</th><th>Octal Expr</th><th>Hex Expr</th><th>ASCI Code</th><th>Char</th></tr>
<tr><td>0’A</td><td>0’\101</td><td>0’\x41</td><td>65</td><td>Upper case A</td></tr>
<tr><td>0’c</td><td>0’\143/td><td>0’\x63/td><td>99/td><td>Lower case c</td></tr>
<tr><td>0’~</td><td>0’\176</td><td>0’\x7e</td><td>126</td><td>Tilde character</td></tr>
</table>

There also exists a small collection of symbolic control characters which can be thought of as synonyms for certain of the ASCI control character codes. These are presented in the following table:

<table>
<tr><th>Expr</th><th>Octal Expr</th><th>Hex Expr</th><th>ASCI Code</th><th>Char</th></tr>
<tr><td>0’\a</td><td>0’\007</td><td>0’\x7</td><td>7</td><td>alert (‘bell’)</td></tr>
<tr><td>0’\b</td><td>0’\010</td><td>0’x\8</td><td>8</td><td>backspace</td></tr>
<tr><td>0’\f</td><td>0’\014</td><td>0’\xC</td><td>12</td><td>form feed</td></tr>
<tr><td>0’\n</td><td>0’\012</td><td>0’\xA</td><td>10</td><td>new line</td></tr>
<tr><td>0’\r</td><td>0’\015</td><td>0’\xD</td><td>13</td><td>return</td></tr>
<tr><td>0’\t</td><td>0’\011</td><td>0’\x9</td><td>9</td><td>horizontal tab</td></tr>
<tr><td>0’\v</td><td>0’\147</td><td>0’\x77</td><td>119</td><td>vertical tab</td></tr>
</table>

#####Atoms

An atom is a sequence of characters that are parsed together as a constant.

######Alphanumeric atoms

An alphanumeric atom is a sequence of characters that begins with a lower case letter, and is followed by zero or more alphanumeric characters, possibly including
‘_’.  Here are some examples of alphanumeric atoms:  

    foobar123  zIPPY  bread_and_butter  money

######Quoted atoms

A quoted atom is formed by placing any sequence of characters between single
quotes (’).  A single quote can be included in the text of the atom by using two
consecutive single quotes for each one desired, or by prefixing the embedded single
quote with the backslash (&#92;) escape character. The following are all quoted atoms:  

    ’any char will do’    ’$*#!#@%#*’
    ’Can’’t miss’    ’Can\’t miss’    ’99999’

If the characters that compose a quoted atom can be interpreted as an atom when
they occur without the enclosing single quotes, then it is not necessary to use the
quoted form. However, if the atom contains characters that aren’t allowed in a simple atom, then the quotes are required. Note that the last example above is an atom
whose print name is 99999, not the integer 99999.
Quoted atoms can span multiple lines, but in this case the end of each such line must
be preceeded by the backslash escape character, as in the following example of an
atom:  

    ’We are the stars which sing. \
    We sing with our light; \
    We are the birds of fire, \
    We fly over the sky. \
    -- Algonquin poem.’

######Special atoms

A special atom is any sequence of characters from the following set:  

    +-*/\^<>=‘:.?@#&.

In addition, the atoms, [], !, ; and , are considered to be special atoms. Some oth-
er examples of special atoms are:  

      +=   &&   @>=   ==   <---------

Most special atoms are automatically read as quoted atoms unless they have been declared as operators (See Section 1.7 Operators {ADD LINK}).

###1.2 Variables

A variable consists of either a _ (underbar character) or an upper case letter, followed by a sequence of alphanumeric characters and dollar signs. Here are some variables:  

    Variable X123a _a$bc _123 _

###1.3 Compund Terms

A compound term is consists of a symbolic constant, called a functor, followed
by a left parenthesis followed by one or more terms separated by commas, followed
by a right parenthesis. The number of terms separated by commas enclosed in the
parentheses is called the arity of the structure. For example, the compound term  

    f(a,b(X),y)  

has arity 3.

###1.4 Curly Braces

Instead of prefixing a structured term with a functor, the curly brace notation allows
a sequence of terms, separated by commas, to be grouped together in a comma list
with ‘{}’ as the principal functor. For example,  

    {all,the,young,dudes}  

parses internally into:  

    ’{}’((all,the,young,dudes))  

###1.5 Lists

The simplest list is the empty list, represented by the atom ‘[]’. Any other list is
a structured term with ./2 as principal functor and whose second argument is a list.
Lists can be written by using ‘.’ explicitly as a functor, or using the special list notation.
A list using list notation is written as a [ followed by the successive first arguments
of all the sublists in order seperated by commas, followed by ]. The following are
all different ways of writing the same list:  

    a.b.c.[]
    [a,b,c]
    ’.’(a,’.’(b,’.’(c,[])))

Unless specified, the last tail of a list is assumed to be []. A tail of a list can be
specified explicitly by using |, as in these examples:  

    [a|X]
    [1,2,3|[]]
    [Head|Tail]  

The list notation for lists is preferrable to using ‘.’ explicitly because the dot is also
used in floating point numbers and to signal termination of input terms.

###1.6 Strings

A string is any sequence of characters enclosed in double quotes ("). The parser automatically translates any string into the list of ASCII codes that corresponds to the characters between the quotes. For example, the string  

    "It’s a dog’s life"  

is translated into  

    [73,116,39,115,32,97,32,100,111,103,39,115,32,108,10,5,102,101]

Double quotes can be embedded in strings by either repeating the double quote or by using the backslash escape character before the embedded “, as for example in  

    "She said, ""hi.""".
    "She said, \"hi.\"".

###1.7 Operators

The prefix functor notation is convenient for writing terms with many arguments. However, Prolog allows a program to define a more readable syntax for structured terms with one or arguments. For example, the parser recognizes the text  

    a+b+c  

as an expression representing  

    +(+(a,b),c)  

because the special atom + is declared as an infix operator. Infix operators are written between their two arguments. For the other operator types, prefix and postfix, the operator (functor) is written before (prefix) or after (postfix) the single argument to the term.

#####What Makes an Operator?

Operators are either alphanumeric atoms or special atoms which have a corresponding precedence and associativity. The associativity is sometimes referred to as the type of an operator. Operators may be declared by using the op/3 builtin.  Precedences range from 1 to 1200 with the lower precedences having the tightest binding. Another way of looking at this is that in an expression such as 1*X+Y, the
operator with the highest precedence will be the principal functor. So 1*X+Y is
equivalent to ’+’(’*’(1,X),Y) because the ‘*’ binds tighter than the ‘+’.  The types of operators are named  

    fx, fy, xf, yf, xfx, yfx, and xfy,

where the ‘f’ shows the position of the operator. Hence, fx and fy indicate prefix
operators, yf, and xf indicate postfix operators, and xfx, yfx, and xfy indicate
infix operators. An ‘x’ indicates that the operator will not associate with operators
of the same or greater precedence, while a ‘ y’ indicates that it will associate with
operators of the same or lower precedence, but not operators of greater precedence.  The table below describes all of the predefined binary operators in ALS Prolog:


<table>
<tr><th>Operator</th><th>Specifier</th><th>Precedence</th><th>Operator</th><th>Specifier</th><th> Precedence</th></tr>
<tr><td>:-</td><td>
xfx</td><td>
1200</td><td>
=:=</td><td>
xfx</td><td>
700</td></tr>

<tr><td>--></td><td>
xfx</td><td>
1200</td><td>
=\=</td><td>
xfx</td><td>
700</td></tr>

<tr><td>==></td><td>
xfy</td><td>
1200</td><td>
<</td><td>
xfx</td><td>
700</td></tr>

<tr><td>when</td><td>
xfx</td><td>
1190</td><td>
=<</td><td>
xfx</td><td>
700</td></tr>

<tr><td>where</td><td>
xfx</td><td>
1180</td><td>
></td><td>
xfx</td><td>
700</td></tr>

<tr><td>with</td><td>
xfx</td><td>
1170</td><td>
>=</td><td>
xfx</td><td>
700</td></tr>

<tr><td>if</td><td>
xfx</td><td>
1160</td><td>
:=</td><td>
xfy</td><td>
600</td></tr>

<tr><td>;</td><td>
xfy</td><td>
1100</td><td>
+</td><td>
yfx</td><td>
500</td></tr>

<tr><td>|</td><td>
xfy</td><td>
1100</td><td>
-</td><td>
yfx</td><td>
500</td></tr>
</table>
