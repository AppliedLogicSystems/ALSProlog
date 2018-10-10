—-
title: 'bufread/[2,4]'
predicates:
 - 'bufread/2' : runs the Prolog parser on a string of text
 - 'bufread/4' : similiar to bufread/2, giving additional information
—-
`bufread/2` `—` runs the Prolog parser on a string of text

`bufread/4` `—` similiar to bufread/2, giving additional information


## FORMS

bufread(Buffer, [Structure | Vars ])

bufread(Buffer, [Structure | Vars ], FullStop, LeftOver)


## DESCRIPTION

bufread/2 takes a Prolog string, Buffer, and attempts to transform it into a Prolog term. It does this by :

- Reading the first term out of Buffer(trailing characters from Buffer are ignored) and unifying it with Structure.

- Returning in Vars a list of the quoted variable names occurring in the term read.

If an error has occured, the head will be an error message(a Prolog string), and the tail will be the column number where the error is suspected to have occurred. bufread/4 is the same as bufread/2, except that more information is provided. FullStop is a flag indicating whether a full stop has been typed. Its value is 1 if there was a full stop, and 0 if there was not. LeftOver is the text that is not transformed yet. Although, bufread/4 only transforms one term at a time, it returns the text it has not transformed yet in the LeftOver argument. This text can then be transformed by issuing another bufread call, with LeftOver given as the Buffer.


## EXAMPLES

The following example converts the buffer :


&quot; f(abc, Bob) some stuff &quot;

to the term f(abc, _53) .

? - bufread(&quot; f(abc, Bob) some stuff &quot;, [T | Vars ]) .

T = f(abc, _53)

Vars = [ ' Bob ' ]

yes.

Observe that the characters &quot; some stuff &quot; are discarded by bufread/2. This next examples demonstrates how error messages are returned :

? - bufread(&quot; hello(&quot;, [Message | Column ]),

printf(&quot; \nMessage : %s\nColumn : %d\n &quot;, [Message, Column ]) .

Message : Non-empty term expected.

Column : 6

Message = [78, 111, 110, 45, 101, 109, 112, 116, 121, 32, 116, 101,

114, 109, 32, 101, 120, 112, 101, 99, 116, 101, 100, 46]

Column = 6

yes.

bufread/2 can be used to convert strings to integers in the following manner :

? - bufread(&quot; 123 &quot;, [Int | _ ]) .

Int = 123

yes.

In the following example, the term inside(Where) was not terminated with a full stop, so FullStop is bound to 0 There is no leftover text to run, so LeftOver is bound to the empty list.

? - bufread(&quot; inside(Where) &quot;, [Term | Vars ], FullStop, LeftOver) .

Term = inside(_38)

Vars = [ ' Where ' ]

FullStop = 0

LeftOver = [ ]

yes.

If you are writing a shell in Prolog using bufread/4, you could write a continuation prompt to tell the user of your shell that they must terminate the term with a full stop. In the next example, the term food(tai) was terminated by a full stop, so FullStop is bound to 1 This time, there is some leftover text that can be processed, so LeftOver is bound to the remaining Prolog string :

&quot; food(indian) . food(chinese) . &quot;


? - bufread(&quot; food(tai) . food(indian) . food(chinese) . &quot;,

[Term | Vars ], FullStop, Rest),

printf(&quot; Rest = %s\n &quot;, [LeftOver ]) .

LeftOver = food(indian) . food(chinese) .

Term = food(tai)

Vars = [ ]

FullStop = 1

LeftOver = [32, 102, 111, 111, 100, 40, 105, 110, 100, 105, 97, 110, 41, 46, 32,

102, 111, 111, 100, 40, 99, 104, 105, 110, 101, 115, 101, 41, 46, 32]

yes.

If you are writing a shell such as mentioned above, you can use use the LeftOver argument to allow multiple goals per line. You do this by continually calling bufread/4 and checking whether you still have further input to process in LeftOver.

