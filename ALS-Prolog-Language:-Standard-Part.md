##1 The Syntax of ALS Prolog
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
• integer
• floating point
When it is impossible to use an integer representation due to the size of a nominal
integer , a floating point representation can be used instead. This means that extremely large integers may actually require the extended precision of a floating
point value. Any operation involving integers, such as a call to is/2, will first attempt to usean integer representation for the result, and will use a floating point
value only when necessary.
This type coercion is carried out consistently within
the Prolog system.
There is no automatic conversion of floating point numbers into integers1.
Integers
The textual representation of an integer consists of a sequence of one or more digits

(0 through 9) optionally preceeded by a ‘-’ to signify a negative number. The parser
assumes that all integers are written using base ten, unless the special binary, octal,
or hexadecimal notation is used.
The hexadecimal notation is a 0x followed by a sequence of valid hexadecimal digits. The following are valid hexadecimal digits:
0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F
The octal notation is a 0o followed by a sequence of valid octal digits. The octal digits are:
0 1 2 3 4 5 6 7
The binary notation is a 0b follwed by a sequence of 0’s and 1’s.
Here are some examples of integers:
0
4532
-273 0000001

0x1fff

0b1001 0o123