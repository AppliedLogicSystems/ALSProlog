---
title: 'op/3'
module: sio
iso: operators
predicates:
- {sig: 'op/3', desc: 'define operator associativity and precedence'}
---

## FORMS
```
op(Precedence, Associativity, Atom)

op(Precedence, Associativity, [Atom | Atoms ])
```
## DESCRIPTION

If `Precedence` is instantiated to an integer between 0 and 1200 then `Associativity` must be one of the indicators found in the Prolog Operator Types table below.  `Atom`, which will henceforth be parsed as an operator according to the assigned `Precedence` and `Associativity`, can be any atom, but it should not require the use of single quotes(') in order to be parsed.

<hr>
**Prolog Operator Types**

Indicator|Interpretation|
|----------|---------------|
| xfy | infix -- right associative | 
| yfx | infix -- left associative | 
| xfx | infix -- non-multiple | 
| fy | prefix -- multiple | 
| fx | prefix -- non-multiple | 
| yf | postfix -- multiple | 
| xf | postfix -- non-multiple | 


If `Precedence` is uninstantiated, but `Associativity` and `Atom` correspond to an existing operator, `Precedence` will be unified with the previously declared precedence of the operator. The default operator precedences and associativities can be found in the Default Operator Associativity and Precedence table below.

A number of convenience predicates are declared as prefix operators(fx) of precedence 1125 These include:

- `trace, module, use, export, dynamic`

- `cd dir ls edit vi`

<hr>
**Default Operator Associativity and Precedence**

Operator(s)|Associativity|Precedence|
|---------------|--------------|-----------|
| :- | fx and xfx | 1200 | 
| ?- | fx | 1200 | 
| --&gt; | xfx | 1200 | 
| ; | xfy | 1100 | 
| -&gt; | yfx | 1050 | 
|, | xfy | 1000 | 
| : | yfx | 950 | 
| + not | fy | 900 | 
| . | xfy | 800 | 
| spy nospy | fx | 800 | 
| =\\= | xfx | 700 | 
| == \\== | xfx | 700 | 
| @&lt;  @&gt;  @=&lt;  @&gt;= | xfx | 700 | 
| is | xfx | 700 | 
| =:= =\\= | xfx | 700 | 
| &lt; &gt; &nbsp;=&lt; &nbsp;&gt;= | xfx | 700 | 
| =.. | xfx | 700 | 
| + | yfx | 500 | 
| - | yfx | 500 | 
| \\/ \/\ | yfx | 500 | 
| / // div  &gt;  &gt;&lt;  &lt; | yfx | 400 | 
| mod | yfx | 300 | 
| + - \ | fy | 200 | 
| ^ | yfx | 200 | 

## EXAMPLES
```
?- read(X).
get to work on time.

get to work on time.
    ^Syntax error 
	'standard input', line 1: Fullstop (period) expected
?- op(200,yfx,to),op(200,yfx,on).

yes.
?- read(X).

get to work on time.

X=get to work on time 

yes.
?- display(get to work on time).
on(to(get,work),time)
yes.
```
## SEE ALSO

- {% include book.md id="bowen91"    sec="7.9" %}
- {% include book.md id="sterling86" sec="8.1" %}
- {% include book.md id="bratko86"   sec="3.3" %}
- {% include book.md id="clocksin81" sec="5.5" %}
