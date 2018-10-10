---
title: 'op/3'
predicates:
 - 'op/3' : define operator associativity and precedence
---
`op/3` — define operator associativity and precedence


## FORMS

op(Precedence, Associativity, Atom)

op(Precedence, Associativity, [Atom | Atoms ])


## DESCRIPTION

If Precedence is instantiated to an integer between 0 and 1200 then Associativity must be one of the indicators found in
[Table 9(_[Prolog,Operator,Types.]_)](href = start.htm)
. Atom can be any atom, but should not require the use of single quotes(') in order to be parsed.



Prolog Operator Types.




Indicator|Interpretation**|
|----------|---------------|
| xfy | infix -- right associative | 
| yfx | infix -- left associative | 
| xfx | infix -- non-multiple | 
| fy | prefix -- multiple | 
| fx | prefix -- non-multiple | 
| yf | postfix -- multiple | 
| xf | postfix -- non-multiple | 


If Precedence is uninstantiated, but Associativity and Atom correspond to an existing operator, Precedence will be unified with the previously declared precedence of the operator. The default operator precedences and associativities can be found in
[Table 9(_[Default,Operator,Associativity,and,Precedence.]_)](href = alshelpop3.htm#e8f17993)
.

A number of convenience predicates are declared as prefix operators(fx) of precedence 1125 These include :

- trace, module, use, export, dynamic

- cd dir ls edit vi






Operator(s)|Associativity|Precedence**|
|---------------|--------------|-----------|
| :- | fx and xfx | 1200 | 
| ? - | fx | 1200 | 
| -- &gt; | xfx | 1200 | 
| ; | xfy | 1100 | 
| - &gt; | yfx | 1050 | 
|, | xfy | 1000 | 
| : | yfx | 950 | 
| + not | fy | 900 | 
| . | xfy | 800 | 
| spy nospy | fx | 800 | 
| = \ = | xfx | 700 | 
| = = \ = = | xfx | 700 | 
| @ &lt; @ &gt; @ = &lt; @ &gt; = | xfx | 700 | 
| is | xfx | 700 | 
| = : = = \ = | xfx | 700 | 
| &lt; &gt; = &lt; &gt; = | xfx | 700 | 
| = .. | xfx | 700 | 
| + | yfx | 500 | 
| - | yfx | 500 | 
| \/ / \ | yfx | 500 | 
| / / / div &gt; &gt; &lt; &lt; | yfx | 400 | 
| mod | yfx | 300 | 
| + - \ | fy | 200 | 
| ^ | yfx | 200 | 


Table 9
Default Operator Associativity and Precedence.


## EXAMPLES

```
?- op(200,yfx,to),op(200,yfx,on).
yes.
```

```
?- display(gettoworkontime).
on(to(get,work),time)
yes.
```


## SEE ALSO

- [Bowen 91, 7.9]
- [Sterling 86, 8.1]
- [Bratko 86, 3.3]
- [Clocksin 81, 5.5]. 
