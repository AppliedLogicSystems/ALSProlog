---
title: 'gensym/2'
predicates:
 - 'gensym/2' : generates families of unique symbolsr
---
`gensym/2` `--` generates families of unique symbolsr


## FORMS

gensym(Prefix, Symbol)


## DESCRIPTION

If Prefix is a symbol(either an interned or uninterned atom), then Symbol is a new UIA which has not previously existed in the system and which involves Prefix as a subsymbol. . The string also involves the system time that the current ALS Prolog image was started, together with the value of a counter for these generated symbols. Consequently, the symbols are almost guaranteed to be unique across invokations of the system, execpt for the possibility of the system clock wrapping around.


## EXAMPLES

? - gensym(' &lt; Prefix &gt; ', Symbol) .


Symbol = ' \376 &lt; Prefix &gt; _839678026_0 '


? - gensym(airplane, X) .


X = ' \376airplane_839678026_1 '


