---
title: 'split_ll_by_blank/3'
package: alslib
group: Lists
module: builtins
predicates:
- {sig: 'split_ll_at_start/5', desc: 'splits a list of atoms according to an initial subatom'}
- {sig: 'split_ll_by_blank/3', desc: 'splits a list of atoms by the first null atom'}
---
## FORMS

`split_ll_at_start(Lines, SplitInitSeg, Head, Tail, SplitterLine)`

`split_ll_by_blank(Lines, Head, Tail)`

## DESCRIPTION

**`split_ll_at_start/5`** If Lines is a list of atoms (or UIAs), and if SplitInitSeg is an atom,  
    then, if there is any element of Lines with SplitInitSeg at its initial segment,  
    then:  
    -	SplitterLine is the first such line;  
    -	Head is the initial sublist of Lines up to but not including SplitterLine;  
    -	Tail is the sublist of Lines following SplitterLine  
    If there is no such line, then:  
    -	SplitterLine = ''  
    -	Head = Lines  
    -	Tail = []  

**`split_ll_by_blank/3`** If Lines is a list of atoms (or UIAs), then, if '' belongs to Lines,  
    then:  
    -	Head is the initial sublist of Lines up to the first occurrence of '';  
    -	Tail is the sublist of Lines following Head  
    If there is no such line, then:  
    -	Head = Lines  
    -	Tail = []  

{% comment %}
## EXAMPLES
{% endcomment %}

