---
title: 'split_ll_by_blank/3'
package: ALS Library
group: Lists
predicates:
- {sig: 'split_ll_at_start/5', desc: 'splits a list of atoms according to an initial sub-atom'}
- {sig: 'split_ll_by_blank/3', desc: 'splits a list of atoms by the first null atom'}
---
## FORMS

`split_ll_at_start(Lines, SplitInitSeg, Head, Tail, SplitterLine)`

`split_ll_by_blank(Lines, Head, Tail)`

## DESCRIPTION

**`split_ll_at_start/5`** If Lines is a list of atoms (or UIAs), and if SplitInitSeg is an atom,
    then, if there is any element of Lines with SplitInitSeg at its initial segment,
    then:<br>
    SplitterLine is the first such line;<br>
    Head is the initial sublist of Lines up to but not including SplitterLine;<br>
    Tail is the sublist of Lines following SplitterLine<br>
    If there is no such line, then:<br>
    SplitterLine = ''<br>
    Head = Lines<br>
    Tail = []

**`split_ll_by_blank/3`** If Lines is a list of atoms (or UIAs), then, if '' belongs to Lines,
    then:<br>
    Head is the initial sublist of Lines up to the first occurrence of '';<br>
    Tail is the sublist of Lines following Head<br>
    If there is no such line, then:<br>
    Head = Lines<br>
    Tail = []

## EXAMPLES

