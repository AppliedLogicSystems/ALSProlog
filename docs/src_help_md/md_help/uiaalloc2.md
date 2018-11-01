---
title: '$uia_alloc/2'
predicates:
 - '$uia_alloc/2' : allocates a UIA of specified length
 - '$uia_size/2' : obtains the actual size of a UIA
 - '$uia_clip/2' : clip the given UIA
 - '$uia_pokeb/3' : modifies the specified byte of a UIA
 - '$uia_peekb/3' : returns the specified byte of a UIA
 - '$uia_pokew/3' : modifies the specified word of a UIA
 - '$uia_peekw/3' : returns the specified word of a UIA
 - '$uia_pokel/3' : modifies the specified long word of a UIA
 - '$uia_peekl/3' : returns the specified long word of a UIA
 - '$uia_poked/3' : modifies the specified double of a UIA
 - '$uia_peekd/3' : returns the specified double of a UIA
 - '$uia_pokes/3' : modifies the specified substring of a UIA
 - '$uia_peeks/3' : returns the specified substring of a UIA
 - '$uia_peeks/4' : returns the specified substring of a UIA
 - '$uia_peek/4' : returns the specified region of a UIA
 - '$uia_poke/4' : modifies the specified region of a UIA
 - '$strlen/2' : returns the length of the specified symbol
---
`$uia_alloc/2` — allocates a UIA of specified length

`$uia_size/2` — obtains the actual size of a UIA

`$uia_clip/2` — clip the given UIA

`$uia_pokeb/3` — modifies the specified byte of a UIA

`$uia_peekb/3` — returns the specified byte of a UIA

`$uia_pokew/3` — modifies the specified word of a UIA

`$uia_peekw/3` — returns the specified word of a UIA

`$uia_pokel/3` — modifies the specified long word of a UIA

`$uia_peekl/3` — returns the specified long word of a UIA

`$uia_poked/3` — modifies the specified double of a UIA

`$uia_peekd/3` — returns the specified double of a UIA

`$uia_pokes/3` — modifies the specified substring of a UIA

`$uia_peeks/3` — returns the specified substring of a UIA

`$uia_peeks/4` — returns the specified substring of a UIA

`$uia_peek/4` — returns the specified region of a UIA

`$uia_poke/4` — modifies the specified region of a UIA

`$strlen/2` — returns the length of the specified symbol


## FORMS

```
'$uia_alloc'(BufLen, UIABuf)
'$uia_size'(UIABuf, Size)
'$uia_clip'(UIABuf, Size)
'$uia_pokeb'(UIABuf, Offset, Value)
'$uia_peekb'(UIABuf, Offset, Value)
'$uia_pokew'(UIABuf, Offset, Value)
'$uia_peekw'(UIABuf, Offset, Value)
'$uia_pokel'(UIABuf, Offset, Value)
'$uia_peekl'(UIABuf, Offset, Value)
'$uia_poked'(UIABuf, Offset, Value)
'$uia_peekd'(UIABuf, Offset, Value)
'$uia_pokes'(UIABuf, Offset, Symbol)
'$uia_peeks'(UIABuf, Offset, Symbol)
'$uia_peeks'(UIABuf, Offset, Size, Symbol)
'$uia_peek'(UIABuf, Offset, Size, Value)
'$uia_poke'(UIABuf, Offset, Size, Value)
'$strlen'(Symbol, Size)
```

## DESCRIPTION

A call to `'$uia_alloc'(BufLen, UIABuf)` creates a UIA of the length specified by `BufLen`. `BufLen` should be instantiated to a positive integer which represents the size(in bytes) of the UIA to allocate; currently the maximum allowable value of `BufLen` is 1024 The actual size of the buffer allocated will be that multiple of four between `BufLen` + 1 and `BufLen` + 4 (UIAs are allocated on word boundaries and an extra byte is added to provide for zero termination of strings when UIAs are used for symbols.) `UIABuf` should be a variable. UIAs are initially filled with zeros, and will unify with the null atom (`''`).

The call `'$uia_size'(UIABuf, Size)` returns the actual size (in bytes) of the given UIA. If `Size` is less than or equal to the actual size of the given `UIABuf`, the call `'$uia_clip'(UIABuf, Size)` reduces the size of `UIABuf` by removing all but one of the trailing zeros (null bytes).

Single-byte values can be inserted into a UIA buffer using `'$uia_pokeb'/3`. The modifications are destructive, and do not disappear upon backtracking. These procedures can be used to modify system atoms (file names and strings that are represented as UIAs). However, this use is strongly discouraged. `UIABuf` should be a buffer obtained from `'$uia_alloc'/2`. Offset is the offset within the buffer to the place where `Value` is to be inserted. Both Offset and Value are integers. In `'$uia_pokeb'/3`, the buffer is viewed as a vector of bytes with the first byte having offset zero. The byte at `Offset` from the beginning of the buffer is changed to Value. The companion predicates `'$uia_pokew'/3`, `'$uia_pokel'/3`, `'$uia_poked'/3`, perform the corresponding operation on words, long words, and doubles, respectively.

`'$uia_peekb'/3` is used to obtain specific bytes from a UIA buffer created by `'$uia_alloc'/2`, or from any other UIA existing in the system. The parameters for these procedures are specified as follows: The arguments of `'$uia_peekb'/3` are interpreted in the same manner as the parameters for `'$uia_pokeb'/3`. The parameter `Symbol` must be a UIA or an atom. The parameter `Size` must be an integer. The companion predicates, `'$uia_peekw'/3`, `'$uia_peekl'/3`, `'$uia_peekd'/3`, perform the corresponding operation on words, long words, and doubles, respectively.

Like `'$uia_pokeb'/3`, `'$uia_pokes'/3` views the buffer as a vector of bytes with offset zero specifying the first byte. But instead of replacing just a single byte, `'$uia_pokes'/3` replaces the portion of the buffer beginning at `Offset` and having length equal to the length of `Symbol`, using the characters of `Symbol` for the replacement. If `Symbol` would extend beyond the end of the buffer, `Symbol` is truncated at the end of the buffer. The parameter `Symbol` must be an atom. The parameter `Size` must be an integer.

`'$uia_peeks'/3` binds Symbol to a UIA consisting of the characters beginning at position `Offset` and extending to the end of the buffer. `'$uia_peeks'/4` binds `Symbol` to a UIA consisting of the characters beginning at position `Offset` and extending to position `End` where `End` = `Offset` + `Size`. If `End` would occur beyond the end of the buffer, `Symbol` simply extends to the end of the buffer.

Provided that `Offset` and `Size` define a proper region within the given `UIABuf` (i.e., not including the final byte of `UIABuf`), `'$uia_poke'(UIABuf, Offset, Size, Value)` modifies the indicated region by copying characters from the given UIA (or symbol) `Value`. The size of the atom or UIA `Value` must be greater than or equal to `Size`. The region copied from `Value` is defined by offset 0 and `Size`.

Provided that `Offset` and `Size` define a proper region within the given `UIABuf` (i.e., not including the final byte of `UIABuf`), `'$uia_peek'(UIABuf, Offset, Size, Value)` extracts the indicated region from `UIABuf`, returning it as a new UIA `Value`.

When `Symbol` is a Prolog symbol (atom or UIA), `'$strlen'(Symbol, Size)` returns the length of the print name of that symbol (thus not counting the terminating null byte).


## EXAMPLES

```
copy_atom_to_uia(Atom, UIABuf) :-
  name(Atom, ExplodedAtom),
  copy_list_to_uia(ExplodedAtom, UIABuf).

copy_list_to_uia(Ints, UIABuf) :-
  length([_ | Ints ], BufLen),
  '$uia_alloc'(BufLen, UIABuf),

copy_list_to_uia(Ints, 0, UIABuf).
copy_list_to_uia([ ], _, _) :- !.
copy_list_to_uia([H | T ], N, Buf) :-
  '$uia_pokeb'(Buf, N, H),
  NN is N + 1,
  copy_list_to_uia(T, NN, Buf).
```

## SEE ALSO

- `atom_concat/3`
- `sub_atom/3`
- `atom_char/2