---
title: 'make_lc/2'
package: ALS Library
group: Strings
module: builtins
predicates:
- {sig: 'asplit/4', desc: 'divides an atom as determined by a character'}
- {sig: 'asplit00/4', desc: 'divides a list of character codes as det. by a list of char codes'}
- {sig: 'atomic_to_uia/2', desc: 'create a UIA corresponding to an atomic item'}
- {sig: 'change_case_sym/2', desc: 'converts the case of characters in a symbol, based on the first'}
- {sig: 'char_in/3', desc: 'Locates the position of a character in an atom.'}
- {sig: 'cnvrt_to_UIA/2', desc: 'create a UIA corresponding to an arbitrary term'}
- {sig: 'convert_to_uc/2', desc: 'converts all items in a list of terms to uppercase'}
- {sig: 'head/4', desc: 'splits an list into segments determined by a character code'}
- {sig: 'head0/4', desc: 'splits a character code list into segments determined by a code'}
- {sig: 'make_lc/2', desc: 'converts a Prolog string to all lowercase character (codes)'}
- {sig: 'make_lc_sym/2', desc: 'converts an atom or UIA to all lowercase characters'}
- {sig: 'make_uc/2', desc: 'converts a Prolog string to all uppercase character (codes)'}
- {sig: 'make_uc_sym/2', desc: 'converts an atom or UIA to all uppercase characters'}
- {sig: 'read_to/5', desc: 'splits a string according to one of several possible chars'}
- {sig: 'read_to_blank/3', desc: 'splits a string around the leftmost occurrence of blank'}
- {sig: 'replace_char_atom/4', desc: 'Replace occurrences of a char in an atom by another char'}
- {sig: 'replace_char_string/4', desc: 'Replace occurrences of a char in a string by another char'}
- {sig: 'same_uc/2', desc: 'Term1,Term2 unify after converting all characters to upper case'}
- {sig: 'string_to_sized_uia/3', desc: 'creates a UIA containg chars corresponding to a string'}
- {sig: 'string_to_uia/2', desc: 'creates a UIA corresponding to an arbitrary string'}
- {sig: 'string_to_uia/3', desc: 'insert list of char (codes) into a UIA '}
- {sig: 'strip_both_white/2', desc: 'strips leading and trailing white space chars from a string'}
- {sig: 'strip_both_white_atom/2', desc: 'strips leading and trailing white space chars from a prolog atom'}
- {sig: 'strip_tail_white/2', desc: 'strips trailing white space  from a string'}
- {sig: 'strip_white/2', desc: 'strips leading white space from a string'}
- {sig: 'truncate/3', desc: 'creates a UIA truncating the input expression'}
---
## FORMS

`asplit(Atom,Splitter,LeftPart,RightPart)`

`asplit00(String,SplitList,LeftPartCs,RightPartCs)`

`atomic_to_uia(Atom, UIABuf)`

`change_case_sym(InSym, OutSym)`

`char_in(Atom, Char, Pos)`

`cnvrt_to_UIA(Term, UIABuf)`

`convert_to_uc(Items, UCItems)`

`head(Atom,Splitter,Head,Tail)`

`head0(List,Splitter,Head,Tail)`

`make_lc(Cs, LCs)`

`make_lc_sym(InSym, LCSym)`

`make_uc(Cs, UCs)`

`make_uc_sym(InSym, UCSym)`

`read_to(Chars, Stoppers, Head, Tail,Stopper)`

`read_to_blank(Chars, Head, Tail)`

`replace_char_atom(AtomIn, OrigCharNum, NewCharNum, AtomOut)`

`replace_char_string(InString, OrigCharNum, NewCharNum, OutString)`

`same_uc(Term1, Term2)`

`string_to_sized_uia(Size, Chars, UIA)`

`string_to_uia(String, UIA)`

`string_to_uia(Chars, Pos, UIA)`

`strip_both_white(String, Result)`

`strip_both_white_atom(Atom, ResultAtom)`

`strip_tail_white(String, Result)`

`strip_white(String, Result)`

`truncate(InField, MaxSize, OutField)`

## DESCRIPTION

**`asplit/4`** If Atom is any atom or UIA, and if Splitter is the character code of  
    of a character, then, if the character with code Splitter occurs in  
    Atom, LeftPart is an atom consisting of that part of Atom from the  
    left up to but not including the leftmost occurrence of the character  
    with code Splitter, and RightPart is the atom consisting of that  
    part of Atom extending from immediately after the occurrence of  
    the character with code Splitter, to the end of Atom.  

**`asplit00/4`** If String and SplitList are both lists of character codes,  
    and if any code on SplitList also occurs on String, then LeftPartCs  
    consists of all those codes of String up to, but not including,  
    the leftmost occurrence of any code CS on SplitList, and RightPartCs  
    consist of all codes on String following CS.  

**`atomic_to_uia/2`** Allocates a new UIA buffer UIABuf of smallest allowed size  
    greater than length(Atom), and copies Atom into UIABuf  
    beginning at position 0.  

**`change_case_sym/2`** Changes the case of characters in a symbol InSym as follows:  
    a) If the first character of InSym is lowercase, applies make_uc  
    to change all characters of InSym to uppercase;  
    b) If the first character of InSym is uppercase, applies make_lc  
    to change all characters of InSym to lowercase.  

**`char_in/3`** If Atom is an atom, and Char is a quoted character, then:  
    a) If Char occurs in atom, then Pos is the position of the  
    leftmost occurrence of Char, counting from 1.  
    b) Fails if Char does not occur in Atom.  

**`cnvrt_to_UIA/2`** If term_codes(Term, Codes) holds, then allocates  
    allocates a new UIA buffer UIABuf of smallest allowed size  
    >= length(Codes) and copies Codes into UIABuf begninning  
    at position 0.  

**`convert_to_uc/2`** If Items is a list of terms including atoms, numbers and  
    compound terms, then UCItems will be the corresponding list,  
    in order, of terms where atoms are converted to uppercase  
    (make_uc is applied), numbers and variables are left unchanged,  
    and for any compound term, it's functor and args are are  
    converted by recursively applying convert_to_uc.  

**`head/4`** If Atom is a list of character codes, splits Atom into Head and  
    Tail similar to the way asplit would for the list of chars,, using  
    the leftmost occurrence of Splitter. On successive retrys, uses the  
    succeeding occurrences of Splitter in Tail as the split point.  

**`head0/4`** If List is a list of character codes, splits List into Head and  
    tail the way asplit0 would, using the leftmost occurrence of  
    Splitter. On successive retrys, uses the succeeding occurrences  
    of Spliter in Tail as the split point, effectively breaking  
    List into segments determined by Splitter.  

**`make_lc/2`** If Cs is a list of character codes, then LCs is the list of  
    codes corresponding to Cs, with every uppercase code converted  
    to the corresponding lowercase code.  

**`make_lc_sym/2`** If InSym is a symbol, then LCSym is that symbol consisting of  
    the same characters, in order, as InSym, except that all  
    uppercase symbols will have been converted to lowercase.  

**`make_uc/2`** If Cs is a list of character codes, then UCs is the list of  
    codes corresponding to Cs, with every lowercase code converted  
    to the corresponding uppercase code.  

**`make_uc_sym/2`** If InSym is a symbol, then UCSym is that symbol consisting of  
    the same characters, in order, as InSym, except that all  
    lowercase symbols will have been converted to uppercase.  

**`read_to/5`** If:  
    -Chars is a prolog string;  
    -Stoppers is a list of codes of chars  
    and if intersect(Chars,Stoppers) \= [],  
    Then:  
    Stopper is the element of Stopper with leftmost occurrence in Chars,  
    Head is the portion of Chars to the left of the occurrence of Stopper,  
    and  
    Tail is the portion of Chars to the right of the occurrence of Stopper.  

**`read_to_blank/3`** If:  
    -Chars is a prolog string containing at least one blank,  
    Then:  
    Head is the portion of Chars to the left of the first occurrence  
    of a blank, and  
    Tail is the portion of Chars to the right of the first occurrence  
    of a blank  

**`replace_char_atom/4`** Creates AtomOut by replacing all occurrences (possibly zero) of  
    OrigCharNum in AtomIn by NewCharNum.  

**`replace_char_string/4`** Creates OutString by replacing all occurrences (possibly zero) of  
    OrigCharNum in InString by NewCharNum.  

**`same_uc/2`** Applies convert_to_uc/2 to both Term1,Term2, and tests  
    whether the results unify.  

**`string_to_sized_uia/3`** If Size is an integer, and if Chars is a list of character codes,  
    then:  
    a) if Size >= length(Chars), allocates a (new) UIA and  
    copies Chars into UIA starting at position 0.  
    b) if Size < length(Chars), fails.  

**`string_to_uia/2`** Given a String of arbitrary characters, UIA will be a uia  
    containing exactly the same characters, in order.  

**`string_to_uia/3`** If Chars is a list of charater codes of length L, if UIA is a  
    uia, if Pos is a positive integer =< length(UIA), then:  
    This predicate attempts to inserts the characters (in order)  
    corresponding to Chars into UIA beginning at position Pos  
    (counting from 0 at the beginning of UIA).  This succeeds if  
    length(UIA) - Pos >= L.  If length(UIA) - Pos < L, thisfails;  
    however, note that this call will have descructively modified  
    UIA to insert the first length(UIA) - Pos Chars.  

**`strip_both_white/2`** Removes all leading and trailing whitespace chars (space, tab)  
    from the input String to produce Result.  

**`strip_both_white_atom/2`** Removes all leading and trailing whitespace chars (space, tab)  
    that occur in the input symbol.  

**`strip_tail_white/2`** Removes all trailing whitespace chars (space, tab) from the  
    input String to produce Result.  

**`strip_white/2`** Removes all leading whitespace chars (space, tab) from the  
    input String to produce Result.  

**`truncate/3`** Obtains the list of character codes corresponding to the  
    input expression, truncates that list at MaxSize, allocates  
    a UIA of that size, and copies the list of character codes  
    into that UIA.  

## EXAMPLES

