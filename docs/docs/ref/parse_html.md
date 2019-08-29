---
title: 'grab_pxml/2'
package: ALS Library
group: Web
module: pxml
predicates:
- {sig: 'grab_pxml/2', desc: 'reads the pxml term found in file Path'}
- {sig: 'grab_pxml_with_tagged/3', desc: 'read PXML term in FilePath, including tagged components '}
- {sig: 'grab_pxml_with_paths/5', desc: 'read PXML term in FilePath, tagged component tags and paths'}
- {sig: 'parse_html_toks_to_pxml_vals/3', desc: 'parse a list of HTML-tokens'}
- {sig: 'parse_html_toks_to_pxml/5', desc: 'parse a list of HTML-tokens'}
- {sig: 'read_pxml_term/7', desc: '- read a PXML term out of Tokens'}
- {sig: 'read_pxml_comment/3', desc: 'read an HTML comment into PXML'}
- {sig: 'unary_tag/1', desc: 'specifies syntactic roles tags'}
---
## FORMS

`grab_pxml(Path, PXML)`

`grab_pxml_with_tagged(FilePath, PXML, TagVals)`

`grab_pxml_with_paths(FilePath, PXML, TagVals, TgtTags, Paths)`

`parse_html_toks_to_pxml_vals(Tokens, PXML, TagVals)`

`parse_html_toks_to_pxml(Tokens, Terms, StackIn, StackOut, TagsValsDList)`

`read_pxml_term(Tokens, Term, RestTokens, StackIn, StackOut,`

`read_pxml_comment(Tokens, Features, RestTokens)`

`unary_tag(T)`

## DESCRIPTION

**`grab_pxml/2`** Calls `grab_html_tokens/2` to read the list L of HTML tokens out
    of Path, and then parses L into a (single !doctype) PXML term.

**`grab_pxml_with_tagged/3`** Calls `grab_html_tokens/2` to read the list L of HTML tokens out
    of FilePath, and then parses L into a (single !doctype) PXML term,
    where it accumulates tags of component terms in TagVals, with
    the tagged terms accumulated in (lists) on TagVals.

**`grab_pxml_with_paths/5`** Calls `grab_html_tokens/2` to read the list L of HTML tokens out
    of FilePath, and then parses L into a (single !doctype) PXML term,
    the the tagged terms (eqns)  accumulated in (lists) on TagVals,
    and also accumulates pairs (Stack, Term) on Paths, where:<br>
    1) TgtTags is a list of HTML tags,<br>
    2) Stack is list [Tg1, Tg2 | ...] of HTML terms representing
    the reversed parser stack with Tg1 belonging to TgtTags, and<br>
    3) Term was parsed out as Tg1 was popped from Stack.

**`parse_html_toks_to_pxml_vals/3`** Calls parse_html_toks_to_pxml/5, ignoring the Stack arguments.

**`parse_html_toks_to_pxml/5`** The workhorse. Parses a list of HTML-tokens, as produced by
    `read_tokens/5` in `html_tokens.pro`, into a list of Prolog Terms
    consituting a PXML representation of the source.
    The pair (StackIn, StackOut) implements the parser stack.<br>
    The difference list<br>
    <br>
    &emsp; 		TagsValsDList<br>
    <br>
    provides a means of capturing components of the PXML output. ( See the
    comment for `handle_tag/6` for a description of TagsValsDList. )

**`read_pxml_term/7`** Reads the (largest) PXML term possible starting at the
    beginning of Tokens.

**`read_pxml_comment/3`** Read from Tokens an HTML comment into Features,
    leaving RestTokens.

**`unary_tag/1`** Syntactic roles of tags:<br>
    Spec rules about optional tags:<br>
    &emsp;    https://html.spec.whatwg.org/multipage/syntax.html#optional-tags<br>
    See also:<br>
    &emsp;    https://html.spec.whatwg.org/multipage/syntax.html<br>
    &emsp;    https://html.spec.whatwg.org/multipage/parsing.html<br>
    unary_tag/1 is exported for use by pxml_utils.pro.

## EXAMPLES

