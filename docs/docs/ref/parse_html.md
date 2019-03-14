---
title: 'grab_pxml/2'
package: alslib
group: Web
module: pxml
predicates:
- {sig: 'grab_pxml/2', desc: 'reads the pxml term found in file Path'}
- {sig: 'grab_pxml_with_paths/5', desc: 'read PXML term in FilePath, tagged component tags and paths'}
- {sig: 'grab_pxml_with_tagged/3', desc: 'read PXML term in FilePath, including tagged components '}
- {sig: 'parse_html_toks_to_pxml/5', desc: 'parse a list of HTML-tokens'}
- {sig: 'parse_html_toks_to_pxml_vals/3', desc: 'parse a list of HTML-tokens'}
- {sig: 'read_pxml_comment/3', desc: 'read an HTML comment into PXML'}
- {sig: 'read_pxml_term/7', desc: '- read a PXML term out of Tokens'}
- {sig: 'unary_tag/1', desc: 'specifies syntactic roles tags'}
---
## FORMS

`grab_pxml(Path, PXML)`

`grab_pxml_with_paths(FilePath, PXML, TagVals, TgtTags, Paths)`

`grab_pxml_with_tagged(FilePath, PXML, TagVals)`

`parse_html_toks_to_pxml(Tokens, Terms, StackIn, StackOut, TagsValsDList)`

`parse_html_toks_to_pxml_vals(Tokens, PXML, TVals)`

`read_pxml_comment(Tokens, Features, RestTokens)`

`read_pxml_term(Tokens, Term, RestTokens, StackIn, StackOut,`

`unary_tag(T)`

## DESCRIPTION

**`grab_pxml/2`** Calls `grab_html_tokens/2` to read the list L of HTML tokens out  
    of Path, and then parses L into a (single !doctype) PXML term.  

**`grab_pxml_with_paths/5`** Calls `grab_html_tokens/2` to read the list L of HTML tokens out  
    of FilePath, and then parses L into a (single !doctype) PXML term,  
    where it accumulates tags of component terms in MTags, with  
    the tagged terms accumulated in (lists) on TagVals, and  
    also accumulates pairs (Stack, Term) on Paths, where:  
    1) TgtTags is a list of HTML tags,  
    2) Stack is list [Tg1, Tg1 | ...] of HTML terms representing  
    the reversed parser stack with Tg1 belonging to TgtTags, and  
    3) Term was parsed out as Tg1 was popped from Stack.  

**`grab_pxml_with_tagged/3`** Calls `grab_html_tokens/2` to read the list L of HTML tokens out  
    of FilePath, and then parses L into a (single !doctype) PXML term,  
    where it accumulates tags of component terms in MTags, with  
    the tagged terms accumulated in (lists) on MTagVals.  

**`parse_html_toks_to_pxml/5`** The workhorse. Parses a list of HTML-tokens, as produced by  
    `read_tokens/5` in html_tokens.pro, into a collection of Prolog Terms  
    consituting a PXML representation of the source.  
    The pair (StackIn, StackOut) implements the parser stack.  
    The difference list  
    TagsValsDList  
    provides a means of capturing components of the PXML output.  
    ----  
    MTags is a list of non-comment tags.  Often, MTags = [body].  
    MTagVals is the list of corresponding PXML terms found, if any.  
    So if MTags = [body, table], we might have MTagVals = [body=Body],  
    if there were no table, and  
    MTagVals = [body=Body, table=[list of PXML table terms] ]  
    if there was more than one table.  
    ----  
    If (MTags, MTagVals) was to be big, it could be carried as  
    ani AVL tree.  But moderate examples will probably be typical.  

**`parse_html_toks_to_pxml_vals/3`** parse a list of HTML-tokens

**`read_pxml_comment/3`** Read from Tokens an HTML comment into Features,  
    leaving RestTokens.  

**`read_pxml_term/7`** Reads the (largest) PXML term possible starting at the  
    beginning of Tokens.  

**`unary_tag/1`** Syntactic roles of tags:  
    Spec rules about optional tags:  
    https://html.spec.whatwg.org/multipage/syntax.html#optional-tags  
    See also:  
    https://html.spec.whatwg.org/multipage/syntax.html  
    https://html.spec.whatwg.org/multipage/parsing.html  
    `unary_tag/1` is exported for use by pxml_utils.pro.  

## EXAMPLES

