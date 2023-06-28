---
title: 'read_tokens/2'
package: ALS Library
group: Web
module: pxml
predicates:
- {sig: 'tokenize_file/2', desc: 'extracts all Tokens from the File'}
- {sig: 'grab_html_tokens/2', desc: 'extracts all Tokens from the File'}
- {sig: 'read_tokens/2', desc: 'extracts all Tokens from a stream'}
---
## FORMS

`tokenize_file(File, Tokens)`

`grab_html_tokens(Path, Tokens)`

`read_tokens(S, Tokens)`

## DESCRIPTION

**`tokenize_file/2`** Opens a stream to File, and uses `read_tokens/2` to read all
    tokens found in File into the list Tokens.

**`grab_html_tokens/2`** Just like `tokenize_file/2`, but wraps read_tokens in unwind_protect

**`read_tokens/2`** S is a read-stream to a potential source of tokens;
    Reads all tokens out of stream S into list Tokens by invoking
    the workhorse `read_tokens/5`.

## EXAMPLES

