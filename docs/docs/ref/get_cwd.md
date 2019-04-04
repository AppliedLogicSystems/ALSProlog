---
title: 'get_cwd/1'
predicates:
- {sig: 'canon_path/2', desc: 'canonicalizes a path name'}
- {sig: 'change_cwd/1', desc: 'change the current working directory'}
- {sig: 'filename_equal/2', desc: 'OS portable check for equality of file names'}
- {sig: 'getDirEntries/3', desc: 'returns the file in a directory which matches a pattern'}
- {sig: 'get_cwd/1', desc: 'returns the current working directory'}
- {sig: 'must_exist_file/1', desc: 'raises a system_error if exists_file fails'}
- {sig: 'remove_file/1', desc: 'removes a file from the current working directory'}
---

## FORMS
```
canon_path(SrcPath,CanonPath)

change_cwd(NewDir)

filename_equal(Name1, Name2)

getDirEntries(Path, FilePattern, FilesList)

get_cwd(Path)

must_exist_file(FileName)

remove_file(FileName)
```
## DESCRIPTION

**`canon_path/2`** If `SrcPath` is a path name, either to a file or to a directory,
    `CanonPath` is a canonicalized version of that path name, in the
    sense that all symbolic links  in the path (to either subdirs
    or the file at the end) are dereferenced out.

**`change_cwd/1`** If `NewDir` is a (quoted) atom representing an existing
    path in the filesystem, this predicates changes the 
    current working directory being used by the program
    to become `NewDir`.  Under DOS or Windows, this won't change the drive. 


**`filename_equal/2`** Checks, in an OS portable way, whether or not `Name1` and `Name2`
    specify the same file.


**`getDirEntries/3`** If Path is a (quoted) atom representing a path to a
    folder (directory) in the file system, and if FilePattern
    is a pattern (possibly using *), then FilesList is the
    list of all files in folder Path (possibly including subfolders)
    which match FilePattern. 


**`get_cwd/1`** Obtains the current working directory being used by the program
    as a quoted atom, and unifies it with `Path`. Under DOS or Windows, the drive is included.


**`must_exist_file/1`** If FileName is a (quoted) atom representing a possible entry
    in the file system, calls `exists_file/1` to determine if FileName exists.  If FileName
    does not exist, raises a system error (while `exists_file/1` simply fails). 


**`remove_file/1`** If FileName is a (quoted) atom naming a file in the
    current working directory, removes that file. 


## EXAMPLES

```
?- get_cwd(Path).

Path='/Users/ken' 

yes.
?- change_cwd('./Documents').

yes.
?- get_cwd(Path).

Path='/Users/ken/Documents' 

yes.
?- getDirEntries('./', 'b*', FilesList).

FilesList=[bibliographies,'bird-of-prey.jpg','bp-sageMath.pdf'] 

yes.

?-  must_exist_file('./zpper.foo').
Error: System error: must_exist_file('./zpper.foo')
- must_exist_file: './zpper.foo'
- Throw pattern: error(system_error,[must_exist_file('./zpper.foo')])
```

{% comment %}
## ERRORS
{% endcomment %}

