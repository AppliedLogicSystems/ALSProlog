---
title: 'file_extension/3'
group: File System
predicates:
- {sig: 'file_extension/3', desc: 'Access or add the extension for a filename.'}
- {sig: 'path_directory_tail/3', desc: 'Compose or decompose a path to a file in a directory.'}
- {sig: 'is_absolute_path/1', desc: 'Determines whether Path begins at the file system root.'}
- {sig: 'tilda_expand/2', desc: 'Expands a tilda-path to an absolute file system path.'}
- {sig: 'make_change_cwd/1', desc: 'Creates path P to a directory, as needed, and changes to it.'}
- {sig: 'pathPlusFile/3', desc: 'Compose/decompose a path with a terminating file.'}
- {sig: 'pathPlusFilesList/3', desc: 'attaches a path to each of a list of file names'}
- {sig: 'same_path/2', desc: 'determines whether two file paths are the same'}
- {sig: 'same_disk/2', desc: 'determines whether two disks are the same'}
---
## FORMS

`file_extension(FullName, Name, Ext)`

`path_directory_tail(Path, Directory, Tail)`

`is_absolute_path(Path)`

`tilda_expand(TildaPath, Path)`

`make_change_cwd(P)`

`pathPlusFile(Path, File, PathAndFile)`

`pathPlusFilesList(SourceFilesList, Path, ExtendedFilesList)`

`same_path(Path1, Path2)`

`same_disk(Disk1, Disk2)`

## DESCRIPTION

**`file_extension/3`** If FullName is instantiated, decomposes it to yield the
    underlying Filename (unified with Name)  and extension (unified
    with Ext.  If FullName is instantiated but has no extension, then
    Name is unified with FullName and Ext is unified with ''.
    If FullName is uninstantiated and both Name, Ext are instantiated,
    composes the filename plus extension out of Name, Ext and unifies
    that with FullName.

**`path_directory_tail/3`** If Path is uninstantiated, then joins Directory with Tail, and
    unifies the result with Path.
    If Path is instantiated, removes the last element from Path,
    leaving Not-Last, unifies the last element with Tail, and:
    if Not-Last is empty, unifies Directory with '.', else
    unifies Directory with Not-Last.

**`is_absolute_path/1`** Succeeds if Path begins at the file system root, fails otherwise.

**`tilda_expand/2`** Replaces the leading tilde (~) appropriately.

**`make_change_cwd/1`** If P is instantiated to a legal path to a directory, then:
    If the directory exists, executes change_cwd(P).
    Otherwise, creates the directory, including all intermediate
    directories, and then executes change_cwd(P).

**`pathPlusFile/3`** If PathAndFile is uninstantiated, adds File to the end of
    Path and unifies that with PathAndFile.  If PathAndFile is
    instantiated, removes the final element from PathAndFile,
    unifies the remainder with Path, and unifies that last
    element with File.

**`pathPlusFilesList/3`** If SourceFilesList is list of items denoting files, and
    if Path denotes a path, creates a list of atoms which
    consist of the Path prepended to each of the file names.

**`same_path/2`** If Path1 and Path2 are two lists denoting file paths,
    determines whether they denote the same path, allowing
    for identification of uppercase and lowercase names
    as appropriate for the OS.

**`same_disk/2`** If Disk1 and Disk2 are atoms denoting disks, determines
    whether they are the same, allowing for identification
    of upper and lower case letters, as appropriate for the OS.

## EXAMPLES

**`file_extension/3`**
```
?- file_extension(foo, Name1, Ext1).
Name1=foo 
Ext1='' 
?- file_extension('foo.pro', Name2, Ext2).
Name2=foo 
Ext2=pro 
?- file_extension(FullName, bar, pro).
FullName='bar.pro' 
```

**`path_directory_tail/3`**
```
?- path_directory_tail(Path, 'mom/kids', 'bar/zip.pro').
Path == 'mom/kids/bar/zip.pro'
?- path_directory_tail('mom/kids/bar/zip.pro', 'mom/kids/bar', Tail).
Tail == 'zip.pro'
?- path_directory_tail('mom/kids/bar/zip.pro', Directory, 'zip.pro').
Directory == 'mom/kids/bar'
?- path_directory_tail('mom/kids/bar/zip.pro', Directory, Tail).
Directory == 'mom/kids/bar'
Tail='zip.pro'
?- path_directory_tail('zip.pro', Directory, Tail).
Directory='.'
Tail='zip.pro'
```

**`is_absolute_path/1`**
```
?- is_absolute_path('/foo/bar').
yes.
?- not(is_absolute_path('foo/bar')).
yes.
```

**`tilda_expand/2`**
```
?- tilda_expand('~/foo/bar.pro', Path).
Path='/Users/mike/foo/bar.pro'
```

**`pathPlusFile/3`**
```
?- pathPlusFile('foo/bar', 'zip.pro', PF).
PF='foo/bar/zip.pro' 
?- pathPlusFile(P,F,'foo/bar/zip.pro').
P='foo/bar' 
F='zip.pro'
```

**`pathPlusFilesList/3`**
```
?- pathPlusFilesList(['foo.pro','bar.pro','zip.pro'], 'mom/kids', EFL).
EFL=['mom/kids/foo.pro','mom/kids/bar.pro','mom/kids/zip.pro']
```

**`same_path/2`**
```
?- same_path(['/mOM','fIle1.pro'], ['/mOM','fIle1.pro']).
yes.
```

**`same_disk/2`**
```
?- same_disk('mYDiskA', 'mYDiskA').
yes.
```

