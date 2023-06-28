---
---

# 2 Prolog Source Code
{:.no_toc}

* TOC
{:toc}

Just like most other computer languages, Prolog allows you to store programs and
data in text files. The builtin predicates consult/1, reconsult/1, read/1,
and their variants will translate the textual representation for programs and data into
the internal representations used by the Prolog system. This section describes what
kinds of syntactic objects can appear in source files and how they are interpreted.

## 2.1 Source Terms

Every Prolog source file must be a sequence of zero or more Prolog terms, each
term followed by a period (.) and a white space character. A period followed by a
white space character is called a full stop. Full stops are needed in source files to
show where one term ends and another begins. Each term in a file is treated as a
closed logical formula. This means that even though two separate terms have variable names in common, each term's variables are actually distinct from the variables in any other term. Most variables are quantified once for each term. However, there is a special variable, the anonymous variable (written '_ '), which is quantified for each occurrence. This means that within a single term, every occurrence
of '_' is a different variable.

### 2.1.1 Rules

A rule is the most common programming construct in Prolog. A rule says that a
particular property holds if a conjunction of properties holds. Rules are always written with :-/2 as the principal functor. The first argument of the :- is called the
head, and the second argument is called the body. Here are some examples of rules:

    a(X) :- b(X).  
    blt :- bacon, lettuce, tomato.  
    test(A,B,C) :- cond1(A,B), cond2(B,C).  

consult/1 and reconsult/1 load rules (and facts - see below) from a source
file into the internal run-time Prolog database in the order they occur in the source
file.

### 2.1.2 Facts

A fact is any term which is not a rule and which cannot be interpreted as a directive
or declaration. For example,

    module mymodule.

would not be interpreted as a fact since it is a module declaration - see [Chapter 3
Modules](3-Modules.html).  More specifically, a fact is any term that cannot be interpreted as a declaration and whose principal functor is not :-/1, ?-/1, or :-/2. One way to understand a fact is to say that it is a rule without a body, or a rule with a trivial body
(one that is always true). These are example facts:  

    mortal(socrates).  
    big(ben).  
    identical(X,X).  

As with rules, consult/1 and reconsult/1 load facts (and rules)from a source
file into the internal database in the order they occur in the source file.

### 2.1.3 Commands and Queries

A command or directive is any term whose principal functor is :-/1. Queries are
terms whose principal functor is ?-/1. The single argument to a command or query
is a conjunction of goals to be executed by the prolog engine.  Commands and queries can be submitted via the prolog shell, or can be included in a prolog program, to be executed whenever the program is loaded (consulted) by the engine.  

Commands and queries either succeed or fail depending on the program context in which they occur.  Commands are silent in all contexts unless they fail, or unless some subsidiary goal inside the command invokes a write on the current output stream.  When a command fails (either using the prolog shell or during consulting a file), a warning message is written to standard output, which is usually the screen or console window.

Like commands, when queries fail, a warning message is written to standard output.  However, when queries succeed, a success message is also written to standard output.  If a query contains unbound prolog variables and it is submitted via the prolog shell, the success message will describe values for those variables which were bound during the successful execution.

Commands are often used in prolog source files to require the consult or reconsult of other prolog source files, or to add operator declarations to the prolog parser.

Here are some examples of commands:

    :- op(300, xfx, #).
    :- [file1], [file2], write('Files 1&2 have been loaded').
    :- initializeProgram, topLevelGoal.

The prolog shell accepts queries from a terminal or window, and prints the results.  Because the shell assumes that each input is a query, it uses the query symbol ?- as a prompt.
Suppose the following program of simple facts has been loaded:

    wavelength(red, 620-750).
    wavelength(yellow, 570-590).
    wavelength(blue, 450-495).

Then the follow are some simple query interactions with this program using the shell:
```
?- wavelength(blue,W).

W=450-495 

yes.
?- wavelength(orange, W).

no.
?- wavelength(C, W).

C=red 
W=620-750 ;

C=yellow 
W=570-590 ;

C=blue 
W=450-495 ;

no.
```

Notice that when a success answer is printed, the shell waits for input from the user.  If the user types a semi-colon (;), the shell attempts to find further solutions for the query, and prints the next one if found, or "no." if no solutions are found.  Any input other than a semi-colon is interpreted to mean that no further solutions are required.

### 2.1.4 Declarations

Declarations are terms that have a special interpretation when seen by consult
or reconsult. Here are some example declarations:
```
use builtins.
export a/1, b/2, c/3.
module foobar.
```

## 2.2 Program Files
Program files are sequences of source terms that are meant to be read in by consult/1 or reconsult/1, which interpret the terms as either clauses, declarations, commands, or queries.

### 2.2.1 Consulting Program Files

To consult a file means to read the file, load the file's clauses into the internal Prolog database, and execute any commands or directives occurring in the file. Reconsulting a file causes part or all of the current definitions in the internal prolog database for
procedures which occur in the file to be discarded and the new ones (from the file) to be loaded, as well as executing any commands or directives in the file (again). See [Chapter 11 (Prolog Builtins: Non-I/O) consult/1](11-Prolog-Builtins-Non-I-O.html#119-program-and-system-management).

A file is consulted by the query or goal

    ?- consult(filename).    or    :-  consult(filename).

or reconsulted by the query or goal

    ?- reconsult(filename).    or    :- reconsult(filename).

Several files can be consulted or reconsulted at once by enclosing the file names in
list brackets, as in

    ?- [file1,file2,file3].

By default, files listed this way (inside list brackets) are _reconsulted_.

To insist that one or more files in such a list be consulted (which might cause some of the clauses
from the files to be doubled in memory), prefix a '+' to the filename, as in:

    ?- [file1,+file2,file3].

In this case, file2 will be consulted instead of reconsulted. For consistency and
backwards compatibility, one can also prefix a '-' to indicate that the file should be
reconsulted, even though this is redundant:

    ?- [file1,+file2,-file3].

When any of these consult goals are presented, first the terms from file1 are processed, then the terms from file2, and finally the terms from file3. It is permitted that clauses for the same procedure to occur in more than one file being consulted. In this case, clauses from the earlier file are listed in the internal database before clauses from the later file. Thus, if both file1 and file2 contain clauses
for procedure p, those from file1 will be listed in the internal database before
those from file2. 

Clauses in the internal database are 'tagged' with the file from which they originated. When a file is reconsulted, only those clauses in memory which are tagged as originating from that file will be discarded at the start of the reconsult operation. Thus, suppose that both file1 and file3 contain clauses for the procedure p, and that we initially perform

    ?- [file1,file2,file3].

Then suppose that we edit file3, and then perform

    ?- [file3].

The clauses for p originally loaded from file1 will remain undisturbed. The
clauses currently in memory for p originally from file3 will be discarded, and the
new clauses from file3 will be loaded.

### 2.2.2 Using Filenames in Prolog

Note: Complete path names to files are of course quite variable across operating
systems. The discussions below are only intended to describe those aspects of file
names and path names which affect how ALS Prolog locates files. Examples are
provided for all the operating systems supported by ALS Prolog. File names follow
the ordinary naming conventions of the host operating system. Thus all of the following are acceptable file names:
```
Linux (including Mac OS X):
    fighter  cave.man  hack/cave.man
    /usr/hack/cave.man

Win32:
    fighter  cave.man  hack\cave.man
    C:\usr\hack\cave.man
```

In general, file names should be enclosed in single quotes (making them quoted atoms). The exception is any file name which is acceptable as an atom by itself.

Simple file names consist of only the file name, or a file name together with an extension. All others are complex file names. Absolute path names provide a complete description of the location of a file in the file system, while relative path names provide a description of a file's location relative to the current directory. Simple file names are interpreted as relative path names.

The way that the program-loading predicates react to the different kinds of path names is described below. In general, the loading predicates attempt to determine whether a file exists, and if so, they load the clauses from the file. If the file does not exist, the loading predicates raise an error exception.

If an absolute path name is used as an argument to one of the program loading predicates (consult/1, reconsult/1, etc.), that file is loaded if it exists. If the file does not exist, an error exception is raised.

If a complex relative path name or a simple file name is passed to consult, the system first attempts to locate the file relative to the current directory. In particular, for a simple file name, the system simply looks in the current directory for the file.  In either case, if the file exists, it is loaded.
If the file cannot be found relative to the current directory, ALS Prolog searches for another directory containing that file. Ultimately, the directories (folders) through
which ALS Prolog searches are determined by a dynamic collection of facts

    searchdir/1 

maintained in the system (or builtins) module. 

Operationally, ALS Prolog forms the list PlacesToTry consisting of all directories D such that

    searchdir(D).

is true in the module builtins, putting the current directory at the head of this
list, even when no searchdir/1 assertion mentions it. Then ALS Prolog  works its way
through the elements D of PlacesToTry, attempting to locate the sought-for file
relative to directory D. The first file located in this manner is loaded. This process
is determinate: the system never restarts the search process once a file meeting the
relative path description has been found.

If none of the directories listed on PlacesToTry provide a path to the sought-for file,
ALS Prolog locates the alsdir subdirectory from its own installation, and attempts
to locate the file relative to two of the subdirectories, builtins and shared, which
are found in alsdir.  If none of these directories provides a means of locating a file with the the original complex relative path name or simple file name, the system raises an error exception.

The facts searchdir/1 in module builtins can be manipulated by a user program or by the user at the console. However, ALS Prolog provides several automatic facilities for installing these facts.

* On Linux (including Mac OS X) and Windows, if the ALSPATH environment variable is set , the
entries from this are used to create searchdir/1 assertions.
* If ALS Prolog was started from the command line, any '-s' switches on the command line will cause searchdir/1assertions to be added.

Thus, the directories which will be searched appear as follows:

1. First, the current directory is searched.
2. Next, any directories appearing as '-s' command line switches are searched, in the order they appear from left to right on the command line.
3. Next, any directories appearing in an ALSPATH environment variable are
searched, in the order they appear in the variable statement.
4. The subdirectory builtins of alsdir is searched.
5. The subdirectory shared of alsdir is searched.

Of course, if additional searchdir/1 have been asserted or retracted, this order
will be modified. Note, in particular, that searchdir/1 assertions for module
builtins can be included in an ALS Prolog autoload file.

### 2.2.3 How are Filename Extensions treated?

For your convenience, if you have a file ending with a .pro or a .pl extension, you
don't have to type the extension in calls to the program loading predicates. The
following goal loads the Prolog file wands.pro:

    ?- consult(wands).

What really happens is this. On a call to load a file (simple or complex) with no
extension, ALS Prolog first searches for a file with exactly that name. If found, that
file (with no .pro or .pl extension) is loaded. If no such file is found, then ALS Prolog
attempts to find a file of that name with a .pro extension, and following that, with
a .pl extension. Thus the example above will load wands.pro only if there is no
file wands to be found, not only in the current directory, but also in the directories
on the search path described above.

Whenever ALS Prolog loads a Prolog source file, it compiles the file and immediately loads and links the resulting code in memory. If the source file had a .pro extension, but the call to load it omitted the .pro extension, ALS  Prolog also creates a file on the disk containing a relocatable object version of the compiled code. On all operating systems, if the source file had a .pro extension, but the call to load the file omitted the .pro extension, a file with the same name, but the extension .obp is
created to hold the relocatable object code. Once a relocatable object file has been
created, any call to load the original file will cause the relocatable object file to be
loaded instead, provided that the original source file has not been modified since the object file was created. (This is determined by the date-time stamps on the two
files.) The advantage of this lies in the fact that object files load much more quickly
than source files. Note that on all systems, the following call will _not_ create an object file for wands:

    ?- consult('wands.pro').

Thus, when consulting or reconsulting a file with no extension, ALS Prolog proceeds as follows:

1. The system will first look for the file without any extension; if found, it will
load the file as is and will not create an object file.
2. If the file is not found, the system will then attach the extensions .pro and
.obp and look for both of these files.
3. If a .obp version of the file exists and is newer than the .pro version, then
the .obp version is loaded.
4. On the other hand if the .pro version is newer, then the .pro version is loaded and a new .obp version is created (which is now newer than the .pro version).

For the system to correctly decide which file is newer, .pro or .obp, the system date and time should always be set correctly.

The directories in which Prolog files reside should be writeable by ALS Prolog so that .obp versions of Prolog source files can be produced. ALS Prolog has facilities for controlling where these *.obp files are placed, and correspondingly, where they are searched for when (re-)loading files.

### 2.2.4 Splitting up Prolog Programs

It is common practice to place lines of the form

    :- [file1,file2].

in files which are being consulted. This will cause both file1 and file2 to also be
consulted. For both the consult and reconsult operations, this directive behaves as
if the text for file1 and file2 was placed in the file being consulted at the place
where the command occurred. This facility is similar to the #include facility found
in C. A full description of all predicates for loading programs can be found on the
builtins reference page for consult/1.

## 2.3 Preprocessor Directives

Assume that PFile is the name of a file being consulted into ALS Prolog. If <Filename> is the name of another valid Prolog source file, then the effect of the preprocessor directive

    #include “<Filename>”

is to textually include the lines of <Filename> into PFile as if they had actually
occurred in PFile at the point of the directive.

The conditional preprocessor directives #if, #else, #elif, and #endif behave more or less as they do for C programs. However, the expressions following the #if and #elif are taken to be Prolog goals, and are evaluated in the current environment, just as for embedded commands of the form :- G. Here are some
examples. Let f1.pro be the following file:
```
:-dynamic(z/1).
%z(f).
p(a).
#if (user:z(f))
p(b).
#else
p(c).
#endif
p(ff).
```
After consulting f1.pro, we use listing/0 to see what happened:
```
?- listing.
% user:p/1
p(a).
p(c).
p(ff).
yes.
```
In this case, p(c) was loaded, but not p(b). Now let f2.pro be the following file:
```
:-dynamic(z/1).
z(f).
p(a).
#if (user:z(f))
p(b).
#else
p(c).
#endif
p(ff).
```
After consulting f2.pro to a clean image, we obtain the following:
```
?- listing.
% user:p/1
p(a).
p(b).
p(ff).
% user:z/1
z(f).
```
This time, p(b) was loaded instead of p(c).
