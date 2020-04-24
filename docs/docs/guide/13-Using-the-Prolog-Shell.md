---
---

# 13 Using the Prolog Shell
{:.no_toc}

* TOC
{:toc}

13 TTY Development Environment
The TTY Development interface for ALS Prolog is similar to the original DEC-10
system constructed in Edinburgh.
### 13.0.1 Starting up ALS Prolog
Starting up ALS Prolog varies from system to system. Under some systems such
as ordinary Unix shells or DOS, one starts ALS Prolog by typing a shell command
such as
```
C:> alspro
wizard% alspro
```
or
```
$ alspro
```
or
```
C:\> alspro
```
The various versions usually show a startup banner such as the following:
```
ALS-Prolog Version 1.7
Copyright (c) 1987-95 Applied Logic Systems
?-
```

### 13.0.2 Exiting Prolog
There are several ways to exit ALS Prolog. The normal way to exit is to submit the
goal
```
?- halt.
```
from the Prolog shell or from a Prolog program. The second way to exit can only
be accomplished from the top level of the Prolog shell. There, you can type a character (such as Control-D on Unix) or sequence. of characters (such as Control-Z
followed by a return on DOS. or # at the beginning of a line on the Mac) which signifies closing the default input stream. See halt/0 in the reference section. Finally, under the GUI or windowed interfaces, one can select an Exit menu button.

## 13.1 Asking Prolog to Do Something
The most common way of telling Prolog what you want it to do is to submit a goal.
If you are in the Prolog shell, and ?- is the prompt, then anything you type is considered to be a goal. A goal must end with a period, ' . ', followed by a white space
character (carriage return, blank, etc.). This is called a full stop. Goals must be correct Prolog terms. See Chapter 1 of the User Guide for a discussion concerning the
construction of correct Prolog terms. The following is an example of a goal submitted from the ALS Prolog shell:
```
?- length([a,b,c],Answer).
Answer = 3
```
The goal issued was length([a,b,c],Answer). The system responded by
showing that the variable Answer was instantiated with the number 3, and that the
goal succeeded. The Prolog user had to press the return key after the
```
Answer = 3
```
was displayed, in order to get the yes.printed. Not all goals succeed, of course.
For example the following goal fails:
```
?- length([a,b,c], 4).
no.
```
This goal fails because the list [a,b,c] is not four elements long.
After submitting a goal in the Prolog shell, if any variables have become instantiated, their values will be displayed to you as in the first example above with
length/2. When this occurs, the shell waits for you to type either a ';' followed
by a return, or just a return. The two choices have the following effect:
* ; — Forces the goal to fail, thus causing backtracking and retrying of
the goal.
* return — Causes the goal to succeed.
If a recursive data structure is created, such as is done by the following goal
```
?- X = f(X).
```
the part of the Prolog shell which prints answers will go into a loop, and continue
writing the data structure onto your screen until the structure gets too deep. When
this happens, an ellipsis ( ... ) is eventually displayed as shown below:
```
X = f(f(f(f(f(f(f(...)))))))
yes.
```
The actual depth of the structure shown by the answer printer is much deeper than
is shown above.
Goals can also be submitted from within a file. There are two forms of submitting
goals from files:
* Commands
* Queries
Commands are specified by the :- prefix, while queries are specified by the ?-prefix. The only difference between the two is that queries write the message
'yes.' to the screen if the goal succeeds, and 'no.' if the goal fails, while commands do not write any result on the screen.

## 13.1.2 Command Line Editing and History
The ALS Prolog TTY shell supports command line editing and history similar to that for the Linux Bash shell.  One can move left or right on the line using the left or right arrow keys, can delete characters using the delete key, can insert characters by typing or by selecting some text and using the ^V (control-v) key.  If the command history is turned on, the up arrow key allows you to walk backward through previously submitted commands.

The command line history keeping is turned on by placing 
```
	alspro_history_file_locn(LOCN).
```
in the .alspro startup file, in either the current directory, or in the HOME directory.  The history
file is named 
```
	.alspro_history
```
If LOCN is the atom local, then .alspro_history is stored in the current directory.  If LOCN is the atom home, then .alspro_history is stored in the HOME directory.

By default, the history for previous sessions is loaded at the start of each session (i.e., each start of alspro).  However, if
```
	no_load_prev_history.
```
occurs in the .alspro startup file, then the existing history file is NOT loaded when alspro starts.


## 13.2 How to Load Prolog Programs
There are basically two ways of loading Prolog programs into the ALS Prolog system:
1.  When you start alspro from the command line you can give a list of
files for the Prolog system to load as programs.
2.  If you want to load Prolog predicates from inside a program, or from the
Prolog shell, you can use the consult/1 builtin in the following manner:
    ?- consult(File).
where File is instantiated to a Prolog program's file name. Alternatively, one
can use
    ?- reconsult(File).
Finally, one can use the top-level list-as-reconsult construct:
```
?- [File1, File2,...].
```
For more information on how to use the consulting predicates, see [Section 2.2.1
(Consulting Program Files)](2-Prolog-Source-Code.html#221-consulting-program-files) in the User Guide.

## 13.3 Stopping a Running Prolog Program
If you wish to interrupt a running ALS Prolog program, simply press the interrupt
key for your system (e.g., the Control-C key on Linux, including Mac OS X, or the Control-Break key on DOS). You will be returned to the top level of the ALS Prolog shell.

## 13.4 How ALS Prolog Finds Prolog Files
When a request that a file be loaded is made (such as reconsult(myfile) ), ALS Prolog looks for the file in the following manner:  
### 13.4.1 Complex Pathnames
If the file is not a simple pathname, that is, any file with a 'file-slash' character ('/
') in it (on Unix or DOS), or the 'file-color' character (":") (on the Mac), the file
will be loaded as specified. Some examples are:
```
?- consult('/usr/gorilla/banana.pro').
Consulting /usr/gorilla/banana.pro...
.../usr/gorilla/banana.pro consulted.
yes.
```
### 13.4.2 Simple Pathnames
If the file name is a simple pathname, then the file will be searched for in several
directories, as follows:  
1.  The current directory is searched first;
2.  Next, the directories listed on the ALSPATH environment variable (if it is defined) are searched in order of their left-to-right appearance;
3.  Next, the directory named in the ALSDIR environment variable (if it is defined) is searched;
4.  Finally, the directory in which the current image resides is searched.

If the specified file is located in any of the indicated directories, it will be loaded
into ALS Prolog, and no further search is made for this file. (Thus, if multiple versions of a file exist in some of the indicated directories, only the first will be loaded.)
The following example uses the Unix C shell to illustrate the use of the ALS Prolog
pathlist. We assume for this example that ALS Prolog was installed in /usr/prolog.
Then the file dc.pro (which is one of ALS Prolog examples) will be contained in
the directory /usr/prolog/alsdir/examples. The following illustrates the use of
ALSPATH:
```
wizard%setenv ALSPATH /usr/prolog/alsdir/examples:/
usr/gorilla
wizard% alspro
ALS-Prolog Version 1.0
Copyright (c) 1987-90 Applied Logic Systems, Inc.
?- consult(dc).
Consulting dc...
.../usr/prolog/alsdir/examples/dc consulted.
yes.
```
The method used to implement the use of the ALSPATH pathlist actually provides
greater flexibility than the foregoing discussion indicates. When ALS Prolog is initialized, it reads the Unix ALSPATH environment variable (if it is defined), together
with the ALSDIR environment variable, and uses them to create a set of facts in the
builtins module. These facts all have the following form:  
    searchdir(".../.../").
The expression within the quotes can be any meaningful Unix path describing a directory (hence the terminal ('/'). At startup time, the assertions correspond to the
expressions found on the ALSPATH pathlist. Thus, the facts corresponding to the
example above would be:
```
searchdir("/usr/prolog/alsdir/examples/").
searchdir("/usr/gorilla/").
searchdir("/usr/prolog/alsdir/").
searchdir("/usr/prolog/").
```
Note that the current directory (or '.' to represent it) does not appear among these
facts; however, it is always automatically searched first. Additional entries can be
made to this collection of facts by using assert/1 (or asserta/1 or assertz/1). For example, the goal
    :-builtins:asserta(searchdir("chimpanzee/")).
would cause the subdirectory chimpanzee of the current directory to be searched
immediately after the current directory and before any other directories. And
    :-builtins:asserta(searchdir("../widget/")).
would cause the sibling subdirectory widget of the current directory to be searched
immediately after the current directory and before any other directories. Assertions
such as these can be placed in the ALS Prolog startup file (described below) to customize search paths for particular directories. See the User Guide for more information concerning loading files.

## 13.5 Controlling the Search Path
If you want to be able to consult some of your files that are not in your current directory, and you don't want to use absolute pathnames, you can put the directories
where those files reside on a path searchlist called ALSPATH. In addition, you can
add directories using the command-line switch -S at start-up time (see [Section 13.7
(ALS Prolog Command Line Options)](13-Using-the-Prolog-Shell.html#137-als-prolog-command-line-options)). The following is an example use of the ALSPATH variable on Linux or DOS:

    setenv ALSPATH /usr/eddie/programs:/usr/sue/src/prolog

If you want to also automatically search the ALS Prolog directory, you could use
the following:  
    setenv ALSPATH /usr/eddie/programs:/usr/sue/src/prolog:/usr/prolog/alsdir
The definition of the ALSPATH variable can also be placed in your .cshrc startup
file. Combining the examples above, your .cshrc startup file might include the
following lines:
```
setenv ALSPATH /usr/eddie/programs:/usr/sue/src/prolog:/usr/prolog/alsdir
```
On DOS, this would look like:
```
set ALSPATH /usr/eddie/programs:/usr/sue/src/prolog:/usr/prolog/alsdir
```
Note that even though it is running under DOS, ALS Prolog utilizes Unix-style directory separators.

As noted in Section 1.4, ALS Prolog
uses the value of the ALSPATH variable to create and assert facts for the predicate
builtins:searchdir/1. The predicate which processes this is called builtins:ss_init_searchdir/1. In fact, the reading and processing of ALSPATH, when it exists, is done as follows (in blt_shl.pro):
```
ss_init_searchdir
    :-getenv('ALSPATH',ALSPATH),
    ss_init_searchdir(ALSPATH).
```
What really happens in the code is that ss_init_searchdir/1 takes apart the
value it has obtained for ALSPATH, and produces a list of atoms representing the
individual directories in the path. It then calls a subsidiary predicate,
ss_init_searchdir0/1 which recurses down this list, asserting the fact
    builtins:searchdir(SDir)
for each atom SDir on the list. So on the Mac, there are two approaches. On the
one hand, one can directly make assertions on builtins:searchdir/1 as above to set
up the search path. Or, one can directly call ss_init_searchdir0/1 with an
appropriate argument. So one of the animals examples from the last section would
work like this:
    :-builtins:ss_init_searchdir0(['usr:prolog:alsdir:examples', 'usr:gorilla']).
Such a call can be placed in the Prolog startup file or in one of your source files to
occur automatically, as described in the next section.

## 13.6 Using the Prolog Startup File
When ALS Prolog starts up, it looks first in the current directory and then in your
home directory for a file named either .alspro (on Unix or the Mac) or alspro.pro
(on DOS). After the Prolog builtins are loaded the .alspro (or alspro.pro) file is
consulted if it exists. The purpose of the Prolog startup file is to allow you to automatically load various predicates and files which you routinely use, and to carry out
possible customizations of your environment such as the modifications to the standard search path described in the previous section.

## 13.7 ALS Prolog Command Line Options
There are a number of options that can be included on the operating system shell
command line when starting ALS Prolog. Any Prolog goals and terms appearing on
the command line may need to be quoted (with OS shell quotes) to avoid 
interpretation by the OS shell.  All switches which take an argument must be
separated from that argument by a space.  The following is a list of the options:
```
-s This switch must be followed by a space and a path to a directory. The path
is added to the searchdir/1 sequence. Multiple occurrences of -s with
a path may occur on the command line; the associated paths are processed
and added to the searchdir/1 facts in order corresponding to their left-toright
occurrence on the command line. All paths occurring with -s on the
command line are added to the searchdir/1 facts before any paths obtained
from the ALSPATH environment variable.
```
```
-g The option -g followed by an arbitrary Prolog goal, instructs ALS Prolog
to run the goal when it starts up as if it was the first goal typed to the 
Prolog shell after the system is started. The goal might have to be quoted 
depending on the rules of the operating system shell you are running in, and 
if the goal contains any of your shell's special characters. You do not have 
to put a full stop after a goal, and you can submit multiple goals, provided 
there is no white space anywhere in the given goals. When the submitted goal 
finishes running (with success or failure), control is passed to the normal 
Prolog shell unless the -b command line option has also been used, in which 
case control returns to the operating system shell.  
```
```
-b The option -b prevents the normal the Prolog shell from running. This
means control will return to the operating system shell when all command
line processing is complete, including processing of source files and 
execution of -g goals.
```
```
-q The option -q causes all standard system loading messages to be suppressed, 
including the banner. One of the uses of -q is to permit you to use
ALS Prolog as a Unix filter. Note that this does not turn off prompts issued
by the Prolog shell.
```
```
-v The option -v turns on verbose mode. This causes all system loading 
messages,  including some which are normally suppressed, to be printed.
```
```
-A, -a This switch must be followed by a space and a Prolog Goal ( enclosed in 
single quotes if necessary to defeat the OS shell) and is used to force one or
more assertions, as follows:  
If Goal is of the form M:(H1, ...Hk), then each of H1, ..., Hk is asserted in 
module M. Thus,
    alspro -A 'ice_cream:(jerry, ben)'
would cause the two facts facts jerry and ben to be asserted in module 
ice_cream.   If Goal is of the form M:H, then H is asserted in module M.  
If Goal is of the form (H1, ...Hk), then each of H1, ..., Hk is
asserted in module user. Thus,  
    alspro -A '(jerry, ben)'
would cause the two facts facts jerry and ben to be asserted in module user.  
Otherwise, Goal is asserted in module user.  

Note that occurrences of the -A (or -a) switch must occur to the left
of any occurrence of the -p switch. (This switch was designed for use in
makefiles.)
```
```
-heap The option -heap followed immediately by space and a number w sets
the size of the ALS Prolog heap to w *1024, where w is the number of Kilobytes 
to allocate, if w is large.  For small w, the size of the heap is set to
the base heap size incremented by w *1024.  Heap overflow will cause exit 
to the operating system.  Cf. statistics/0.
```
```
-stack The option -stack followed immediately by a space and a number w sets 
the size of the ALS Prolog stack to w *1024, where w is the number of Kilobytes 
to allocate. Stack overflow will cause exit to the operating system.
Cf. statistics/0.

These two options (-heap, -stack) were formerly only controlled by the use of 
an environment variable, ALS_OPTIONS. Now, either or both the command-line 
and environment variable method can be used. Use of one of the command-line 
options overrides use of the corresponding option with the environment variable.
The ALS_OPTIONS environment variable is used as follows. If w1 and w2 are
similar to the value w described above for -h and -s, then:
Under Bourne shell, Korn shell, and Bash:

    ALS_OPTIONS=stack_size:w1,heap_size:w2
    export ALS_OPTIONS

Under csh:

    setenv ALS_OPTIONS stack_size:w1,heap_size:w2

Under MS Windows:

    ALS_OPTIONS=stack_size:w1,heap_size:w2

[Under MS Windows 95, such a line is placed in the AUTOEXEC.BAT file. 
Under Windows NT, one uses the Environment section of the System 
Properties control panel.]
```
```
For the following five switches, note that consulting <path><file>.pro 
suppresses creation of <file>.obp, while consulting <path><file> without 
'.pro' requests creation of <file>.obp.
```
```
-obp The option -obp causes the *.obp files which are generated for 
consulted files to be created in the current working directory
of the running image when the files are consulted.
```
```
-gic The option -gic (Generated In Current”) causes the *.obp files which are
generated for consulted files to be created in the current working directory
of the running image when the files are consulted.
```
```
-gis The option -gis (“Generated In Source”) causes the *.obp files which are 
created for consulted files to be created in the same directory as the source 
file from which they were generated.
```
```
-giac The option -giac ("Generated In Architecture sub-directory of 
Current directory") causes the *.obp files which are created for consulted
files to be stored in a subdirectory of the current working directory of the
running image when the files are consulted; the subdirectory corresponds to
the architecture of the running ALS Prolog image. Thus, for example, *.obp
files generated by a Solaris2.4 image will be stored in a subdirectory named
solaris2.4, etc.
```
```
-gias The option -gias ("Generated In Architecture sub-directory of 
Sources directory") causes the *.obp files which are created for consulted
files to be stored in a subdirectory of the directory containing the source
*.pro files when the files are consulted; the subdirectory corresponds to the
architecture of the running ALS Prolog image. Thus, for example, *.obp
files generated by a Solaris2.4 image will be stored in a subdirectory named
solaris2.4, etc.
```
```
-no_obp The option -no_obp causes alspro to suppress all generation of 
*.obp files.
```
```
-no_dot_alspro The -no_dot_alspro option causes alspro to avoid loading 
.alspro or alspro.pro.
```
```
-p -P The option -p is used by ALS Prolog to distinguish between command line
switches intended for the system and those switches intended for an application
(whether invoked with the -g command line switch or from the Prolog shell). 
The -p divides the command line into two portions: All switches to 
the left of the -p are interpreted as being for the ALS Prolog system,
while all switches to the right of the -p are interpreted as being intended for
a Prolog application. To make the latter available to Prolog applications,
when ALS Prolog is initialized, a list SWITCHES of atoms and UIAs representing
the items to the right of the -p is created, and a fact
command_line(SWITCHES) is asserted in module builtins. For example, the command
line

    alspro -g my_appl -b applfile -p -k fast -s initstate foofile

would result in the following fact being asserted in module builtins:

    command_line(['-k',fast,'-s',initstate,foofile]).

This assertion is always made, even when -p is not used, in which case
the argument of command_line/1 is the empty list. It is important to
note that command_line/1 is not exported from module builtins, so that
accesses to it from other modules must be prefixed with 'builtins:' as in

    ...,builtins:command_line(Cmds),...

The option -P is just like option -p, except that the name of the running 
image (alspro, alsdev, or possibly an application image built from them)
is pushed onto the front of the list returned by command_line/1.  So if 
my_appl_img is the name of an image built from alspro, then

    my_appl_img -heap 100 -q -no_dot_alspro -P -icecream -special_deal

would result in the following fact being asserted in module builtins:

    command_line([my_appl_img, '-icecream', '-special_deal']).

```
```
-nwd The -nwd option instructs the debugger to only use the system console for
display (Only meaningful in alsdev).
```
```
-shell <shell> This option causes alspro to use the non-default <shell> after 
processing the command line; currently, the only known other <shell> is alsdev.
```
```
-h This option causes alspro to display the help info on the console.
```