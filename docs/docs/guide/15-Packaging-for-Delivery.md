---
---

# 15 Packaging for Delivery
{:.no_toc}

* TOC
{:toc}

A typical complex ALS Prolog application involves the following elements:
* the ALS Prolog system
* various ALS Prolog source files
* various foreign C language source files

During development, the object versions of the foreign C language files may be dynamically loaded into a running ALS Prolog image (in the versions of ALS Prolog
which support this), or may be statically linked with the ALS Prolog run-time library to create an extended ALS Prolog image. The total application under development is started by invoking either the basic ALS Prolog image or the extended
image, loading the foreign C language object files if necessary, and dynamically
consulting the ALS Prolog files. However, for delivery of application programs to
users, it is desirable to be able to package all these elements of the program together
in one single executable file. ALS Prolog provides packaging tools for achieving
this goal. 
The packaging tools are very straight-forward to use. Simply proceed through the
following steps:

1.  Start the image (either the basic ALS Prolog image, or an extended image);  
2.  Load the Prolog files constituting the application.  
3.  Invoke save_image/2.

Apart from the details necessary to flesh out step 3, this is all there is to it! The information necessary for step 3 is the following:

A.  The name of the file in which the executable image is to be stored (e.g.,
my_app, or your_app.exe, etc.).  
B.  The name of a 0-ary Prolog predicate which is the entry point to the application (e.g., start_my_app/0).  
C.  The list of names of library files (if any) to be included in the application.

For example, suppose that the application is to be stored in the file my_app, that
its entry point is the 0-ary predicate start_my_app/0, and that the list of library
files which the application uses is
	[cmdline, listutl1, listutl2, misc_db, misc_io, objs_run, strings]
Then, for step 3, simply issue the following goal:
```
?- save_image(my_app,
         [start_goal( start_my_app ),
          select_lib([cmdline, listutl1, listutl2,
                      misc_db, misc_io, objs_run, strings] ) ] ).
```
You will see a number of cryptic messages, and eventually, the goal will succeed.
If you exit the running prolog image, and perform a listing of your working directory, you will find a file my_app. Running this file is roughly equivalent to (i.e.,
will have the same effect as) the following ALS Prolog command-line:
    alspro <my_app component files> -g start_my_app
or, if you are using an extended image,
    my_ex_alspro <my_app component files> -g start_my_app
However, none of the component files need be consulted nor do the library files
have to be loaded: they are all pre-packaged into the my_app image. This image
is suitable for distribution (assuming your program my_app is ready for its debut
to the outside world).  

To explain how this works, let's assume that you are using a statically linked extended image my_ex_alspro as your starting point. Then, from start to finish,
here is what happens.  

1.  You issue the command to run my_ex_alspro , and it is loaded and running.  
2.  You issue a consult to load the files making up your application my_app.  
3.  Your submit the goal
```
?- save_image( my_app,
         [start_goal( start_my_app ),
          select_lib([cmdline, listutl1, listutl2,
                      misc_db, misc_io, objs_run, strings] ) ] ).
```
4.  ALS Prolog loads the library files  
    [cmdline, listutl1, listutl2, misc_db, misc_io, objs_run, strings]
5.  ALS Prolog changes the usual Prolog shell startup to run your goal
start_my_app instead. For the curious, the definition of the 0-ary predicate '$start'/0 at the end of the builtins file builtins.pro is retracted, and
the following clause is asserted in module builtins:  
    '$start' :- start_my_app.
6.  The entire (Prolog) code space is copied out in appropriate format to a temporary file, call it TF.  
7.  The file my_app is opened, and the original image my_ex_alspro is copied into the file my_app, which is left open.  
8.  The entire temporary file TF is copied onto the end of the file my_app.  
9.  A fixup to a certain global variable (call it G) is made and my_app is closed.

The 'copying' which is carried out is really writing the various code and data elements in the executable object file format appropriate to the given machine and operating system (e.g., a.out, coff, elf, etc.) So the final file my_app is really an executable file whose initial segment is the original image my_ex_alspro, followed by some 'extra stuff'. Moreover, the entry point for my_app is the original
entry point for my_ex_alspro, which is just a version of the ALS Prolog image.

When any such image (be it plain ALS Prolog or an extended image) starts up, at a
point very early in its initialization, it looks at the global variable G. In the plain
ALS Prolog image, or one which has simply been statically linked with some additional C code, G = 0. But in a 'packaged application' such as my_app, G contains
a number effectively telling the system where the end of the image
my_ex_alspro lies, and where the 'extra stuff', the packaged Prolog code, starts.
The system uses this to appropriately allocate memory, and then to load the packaged ALS Prolog code from the application, from the builtins, and from the loaded
library files, into the approprate code area. (This is a block move, so it happens very
quickly. The difference in the startup times for alspro_b and alspro reflects the difference between loading the builtins *.obp files from disk, and this simple block
move of the builtins code. Why? Because alspro is just a packaged version of
alspro_b, packaging the builtins.)

Note that the 3-step process of building the application can be combined into a single-step operating system shell command-line action:
```
my_ex_alspro <my_app component files> \
    -g save_image( my_app, [start_goal( start_my_app ), \
    select_lib([cmdline, listutl1, listutl2, \
    misc_db, misc_io, objs_run, strings] ) ] ).
```
Such approaches are especially suitable for use in makefiles. (Note the continuation
characters '\' at the end of each line except the final one.)

Like the ALS Prolog environments themselves, some packaged applications will
want to accept command line arguments. Two predicates can be used in this regard.
To obtain the complete original command line, including the name of the program
being run, use pbi__get_command_line/1. For example, if the packaged application is named foo, then issuing the following operating system command line,
    foo zipper -f zap
would cause any call
    pbi__get_command_line(X)
to yield the value
    X = [foo,zipper, '-f', zap].
If you want the packaged version (“foo”) of the application to co-ordinate with the
“unpackaged version” developed under the ALS environment, you can use
setup_cmd_line/0. If the packaged application “foo” starts with
start_my_app/0, simply ensure that start_my_app/0 makes a call on
setup_cmd_line/0 any time before any call is made on command_line/1.
