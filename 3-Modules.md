ALS Prolog provides a module system to facilitate the creation and maintenance of
large programs. The main purpose of the module system is to partition procedures
into separate groups to avoid naming conflicts between those groups. The module
system provides controlled access to procedures within those groups.
The ALS module system only partitions procedures, not constants. This means that
the procedure foo/2 may have different meanings in different modules, but that
the constant bar is the same in every module.

##3.1 Declaring a Module

New modules are created when the compiler sees a module declaration in a source
file for the first time during a consult or reconsult. Every module has a name which must be a
non-numeric constant. Here are a few valid module declarations:
````
module dingbat.
module parser.
module compiler.
````
Following a module declaration, all clauses will be asserted into that module using
assertz until the end of the module or until another module declaration is encountered. In addition, any commands that appear within the scope of the module will be executed from inside that module. The end of a module is signified by an endmod. The following example defines the predicate test/0 in two different modules. The definitions don’t conflict with each other because they appear in different modules.
````
module mod1.
test :- write(’Module #1’), nl.
endmod.
module mod2.
test :- write(’Module #2’), nl.
endmod.
````
Clauses which are not contained inside an explicit module declaration are added to
the default module user.

If a module declaration is encountered for a module that already exists, the clauses
appearing within that declaration are simply added to the existing contents of that module, following the clauses which have already been inserted in that module. In this way, the code for a single module can be spread across multiple files ---- as long as each file has the appropriate module declaration and ends with a corresponding end mod.

The end of a file does not signal the end of the module as shown in the following
conversation with the Prolog shell:
````
?- [user].
Consulting user ...
module hello.
a.
b.
c.
user consulted
yes.
?- [user].
Consulting user ...
module hello.
d.
endmod.
user consulted
yes.
?- listing(hello:_).
% hello:a/0
a.
% hello:b/0
b.
% hello:c/0
c.
% hello:d/0
d.
````
If module hello had been closed by the EOF of the user file, then the d/0 fact would have appeared in the user module instead of the hello module. Consequently, it is important to always terminate a module with endmod. That is, module and end mod should always be used in matched pairs.

